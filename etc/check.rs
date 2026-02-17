#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
clap = "4.5.58"
walkdir = "2.5.0"
---
#![feature(exit_status_error)]

#[cfg(not(unix))]
compile_error!("non-Unix platforms not supported");

use std::{
    env,
    path::{Path, PathBuf},
    process::{Command, ExitCode, ExitStatus, Stdio},
    sync::{Arc, Mutex},
    time::Instant,
};

const TMP_DIR_PATH: &str = "/tmp/rasurck";

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let opts = interface::opts();

    let rasur_path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/rasur"))
        .with_extension(env::consts::EXE_EXTENSION);
    let rustc_path = {
        let mut output = Command::new("rustup")
            .arg(format!("+{}", opts.toolchain))
            .args(["which", "rustc"])
            .output()
            .unwrap();
        output.status.exit_ok().unwrap();
        output.stdout.pop(); // \n
        PathBuf::from(String::from_utf8(output.stdout).unwrap())
    };

    let entries = opts.paths.iter().flat_map(walkdir::WalkDir::new);
    let entries = Arc::new(Mutex::new(entries));

    let time = Instant::now();

    let stats = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..opts.jobs.get())
            .map(|_| {
                let entries = entries.clone();
                let rasur_path = &rasur_path;
                let rustc_path = &rustc_path;
                let test_opts = &opts.test;

                scope.spawn(move || {
                    let chunk_size = opts.chunk_size.get();

                    let mut local_entries = Vec::with_capacity(chunk_size);
                    let mut stats = Stats::default();

                    loop {
                        {
                            let mut entries = entries.lock().unwrap();

                            for _ in 0..chunk_size {
                                local_entries.push(match entries.next() {
                                    Some(entry) => entry,
                                    None => break,
                                });
                            }
                        }

                        if local_entries.is_empty() {
                            break;
                        }

                        for entry in local_entries.drain(..) {
                            check(entry, rasur_path, rustc_path, test_opts, &mut stats);
                        }
                    }

                    stats
                })
            })
            .collect();

        handles
            .into_iter()
            .fold(Stats::default(), |stats, handle| stats.adjoin(handle.join().unwrap()))
    });

    let duration = time.elapsed();

    stats.render(&opts);
    println!("    {duration:?}");

    if let Measure::CfgFalse = opts.test.measure
        && stats.total > 0
    {
        _ = std::fs::remove_dir_all(TMP_DIR_PATH);
    }

    if stats.failed.is_empty() { Ok(()) } else { Err(()) }
}

#[derive(Default)]
struct Stats {
    failed: Vec<(PathBuf, ExitStatus, ExitStatus)>,
    total: usize,
    ignored: usize,
}

impl Stats {
    fn adjoin(mut self, mut other: Self) -> Self {
        self.failed.append(&mut other.failed);
        self.total += other.total;
        self.ignored += other.ignored;
        self
    }

    fn render(&self, opts: &interface::Opts) {
        let Self { ref failed, total, ignored } = *self;

        if !failed.is_empty() {
            println!("=== FAILURES ===");
            for &(ref path, rasur_exit_status, rustc_exit_status) in failed {
                if opts.verbose {
                    // FIXME: Handle non-codes (signals, ..)
                    let print = |status: ExitStatus| {
                        if let Some(code) = status.code() {
                            print!("{code}");
                        } else {
                            print!("?");
                        }
                    };

                    print(rasur_exit_status);
                    print!("v");
                    print(rustc_exit_status);
                    print!("  ");
                }

                println!("{}", path.display());
            }
            println!("================");
            println!();
        }

        println!("{}", if failed.is_empty() { "ALL TESTS PASSED!" } else { "SOME TESTS FAILED!" });

        let failed = failed.len();
        let passed = total - failed;
        let percentage = if total == 0 { 1. } else { (passed as f32 / total as f32) * 100. };

        println!(
            "    {passed} passed ({percentage:.2}%) | {failed} failed | {ignored} ignored | {total} in total"
        );
    }
}

fn check(
    entry: Result<walkdir::DirEntry, walkdir::Error>,
    rasur_path: &Path,
    rustc_path: &Path,
    opts: &TestOpts,
    stats: &mut Stats,
) {
    // FIXME: Mark file as invalid instead!
    let entry = entry.expect("failed to read dir entry");

    if entry.file_type().is_file()
        && let path = entry.path()
        && let Some(ext) = path.extension()
        && ext == "rs"
    {
        let result = compare(path, rasur_path, rustc_path, opts);
        if let Some(result) = result {
            stats.total += 1;

            if let Err((rasur_exit_status, rustc_exit_status)) = result {
                stats.failed.push((entry.into_path(), rasur_exit_status, rustc_exit_status));
            }
        } else {
            stats.ignored += 1;
        }
    }
}

fn compare(
    path: &Path,
    rasur_path: &Path,
    rustc_path: &Path,
    opts: &TestOpts,
) -> Option<Result<(), (ExitStatus, ExitStatus)>> {
    let mut rustc_call = Command::new(rustc_path);
    rustc_call.stdout(Stdio::null()).stderr(Stdio::null()).arg(&path);
    match opts.measure {
        Measure::ParseOnly => rustc_call.arg("-Zparse-crate-root-only"),
        Measure::CfgFalse => rustc_call.args([
            "-Zcrate-attr=cfg(false)",
            "--crate-type=lib",
            "--emit=metadata",
            "--out-dir",
            TMP_DIR_PATH,
        ]),
    };
    if let Some(edition) = &opts.edition {
        rustc_call.arg("--edition");
        rustc_call.arg(edition);
    }

    let rustc_exit_status = rustc_call.status().expect("failed to execute `rustc`");

    let mut rasur_call = Command::new(rasur_path);
    rasur_call.stdout(Stdio::null()).stderr(Stdio::null()).arg(&path);
    if let Some(edition) = &opts.edition {
        rasur_call.arg("--edition");
        rasur_call.arg(edition);
    }
    let rasur_exit_status = rasur_call.status().expect("failed to execute `rasur`");

    if (rasur_exit_status == rustc_exit_status) == opts.invert {
        return Some(Err((rasur_exit_status, rustc_exit_status)));
    }

    Some(Ok(()))
}

struct TestOpts {
    edition: Option<std::ffi::OsString>,
    measure: Measure,
    invert: bool,
}

#[derive(Clone, Copy)]
enum Measure {
    ParseOnly,
    CfgFalse,
}

mod interface {
    use super::{Measure, TestOpts};
    use clap::{Arg, ArgAction, Command, value_parser as P};
    use std::{num::NonZeroUsize, path::PathBuf};

    const DEFAULT_CHUNK_SIZE: &str = "10";
    const DEFAULT_MEASURE: &str = "parse-only";

    pub(super) fn opts() -> Opts {
        let jobs = Arg::new(id::JOBS).short('j').long("jobs").value_parser(P!(NonZeroUsize));
        let jobs = match std::thread::available_parallelism() {
            // FIXME: unfortunately, clap requires the &str to live for the static lifetime.
            Ok(value) => jobs.default_value(&*value.to_string().leak()),
            Err(_) => jobs.required(true),
        };

        let mut matches = Command::new(env!("CARGO_PKG_NAME"))
            // FIXME: is specifying this value parser necessary? ig ig
            .arg(
                Arg::new(id::PATHS)
                    .action(ArgAction::Append)
                    .value_parser(P!(PathBuf))
                    .help("Paths to the source files"),
            )
            .arg(
                Arg::new(id::EDITION)
                    .short('e')
                    .long("edition")
                    .value_parser(P!(std::ffi::OsString))
                    .help("Set the edition of the source files"),
            )
            .arg(
                Arg::new(id::TOOLCHAIN)
                    .short('T')
                    .long("toolchain")
                    .default_value("nightly")
                    .help("Override the rustup toolchain"),
            )
            .arg(
                Arg::new(id::MEASURE)
                    .short('m')
                    .long("measure")
                    .value_parser(parse_measure)
                    .default_value(DEFAULT_MEASURE),
            )
            .arg(Arg::new(id::INVERT).short('I').long("invert").action(ArgAction::SetTrue))
            .arg(jobs)
            .arg(
                Arg::new(id::CHUNK_SIZE)
                    .short('c')
                    .long("chunk")
                    .value_parser(P!(NonZeroUsize))
                    .default_value(DEFAULT_CHUNK_SIZE),
            )
            .arg(Arg::new(id::VERBOSE).short('v').long("verbose").action(ArgAction::SetTrue))
            .get_matches();

        Opts {
            toolchain: matches.remove_one(id::TOOLCHAIN).unwrap(),
            paths: matches.remove_many(id::PATHS).map(Iterator::collect).unwrap_or_default(),
            jobs: matches.remove_one(id::JOBS).unwrap(),
            chunk_size: matches.remove_one(id::CHUNK_SIZE).unwrap(),
            test: TestOpts {
                edition: matches.remove_one(id::EDITION),
                measure: matches.remove_one(id::MEASURE).unwrap(),
                invert: matches.remove_one(id::INVERT).unwrap_or_default(),
            },
            verbose: matches.remove_one(id::VERBOSE).unwrap_or_default(),
        }
    }

    pub(super) struct Opts {
        pub(super) toolchain: String,
        pub(super) paths: Vec<PathBuf>,
        pub(super) jobs: NonZeroUsize,
        pub(super) chunk_size: NonZeroUsize,
        pub(super) test: TestOpts,
        pub(super) verbose: bool,
    }

    macro_rules! parse {
        ($( $key:literal => $value:expr ),+ $(,)?)  => {
            |source| Ok(match source {
                $( $key => $value, )+
                _ => return Err(format!("possible values: {}", [$(concat!("`", $key, "`")),+].join(", "))),
            })
        }
    }

    fn parse_measure(source: &str) -> Result<Measure, String> {
        parse!(
            "parse-only" => Measure::ParseOnly,
            "cfg-false" => Measure::CfgFalse,
        )(source)
    }

    macro_rules! ids {
        ($( $id:ident ),+ $(,)?) => {
            mod id {
                $( pub(super) const $id: &str = stringify!($id); )+
            }
        }
    }

    ids! {
        CHUNK_SIZE,
        EDITION,
        INVERT,
        JOBS,
        MEASURE,
        PATHS,
        TOOLCHAIN,
        VERBOSE,
    }
}
