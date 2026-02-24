#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
clap = "4.5.58"
painter = { path = "lib/painter" }
walkdir = "2.5.0"
---
#![feature(exit_status_error)]

#[cfg(not(unix))]
compile_error!("non-Unix platforms not supported");

use painter::{AnsiColor, Effects, Painter};
use std::{
    env, fs,
    io::{self, BufRead as _, Write as _},
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
    let mut opts = interface::opts();

    let mut p = Painter::new(io::stdout(), io::BufWriter::new);

    if let Some(path) = &opts.save {
        write!(p, "Do you really want to save test failures as `{}`? [y/N]: ", path.display())
            .unwrap();
        p.flush().unwrap();

        let mut answer = String::new();
        io::stdin().read_line(&mut answer).unwrap();
        let answer = answer.trim();

        if !answer.eq_ignore_ascii_case("y") && !answer.eq_ignore_ascii_case("yes") {
            eprintln!("Operation denied; aborting.");
            return Err(());
        } else {
            writeln!(p).unwrap();
        }
    }

    if let Some(path) = &opts.load {
        opts.paths.extend(
            io::BufReader::new(fs::File::open(path).unwrap())
                .lines()
                .map(|line| PathBuf::from(line.unwrap())),
        );
    }

    let entries = opts.paths.iter().flat_map(walkdir::WalkDir::new);
    let entries = Arc::new(Mutex::new(entries));

    let rasur_path = &rasur_path();
    let rustc_path = &rustc_path(&opts);
    let time = Instant::now();

    let mut stats = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..opts.jobs.get())
            .map(|_| {
                let entries = entries.clone();
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

    // FIXME: Collect into Vec of `.display()`'ed `Path`s that can be used by save  & render.
    stats.failures.sort_unstable_by(|(path0, ..), (path1, ..)| path0.cmp(path1));

    stats.render(duration, &opts, &mut p).unwrap();

    if let Some(path) = opts.save
        && !stats.failures.is_empty()
    {
        let mut file = io::BufWriter::new(fs::File::create(path).unwrap());

        for (path, ..) in &stats.failures {
            writeln!(file, "{}", path.display()).unwrap();
        }
    }

    if let Measure::CfgFalse = opts.test.measure
        && stats.total > 0
    {
        _ = fs::remove_dir_all(TMP_DIR_PATH);
    }

    if stats.failures.is_empty() { Ok(()) } else { Err(()) }
}

fn rasur_path() -> PathBuf {
    Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/rasur-cli"))
        .with_extension(env::consts::EXE_EXTENSION)
}

fn rustc_path(opts: &interface::Opts) -> PathBuf {
    let mut output = Command::new("rustup")
        .arg(format!("+{}", opts.toolchain))
        .args(["which", "rustc"])
        .output()
        .unwrap();
    output.status.exit_ok().unwrap();
    output.stdout.pop(); // \n
    PathBuf::from(String::from_utf8(output.stdout).unwrap())
}

#[derive(Default)]
struct Stats {
    failures: Vec<(PathBuf, ExitStatus, ExitStatus)>,
    total: usize,
}

impl Stats {
    fn adjoin(mut self, mut other: Self) -> Self {
        self.failures.append(&mut other.failures);
        self.total += other.total;
        self
    }

    fn render(
        &self,
        duration: std::time::Duration,
        opts: &interface::Opts,
        p: &mut Painter<impl io::Write>,
    ) -> io::Result<()> {
        let Self { ref failures, total } = *self;

        if !failures.is_empty() {
            const BAR_WIDTH: usize = 80;

            writeln!(p, "{:─^BAR_WIDTH$}", " FAILURES ")?;
            p.set(AnsiColor::Red)?;
            for &(ref path, rasur_exit_status, rustc_exit_status) in failures {
                if opts.verbose {
                    // FIXME: Handle non-codes (signals, ..)
                    let write = |p: &mut Painter<_>, status: ExitStatus| {
                        if let Some(code) = status.code() {
                            write!(p, "{code}")
                        } else {
                            write!(p, "?")
                        }
                    };

                    write(p, rasur_exit_status)?;
                    write!(p, "v")?;
                    write(p, rustc_exit_status)?;
                    write!(p, "  ")?;
                }

                writeln!(p, "{}", path.display())?;
            }
            p.unset()?;
            writeln!(p, "{:─^BAR_WIDTH$}", "")?;
            writeln!(p)?;
        }

        const PADDING: usize = 4;

        let (color, tag) = match (failures.as_slice(), total) {
            (_, 0) => (AnsiColor::Yellow, "NO TESTS WERE RUN!"),
            ([], _) => (AnsiColor::Green, "ALL TESTS PASSED!"),
            ([_, ..], _) => (AnsiColor::Red, "SOME TESTS FAILED!"),
        };
        p.with(color.on_default().effects(Effects::BOLD), |p| {
            write!(p, "{tag}{}", " ".repeat(PADDING))
        })?;
        let indent = " ".repeat(tag.len() + PADDING);

        let failed = failures.len();
        let passed = total - failed;
        let percentage = if total == 0 { 1. } else { (passed as f32 / total as f32) * 1. } * 100.;

        let column = |p: &mut Painter<_>, count, tag, color| {
            if count > 0 {
                p.set(color)?;
                p.set(Effects::BOLD)?;
            }
            write!(p, "{count}")?;
            if count > 0 {
                p.unset()?;
            }
            write!(p, " {tag}")?;
            if count > 0 {
                p.unset()?;
            }
            io::Result::Ok(())
        };

        const SEP: &str = " │ ";

        column(p, passed, "passed", AnsiColor::Green)?;
        write!(p, " ({percentage:.2}%)")?;
        write!(p, "{SEP}")?;
        column(p, failed, "failed", AnsiColor::Red)?;
        write!(p, "{SEP}")?;
        write!(p, "{total} in total")?;
        writeln!(p)?;

        write!(p, "{indent}")?;
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{duration:?}"))?;

        writeln!(p)
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
        stats.total += 1;

        if let Err((rasur_exit_status, rustc_exit_status)) = result {
            stats.failures.push((entry.into_path(), rasur_exit_status, rustc_exit_status));
        }
    }
}

fn compare(
    path: &Path,
    rasur_path: &Path,
    rustc_path: &Path,
    opts: &TestOpts,
) -> Result<(), (ExitStatus, ExitStatus)> {
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
        return Err((rasur_exit_status, rustc_exit_status));
    }

    Ok(())
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
    const DEFAULT_STORAGE_PATH: &str = "FAILURES.txt";

    pub(super) fn opts() -> Opts {
        let jobs = Arg::new(id::JOBS)
            .short('j')
            .long("jobs")
            .value_parser(P!(NonZeroUsize))
            .help("Set the number of threads used for running tests in parallel");
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
            .arg(Arg::new(id::INVERT).short('I').long("invert").action(ArgAction::SetTrue).help("Invert the test expectation (so unequal exit statuses means success)"))
            .arg(jobs)
            .arg(
                Arg::new(id::CHUNK_SIZE)
                    .short('c')
                    .long("chunk")
                    .value_parser(P!(NonZeroUsize))
                    .default_value(DEFAULT_CHUNK_SIZE).help("Set the amount of tests a single thread should run sequentially each 'step'"),
            )
            .arg(
                Arg::new(id::SAVE)
                    .long("save")
                    .value_name("PATH")
                    .require_equals(true)
                    .num_args(..=1)
                    .default_missing_value(DEFAULT_STORAGE_PATH)
                    .value_parser(P!(PathBuf))
                    .help(format!(
                        "Save the paths to failing tests in a file [fallback: {DEFAULT_STORAGE_PATH}]"
                    )),
            )
            .arg(
                Arg::new(id::LOAD)
                    .long("load")
                    .value_name("PATH")
                    .require_equals(true)
                    .num_args(..=1)
                    .default_missing_value(DEFAULT_STORAGE_PATH)
                    .value_parser(P!(PathBuf))
                    .conflicts_with(id::PATHS)
                    .help(format!("Load the paths from a file [fallback: {DEFAULT_STORAGE_PATH}]")),
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
            save: matches.remove_one(id::SAVE),
            load: matches.remove_one(id::LOAD),
            verbose: matches.remove_one(id::VERBOSE).unwrap_or_default(),
        }
    }

    pub(super) struct Opts {
        pub(super) toolchain: String,
        pub(super) paths: Vec<PathBuf>,
        pub(super) jobs: NonZeroUsize,
        pub(super) chunk_size: NonZeroUsize,
        pub(super) test: TestOpts,
        pub(super) save: Option<PathBuf>,
        pub(super) load: Option<PathBuf>,
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
        LOAD,
        MEASURE,
        PATHS,
        SAVE,
        TOOLCHAIN,
        VERBOSE,
    }
}
