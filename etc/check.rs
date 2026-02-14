#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
walkdir = "2.5.0"
---
#![feature(exit_status_error)]

#[cfg(not(unix))]
compile_error!("non-Unix platforms not supported");

use std::{
    env,
    ffi::OsString,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    process::{Command, ExitCode, ExitStatus, Stdio},
    sync::{Arc, Mutex},
    time::Instant,
};

const FALLBACK_JOBS: NonZeroUsize = NonZeroUsize::new(1).unwrap();
const DEFAULT_CHUNK_SIZE: NonZeroUsize = NonZeroUsize::new(10).unwrap(); // idk

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let mut args = env::args_os().skip(1);

    let mut opts = Opts::default();
    let mut paths = Vec::new();
    let mut jobs = None::<NonZeroUsize>;
    let mut chunk_size = None::<NonZeroUsize>;

    let mut parse_opts = true;

    while let Some(arg) = args.next() {
        if parse_opts && let Some(opt) = arg.as_encoded_bytes().strip_prefix(b"-") {
            match opt {
                b"-" => parse_opts = false,
                b"c" | b"-chunk" => {
                    chunk_size = Some(
                        args.next()
                            .ok_or_else(|| eprintln!("error: missing required argument `SIZE`"))?
                            .to_str()
                            .and_then(|chunk| chunk.parse().ok())
                            .ok_or_else(|| eprintln!("error: argument `SIZE` is invalid"))?,
                    )
                }
                b"e" | b"-edition" => {
                    opts.edition =
                        Some(args.next().ok_or_else(|| {
                            eprintln!("error: missing required argument `EDITION`")
                        })?);
                }
                b"I" | b"-invert" => opts.invert = true,
                b"j" | b"-jobs" => {
                    jobs = Some(
                        args.next()
                            .ok_or_else(|| {
                                eprintln!("error: missing required argument `JOBS`");
                            })?
                            .to_str()
                            .and_then(|jobs| jobs.parse().ok())
                            .ok_or_else(|| {
                                eprintln!("error: argument `JOBS` is invalid");
                            })?,
                    );
                }
                b"-skip-true-ill" => opts.skip_true_ill = true,
                b"v" | b"-verbose" => opts.verbose = true,
                _ => {
                    eprintln!("error: unknown flag `{}`", arg.display());
                    return Err(());
                }
            }
        } else {
            paths.push(PathBuf::from(arg));
        }
    }

    let rasur_path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/rasur"))
        .with_extension(env::consts::EXE_EXTENSION);
    let rustc_path = {
        let mut output =
            Command::new("rustup").args(["+nightly", "which", "rustc"]).output().unwrap();
        output.status.exit_ok().unwrap();
        output.stdout.pop(); // \n
        PathBuf::from(String::from_utf8(output.stdout).unwrap())
    };

    let jobs =
        jobs.unwrap_or_else(|| std::thread::available_parallelism().unwrap_or(FALLBACK_JOBS));
    let chunk_size = chunk_size.unwrap_or(DEFAULT_CHUNK_SIZE);

    let entries = paths.into_iter().flat_map(walkdir::WalkDir::new);
    let entries = Arc::new(Mutex::new(entries));

    let time = Instant::now();

    let stats = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..jobs.get())
            .map(|_| {
                let entries = entries.clone();
                let rasur_path = &rasur_path;
                let rustc_path = &rustc_path;
                let opts = &opts;

                scope.spawn(move || {
                    let mut local_entries = Vec::with_capacity(chunk_size.get());
                    let mut stats = Stats::default();

                    loop {
                        {
                            let mut entries = entries.lock().unwrap();

                            for _ in 0..chunk_size.get() {
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
                            check(entry, rasur_path, rustc_path, opts, &mut stats);
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

    if !stats.failed.is_empty() {
        return Err(());
    }

    Ok(())
}

#[derive(Default)]
struct Opts {
    invert: bool,
    skip_true_ill: bool,
    edition: Option<OsString>,
    verbose: bool,
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

    fn render(&self, opts: &Opts) {
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
    opts: &Opts,
    stats: &mut Stats,
) {
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
    opts: &Opts,
) -> Option<Result<(), (ExitStatus, ExitStatus)>> {
    let mut rustc_call = Command::new(rustc_path);
    rustc_call
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .arg(&path)
        .arg("-Zparse-crate-root-only");
    if let Some(edition) = &opts.edition {
        rustc_call.arg("--edition");
        rustc_call.arg(edition);
    }

    let rustc_exit_status = rustc_call.status().expect("failed to execute `rustc`");

    if opts.skip_true_ill && !rustc_exit_status.success() {
        return None;
    }

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
