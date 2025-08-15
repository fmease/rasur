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
    path::{Path, PathBuf},
    process::{Command, ExitCode, ExitStatus, Stdio},
    time::Instant,
};

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let mut args = env::args_os().skip(1);
    let mut path = None;
    let mut opts = Opts::default();

    while let Some(arg) = args.next() {
        if let Some(opt) = arg.as_encoded_bytes().strip_prefix(b"--") {
            match opt {
                b"skip-true-ill" => opts.skip_true_ill = true,
                b"invert" => opts.invert = true,
                b"edition" => {
                    // FIXME: Deny passing that args mult times?
                    opts.edition = Some(args.next().ok_or_else(|| {
                        eprintln!("error: missing required argument `<EDITION>`")
                    })?);
                }
                _ => {
                    eprintln!("error: unknown flag `{}`", arg.display());
                    return Err(());
                }
            }
        } else {
            if path.is_some() {
                eprintln!("error: unexpected argument `{}`", arg.display());
                return Err(());
            }
            path = Some(PathBuf::from(arg));
        }
    }

    let path = path.ok_or_else(|| eprintln!("error: missing required argument `<PATH>`"))?;

    let rasur_path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/rasur"))
        .with_extension(env::consts::EXE_EXTENSION);
    let rustc_path = {
        let mut output = Command::new("rustup").args(["which", "rustc"]).output().unwrap();
        output.status.exit_ok().unwrap();
        output.stdout.pop(); // \n
        PathBuf::from(String::from_utf8(output.stdout).unwrap())
    };

    let mut total = 0usize;
    let mut failures = 0usize;

    // FIXME: Parallelize
    // FIXME: Don't eagerly report to stderr (slows things down)
    // FIXME: Check all editions 2015..2024 per file!

    let time = Instant::now();

    for entry in walkdir::WalkDir::new(path) {
        let entry = entry.expect("failed to read dir entry");

        if entry.file_type().is_file()
            && let path = entry.path()
            && let Some(ext) = path.extension()
            && ext == "rs"
        {
            let result = compare(path, &rasur_path, &rustc_path, &opts);
            if let Some(result) = result {
                total += 1;

                if let Err((rasur_exit_status, rustc_exit_status)) = result {
                    failures += 1;

                    // FIXME: Log non-exit codes (signals, …)
                    let Some(rasur_exit_code) = rasur_exit_status.code() else { continue };
                    let Some(rustc_exit_code) = rustc_exit_status.code() else { continue };

                    let op = if opts.invert { "==" } else { "/=" };

                    println!(
                        "{}: rasur({rasur_exit_code}) {op} rustc({rustc_exit_code})",
                        path.display()
                    );
                }
            }
        }
    }

    let status = if failures > 0 { "FAIL" } else { "OK" };
    let successes = total - failures;
    let percentage = if total == 0 { 1. } else { (successes as f32 / total as f32) * 100. };

    if failures > 0 {
        // Visual separator
        println!();
    }

    println!("[{status}] {successes}/{total} ({percentage:.2}%) | {:?}", time.elapsed());

    if failures > 0 {
        return Err(());
    }

    Ok(())
}

#[derive(Default)]
struct Opts {
    skip_true_ill: bool,
    invert: bool,
    edition: Option<OsString>,
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
