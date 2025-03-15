#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
walkdir = "2.5.0"
---
#![feature(let_chains)]

use std::{
    env,
    path::Path,
    process::{Command, ExitCode, ExitStatus, Stdio},
};

fn main() -> ExitCode {
    let mut args = env::args_os().skip(1);
    let path = args.next().expect("missing path");

    let rasur_path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/rasur"))
        .with_extension(env::consts::EXE_EXTENSION);

    let mut total = 0usize;
    let mut failures = 0usize;

    // FIXME: Parallelize
    // FIXME: Don't eagerly report to stderr (slows things down)
    // FIXME: Check all editions 2015..2024 per file!
    // FIXME: Report most common exit statuses per engine

    for entry in walkdir::WalkDir::new(path) {
        let entry = entry.expect("failed to read dir entry");

        if entry.file_type().is_file()
            && let path = entry.path()
            && let Some(ext) = path.extension()
            && ext == "rs"
        {
            total += 1;
            if let Err((rasur_status, rustc_status)) = compare(path, &rasur_path) {
                failures += 1;
                let rasur_status = format!("{rasur_status}");
                let rasur_status =
                    rasur_status.strip_prefix("exit status: ").unwrap_or(&rasur_status);

                let rustc_status = format!("{rustc_status}");
                let rustc_status =
                    rustc_status.strip_prefix("exit status: ").unwrap_or(&rustc_status);

                println!("{}: rasur({rasur_status}) != rustc({rustc_status})", path.display());
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

    println!("[{status}] {successes}/{total} ({percentage:.2}%)");

    if failures > 0 {
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

fn compare(path: &Path, rasur_path: &Path) -> Result<(), (ExitStatus, ExitStatus)> {
    let rasur_status = Command::new(rasur_path)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .arg(&path)
        .status()
        .expect("failed to execute `rasur`");

    let rustc_status = Command::new("rustc")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .arg(&path)
        .arg("-Zparse-crate-root-only")
        .status()
        .expect("failed to execute `rustc`");

    if rasur_status != rustc_status {
        return Err((rasur_status, rustc_status));
    }

    Ok(())
}
