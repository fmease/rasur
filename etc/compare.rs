#!/usr/bin/env -S cargo -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
clap = "4.5.58"
painter = { path = "lib/painter" }
---

use painter::{AnsiColor, Effects, Painter};
use std::{
    io::{self, Write as _},
    process::{Command, ExitCode, ExitStatus},
};

// FIXME: replace unwraps with proper error handling

fn main() -> ExitCode {
    let opts = interface::opts();

    let mut stdout = Painter::new(io::stdout(), io::BufWriter::new);

    banner(&mut stdout, "RUSTC", '-').unwrap();
    let rustc_status = rustc(&opts);

    banner(&mut stdout, "RASUR", '.').unwrap();
    let rasur_status = rasur(&opts);

    if rustc_status == rasur_status {
        stdout.with(AnsiColor::Green, |stdout| banner(stdout, "MATCH!", '.')).unwrap();
        ExitCode::SUCCESS
    } else {
        stdout.with(AnsiColor::Red, |stdout| banner(stdout, "MISMATCH!", '.')).unwrap();
        ExitCode::FAILURE
    }
}

fn banner(
    stdout: &mut Painter<io::BufWriter<io::Stdout>>,
    title: &str,
    symbol: char,
) -> io::Result<()> {
    let mut buffer = [0; 4];
    let symbol = symbol.encode_utf8(&mut buffer);

    stdout.set(Effects::INVERT)?;

    for _ in 0..2 {
        write!(stdout, "{symbol}")?;
    }

    write!(stdout, " {title} ")?;

    for _ in 0..70usize.saturating_sub(title.len()) {
        write!(stdout, "{symbol}")?;
    }

    writeln!(stdout)?;
    stdout.unset()?;
    stdout.flush()
}

fn rustc(opts: &interface::Opts) -> ExitStatus {
    let mut cmd = Command::new("rustc");
    cmd.arg(format!("+{}", opts.toolchain));

    match &opts.source {
        interface::Source::String(_) => {
            cmd.stdin(std::process::Stdio::piped());
            cmd.arg("-")
        }
        interface::Source::Path(path) => cmd.arg(path),
    };

    if !opts.alt {
        cmd.arg("-Zparse-crate-root-only");
    } else {
        cmd.arg("-Zcrate-attr=cfg(false)");
        cmd.arg("--crate-type=lib");
    }
    if opts.int {
        cmd.arg("-Zinternal-testing-features");
    }
    if let Some(edition) = &opts.edition {
        cmd.args(["--edition", edition]);
    }
    if opts.terse {
        cmd.arg("--error-format=short");
    }
    if opts.fmt {
        cmd.arg("-Zunpretty=normal");
    }
    if opts.ast {
        cmd.arg("-Zunpretty=ast-tree");
    }

    let mut child = cmd.spawn().unwrap();

    if let interface::Source::String(source) = &opts.source {
        let mut stdin = child.stdin.take().unwrap();
        write!(stdin, "{source}").unwrap();
        stdin.flush().unwrap();
    }

    child.wait().unwrap()
}

fn rasur(opts: &interface::Opts) -> ExitStatus {
    let mut cmd = Command::new("./rasur");

    match &opts.source {
        interface::Source::String(source) => cmd.args(["--source", source]),
        interface::Source::Path(path) => cmd.arg(path),
    };

    if opts.alt {
        cmd.arg("--gatekeep");
    }
    if opts.int {
        cmd.arg("--unlock-super-internal-features");
    }
    if let Some(edition) = &opts.edition {
        cmd.args(["--edition", edition]);
    }
    if opts.terse {
        cmd.arg("--short");
    }
    if opts.fmt {
        cmd.arg("--emit=fmt");
    }
    if opts.ast {
        cmd.arg("--emit=ast");
    }

    cmd.status().unwrap()
}

mod interface {
    use clap::{Arg, ArgAction, Command};
    use std::path::PathBuf;

    pub(super) fn opts() -> Opts {
        let mut matches = Command::new(env!("CARGO_PKG_NAME"))
            .arg(
                Arg::new(id::PATH)
                    .value_parser(clap::builder::ValueParser::path_buf())
                    .required_unless_present(id::SOURCE)
                    .help("Path to the source file"),
            )
            .arg(
                Arg::new(id::SOURCE)
                    .short(':')
                    .long("source")
                    .conflicts_with(id::PATH)
                    .help("Provide the source code"),
            )
            .arg(
                Arg::new(id::EDITION)
                    .short('e')
                    .long("edition")
                    .help("Set the edition of the source file"),
            )
            .arg(
                Arg::new(id::TOOLCHAIN)
                    .short('T')
                    .long("toolchain")
                    .default_value("nightly")
                    .help("Override the rustup toolchain"),
            )
            // FIXME: Add help to all of these.
            .arg(Arg::new(id::FMT).long("fmt").action(ArgAction::SetTrue))
            .arg(Arg::new(id::AST).long("ast").action(ArgAction::SetTrue).conflicts_with(id::FMT))
            // That's because `-Zunpretty=normal` halts before AST pretty just like `-Zparse-crate-root-only`.
            .arg(Arg::new(id::ALT).long("alt").action(ArgAction::SetTrue).conflicts_with(id::FMT))
            .arg(Arg::new(id::INT).long("int").action(ArgAction::SetTrue))
            .arg(Arg::new(id::TERSE).long("terse").action(ArgAction::SetTrue))
            .get_matches();

        let source = matches
            .remove_one(id::PATH)
            .map(Source::Path)
            .xor(matches.remove_one(id::SOURCE).map(Source::String))
            .unwrap();

        Opts {
            toolchain: matches.remove_one(id::TOOLCHAIN).unwrap(),
            source,
            edition: matches.remove_one(id::EDITION),
            fmt: matches.remove_one(id::FMT).unwrap_or_default(),
            ast: matches.remove_one(id::AST).unwrap_or_default(),
            alt: matches.remove_one(id::ALT).unwrap_or_default(),
            int: matches.remove_one(id::INT).unwrap_or_default(),
            terse: matches.remove_one(id::TERSE).unwrap_or_default(),
        }
    }

    pub(super) struct Opts {
        pub(super) toolchain: String,
        pub(super) source: Source,
        pub(super) edition: Option<String>,
        pub(super) fmt: bool,
        pub(super) ast: bool,
        pub(super) alt: bool,
        pub(super) int: bool,
        pub(super) terse: bool,
    }

    pub(super) enum Source {
        String(String),
        Path(PathBuf),
    }

    macro_rules! ids {
        ($($name:ident),+ $(,)?) => {
            mod id {
                $( pub(super) const $name: &str = stringify!($name); )+
            }
        };
    }

    ids! {
        ALT,
        AST,
        EDITION,
        FMT,
        INT,
        PATH,
        SOURCE,
        TERSE,
        TOOLCHAIN,
    }
}
