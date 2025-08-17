// Features
#![feature(assert_matches)]
#![feature(decl_macro)]
#![feature(deref_patterns)]
#![feature(if_let_guard)]
#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(negative_impls)]
#![feature(super_let)]
// Lints
#![expect(incomplete_features, reason = "deref_patterns")]
#![deny(unused_must_use, rust_2018_idioms)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::items_after_statements)]
#![allow(clippy::match_bool)]
#![allow(clippy::option_option)]

use Default::default;
use edition::Edition;
use std::{path::PathBuf, process::ExitCode};

mod ast;
mod edition;
mod fmter;
mod lexer;
mod parser;
mod span;
mod token;

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let mut opts = Opts::default();

    let mut args = std::env::args_os().skip(1);
    while let Some(arg) = args.next() {
        if let Some(opt) = arg.as_encoded_bytes().strip_prefix(b"--") {
            match opt {
                b"tok" => opts.emit_tokens = true,
                b"ast" => opts.emit_ast = true,
                b"lex-only" => opts.lex_only = true,
                b"edition" => {
                    let edition = args.next().ok_or_else(|| {
                        eprintln!("error: missing argument to `--edition`");
                    })?;
                    if opts.edition.is_some() {
                        eprintln!("error: `--edition` can't be passed multiple times");
                        return Err(());
                    }
                    opts.edition = Some(match edition.as_encoded_bytes() {
                        b"2015" => Edition::Rust2015,
                        b"2018" => Edition::Rust2018,
                        b"2021" => Edition::Rust2021,
                        b"2024" => Edition::Rust2024,
                        b"future" => Edition::Future,
                        _ => {
                            eprintln!("error: invalid edition `{}`", edition.display());
                            return Err(());
                        }
                    });
                }
                b"fmt" => opts.fmt = true,
                // FIXME: reject unless --fmt set anytime
                b"skip-marker" => {
                    let skip_marker = args.next().ok_or_else(|| {
                        eprintln!("error: missing argument to `--skip-marker`");
                    })?;
                    if opts.skip_marker.is_some() {
                        eprintln!("error: `--skip-marker` can't be passed multiple times");
                        return Err(());
                    }
                    opts.skip_marker = Some(match skip_marker.as_encoded_bytes() {
                        b"none" => fmter::SkipMarker::None,
                        b"all" => fmter::SkipMarker::All,
                        b"rustfmt" => fmter::SkipMarker::Rustfmt,
                        b"rasur" => fmter::SkipMarker::Rasur,
                        _ => {
                            eprintln!("error: invalid skip marker `{}`", skip_marker.display());
                            return Err(());
                        }
                    });
                }
                _ => {
                    eprintln!("error: unknown flag `{}`", arg.display());
                    return Err(());
                }
            }
        } else {
            if opts.path.is_some() {
                eprintln!("error: unexpected argument `{}`", arg.display());
                return Err(());
            }
            opts.path = Some(PathBuf::from(arg));
        }
    }

    if !opts.fmt && opts.skip_marker.is_some() {
        eprintln!("`--skip-marker` requires `--fmt` to be set");
        return Err(());
    }

    let path = opts.path.ok_or_else(|| eprintln!("error: missing required path argument"))?;
    let source = std::fs::read_to_string(&path)
        .map_err(|error| eprintln!("error: failed to read `{}`: {error}", path.display()))?;

    let tokens = lexer::lex(&source, lexer::StripShebang::Yes);

    if opts.emit_tokens {
        let mut stderr = std::io::stderr().lock();

        for token in &tokens {
            use std::io::Write as _;
            writeln!(stderr, "{token:?} {:?}", &source[token.span.range()]).unwrap();
        }
    }

    if opts.lex_only {
        return Ok(());
    }

    let file = parser::parse(&tokens, &source, opts.edition.unwrap_or_default())
        .map_err(|error| error.print(&source, &path))?;

    if opts.emit_ast {
        eprintln!("{file:#?}");
    }

    if opts.fmt {
        let result = fmter::fmt(
            file,
            &source,
            fmter::Cfg { skip_marker: opts.skip_marker.unwrap_or_default(), ..default() },
        );
        println!("{result}");
    }

    Ok(())
}

#[derive(Default)]
struct Opts {
    path: Option<PathBuf>,
    edition: Option<Edition>,
    emit_tokens: bool,
    emit_ast: bool,
    lex_only: bool,
    fmt: bool,
    skip_marker: Option<fmter::SkipMarker>,
}
