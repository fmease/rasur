#![feature(decl_macro)]
#![feature(if_let_guard)]
#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(macro_metavar_expr)]
#![deny(unused_must_use, rust_2018_idioms)]

use std::{path::PathBuf, process::ExitCode};

use edition::Edition;

mod ast;
mod edition;
mod fmter;
mod lexer;
mod parser;
mod span;

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
                b"edition" => {
                    let edition = args.next().ok_or_else(|| {
                        eprintln!("error: missing argument to `--edition`");
                    })?;
                    if opts.edition.is_some() {
                        eprintln!("error: flag can't be passed multiple times");
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

    let path = opts.path.ok_or_else(|| eprintln!("error: missing required path argument"))?;
    let source = std::fs::read_to_string(&path)
        .map_err(|error| eprintln!("error: failed to read `{}`: {error}", path.display()))?;

    let tokens = lexer::lex(&source);

    if opts.emit_tokens {
        let mut stderr = std::io::stderr().lock();

        for token in &tokens {
            use std::io::Write as _;
            writeln!(stderr, "{token:?} {:?}", &source[token.span.range()]).unwrap();
        }
    }

    let file = parser::parse(tokens, &source, opts.edition.unwrap_or_default())
        .map_err(|error| error.print(&source))?;

    if opts.emit_ast {
        eprintln!("{file:#?}");
    }

    if opts.fmt {
        let result = fmter::fmt(file, &source, fmter::Cfg { ..Default::default() });
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
    fmt: bool,
}
