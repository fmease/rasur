#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(macro_metavar_expr)]
#![deny(unused_must_use, rust_2018_idioms)]

use std::path::PathBuf;

use edition::Edition;

mod ast;
mod edition;
#[cfg(any())] // FIXME: Progress on parser has priority atm.
mod fmter;
mod lexer;
mod parser;
mod span;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut opts = Opts::default();

    let mut args = std::env::args_os().skip(1);
    while let Some(arg) = args.next() {
        if let Some(opt) = arg.as_encoded_bytes().strip_prefix(b"--") {
            match opt {
                b"tok" => opts.emit_tokens = true,
                b"ast" => opts.emit_ast = true,
                b"edition" => {
                    let edition = args.next().ok_or("missing argument to `--edition`")?;
                    if opts.edition.is_some() {
                        return Err(format!("flag can't be passed multiple times").into());
                    }
                    opts.edition = Some(match edition.as_encoded_bytes() {
                        b"2015" => Edition::Rust2015,
                        b"2018" => Edition::Rust2018,
                        b"2021" => Edition::Rust2021,
                        b"2024" => Edition::Rust2024,
                        b"future" => Edition::Future,
                        _ => return Err(format!("invalid edition `{}`", edition.display()).into()),
                    });
                }
                _ => return Err(format!("unknown flag `{}`", arg.display()).into()),
            }
        } else {
            if opts.path.is_some() {
                return Err(format!("unexpected argument `{}`", arg.display()).into());
            }
            opts.path = Some(PathBuf::from(arg));
        }
    }

    let path = opts.path.ok_or("missing required path argument")?;
    let source = std::fs::read_to_string(path)?;

    let tokens = lexer::lex(&source);

    if opts.emit_tokens {
        let mut stderr = std::io::stderr().lock();

        for token in &tokens {
            use std::io::Write as _;
            writeln!(stderr, "{token:?} {:?}", &source[token.span.range()])?;
        }
    }

    let file = parser::parse(tokens, &source, opts.edition.unwrap_or_default())?;

    if opts.emit_ast {
        eprintln!("{file:#?}");
    }

    // FIXME:
    // let result = fmter::fmt(file, fmter::Cfg { ..Default::default() });
    // println!("{result}");

    Ok(())
}

#[derive(Default)]
struct Opts {
    path: Option<PathBuf>,
    edition: Option<Edition>,
    emit_tokens: bool,
    emit_ast: bool,
}
