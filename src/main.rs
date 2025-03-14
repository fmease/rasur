#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(macro_metavar_expr)]
#![deny(unused_must_use, rust_2018_idioms)]

mod ast;
mod fmter;
mod lexer;
mod parser;
mod span;

static SNIPPET: &str = "

#[repr] #[allow] struct Foo { a: Ty, b: Ty }

mod x { mod y { mod z { struct Main {} fn main() { DUMMY } } } }
";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut opts = Opts::default();

    for arg in std::env::args_os().skip(1) {
        match arg.as_encoded_bytes() {
            b"--emit-tokens" => opts.emit_tokens = true,
            b"--emit-ast" => opts.emit_ast = true,
            _ => return Err(format!("unknown option `{}`", arg.display()).into()),
        }
    }

    let source = SNIPPET;
    let tokens = lexer::lex(source);

    if opts.emit_tokens {
        let mut stderr = std::io::stderr().lock();

        for token in &tokens {
            use std::io::Write as _;
            writeln!(stderr, "{token:?} {:?}", &SNIPPET[token.span.range()])?;
        }
    }

    let file = parser::parse(tokens, source)?;

    if opts.emit_ast {
        eprintln!("{file:#?}");
    }

    let result = fmter::fmt(file, fmter::Cfg { ..Default::default() });

    println!("{result}");

    Ok(())
}

#[derive(Default)]
struct Opts {
    emit_tokens: bool,
    emit_ast: bool,
}
