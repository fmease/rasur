#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(super_let)]
#![deny(unused_must_use, rust_2018_idioms)]

use Default::default;
use std::process::ExitCode;

mod diagnostics;
mod interface;

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let opts = interface::opts().map_err(|error| eprintln!("error: {error}"))?;

    let (source, path) = match opts.source {
        interface::Source::Path(path) => {
            let source = std::fs::read_to_string(&path).map_err(|error| {
                eprintln!("error: failed to read `{}`: {error}", path.display())
            })?;
            (source, path)
        }
        // FIXME: Use structured paths, `SourcePath::Anon`
        interface::Source::String(string) => (string, "<anon>".into()),
    };

    let edition = opts.edition.unwrap_or_default();
    let cx = diagnostics::RenderCx { source: &source, path: &path, short: opts.short };

    let mut errors = rasur::error::Buffer::Hold(Vec::new());

    let tokens = rasur::lexer::lex(&source, edition, rasur::lexer::StripShebang::Yes, &mut errors);

    if opts.emit_tokens {
        let mut stderr = std::io::stderr().lock();

        for token in &tokens {
            use std::io::Write as _;
            writeln!(stderr, "{token:?} {:?}", &source[token.span.range()]).unwrap();
        }
    }

    if opts.lex_only {
        if let Some(errors) = errors.non_empty() {
            errors.into_iter().for_each(|error| diagnostics::eprint(error, cx));
            return Err(());
        }

        return Ok(());
    }

    let file = rasur::parser::parse(&tokens, &source, edition, &mut errors);

    if let Ok(file) = &file
        && opts.emit_ast
    {
        eprintln!("{file:#?}");
    }

    if let Some(errors) = errors.non_empty() {
        errors.into_iter().for_each(|error| diagnostics::eprint(error, cx));
        return Err(());
    }

    let file = file.unwrap();

    if opts.fmt {
        let result = rasur::fmter::fmt(
            file,
            &source,
            rasur::fmter::Cfg { skip_marker: opts.skip_marker, ..default() },
        );
        println!("{result}");
    }

    Ok(())
}
