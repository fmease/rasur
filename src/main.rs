#![feature(import_trait_associated_functions)]
#![deny(unused_must_use, rust_2018_idioms)]

use Default::default;
use std::process::ExitCode;

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

    let tokens = rasur::lexer::lex(&source, edition, rasur::lexer::StripShebang::Yes);

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

    let cx = rasur::parser::RenderCx { source: &source, path: &path, short: opts.short };
    let (file, errors) = rasur::parser::parse(&tokens, &source, edition);
    errors.into_iter().for_each(|error| error.print(cx));
    let file = file?;

    if opts.emit_ast {
        eprintln!("{file:#?}");
    }

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
