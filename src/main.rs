#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(super_let)]
#![feature(type_alias_impl_trait)]
#![deny(unused_must_use, rust_2018_idioms)]

use Default::default;
use rasur::lexer::{StripFrontmatter, StripShebang};
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

    let strip_shebang = if opts.strip_shebang { StripShebang::Yes } else { StripShebang::No };
    let strip_frontmatter =
        if opts.strip_frontmatter { StripFrontmatter::Yes } else { StripFrontmatter::No };

    let file = rasur::lexer::lex(&source, edition, strip_shebang, strip_frontmatter, &mut errors);

    if opts.emit_tokens {
        emit_tokens(&file, &source).unwrap();
    }

    if opts.lex_only {
        if let Some(errors) = errors.non_empty() {
            errors.into_iter().for_each(|error| diagnostics::eprint(error, cx));
            return Err(());
        }

        return Ok(());
    }

    let file = rasur::parser::parse(file, &source, edition, &mut errors);

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

fn emit_tokens(file: &rasur::lexer::File, source: &str) -> std::io::Result<()> {
    use anstyle::{AnsiColor, Style};
    use std::io::Write;

    let mut stderr: Stderr = std::io::BufWriter::new(std::io::stderr().lock());
    type Stderr = impl Write;

    let render = |stderr: &mut Stderr, span: rasur::span::Span| {
        fn color(color: anstyle::AnsiColor) -> Style {
            anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(color)))
        }

        paint(stderr, color(AnsiColor::BrightBlack), |stderr| write!(stderr, "{span:?} "))?;
        paint(stderr, color(AnsiColor::Yellow), |stderr| {
            write!(stderr, "{:?}", &source[span.range()])
        })
    };

    if let Some(shebang) = file.shebang {
        paint(&mut stderr, Style::new().italic(), |stderr| write!(stderr, "Shebang "))?;
        render(&mut stderr, shebang)?;
        writeln!(stderr)?;
    }

    if let Some(frontmatter) = file.frontmatter {
        paint(&mut stderr, Style::new().italic(), |stderr| write!(stderr, "Frontmatter "))?;
        render(&mut stderr, frontmatter)?;
        writeln!(stderr)?;
    }

    for token in &file.tokens {
        write!(stderr, "{:?} ", token.kind)?;
        render(&mut stderr, token.span)?;
        writeln!(stderr)?;
    }

    fn paint(
        stderr: &mut Stderr,
        style: Style,
        write: impl FnOnce(&mut Stderr) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        style.write_to(stderr)?;
        write(stderr)?;
        style.write_reset_to(stderr)
    }

    Ok(())
}
