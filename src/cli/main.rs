#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(super_let)]
#![feature(type_alias_impl_trait)]
#![deny(unused_must_use, rust_2018_idioms)]

use Default::default;
use painter::Painter;
use rasur::normalizer::Normalized;
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
    let opts = interface::opts();

    match opts.color {
        clap::ColorChoice::Always => painter::ColorChoice::Always.write_global(),
        clap::ColorChoice::Never => painter::ColorChoice::Never.write_global(),
        clap::ColorChoice::Auto => {}
    }

    let (source, path) = match opts.source {
        interface::Source::Path(path) => {
            let source = std::fs::read_to_string(&path).map_err(|error| {
                // FIXME: use annotate-snippet for this error, too
                eprintln!("error: failed to read `{}`: {error}", path.display())
            })?;
            (source, path)
        }
        // FIXME: Use structured paths, `SourcePath::Anon`
        interface::Source::String(string) => (string, "<anon>".into()),
    };

    let source = rasur::normalizer::normalize(&source);
    let source = source.as_ref();

    let edition = opts.edition.unwrap_or_default();
    let cx = diagnostics::RenderCx::new(source, &path, opts.short);

    let errors = rasur::error::Buffer::default();

    let mut offset = rasur::span::ByteIndex::default();
    let shebang = opts
        .strip_shebang
        .then(|| rasur::lexer::strip_shebang(source.into_inner(), &mut offset, edition))
        .flatten();
    let frontmatter = opts
        .strip_frontmatter
        .then(|| rasur::lexer::strip_frontmatter(source.into_inner(), &mut offset, &errors))
        .flatten();
    let tokens = rasur::lexer::lex(source, offset, edition, &errors);

    // FIXME: Make it possible again to continue parsing after emitting tokens.
    if opts.emit_tokens || opts.lex_only {
        if opts.emit_tokens {
            emit_tokens(tokens, shebang, frontmatter, source).unwrap();
        } else {
            drop(tokens);
        }

        if let errors = errors.into_inner()
            && !errors.is_empty()
        {
            diagnostics::render(errors, cx);
            return Err(());
        }

        return Ok(());
    }

    let file = rasur::parser::parse(tokens, shebang, frontmatter, source, edition, &errors);

    if let Ok(file) = &file
        && opts.emit_ast
    {
        eprintln!("{file:#?}");
    }

    let result = if let errors = errors.into_inner()
        && !errors.is_empty()
    {
        diagnostics::render(errors, cx);
        Err(())
    } else {
        Ok(())
    };

    if opts.fmt
        && let Ok(file) = file
    {
        let result = rasur::fmter::fmt(
            file,
            source,
            edition,
            rasur::fmter::Cfg { skip_marker: opts.skip_marker, ..default() },
        );
        println!("{result}");
    }

    result
}

fn emit_tokens(
    tokens: rasur::lexer::Tokens<'_, '_>,
    shebang: Option<rasur::span::Span>,
    frontmatter: Option<rasur::lexer::Frontmatter>,
    source: Normalized<&str>,
) -> std::io::Result<()> {
    use painter::{AnsiColor, Effects};
    use std::io::{self, Write as _};

    let mut p = Painter::new(io::stderr(), io::BufWriter::new);

    let render = |p: &mut Painter<_>, span: rasur::span::Span| {
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{span:?} "))?;
        p.with(AnsiColor::Yellow, |p| write!(p, "{:?}", &source.into_inner()[span.range()]))
    };

    if let Some(shebang) = shebang {
        p.with(Effects::ITALIC, |p| write!(p, "Shebang "))?;
        render(&mut p, shebang)?;
        writeln!(p)?;
    }

    if let Some(frontmatter) = frontmatter {
        p.with(Effects::ITALIC, |p| writeln!(p, "Frontmatter"))?;
        p.with(Effects::ITALIC, |p| write!(p, "    Infostring "))?;
        render(&mut p, frontmatter.infostring)?;
        writeln!(p)?;
        p.with(Effects::ITALIC, |p| write!(p, "    Content "))?;
        render(&mut p, frontmatter.content)?;
        writeln!(p)?;
    }

    for token in tokens {
        // FIXME: Allow the CLI to dictate if we print all tokens instead of "most".
        if let rasur::token::TokenKind::Trivia | rasur::token::TokenKind::Error = token.kind {
            continue;
        }

        write!(p, "{:?} ", token.kind)?;
        render(&mut p, token.span)?;
        writeln!(p)?;
    }

    Ok(())
}
