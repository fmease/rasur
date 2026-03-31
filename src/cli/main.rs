#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(super_let)]
#![feature(type_alias_impl_trait)]
#![deny(unused_must_use, rust_2018_idioms)]

mod diagnostics;
mod interface;

use crate::{
    diagnostics::{Diag, RenderExt as _, SourcePathBuf},
    interface::ArtifactType,
};
use Default::default;
use painter::Painter;
use std::process::ExitCode;

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

    let cx = diagnostics::RenderCx::new(opts.short);

    let (source, path) = match opts.source {
        interface::Source::Path(path) => {
            let source = std::fs::read_to_string(&path).map_err(|error| {
                Diag::error(format!("failed to read `{}`: {error}", path.display())).render(&cx);
            })?;
            (source, SourcePathBuf::Real(path))
        }
        interface::Source::Stdin => (
            std::io::read_to_string(std::io::stdin()).map_err(|error| {
                Diag::error(format!("failed to read stdin: {error}")).render(&cx)
            })?,
            SourcePathBuf::Anon,
        ),
        interface::Source::String(string) => (string, SourcePathBuf::Anon),
    };

    let source = rasur::lexer::normalize(&source);
    let source = source.as_ref();

    let edition = opts.edition.unwrap_or_default();
    let cx = cx.file(path.as_ref(), source);

    let store = rasur::store::Store::default();

    let mut offset = rasur::span::ByteIndex::default();
    let shebang = opts
        .strip_shebang
        .then(|| rasur::lexer::strip_shebang(source, &mut offset, edition))
        .flatten();
    let frontmatter = opts
        .strip_frontmatter
        .then(|| rasur::lexer::strip_frontmatter(source, &mut offset, &store))
        .flatten();
    let tokens = rasur::lexer::lex(source, offset, edition, &store);

    // FIXME: Make it possible again to continue parsing after emitting tokens.
    if opts.lex_only || matches!(opts.emit, Some(ArtifactType::Tokens)) {
        if opts.lex_only {
            tokens.for_each(drop);
        } else {
            emit_tokens(tokens, shebang, frontmatter, source).unwrap();
        }

        return report(store, opts.gatekeep, &cx);
    }

    let file = rasur::parser::parse(tokens, shebang, frontmatter, source, edition, &store);

    if let Ok(file) = &file
        && let Some(ArtifactType::Ast) = opts.emit
    {
        eprintln!("{file:#?}");
    }

    let result = report(store, opts.gatekeep, &cx);

    if let Some(ArtifactType::Fmt) = opts.emit
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

fn report(
    store: rasur::store::Store,
    gatekeep: bool,
    cx: &diagnostics::RenderCx<'_>,
) -> Result<(), ()> {
    let mut result = Ok(());

    for error in store.errors {
        result = Err(());
        error.render(cx);
    }

    if gatekeep {
        for (feature, span) in store.features {
            let level = if feature.protected() {
                result = Err(());
                annotate_snippets::Level::ERROR
            } else {
                annotate_snippets::Level::WARNING
            };
            let diag = Diag::new(level, format!("use of experimental feature `{feature}`"));
            let diag = match span {
                Some(span) => diag.highlight(span),
                None => diag,
            };
            diag.render(cx);
        }
    }

    result
}

fn emit_tokens(
    tokens: rasur::lexer::Tokens<'_, '_>,
    shebang: Option<rasur::span::Span>,
    frontmatter: Option<rasur::lexer::Frontmatter>,
    source: &str,
) -> std::io::Result<()> {
    use painter::{AnsiColor, Effects};
    use std::io::{self, Write as _};

    let mut p = Painter::new(io::stderr(), io::BufWriter::new);

    let render = |p: &mut Painter<_>, span: rasur::span::Span| {
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{span:?} "))?;
        p.with(AnsiColor::Yellow, |p| write!(p, "{:?}", &source[span.range()]))
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
        if let rasur::token::TokenKind::Comment
        | rasur::token::TokenKind::Error
        | rasur::token::TokenKind::Whitespace = token.kind
        {
            continue;
        }

        write!(p, "{:?} ", token.kind)?;
        render(&mut p, token.span)?;
        writeln!(p)?;
    }

    Ok(())
}
