#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(super_let)]
#![feature(type_alias_impl_trait)]
#![deny(unused_must_use, rust_2018_idioms)]

use Default::default;
use painter::Painter;
use rasur::{
    lexer::{StripFrontmatter, StripShebang},
    normalizer::Normalized,
};
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

    let mut errors = rasur::error::Buffer::Hold(Vec::new());

    let strip_shebang = if opts.strip_shebang { StripShebang::Yes } else { StripShebang::No };
    let strip_frontmatter =
        if opts.strip_frontmatter { StripFrontmatter::Yes } else { StripFrontmatter::No };

    let file = rasur::lexer::lex(source, edition, strip_shebang, strip_frontmatter, &mut errors);

    if opts.emit_tokens {
        emit_tokens(&file, source).unwrap();
    }

    if opts.lex_only {
        if let Some(errors) = errors.non_empty() {
            diagnostics::render(errors, cx);
            return Err(());
        }

        return Ok(());
    }

    let file = rasur::parser::parse(&file, source, edition, &mut errors);

    if let Ok(file) = &file
        && opts.emit_ast
    {
        eprintln!("{file:#?}");
    }

    let result = if let Some(errors) = errors.non_empty() {
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

fn emit_tokens(file: &rasur::lexer::File, source: Normalized<&str>) -> std::io::Result<()> {
    use painter::{AnsiColor, Effects};
    use std::io::{self, Write as _};

    let mut p = Painter::new(io::stderr(), io::BufWriter::new);

    let render = |p: &mut Painter<_>, span: rasur::span::Span| {
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{span:?} "))?;
        p.with(AnsiColor::Yellow, |p| write!(p, "{:?}", &source.into_inner()[span.range()]))
    };

    if let Some(shebang) = file.shebang {
        p.with(Effects::ITALIC, |p| write!(p, "Shebang "))?;
        render(&mut p, shebang)?;
        writeln!(p)?;
    }

    if let Some(frontmatter) = file.frontmatter {
        p.with(Effects::ITALIC, |p| write!(p, "Frontmatter "))?;
        render(&mut p, frontmatter)?;
        writeln!(p)?;
    }

    for token in &file.tokens {
        write!(p, "{:?} ", token.kind)?;
        render(&mut p, token.span)?;
        writeln!(p)?;
    }

    Ok(())
}
