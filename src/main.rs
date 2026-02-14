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
    let opts = interface::opts();

    match opts.color {
        clap::ColorChoice::Always => anstream::ColorChoice::Always.write_global(),
        clap::ColorChoice::Never => anstream::ColorChoice::Never.write_global(),
        clap::ColorChoice::Auto => {}
    }

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

    let source = rasur::normalize(&source);
    let source = source.as_ref();

    let edition = opts.edition.unwrap_or_default();
    let cx = diagnostics::RenderCx { source, path: &path, short: opts.short };

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

    let file = rasur::parser::parse(file, source, edition, &mut errors);

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
            rasur::fmter::Cfg { skip_marker: opts.skip_marker, ..default() },
        );
        println!("{result}");
    }

    result
}

fn emit_tokens(file: &rasur::lexer::File, source: &str) -> std::io::Result<()> {
    use anstyle::AnsiColor;
    use std::io::Write;

    let stderr = std::io::stderr();
    let colorize = anstream::AutoStream::choice(&stderr) != anstream::ColorChoice::Never;
    let stderr = std::io::BufWriter::new(stderr);
    let mut p = Painter::new(stderr, colorize);

    let render = |p: &mut Painter<_>, span: rasur::span::Span| {
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{span:?} "))?;
        p.with(AnsiColor::Yellow, |p| write!(p, "{:?}", &source[span.range()]))
    };

    if let Some(shebang) = file.shebang {
        p.with(anstyle::Effects::ITALIC, |p| write!(p, "Shebang "))?;
        render(&mut p, shebang)?;
        writeln!(p)?;
    }

    if let Some(frontmatter) = file.frontmatter {
        p.with(anstyle::Effects::ITALIC, |p| write!(p, "Frontmatter "))?;
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

struct Painter<W: std::io::Write> {
    writer: W,
    colorize: bool,
    style: anstyle::Style,
}

impl<W: std::io::Write> Painter<W> {
    pub(crate) fn new(writer: W, colorize: bool) -> Self {
        Self { writer, colorize, style: anstyle::Style::new() }
    }
}

impl<W: std::io::Write> Painter<W> {
    pub(crate) fn set(&mut self, style: impl IntoStyle) -> std::io::Result<()> {
        if !self.colorize {
            return Ok(());
        }

        self.style = style.into_style();
        self.style.write_to(&mut self.writer)
    }

    fn unset(&mut self) -> std::io::Result<()> {
        if !self.colorize {
            return Ok(());
        }

        std::mem::take(&mut self.style).write_reset_to(&mut self.writer)
    }

    fn with(
        &mut self,
        style: impl IntoStyle,
        inner: impl FnOnce(&mut Self) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        self.set(style)?;
        inner(self)?;
        self.unset()
    }
}

impl<W: std::io::Write> std::io::Write for Painter<W> {
    fn write(&mut self, buffer: &[u8]) -> std::io::Result<usize> {
        self.writer.write(buffer)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

trait IntoStyle {
    fn into_style(self) -> anstyle::Style;
}

impl IntoStyle for anstyle::Style {
    fn into_style(self) -> anstyle::Style {
        self
    }
}

impl IntoStyle for anstyle::AnsiColor {
    fn into_style(self) -> anstyle::Style {
        self.on_default()
    }
}

impl IntoStyle for anstyle::Effects {
    fn into_style(self) -> anstyle::Style {
        anstyle::Style::new().effects(self)
    }
}
