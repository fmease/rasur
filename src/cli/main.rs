// Features
#![feature(const_default)]
#![feature(const_trait_impl)]
#![feature(deref_patterns)]
#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
// Lints
#![deny(unused_must_use, rust_2018_idioms)]

mod diagnostics;
mod feature;
mod interface;

use crate::{
    diagnostics::{Diag, IntoDiag, Level, SourcePathBuf},
    interface::{ArtifactType, FragmentKind},
};
use Default::default;
use painter::Painter;
use rasur::{
    ast,
    edition::Edition,
    feature::{Feature, FeatureKind},
    fmter,
    lexer::{self, Frontmatter, Tokens},
    parser,
    span::{At as _, ByteIndex, Span},
    store::Store,
};
use std::{
    collections::HashSet,
    io::{self, Write},
    mem,
    process::ExitCode,
};

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let mut opts = interface::opts();

    match opts.color {
        clap::ColorChoice::Always => painter::ColorChoice::Always.write_global(),
        clap::ColorChoice::Never => painter::ColorChoice::Never.write_global(),
        clap::ColorChoice::Auto => {}
    }

    let cx = diagnostics::RenderCx::new(opts.short);

    let (source, path) = match opts.source {
        interface::Source::Path(ref mut path) => {
            let source = std::fs::read_to_string(&*path).map_err(|error| {
                Diag::error(format!("failed to read `{}`: {error}", path.display())).render(&cx);
            })?;
            (source, SourcePathBuf::Real(mem::take(path)))
        }
        interface::Source::Stdin => (
            io::read_to_string(io::stdin()).map_err(|error| {
                Diag::error(format!("failed to read stdin: {error}")).render(&cx)
            })?,
            SourcePathBuf::Anon,
        ),
        interface::Source::String(ref mut string) => (mem::take(string), SourcePathBuf::Anon),
    };

    let source = source.as_str();
    let source = if opts.strip_bom { lexer::strip_bom(source) } else { source };
    let source = if opts.substitute_crlf { lexer::substitute_crlf(source) } else { source.into() };
    let source = source.as_ref();

    let edition = opts.edition.unwrap_or_default();
    let cx = cx.file(path.as_ref(), source);

    let store = Store::default();

    // FIXME: Don't strip shebang&frontmatter by default for fragment kinds other than file.
    let mut offset = ByteIndex::default();
    let shebang =
        opts.strip_shebang.then(|| lexer::strip_shebang(source, &mut offset, edition)).flatten();
    let frontmatter = opts
        .strip_frontmatter
        .then(|| lexer::strip_frontmatter(source, &mut offset, &store))
        .flatten();
    let tokens = lexer::lex(source, offset, edition, &store);

    // FIXME: Make it possible again to continue parsing after emitting tokens.
    if opts.lex_only || matches!(opts.emit, Some(ArtifactType::Tokens)) {
        if opts.lex_only {
            tokens.for_each(drop);
        } else {
            emit_tokens(tokens, shebang, frontmatter, source).unwrap();
        }

        return report(store, default(), &opts, &cx);
    }

    let frag = Fragment::parse(opts.kind, tokens, source, edition, &store);

    if let Ok(frag) = &frag
        && let Some(ArtifactType::Ast) = opts.emit
    {
        println!("{frag:#?}");
    }

    let (enabled_features, mut result) = if opts.gatekeep
        && let Ok(Fragment::File(file)) = &frag
    {
        feature::enabled_features(file, source, edition, &cx)
    } else {
        (default(), Ok(()))
    };

    result = result.and(report(store, enabled_features, &opts, &cx));

    if let Some(ArtifactType::Fmt) = opts.emit
        && let Ok(frag) = frag
    {
        println!("{}", frag.fmt(source, shebang, frontmatter, edition));
    }

    result
}

fn report(
    store: Store,
    enabled_features: HashSet<Feature>,
    opts: &interface::Opts,
    cx: &diagnostics::RenderCx<'_>,
) -> Result<(), ()> {
    let mut result = Ok(());

    for error in store.errors {
        result = Err(());
        error.into_diag(cx).render(cx);
    }

    if opts.gatekeep {
        for (feature, span) in store.features {
            let kind = feature.kind();

            // FIXME: Warn on enabled but unused features.
            if enabled_features.contains(&feature)
                && (kind != FeatureKind::SuperInternal || opts.unlock_super_internal_features)
            {
                continue;
            }

            let level = if feature.protected() { Level::Error } else { Level::Warning };
            level.apply(&mut result);

            let diag = Diag::new(level).title(format!("use of {kind} feature `{feature}`"));
            let diag = if let Some(issue) = feature.tracking_issue() {
                const ISSUE_BASE_URL: &str = "https://github.com/rust-lang/rust/issues/";
                diag.note(format!("see <{ISSUE_BASE_URL}{issue}> for more information"))
            } else {
                diag
            };
            let extra = if let FeatureKind::SuperInternal = kind {
                format!("\nand pass `--{}`", interface::UNLOCK_SUPER_INTERNAL_FEATURES_OPT)
            } else {
                String::new()
            };
            diag.help(format!(
                "add `#![feature({feature})]` at the top of the file{extra} to enable"
            ))
            .span(span)
            .render(cx);
        }
    } else if let Some(ArtifactType::Features) = opts.emit {
        let mut stdout = io::BufWriter::new(io::stdout().lock());
        for (feature, _) in store.features {
            if !feature.protected() {
                // FIXME: Questionable behavior.
                continue;
            }
            writeln!(stdout, "{feature}").unwrap();
        }
        stdout.flush().unwrap();
    }

    result
}

fn emit_tokens(
    tokens: Tokens<'_, '_>,
    shebang: Option<Span>,
    frontmatter: Option<Frontmatter>,
    source: &str,
) -> io::Result<()> {
    use io::Write as _;
    use painter::{AnsiColor, Effects};

    let mut p = Painter::new(io::stdout(), io::BufWriter::new);

    let render = |p: &mut Painter<_>, span: Span| {
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{span:?} "))?;
        p.with(AnsiColor::Yellow, |p| write!(p, "{:?}", source.at(span)))
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
        if token.kind.is_trivia() {
            continue;
        }

        write!(p, "{:?} ", token.kind)?;
        render(&mut p, token.span)?;
        writeln!(p)?;
    }

    Ok(())
}

enum Fragment<'src> {
    Expr(ast::Expr<'src>),
    File(ast::File<'src>),
    Item(ast::Item<'src>),
    Pat(ast::Pat<'src>),
    Stmt(ast::Stmt<'src>),
    Ty(ast::Ty<'src>),
}

impl<'src> Fragment<'src> {
    fn parse<'sto>(
        kind: FragmentKind,
        tokens: Tokens<'sto, 'src>,
        source: &'src str,
        edition: Edition,
        store: &'sto Store,
    ) -> Result<Self, ()> {
        Ok(match kind {
            FragmentKind::Expr => Self::Expr(parser::parse_expr(tokens, source, edition, &store)?),
            FragmentKind::File => Self::File(parser::parse_file(tokens, source, edition, &store)?),
            FragmentKind::Item => Self::Item(parser::parse_item(tokens, source, edition, &store)?),
            FragmentKind::Pat => Self::Pat(parser::parse_pat(tokens, source, edition, &store)?),
            FragmentKind::Stmt => Self::Stmt(parser::parse_stmt(tokens, source, edition, &store)?),
            FragmentKind::Ty => Self::Ty(parser::parse_ty(tokens, source, edition, &store)?),
        })
    }

    fn fmt(
        self,
        source: &str,
        shebang: Option<Span>,
        frontmatter: Option<Frontmatter>,
        edition: Edition,
    ) -> String {
        match self {
            Self::Expr(expr) => fmter::fmt_expr(expr, source, edition),
            Self::File(file) => fmter::fmt_file(file, source, shebang, frontmatter, edition),
            Self::Item(item) => fmter::fmt_item(item, source, edition),
            Self::Pat(pat) => fmter::fmt_pat(pat, source, edition),
            Self::Stmt(stmt) => fmter::fmt_stmt(stmt, source, edition),
            Self::Ty(ty) => fmter::fmt_ty(ty, source, edition),
        }
    }
}

impl std::fmt::Debug for Fragment<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => expr.fmt(f),
            Self::File(file) => file.fmt(f),
            Self::Item(item) => item.fmt(f),
            Self::Pat(pat) => pat.fmt(f),
            Self::Stmt(stmt) => stmt.fmt(f),
            Self::Ty(ty) => ty.fmt(f),
        }
    }
}
