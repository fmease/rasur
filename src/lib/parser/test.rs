// NOTE: We had to split this into several modules to avoid input lag due to rust-analyzer.

mod expr;
mod file;
mod gen_args;
mod item;
mod lit;
mod misc;
mod pat;
mod stmt;
mod ty;

use super::Parser;
use crate::{
    ast,
    edition::Edition,
    error::Error,
    lexer::{self, lex},
    span::{At as _, ByteIndex, Span},
    store::{Buffer, Store},
    token::TokenKind,
};
use normalizer::{Normalized, normalize};

type Result<T, E = Vec<Error>> = std::result::Result<T, E>;

mod normalizer {
    use std::borrow::Cow;

    pub(super) fn normalize(source: &str) -> Normalized<Cow<'_, str>> {
        Normalized { raw: crate::lexer::normalize(source) }
    }

    #[derive(Clone, Copy)]
    pub(super) struct Normalized<T> {
        raw: T,
    }

    impl<T> Normalized<T> {
        pub(super) fn into_inner(self) -> T {
            self.raw
        }
    }

    impl Normalized<Cow<'_, str>> {
        pub(super) fn as_ref(&self) -> Normalized<&str> {
            Normalized { raw: &self.raw }
        }
    }
}

fn parse_file(source: Normalized<&str>, edition: Edition) -> Result<ast::File<'_>> {
    parse_file_raw(source, edition).map(|(.., file)| file)
}

fn parse_file_full(source: Normalized<&str>, edition: Edition) -> Result<FullFile<'_>> {
    let (shebang, frontmatter, file) = parse_file_raw(source, edition)?;
    let source = source.into_inner();

    Ok(FullFile {
        shebang: shebang.map(|span| source.at(span)),
        frontmatter: frontmatter.map(|frontmatter| Frontmatter {
            infostring: source.at(frontmatter.infostring),
            content: source.at(frontmatter.content),
        }),
        file,
    })
}

fn parse_file_raw(
    source: Normalized<&str>,
    edition: Edition,
) -> Result<(Option<Span>, Option<lexer::Frontmatter>, ast::File<'_>)> {
    let source = source.into_inner();
    let store = Store { errors: Buffer::default(), features: Buffer::sealed() };

    let mut offset = ByteIndex::default();
    let shebang = lexer::strip_shebang(source, &mut offset, edition);
    let frontmatter = lexer::strip_frontmatter(source, &mut offset, &store);

    let tokens = lex(source, offset, edition, &store);
    let file = super::parse(tokens, source, edition, &store);

    if let errors = store.errors.into_inner()
        && !errors.is_empty()
    {
        return Err(errors);
    }

    Ok((shebang, frontmatter, file.unwrap()))
}

#[derive(Debug)]
struct FullFile<'src> {
    shebang: Option<&'src str>,
    frontmatter: Option<Frontmatter<'src>>,
    #[allow(dead_code)]
    file: ast::File<'src>,
}

#[derive(Debug)]
struct Frontmatter<'src> {
    infostring: &'src str,
    content: &'src str,
}

fn parse_item(source: Normalized<&str>, edition: Edition) -> Result<ast::Item<'_>> {
    parse_via(source, edition, |this| this.parse_item(super::item::ItemCx::Boring))
}

fn parse_ty(source: Normalized<&str>, edition: Edition) -> Result<ast::Ty<'_>> {
    parse_via(source, edition, |this| this.parse_ty())
}

fn parse_stmt(source: Normalized<&str>, edition: Edition) -> Result<ast::Stmt<'_>> {
    parse_via(source, edition, |this| this.parse_stmt(TokenKind::EndOfInput))
}

fn parse_expr(source: Normalized<&str>, edition: Edition) -> Result<ast::Expr<'_>> {
    parse_via(source, edition, |this| this.parse_expr())
}

fn parse_pat(source: Normalized<&str>, edition: Edition) -> Result<ast::Pat<'_>> {
    parse_via(source, edition, |this| this.parse_pat(super::pat::OrPolicy::Parse))
}

fn parse_via<'src, T>(
    source: Normalized<&'src str>,
    edition: Edition,
    parse: impl FnOnce(&mut Parser<'_, '_, 'src>) -> super::Result<T>,
) -> Result<T> {
    let source = source.into_inner();
    let store = Store { errors: Buffer::default(), features: Buffer::sealed() };

    let tokens = lex(source, ByteIndex::default(), edition, &store);
    let tokens = super::prepare(tokens);
    let mut p = Parser::new(&tokens, source, edition, &store);

    let node = parse(&mut p).and_then(|r| {
        p.parse(TokenKind::EndOfInput)?;
        Ok(r)
    });

    if let errors = store.errors.into_inner()
        && !errors.is_empty()
    {
        return Err(errors);
    }

    Ok(node.unwrap())
}

macro t($parse:ident, $edition:ident, $source:expr, $ast:pat $(if $guard:expr)? $(,)?) {
    match $parse(normalize($source).as_ref(), $edition) {
        $ast $(if $guard)? => {}
        ast => panic!("{:?}: {} != {:#?}", $source, stringify!($ast), ast),
    }
}

// FIXME: macro_rules! in stmt pos (-> item not stmt); macro_rules! no binder == macro call
// FIXME: almost-assoc-item-constraint due to (  )
