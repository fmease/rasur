// NOTE: We had to split this into several modules to avoid input lag due to rust-analyzer.

mod expr;
mod file;
mod gen_args;
mod item;
mod misc;
mod num_lit;
mod pat;
mod stmt;
mod ty;

use super::Parser;
use crate::{
    ast,
    edition::Edition,
    error::Error,
    lexer::{self, lex},
    span::ByteIndex,
    store::{Buffer, Store},
    token::TokenKind,
};
use normalizer::{Normalized, normalize};

// NOTE: We're not using implicit deref patterns at the moment since rust-analyzer
//       can't handle them yet and would color the entirely red. Use `r!(…)` for now.

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
    let source = source.into_inner();
    let store = Store { errors: Buffer::default(), features: Buffer::sealed() };

    let mut offset = ByteIndex::default();
    let shebang = lexer::strip_shebang(source, &mut offset, edition);
    let frontmatter = lexer::strip_frontmatter(source, &mut offset, &store);

    let tokens = lex(source, offset, edition, &store);
    let file = super::parse(tokens, shebang, frontmatter, source, edition, &store);

    if let errors = store.errors.into_inner()
        && !errors.is_empty()
    {
        return Err(errors);
    }

    Ok(file.unwrap())
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
    parse: impl FnOnce(&mut super::Parser<'_, '_, 'src>) -> super::Result<T>,
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

macro t($parse:ident, $edition:ident, $source:literal, $ast:pat $(,)?) {
    match $parse(normalize($source).as_ref(), $edition) {
        $ast => {}
        ast => panic!("{:?}: {} != {:#?}", $source, stringify!($ast), ast),
    }
}

// FIXME: macro_rules! in stmt pos (-> item not stmt); macro_rules! no binder == macro call
// FIXME: ops
// FIXME: structs in ifs etc.
// FIXME: almost-assoc-item-constraint due to (  )
// FIXME: exprs, pats
// FIXME: A bunch of negative behavior tests!
// FIXME: Add stmt `{ 0 } + 0` error, stmt `&{ 0 } + 0` ok but stmt `..{ 0 } + 0` err! etc.
//        More: `0 + { 0 } + 0` OK. stmt `{ 0 } || 0` err.
