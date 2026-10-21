mod attr;
mod base;
mod common;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod stream;
#[cfg(test)]
mod test;
mod ty;
mod weak;

use crate::{
    ast,
    edition::Edition,
    lexer::Tokens,
    store::Store,
    token::{Token, TokenKind},
};
pub use base::Parser;
use base::{MatchAgainstArbitraryToken, TokenCategory, TokenPrefix};

type Result<T, E = ()> = std::result::Result<T, E>;

#[expect(clippy::missing_errors_doc)] // FIXME: TODO
#[expect(clippy::result_unit_err)] // handled via an out-parameter
pub fn parse_file<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::File<'src>> {
    let tokens = prepare(tokens);
    let mut p = Parser::new(&tokens, source, edition, store);
    p.parse_file()
}

pub fn parse_expr<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::Expr<'src>> {
    parse_node(tokens, source, edition, store, |p| p.parse_expr())
}

pub fn parse_item<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::Item<'src>> {
    parse_node(tokens, source, edition, store, |p| p.parse_item(item::ItemCx::Boring))
}

pub fn parse_pat<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::Pat<'src>> {
    parse_node(tokens, source, edition, store, |p| p.parse_pat(pat::OrPolicy::Parse))
}

pub fn parse_stmt<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::Stmt<'src>> {
    parse_node(tokens, source, edition, store, |p| p.parse_stmt(TokenKind::EndOfInput))
}

pub fn parse_ty<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::Ty<'src>> {
    parse_node(tokens, source, edition, store, |p| p.parse_ty())
}

fn parse_node<'sto, 'src, T>(
    tokens: Tokens<'sto, 'src>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
    parse: impl FnOnce(&mut Parser<'_, 'sto, 'src>) -> Result<T>,
) -> Result<T> {
    let tokens = prepare(tokens);
    let mut p = Parser::new(&tokens, source, edition, store);
    let node = parse(&mut p)?;
    p.parse(TokenKind::EndOfInput)?;
    Ok(node)
}

pub fn prepare(tokens: Tokens<'_, '_>) -> Vec<Token> {
    tokens.filter(|t| !t.kind.is_trivia()).collect()
}

macro frags($( $frag:expr ),+ $(,)?) {
    utility::list1![$( Fragment::from($frag) ),+]
}

#[derive(Clone, Copy, Debug)]
pub enum Fragment {
    Bound,
    ConstArg,
    Expr,
    ExtPath,
    GenericArg,
    GenericParam,
    Item,
    Lit,
    Pat,
    PathSegIdent,
    Predicate,
    Stmt,
    Term,
    Token(TokenKind),
    Ty,
}

impl From<TokenKind> for Fragment {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}
