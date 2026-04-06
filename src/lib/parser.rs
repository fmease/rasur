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
    error::Error,
    lexer::{Frontmatter, Tokens},
    span::Span,
    store::Store,
    token::{Token, TokenKind},
};
pub use base::Parser;
use base::{MatchAgainstArbitraryToken, TokenCategory, TokenPrefix};

type Result<T, E = ()> = std::result::Result<T, E>;

#[expect(clippy::missing_errors_doc)] // FIXME: TODO
#[expect(clippy::result_unit_err)] // handled via an out-parameter
pub fn parse<'sto, 'src>(
    tokens: Tokens<'sto, 'src>,
    shebang: Option<Span>,
    frontmatter: Option<Frontmatter>,
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::File<'src>> {
    let tokens = prepare(tokens);
    let mut p = Parser::new(&tokens, source, edition, store);
    let file = p.parse_file()?;
    Ok(file.lower(shebang, frontmatter, &p))
}

pub fn prepare(tokens: Tokens<'_, '_>) -> Vec<Token> {
    tokens
        .filter(|token| match token.kind {
            TokenKind::Comment | TokenKind::Error | TokenKind::Whitespace => false,
            _ => true,
        })
        .collect()
}

macro one_of($( $frag:expr ),+ $(,)?) {
    ExpectedFragment::OneOf(Box::new([$( ExpectedFragment::from($frag) ),+]))
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub enum ExpectedFragment {
    Bound,
    CommonIdent,
    ConstArg,
    Expr,
    ExtPath,
    GenericArg,
    GenericParam,
    Item,
    Lit,
    OneOf(Box<[Self]>),
    Pat,
    PathSegIdent,
    Predicate,
    Stmt,
    Term,
    Token(TokenKind),
    Ty,
}

impl From<TokenKind> for ExpectedFragment {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}
