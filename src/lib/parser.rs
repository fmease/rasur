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
    lexer::Tokens,
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
    source: &'src str,
    edition: Edition,
    store: &'sto Store,
) -> Result<ast::File<'src>> {
    let tokens = prepare(tokens);
    let mut p = Parser::new(&tokens, source, edition, store);
    p.parse_file()
}

pub fn prepare(tokens: Tokens<'_, '_>) -> Vec<Token> {
    tokens
        .filter(|token| match token.kind {
            TokenKind::Comment | TokenKind::Error | TokenKind::Whitespace => false,
            _ => true,
        })
        .collect()
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
