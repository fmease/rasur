pub use crate::token::TokenKind;

pub type TokenStream<'src> = Vec<Token<'src>>;

#[derive(Clone, Copy, Debug)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub source: &'src str,
}
