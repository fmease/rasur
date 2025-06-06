pub(crate) use crate::lexer::{Token, TokenKind};
use crate::span::Span;
pub(crate) use attr::*;
pub(crate) use expr::*;
pub(crate) use item::*;
pub(crate) use pat::*;
pub(crate) use path::*;
pub(crate) use stmt::*;
pub(crate) use ty::*;

mod attr;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

#[derive(Debug)]
pub(crate) struct File<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) items: Vec<Item<'src>>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) struct MacroCall<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) path: Path<'src, A>,
    pub(crate) bracket: Bracket,
    pub(crate) stream: TokenStream,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Bracket {
    Round,
    Square,
    Curly,
}

pub(crate) type TokenStream = Vec<Token>;

#[derive(Clone, Copy)]
pub(crate) enum Orientation {
    Open,
    Close,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Mut,
    Not,
}
