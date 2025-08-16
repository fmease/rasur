use crate::span::Span;
use crate::token::Token;
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
pub(crate) struct MacroCall<'src, M: GenericArgsMode> {
    pub(crate) path: Path<'src, M>,
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

#[derive(Debug)]
pub(crate) enum Lit<'src> {
    Bool(bool),
    // FIXME: char
    Char(&'src str),
    Num(&'src str),
    Str(&'src str),
}
