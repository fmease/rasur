use crate::span::Span;
use crate::token::Token;
pub(crate) use attr::*;
pub(crate) use expr::*;
pub(crate) use item::*;
pub(crate) use pat::*;
pub(crate) use path::*;
use std::fmt;
pub(crate) use stmt::*;
pub(crate) use ty::*;

pub(crate) mod attr;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

#[derive(Debug)]
pub struct File<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) items: Vec<Item<'src>>,
    pub(crate) span: Span,
}

pub(crate) struct MacroCall<'src, M: GenericArgsMode> {
    pub(crate) path: Path<'src, M>,
    pub(crate) bracket: Bracket,
    pub(crate) stream: TokenStream,
}

impl<'src, M: GenericArgsMode> fmt::Debug for MacroCall<'src, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { path, bracket, stream } = self;

        f.debug_struct("MacroCall")
            .field("path", path)
            .field("bracket", bracket)
            .field("stream", stream)
            .finish()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Bracket {
    Round,
    Square,
    Curly,
}

pub(crate) type TokenStream = Vec<Token>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Orientation {
    Open,
    Close,
}

#[derive(Debug)]
pub(crate) enum Lit<'src> {
    Bool(bool),
    // FIXME: char
    Char(&'src str),
    Num(&'src str),
    Str(&'src str),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Mut,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum BorrowKind<X = ()> {
    Pin,
    Ref,
    Raw(X),
}

#[derive_const(Default)]
#[derive(Clone, Copy, Debug)]
pub(crate) enum Safety<X = !> {
    #[default]
    Inherited,
    Safe(X),
    Unsafe,
}

#[derive_const(Default)]
#[derive(Debug)]
pub(crate) enum Externness<'src> {
    Extern(Option<&'src str>),
    #[default]
    Not,
}
