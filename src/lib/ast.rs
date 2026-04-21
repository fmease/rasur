mod attr;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

use crate::span::Span;
pub use crate::token::{Token, TokenKind};
pub use attr::*;
pub use expr::*;
pub use item::*;
pub use pat::*;
pub use path::*;
use std::fmt;
pub use stmt::*;
pub use ty::*;

pub type TokenStream = Vec<crate::token::Token>;

pub struct MacroCall<'src, M: GenericArgsMode> {
    pub path: Path<'src, M>,
    pub bracket: Bracket,
    pub stream: TokenStream,
}

impl<M: GenericArgsMode> fmt::Debug for MacroCall<'_, M> {
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
pub enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Orientation {
    Open,
    Close,
}

#[derive(Debug)]
pub struct Lit<'src> {
    pub kind: LitKind,
    pub value: &'src str,
    pub suffix: Option<&'src str>,
}

#[derive(Debug)]
pub enum LitKind {
    Bool,
    Char,
    Num,
    Str,
}

#[derive(Debug, Clone, Copy)]
pub enum Mut {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug)]
pub enum BorrowKind<X = ()> {
    Pin,
    Ref,
    Raw(X),
}

#[derive_const(Default)]
#[derive(Clone, Copy, Debug)]
pub enum Safety<X = !> {
    #[default]
    Inherited,
    Safe(X),
    Unsafe,
}

#[derive_const(Default)]
#[derive(Debug)]
pub enum Extern<'src> {
    Yes(Option<&'src str>),
    #[default]
    No,
}
