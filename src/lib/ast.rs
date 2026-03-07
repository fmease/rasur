pub(crate) mod attr;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

use crate::span::{Span, Spanned};
use crate::token::Token;
pub(crate) use attr::*;
pub(crate) use expr::*;
pub(crate) use item::*;
pub(crate) use pat::*;
pub(crate) use path::*;
use std::fmt;
pub(crate) use stmt::*;
pub(crate) use ty::*;

#[derive(Debug)]
pub struct File<'src> {
    pub(crate) shebang: Option<&'src str>,
    pub(crate) frontmatter: Option<Frontmatter<'src>>,
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) items: Vec<Item<'src>>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub struct Frontmatter<'src> {
    pub infostring: Spanned<&'src str>,
    pub content: Spanned<&'src str>,
    pub span: Span,
}

pub(crate) struct MacroCall<'src, M: GenericArgsMode> {
    pub(crate) path: Path<'src, M>,
    pub(crate) bracket: Bracket,
    pub(crate) stream: TokenStream,
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
pub(crate) struct Lit<'src> {
    #[cfg_attr(not(test), expect(dead_code))]
    pub(crate) kind: LitKind,
    pub(crate) value: &'src str,
    pub(crate) suffix: Option<&'src str>,
}

#[derive(Debug)]
pub(crate) enum LitKind {
    Bool,
    Char,
    Num,
    Str,
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
