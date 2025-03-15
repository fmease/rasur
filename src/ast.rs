// FIXME: Create newtype for idents!
#![expect(dead_code)] // FIXME 

use crate::lexer::TokenKind;

#[derive(Debug)]
pub(crate) struct File<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) items: Vec<Item<'src>>,
}

#[derive(Debug)]
pub(crate) struct Item<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) kind: ItemKind<'src>,
}

#[derive(Debug)]
pub(crate) enum ItemKind<'src> {
    Fn(Fn<'src>),
    Mod(Mod<'src>),
    Struct(Struct<'src>),
}

#[derive(Debug)]
pub(crate) struct Fn<'src> {
    pub(crate) name: &'src str,
    pub(crate) generics: Generics<'src>,
    pub(crate) params: Vec<Param<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct Mod<'src> {
    pub(crate) name: &'src str,
    pub(crate) items: Option<Vec<Item<'src>>>,
}

#[derive(Debug)]
pub(crate) struct Struct<'src> {
    pub(crate) name: &'src str,
    pub(crate) generics: Generics<'src>,
    pub(crate) body: StructBody<'src>,
}

#[derive(Debug)]
pub(crate) enum StructBody<'src> {
    // FIXME: Better name for this
    Normal { fields: Vec<(&'src str, Ty<'src>)> },
    Unit,
}

#[derive(Debug)]
pub(crate) struct Generics<'src> {
    pub(crate) params: Vec<GenParam<'src>>,
}

#[derive(Debug)]
pub(crate) struct GenParam<'src> {
    pub(crate) name: &'src str,
}

#[derive(Debug)]
pub(crate) struct Param<'src> {
    pub(crate) name: &'src str,
    pub(crate) ty: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    Ident(&'src str),
    NumLit(&'src str),
    StrLit(&'src str),
    Block(Box<BlockExpr<'src>>),
}

#[derive(Debug)]
pub(crate) struct BlockExpr<'src> {
    pub(crate) expr: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Ident(&'src str),
}

#[derive(Debug)]
pub(crate) struct Attr<'src> {
    pub(crate) path: Path<'src>,
    pub(crate) kind: AttrKind<'src>,
}

#[derive(Debug)]
pub(crate) enum AttrKind<'src> {
    Unit,
    Call(Bracket, TokenStream),
    Assign(Expr<'src>),
}

pub(crate) type TokenStream = Vec<TokenKind>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(Debug)]
pub(crate) struct Path<'src> {
    pub(crate) locality: PathLocality,
    pub(crate) segs: Vec<&'src str>,
}

#[derive(Debug)]
pub(crate) enum PathLocality {
    Global,
    Local,
}
