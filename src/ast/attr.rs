use super::{Bracket, Expr, NoGenericArgs, Path, Safety, TokenStream};

#[derive(Debug)]
pub(crate) struct Attr<'src> {
    pub(crate) style: AttrStyle,
    pub(crate) safety: Safety,
    pub(crate) path: Path<'src, NoGenericArgs>,
    pub(crate) kind: AttrKind<'src>,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) enum AttrStyle {
    Inner,
    Outer,
}

#[derive(Debug)]
pub(crate) enum AttrKind<'src> {
    Unit,
    Call(Bracket, TokenStream),
    Assign(Expr<'src>),
}
