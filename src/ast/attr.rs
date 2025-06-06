use super::{Bracket, Expr, GenericArgsPolicy, Path, TokenStream};

#[derive(Debug)]
pub(crate) struct Attr<'src> {
    pub(crate) style: AttrStyle,
    pub(crate) path: Path<'src, GenericArgsPolicy::Forbidden>,
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
