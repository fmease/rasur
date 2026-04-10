use super::{Bracket, Expr, NoGenericArgs, Path, Safety, TokenStream};
use crate::span::Span;
use std::fmt;

pub struct Attr<'src, M: AttrMode = AnyAttrStyle> {
    pub style: M::Style,
    pub kind: AttrKind<'src>,
}

impl<'src> Attr<'src, OuterAttrStyle> {
    pub fn upcast(self) -> Attr<'src> {
        Attr { style: AttrStyle::Outer, ..self }
    }
}

impl<'src> Attr<'src, InnerAttrStyle> {
    pub fn upcast(self) -> Attr<'src> {
        Attr { style: AttrStyle::Inner, ..self }
    }
}

impl<M: AttrMode> fmt::Debug for Attr<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { style, kind } = self;

        f.debug_struct("Attr").field("style", style).field("kind", kind).finish()
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum AttrStyle {
    Inner,
    Outer,
}

#[derive(Debug)]
pub enum AttrKind<'src> {
    Normal(NormalAttr<'src>),
    DocComment(Span),
}

#[derive(Debug)]
pub struct NormalAttr<'src> {
    pub safety: Safety,
    pub path: Path<'src, NoGenericArgs>,
    pub args: AttrArgs<'src>,
}

#[derive(Debug)]
pub enum AttrArgs<'src> {
    Unit,
    Call(Bracket, TokenStream),
    Assign(Expr<'src>),
}

pub trait AttrMode {
    type Style: fmt::Debug;
}

pub enum AnyAttrStyle {}
pub enum InnerAttrStyle {}
pub enum OuterAttrStyle {}

impl AttrMode for AnyAttrStyle {
    type Style = AttrStyle;
}

impl AttrMode for InnerAttrStyle {
    type Style = ();
}

impl AttrMode for OuterAttrStyle {
    type Style = ();
}

pub trait AttrsExt<'src> {
    fn partition(self) -> (Vec<Attr<'src, OuterAttrStyle>>, Vec<Attr<'src, InnerAttrStyle>>);
}

impl<'src> AttrsExt<'src> for Vec<Attr<'src>> {
    fn partition(self) -> (Vec<Attr<'src, OuterAttrStyle>>, Vec<Attr<'src, InnerAttrStyle>>) {
        let mut outer_attrs = Vec::new();
        let mut inner_attrs = Vec::new();

        for attr in self {
            match attr.style {
                AttrStyle::Inner => inner_attrs.push(Attr { style: (), ..attr }),
                AttrStyle::Outer => outer_attrs.push(Attr { style: (), ..attr }),
            }
        }

        (outer_attrs, inner_attrs)
    }
}
