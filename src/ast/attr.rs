use super::{Bracket, Expr, NoGenericArgs, Path, Safety, TokenStream};
use crate::span::Span;
use std::fmt;

pub(crate) struct Attr<'src, M: AttrMode = Any> {
    pub(crate) style: M::Style,
    pub(crate) kind: AttrKind<'src>,
}

impl<'src> Attr<'src, Outer> {
    pub(crate) fn upcast(self) -> Attr<'src> {
        Attr { style: AttrStyle::Outer, ..self }
    }
}

impl<'src> Attr<'src, Inner> {
    pub(crate) fn upcast(self) -> Attr<'src> {
        Attr { style: AttrStyle::Inner, ..self }
    }
}

impl<'src> Attr<'src, Any> {
    // FIXME: Temporary API (until BlockExpr no longer stores inner attrs).
    pub(crate) fn downcast(self) -> Option<Attr<'src, Inner>> {
        match self.style {
            AttrStyle::Inner => Some(Attr { style: (), ..self }),
            AttrStyle::Outer => None,
        }
    }
}

impl<M: AttrMode> fmt::Debug for Attr<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { style, kind } = self;

        f.debug_struct("Attr").field("style", style).field("kind", kind).finish()
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) enum AttrStyle {
    Inner,
    Outer,
}

#[derive(Debug)]
pub(crate) enum AttrKind<'src> {
    Normal(NormalAttr<'src>),
    DocComment(Span),
}

#[derive(Debug)]
pub(crate) struct NormalAttr<'src> {
    pub(crate) safety: Safety,
    pub(crate) path: Path<'src, NoGenericArgs>,
    pub(crate) args: AttrArgs<'src>,
}

#[derive(Debug)]
pub(crate) enum AttrArgs<'src> {
    Unit,
    Call(Bracket, TokenStream),
    Assign(Expr<'src>),
}

pub(crate) trait AttrMode {
    type Style: fmt::Debug;
}

pub(crate) enum Any {}
pub(crate) enum Inner {}
pub(crate) enum Outer {}

impl AttrMode for Any {
    type Style = AttrStyle;
}

impl AttrMode for Inner {
    type Style = ();
}

impl AttrMode for Outer {
    type Style = ();
}

pub(crate) trait AttrsExt<'src> {
    fn partition(self) -> (Vec<Attr<'src, Outer>>, Vec<Attr<'src, Inner>>);
}

impl<'src> AttrsExt<'src> for Vec<Attr<'src>> {
    fn partition(self) -> (Vec<Attr<'src, Outer>>, Vec<Attr<'src, Inner>>) {
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
