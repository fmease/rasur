use super::{Cx, Fmt, Punctuated, TrailingSpaceExt as _, Tup, fmt};
use crate::ast;

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Inferred => fmt!(cx, "_"),
            Self::FnPtr((), ret_ty) => {
                fmt!(cx, "fn()");
                if let Some(ret_ty) = ret_ty {
                    fmt!(cx, " -> ");
                    ret_ty.fmt(cx);
                }
            }
            Self::Ref(lt, mut_, ty) => {
                fmt!(cx, "&");
                if let Some(lt) = lt {
                    lt.fmt(cx);
                    fmt!(cx, " ");
                }
                mut_.trailing_space().fmt(cx);
                ty.fmt(cx);
            }
            Self::Ptr(mut_, ty) => {
                fmt!(cx, "*");
                match mut_ {
                    ast::Mutability::Mut => fmt!(cx, "mut "),
                    ast::Mutability::Not => fmt!(cx, "const "),
                }
                ty.fmt(cx);
            }
            Self::Never => fmt!(cx, "!"),
            // FIXME: In Rust 2015 if `bounds.is_empty()`, you need to render it as `r#dyn`.
            Self::DynTrait(bounds) => {
                fmt!(cx, "dyn");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::ImplTrait(bounds) => {
                fmt!(cx, "impl");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::Array(ty, expr) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "; ");
                expr.fmt(cx);
                fmt!(cx, "]");
            }
            Self::Slice(ty) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "]");
            }
            Self::Tup(tys) => Tup(tys).fmt(cx),
            Self::Grouped(ty) => {
                fmt!(cx, "(");
                ty.fmt(cx);
                fmt!(cx, ")");
            }
            Self::MacroCall(ty) => ty.fmt(cx),
            Self::Error => fmt!(cx, "/*error*/"),
        }
    }
}

impl Fmt for ast::Lifetime<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(lt) = self;
        fmt!(cx, "{lt}");
    }
}

impl Fmt for ast::Generics<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { params, preds } = self;

        params.fmt(cx);
        preds.fmt(cx);
    }
}

impl Fmt for Vec<ast::GenericParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if !self.is_empty() {
            fmt!(cx, "<");
            Punctuated::new(self, ", ").fmt(cx);
            fmt!(cx, ">");
        }
    }
}

impl Fmt for ast::GenericParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self.kind {
            ast::GenericParamKind::Ty { bounds, default } => {
                fmt!(cx, "{}", self.binder);
                if !bounds.is_empty() {
                    fmt!(cx, ": ");
                    bounds.fmt(cx);
                }
                if let Some(ty) = default {
                    fmt!(cx, " = ");
                    ty.fmt(cx);
                }
            }
            ast::GenericParamKind::Const { ty, default } => {
                fmt!(cx, "const {}: ", self.binder);
                ty.fmt(cx);
                if let Some(expr) = default {
                    fmt!(cx, " = ");
                    expr.fmt(cx);
                }
            }
            ast::GenericParamKind::Lifetime(bounds) => {
                ast::Lifetime(self.binder).fmt(cx);
                if !bounds.is_empty() {
                    fmt!(cx, ": ");
                    Punctuated::new(bounds, " + ").fmt(cx);
                }
            }
        }
    }
}

impl Fmt for Vec<ast::Predicate<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if self.is_empty() {
            return;
        }
        fmt!(cx, " where ");
        Punctuated::new(self, ", ").fmt(cx);
    }
}

impl Fmt for ast::Predicate<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(pred) => {
                pred.ty.fmt(cx);
                fmt!(cx, ":");
                if !pred.bounds.is_empty() {
                    fmt!(cx, " ");
                }
                pred.bounds.fmt(cx);
            }
            Self::Outlives(pred) => {
                pred.lt.fmt(cx);
                fmt!(cx, ":");
                if !pred.bounds.is_empty() {
                    fmt!(cx, " ");
                }
                Punctuated::new(pred.bounds, " + ").fmt(cx);
            }
        }
    }
}

impl Fmt for Vec<ast::Bound<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        Punctuated::new(self, " + ").fmt(cx);
    }
}

impl Fmt for ast::Bound<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait { bound_vars, modifiers, trait_ref } => {
                if !bound_vars.is_empty() {
                    fmt!(cx, "for");
                    bound_vars.fmt(cx);
                    fmt!(cx, " ");
                }
                modifiers.fmt(cx);
                trait_ref.fmt(cx);
            }
            Self::Outlives(lt) => lt.fmt(cx),
        }
    }
}

impl Fmt for ast::TraitBoundModifiers {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { constness, polarity } = self;

        match constness {
            ast::BoundConstness::Never => {}
            ast::BoundConstness::Maybe => fmt!(cx, "[const] "),
            ast::BoundConstness::Always => fmt!(cx, "const "),
        }

        match polarity {
            ast::BoundPolarity::Positive => {}
            ast::BoundPolarity::Negative => fmt!(cx, "!"),
            ast::BoundPolarity::Maybe => fmt!(cx, "?"),
        }
    }
}
