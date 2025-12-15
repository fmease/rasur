use super::{Cx, Fmt, Punctuated, TrailingSpace, TrailingSpaceExt as _, Tup, fmt};
use crate::ast;

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::CVariadics => fmt!(cx, "..."),
            Self::Path(path) => path.fmt(cx),
            Self::Inferred => fmt!(cx, "_"),
            Self::FnPtr(ty) => ty.fmt(cx),
            Self::Ref(ty) => ty.fmt(cx),
            Self::Ptr(mut_, ty) => {
                fmt!(cx, "*");
                match mut_ {
                    ast::Mutability::Mut => fmt!(cx, "mut "),
                    ast::Mutability::Not => fmt!(cx, "const "),
                }
                ty.fmt(cx);
            }
            Self::Never => fmt!(cx, "!"),
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
            Self::Tuple(tys) => Tup(tys).fmt(cx),
            Self::Grouped(ty) => {
                fmt!(cx, "(");
                ty.fmt(cx);
                fmt!(cx, ")");
            }
            Self::MacroCall(ty) => ty.fmt(cx),
            Self::Error => fmt!(cx, "/*error*/"),
            Self::UnsafeBinder(bound_vars, ty) => {
                fmt!(cx, "unsafe");
                bound_vars.fmt(cx);
                fmt!(cx, " ");
                ty.fmt(cx);
            }
        }
    }
}

impl Fmt for ast::FnPtrTy<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { bound_vars, modifiers, inputs, output } = self;

        if !bound_vars.is_empty() {
            fmt!(cx, "for");
            bound_vars.fmt(cx);
            fmt!(cx, " ");
        }

        modifiers.trailing_space().fmt(cx);

        fmt!(cx, "fn(");
        Punctuated::new(inputs, ", ").fmt(cx);
        fmt!(cx, ")");
        if let Some(output) = output {
            fmt!(cx, " -> ");
            output.fmt(cx);
        }
    }
}

impl Fmt for TrailingSpace<ast::FnPtrTyModifiers<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(ast::FnPtrTyModifiers { safety, externness }) = self;

        safety.trailing_space().fmt(cx);
        externness.trailing_space().fmt(cx);
    }
}

impl Fmt for ast::RefTy<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { lt, kind, mut_, pointee } = self;

        fmt!(cx, "&");
        if let Some(lt) = lt {
            lt.fmt(cx);
            fmt!(cx, " ");
        }
        kind.trailing_space().fmt(cx);
        mut_.trailing_space().fmt(cx);
        if let (ast::BorrowKind::Pin, ast::Mutability::Not) = (kind, mut_) {
            fmt!(cx, "const ");
        }
        pointee.fmt(cx);
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

        if !params.is_empty() {
            params.fmt(cx);
        }
        preds.fmt(cx);
    }
}

impl Fmt for Vec<ast::GenericParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "<");
        Punctuated::new(self, ", ").fmt(cx);
        fmt!(cx, ">");
    }
}

impl Fmt for ast::GenericParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, binder, kind } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        match kind {
            ast::GenericParamKind::Ty { bounds, default } => {
                fmt!(cx, "{binder}");
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
                fmt!(cx, "const {binder}: ");
                ty.fmt(cx);
                if let Some(expr) = default {
                    fmt!(cx, " = ");
                    expr.fmt(cx);
                }
            }
            ast::GenericParamKind::Lifetime(bounds) => {
                ast::Lifetime(binder).fmt(cx);
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
        let Self { attrs, kind } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        kind.fmt(cx);
    }
}

impl Fmt for ast::PredicateKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(ast::TraitPredicate { bound_vars, ty, bounds }) => {
                if !bound_vars.is_empty() {
                    fmt!(cx, "for");
                    bound_vars.fmt(cx);
                    fmt!(cx, " ");
                }

                ty.fmt(cx);
                fmt!(cx, ":");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::Outlives(ast::OutlivesPredicate { lt, bounds }) => {
                lt.fmt(cx);
                fmt!(cx, ":");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                Punctuated::new(bounds, " + ").fmt(cx);
            }
            Self::Equality(lhs, rhs) => {
                lhs.fmt(cx);
                fmt!(cx, " == ");
                rhs.fmt(cx);
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
            Self::Outlives(lt) => lt.fmt(cx),
            Self::Use(captures) => {
                fmt!(cx, "use<");
                let mut captures = captures.into_iter();
                if let Some(capture) = captures.next() {
                    fmt!(cx, "{capture}");
                }
                for capture in captures {
                    fmt!(cx, ", {capture}");
                }
                fmt!(cx, ">");
            }
            Self::Trait { bound_vars, modifiers, trait_ref } => {
                if !bound_vars.is_empty() {
                    fmt!(cx, "for");
                    bound_vars.fmt(cx);
                    fmt!(cx, " ");
                }
                modifiers.fmt(cx);
                trait_ref.fmt(cx);
            }
        }
    }
}

impl Fmt for ast::TraitBoundModifiers {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { constness, asyncness, polarity } = self;

        match constness {
            ast::BoundConstness::Never => {}
            ast::BoundConstness::Maybe => fmt!(cx, "[const] "),
            ast::BoundConstness::Always => fmt!(cx, "const "),
        }

        match asyncness {
            ast::BoundAsyncness::Never => {}
            ast::BoundAsyncness::Always => fmt!(cx, "async "),
        }

        match polarity {
            ast::BoundPolarity::Positive => {}
            ast::BoundPolarity::Negative => fmt!(cx, "!"),
            ast::BoundPolarity::Maybe => fmt!(cx, "?"),
        }
    }
}
