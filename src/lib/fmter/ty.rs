use super::{Cx, Fmt, InterleaveExt as _, TrailingSpace, TrailingSpaceExt as _, Tup, fmt};
use crate::{ast, fmter::BuiltinSyntax, lexer::lex_ident, token::TokenKind};

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::All => fmt!(cx, ".."),
            Self::CVariadics => fmt!(cx, "..."),
            Self::Path(path) => path.fmt(cx),
            Self::Inferred => fmt!(cx, "_"),
            Self::FnPtr(ty) => ty.fmt(cx),
            Self::Ref(ty) => ty.fmt(cx),
            Self::Ptr(mut_, ty) => {
                fmt!(cx, "*");
                match mut_ {
                    ast::Mut::Yes => fmt!(cx, "mut "),
                    ast::Mut::No => fmt!(cx, "const "),
                }
                ty.fmt(cx);
            }
            Self::Never => fmt!(cx, "!"),
            Self::DynTrait(kind, bounds) => {
                match kind {
                    ast::DynKind::Dyn => {
                        fmt!(cx, "dyn");
                        if !bounds.is_empty() {
                            fmt!(cx, " ");
                        }
                    }
                    ast::DynKind::Bare => {}
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
            // If the caller wants to treat `ImplicitSelf` special, they should do it themself.
            Self::ImplicitSelf => fmt!(cx, "Self"),
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
            Self::Error(span) => fmt!(cx, "{}", cx.source(span)),
            Self::UnsafeBinder(bound_vars, ty) => {
                fmt!(cx, "unsafe");
                bound_vars.fmt(cx);
                fmt!(cx, " ");
                ty.fmt(cx);
            }
            Self::FieldOf(ty, variant, field) => BuiltinSyntax("field_of", |cx| {
                ty.fmt(cx);
                fmt!(cx, ", ");
                if let Some(variant) = variant {
                    variant.fmt(cx);
                    fmt!(cx, ".");
                }
                field.fmt(cx);
            })
            .fmt(cx),
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
        inputs.interleave(", ").fmt(cx);
        fmt!(cx, ")");
        if let Some(output) = output {
            fmt!(cx, " -> ");
            output.fmt(cx);
        }
    }
}

impl Fmt for TrailingSpace<ast::FnPtrTyModifiers<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(ast::FnPtrTyModifiers { safety, r#extern }) = self;

        safety.trailing_space().fmt(cx);
        r#extern.trailing_space().fmt(cx);
    }
}

impl Fmt for ast::RefTy<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { lt, kind, mut_, pointee } = self;

        fmt!(cx, "&");
        lt.trailing_space().fmt(cx);
        (kind, mut_).trailing_space().fmt(cx);
        pointee.fmt(cx);
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
        self.interleave(", ").fmt(cx);
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
                binder.fmt(cx);
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
                fmt!(cx, "const ");
                binder.fmt(cx);
                fmt!(cx, ": ");
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
                    bounds.interleave(" + ").fmt(cx);
                }
            }
        }
    }
}

// FIXME: LeadingSpace<_>
impl Fmt for Vec<ast::Predicate<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if self.is_empty() {
            return;
        }
        fmt!(cx, " where ");
        self.interleave(", ").fmt(cx);
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
                bounds.interleave(" + ").fmt(cx);
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
        self.interleave(" + ").fmt(cx);
    }
}

impl Fmt for ast::Bound<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Outlives(lt) => lt.fmt(cx),
            Self::Use(captures) => {
                fmt!(cx, "use<");
                captures.interleave(", ").fmt(cx);
                fmt!(cx, ">");
            }
            Self::Trait { bound_vars, modifiers, path } => {
                if !bound_vars.is_empty() {
                    fmt!(cx, "for");
                    bound_vars.fmt(cx);
                    fmt!(cx, " ");
                }
                modifiers.fmt(cx);
                path.fmt(cx);
            }
        }
    }
}

impl Fmt for ast::Capture<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Lifetime(lt) => lt.fmt(cx),
            Self::TyOrConst(param) => param.fmt(cx),
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

impl Fmt for ast::Lifetime<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(ast::Ident { name, span: _ }) = self;

        fmt!(cx, "'");
        match lex_ident(name, cx.edition) {
            TokenKind::CommonIdent | TokenKind::Static | TokenKind::Underscore => {}
            _ => fmt!(cx, "r#"),
        }
        name.fmt(cx);
    }
}
