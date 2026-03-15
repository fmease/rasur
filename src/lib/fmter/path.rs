use super::{Cx, Fmt, InterleaveExt as _, fmt};
use crate::{ast, lexer::lex_ident, token::TokenKind};

impl<M: GenericArgsMode> Fmt for ast::Path<'_, M> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { segs } = self;

        segs.interleave("::").fmt(cx);
    }
}

impl<M: GenericArgsMode> Fmt for ast::PathSeg<'_, M> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args } = self;

        ident.fmt(cx);
        M::fmt(args, cx);
    }
}

impl Fmt for ast::Ident<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { name, span: _ } = self;

        match lex_ident(name, cx.edition) {
            TokenKind::CommonIdent | TokenKind::Underscore => {}
            _ => fmt!(cx, "r#"),
        }
        fmt!(cx, "{name}");
    }
}

impl<S: GenericArgsStyle> Fmt for ast::ExtPath<'_, S> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ext, path } = self;

        ext.fmt(cx);
        path.fmt(cx);
    }
}

impl Fmt for ast::PathExt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { self_ty, trait_ref } = self;

        fmt!(cx, "<");
        self_ty.fmt(cx);
        if let Some(trait_ref) = trait_ref {
            fmt!(cx, " as ");
            trait_ref.fmt(cx);
        }
        fmt!(cx, ">::");
    }
}

pub(super) trait GenericArgsMode: ast::GenericArgsMode {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>);
}

impl GenericArgsMode for ast::NoGenericArgs {
    fn fmt((): Self::Args<'_>, _: &mut Cx<'_>) {}
}

impl GenericArgsMode for ast::UnambiguousGenericArgs {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        args.fmt(cx);
    }
}

impl GenericArgsMode for ast::ObligatorilyDisambiguatedGenericArgs {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        if let Some(args) = args
            && !args.is_empty()
        {
            fmt!(cx, "::");
            args.fmt(cx);
        }
    }
}

pub(super) trait GenericArgsStyle: ast::GenericArgsStyle + GenericArgsMode {}

impl GenericArgsStyle for ast::UnambiguousGenericArgs {}
impl GenericArgsStyle for ast::ObligatorilyDisambiguatedGenericArgs {}

impl ast::GenericArgs<'_> {
    fn is_empty(&self) -> bool {
        match self {
            Self::Angle(args) => args.is_empty(),
            Self::Paren(inputs, output) => inputs.is_empty() && output.is_none(),
            Self::ParenElided => false,
        }
    }
}

impl Fmt for ast::GenericArgs<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Angle(args) => args.fmt(cx),
            Self::Paren(inputs, output) => {
                fmt!(cx, "(");
                inputs.interleave(", ").fmt(cx);
                fmt!(cx, ")");
                if let Some(output) = output {
                    fmt!(cx, " -> ");
                    output.fmt(cx);
                }
            }
            Self::ParenElided => fmt!(cx, "(..)"),
        }
    }
}

impl Fmt for Vec<ast::AngleGenericArg<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "<");
        self.interleave(", ").fmt(cx);
        fmt!(cx, ">");
    }
}

impl Fmt for ast::AngleGenericArg<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Const(expr) => expr.fmt(cx),
            Self::Constraint(constraint) => constraint.fmt(cx),
            Self::Lifetime(lt) => lt.fmt(cx),
            Self::Ty(ty) => ty.fmt(cx),
        }
    }
}

impl Fmt for ast::AssocItemConstraint<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args, kind } = self;

        ident.fmt(cx);
        args.fmt(cx);
        kind.fmt(cx);
    }
}

impl Fmt for ast::AssocItemConstraintKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Equality(term) => {
                fmt!(cx, " = ");
                term.fmt(cx);
            }
            Self::Bound(bounds) => {
                fmt!(cx, ": ");
                bounds.fmt(cx);
            }
        }
    }
}

impl Fmt for ast::Term<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Ty(ty) => ty.fmt(cx),
            Self::Const(expr) => expr.fmt(cx),
        }
    }
}
