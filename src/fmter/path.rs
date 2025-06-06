use super::{Cx, Fmt, Punctuated, fmt};
use crate::ast;

impl<A: FmtGenericArgs> Fmt for ast::Path<'_, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { segs } = self;
        Punctuated::new(segs, "::").fmt(cx);
    }
}

impl<A: FmtGenericArgs> Fmt for ast::PathSeg<'_, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args } = self;

        fmt!(cx, "{ident}");
        A::fmt(args, cx);
    }
}

impl<A: FmtGenericArgs> Fmt for ast::ExtPath<'_, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { self_ty, mut path } = self;
        if let Some(self_ty) = self_ty {
            fmt!(cx, "<");
            self_ty.ty.fmt(cx);
            let segs = path.segs.split_off(self_ty.offset);
            if !path.segs.is_empty() {
                fmt!(cx, " as ");
                path.fmt(cx);
            }
            fmt!(cx, ">::");
            ast::Path { segs }.fmt(cx);
        } else {
            path.fmt(cx);
        }
    }
}

pub(super) trait FmtGenericArgs: ast::GenericArgsPolicy::Kind {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>);
}

impl FmtGenericArgs for ast::GenericArgsPolicy::Forbidden {
    fn fmt((): Self::Args<'_>, _: &mut Cx<'_>) {}
}

impl FmtGenericArgs for ast::GenericArgsPolicy::Allowed {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        args.fmt(cx);
    }
}

impl FmtGenericArgs for ast::GenericArgsPolicy::DisambiguatedOnly {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        if let Some(args) = args {
            let is_empty = match &args {
                ast::GenericArgs::Angle(args) => args.is_empty(),
                ast::GenericArgs::Paren { .. } | ast::GenericArgs::ParenElided => true,
            };
            if !is_empty {
                fmt!(cx, "::");
            }
            args.fmt(cx);
        }
    }
}

impl Fmt for ast::GenericArgs<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Angle(args) => args.fmt(cx),
            Self::Paren { inputs, output } => {
                fmt!(cx, "(");
                Punctuated::new(inputs, ", ").fmt(cx);
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
        if !self.is_empty() {
            fmt!(cx, "<");
            Punctuated::new(self, ", ").fmt(cx);
            fmt!(cx, ">");
        }
    }
}

impl Fmt for ast::AngleGenericArg<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Argument(arg) => arg.fmt(cx),
            Self::Constraint(constraint) => constraint.fmt(cx),
        }
    }
}

impl Fmt for ast::GenericArg<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Ty(ty) => ty.fmt(cx),
            Self::Const(expr) => expr.fmt(cx),
            Self::Lifetime(lt) => lt.fmt(cx),
        }
    }
}

impl Fmt for ast::AssocItemConstraint<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args, kind } = self;
        fmt!(cx, "{ident}");
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
