use super::{Cx, Fmt, Punctuated, TrailingSpaceExt as _, Tup, fmt};
use crate::ast;

impl Fmt for ast::Pat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Ident(ident) => ident.fmt(cx),
            Self::Wildcard => fmt!(cx, "_"),
            Self::Lit(lit) => lit.fmt(cx),
            Self::Borrow(mut_, pat) => {
                fmt!(cx, "&");
                mut_.trailing_space().fmt(cx);
                pat.fmt(cx);
            }
            Self::Tup(pats) => Tup(pats).fmt(cx),
            Self::Grouped(pat) => {
                fmt!(cx, "(");
                pat.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Path(path) => path.fmt(cx),
            Self::MacroCall(call) => call.fmt(cx),
            Self::TupleStruct(pat) => pat.fmt(cx),
            Self::Struct(pat) => pat.fmt(cx),
            // FIXME: Eliminate unnecessary parens.
            Self::Or(left, right) => {
                fmt!(cx, "(");
                left.fmt(cx);
                fmt!(cx, ") | (");
                right.fmt(cx);
                fmt!(cx, ")");
            }

            Self::Range(left, right, kind) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                if let Some(left) = left {
                    fmt!(cx, "(");
                    left.fmt(cx);
                    fmt!(cx, ")");
                }
                let symbol = match kind {
                    ast::RangePatKind::Exclusive => "..",
                    ast::RangePatKind::Inclusive(ast::RangeInclusivePatKind::Normal) => "..=",
                    ast::RangePatKind::Inclusive(ast::RangeInclusivePatKind::Legacy) => "...",
                };
                fmt!(cx, "{symbol}");
                if let Some(right) = right {
                    fmt!(cx, "(");
                    right.fmt(cx);
                    fmt!(cx, ")");
                }
            }
        }
    }
}

impl Fmt for ast::IdentPat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { mut_, by_ref, ident } = self;

        mut_.trailing_space().fmt(cx);

        match by_ref {
            ast::ByRef::Yes(mut_) => {
                fmt!(cx, "ref ");
                mut_.trailing_space().fmt(cx);
            }
            ast::ByRef::No => {}
        }

        fmt!(cx, "{ident}");
    }
}

impl Fmt for ast::TupleStructPat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, fields } = self;

        path.fmt(cx);
        fmt!(cx, "(");
        Punctuated::new(fields, ", ").fmt(cx);
        fmt!(cx, ")");
    }
}

impl Fmt for ast::StructPat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, fields } = self;

        path.fmt(cx);
        fmt!(cx, " {{ ");
        Punctuated::new(fields, ", ").fmt(cx);
        fmt!(cx, " }}");
    }
}

impl Fmt for ast::StructPatField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, body } = self;

        fmt!(cx, "{binder}");
        if let Some(body) = body {
            fmt!(cx, ": ");
            body.fmt(cx);
        }
    }
}
