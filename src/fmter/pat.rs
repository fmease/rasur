use super::{Cx, Fmt, Punctuated, TrailingSpace, TrailingSpaceExt as _, Tup, fmt};
use crate::ast;

// FIXME: Don't print unnecessary parens & properly respect precedence.
impl Fmt for ast::Pat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Binding(binding) => binding.fmt(cx),
            Self::Box(pat) => {
                fmt!(cx, "box ");
                pat.fmt(cx);
            }
            // If the caller wants to treat `WildcardKind::Empty` special, they should do it themself.
            Self::Wildcard(_) => fmt!(cx, "_"),
            Self::Lit(sign, lit) => {
                sign.fmt(cx);
                lit.fmt(cx)
            }
            Self::Borrow(mut_, pat) => {
                fmt!(cx, "&");
                mut_.trailing_space().fmt(cx);
                pat.fmt(cx);
            }
            Self::Tuple(pats) => Tup(pats).fmt(cx),
            Self::Grouped(pat) => {
                fmt!(cx, "(");
                pat.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Path(path) => path.fmt(cx),
            Self::MacroCall(call) => call.fmt(cx),
            Self::TupleStruct(pat) => pat.fmt(cx),
            Self::Struct(pat) => pat.fmt(cx),
            Self::Or(left, right) => {
                fmt!(cx, "(");
                left.fmt(cx);
                fmt!(cx, ") | (");
                right.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Range(left, right, kind) => {
                left.fmt(cx);
                let symbol = match kind {
                    ast::RangePatKind::Exclusive => "..",
                    ast::RangePatKind::Inclusive(ast::RangeInclusivePatKind::Normal) => "..=",
                    ast::RangePatKind::Inclusive(ast::RangeInclusivePatKind::Legacy) => "...",
                };
                fmt!(cx, "{symbol}");
                right.fmt(cx);
            }
            Self::Slice(elems) => {
                fmt!(cx, "[");
                Punctuated::new(elems, ", ").fmt(cx);
                fmt!(cx, "]");
            }
            Self::Never => fmt!(cx, "!"),
        }
    }
}

impl Fmt for ast::BindingPat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { mut_, by_ref, binder, pat } = self;

        mut_.trailing_space().fmt(cx);
        by_ref.trailing_space().fmt(cx);
        fmt!(cx, "{binder}");

        if let Some(pat) = pat {
            fmt!(cx, " @ ");
            pat.fmt(cx);
        }
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
        let Self { path, fields, rest } = self;
        let non_empty = !fields.is_empty();

        path.fmt(cx);
        fmt!(cx, " {{");
        if non_empty {
            fmt!(cx, " ");
        }
        Punctuated::new(fields, ", ").fmt(cx);
        if rest {
            if non_empty {
                fmt!(cx, ", ");
            }
            fmt!(cx, "..");
        }
        if non_empty {
            fmt!(cx, " ");
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::StructPatField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, mut_, by_ref, binder, body } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        mut_.trailing_space().fmt(cx);
        by_ref.trailing_space().fmt(cx);
        fmt!(cx, "{binder}");
        if let Some(body) = body {
            fmt!(cx, ": ");
            body.fmt(cx);
        }
    }
}

impl Fmt for ast::RangePatBound<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Lit(sign, lit) => {
                sign.fmt(cx);
                lit.fmt(cx);
            }
            Self::Path(path) => path.fmt(cx),
        }
    }
}

impl Fmt for ast::Sign {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::None => {}
            Self::Neg => fmt!(cx, "-"),
        }
    }
}

impl Fmt for TrailingSpace<ast::ByRef> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(by_ref) = self;

        match by_ref {
            ast::ByRef::Yes(mut_) => {
                fmt!(cx, "ref ");
                mut_.trailing_space().fmt(cx);
            }
            ast::ByRef::No => {}
        }
    }
}
