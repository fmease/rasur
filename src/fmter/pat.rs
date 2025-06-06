use super::{Cx, Fmt, Tup, fmt};
use crate::ast;

impl Fmt for ast::Pat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Wildcard => fmt!(cx, "_"),
            Self::NumLit(lit) => fmt!(cx, "{lit}"),
            Self::StrLit(lit) => fmt!(cx, "{lit:?}"),
            Self::Borrow(mut_, pat) => {
                fmt!(cx, "&");
                match mut_ {
                    ast::Mutable::Yes => fmt!(cx, "mut "),
                    ast::Mutable::No => {}
                }
                pat.fmt(cx);
            }
            Self::Tup(pats) => Tup(pats).fmt(cx),
            Self::Grouped(pat) => {
                fmt!(cx, "(");
                pat.fmt(cx);
                fmt!(cx, ")");
            }
            Self::MacroCall(call) => call.fmt(cx),
        }
    }
}
