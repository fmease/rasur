use super::{Cx, Fmt, fmt};
use crate::ast;

impl Fmt for ast::Stmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Item(item) => item.fmt(cx),
            Self::Let(stmt) => stmt.fmt(cx),
            Self::Expr(expr, semi) => {
                let needs_semi =
                    matches!(semi, ast::Semicolon::Yes if expr.kind.needs_semicolon_as_stmt());
                expr.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
            Self::Empty => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::LetStmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, ty, body } = self;

        fmt!(cx, "let ");
        pat.fmt(cx);
        if let Some(ty) = ty {
            fmt!(cx, ": ");
            ty.fmt(cx);
        }
        if let Some((body, alternate)) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
            if let Some(alternate) = alternate {
                fmt!(cx, " else ");
                alternate.fmt(cx);
            }
        }
        fmt!(cx, ";");
    }
}
