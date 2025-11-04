use super::{Cx, Fmt, fmt};
use crate::ast;

impl Fmt for ast::Stmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Item(item) => item.fmt(cx),
            Self::Let(stmt) => stmt.fmt(cx),
            Self::Expr(expr, semi) => {
                let needs_semi = matches!(semi, ast::Semicolon::Yes if !expr.kind.is_boundary(ast::CurlyBracketedMacroCallIsBoundary::Yes));
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
        let Self { attrs, pat, ty, body } = self;

        // FIXME: Scan for & respect skip attr.

        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }

        fmt!(cx, "let ");
        pat.fmt(cx);
        if let Some(ty) = ty {
            fmt!(cx, ": ");
            ty.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";");
    }
}

impl Fmt for ast::LetStmtBody<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { consequent, alternate } = self;

        consequent.fmt(cx);
        if let Some(alternate) = alternate {
            fmt!(cx, " else ");
            alternate.fmt(cx);
        }
    }
}
