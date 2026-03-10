use super::{parse_stmt, t};
use crate::{ast, edition::Edition::*};
use deref as r;

#[test]
fn attrs() {
    // This used to trigger a debug assertion.
    t!(
        parse_stmt,
        Rust2015,
        "#[a]match x{#![b]}",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                attrs: r!([
                    ast::Attr { style: ast::AttrStyle::Outer, .. },
                    ast::Attr { style: ast::AttrStyle::Inner, .. },
                ]),
                kind: ast::ExprKind::Match(r!(ast::MatchExpr {
                    scrutinee: ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) },
                    ..
                }))
            },
            _
        ))
    );
}
