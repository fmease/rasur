use super::super::Fragment;
use super::{parse_stmt, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};
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

#[test]
fn macros() {
    // These aren't macro call *expr* stmts but a macro call stmts:

    t!(parse_stmt, Rust2015, "m!();", Ok(ast::Stmt::MacroCall(_)));

    t!(parse_stmt, Rust2015, "m![];", Ok(ast::Stmt::MacroCall(_)));

    t!(parse_stmt, Rust2015, "m!{};", Ok(ast::Stmt::MacroCall(_)));
    t!(parse_stmt, Rust2015, "m!{}", Ok(ast::Stmt::MacroCall(_)));

    // However, these *are* exprs:

    t!(
        parse_stmt,
        Rust2015,
        "m!()",
        Ok(ast::Stmt::Expr(
            ast::Expr { kind: ast::ExprKind::MacroCall(_), .. },
            ast::Semicolon::No
        ))
    );

    t!(
        parse_stmt,
        Rust2015,
        "m![]",
        Ok(ast::Stmt::Expr(
            ast::Expr { kind: ast::ExprKind::MacroCall(_), .. },
            ast::Semicolon::No
        ))
    );
}

#[test]
fn let_else() {
    t!(
        parse_stmt,
        Rust2015,
        "let _ = () else {};",
        Ok(ast::Stmt::Let(r!(ast::LetStmt {
            body: Some(ast::LetStmtBody {
                consequent: ast::Expr { kind: ast::ExprKind::Tuple(r!([])), .. },
                alternate: Some(ast::BlockExpr { stmts: r!([]) }),
            }),
            ..
        })))
    );

    // If the consequent (aka initializer) ends in a curly bracket, let-else is invalid.

    t!(
        parse_stmt,
        Rust2015,
        "let _ = {} else {};",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::Else, .. },
            r!([Fragment::Token(TokenKind::Semicolon)])
        )]))
    );

    // We once used to accept this by mistake.
    t!(
        parse_stmt,
        Rust2015,
        "let _ = () as M! {} else {};",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::Else, .. },
            r!([Fragment::Token(TokenKind::Semicolon)])
        )]))
    );
}
