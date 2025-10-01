use super::error::ParseError;
use crate::{
    ast,
    edition::Edition::{self, *},
    lexer::{StripShebang, lex},
    token::{Token, TokenKind},
};
use std::assert_matches::assert_matches;

fn parse_file(source: &str, edition: Edition) -> super::Result<ast::File<'_>> {
    super::parse(&lex(source, StripShebang::Yes), source, edition)
}

fn parse_via<'src, T>(
    source: &'src str,
    edition: Edition,
    parse: impl FnOnce(&mut super::Parser<'_, 'src>) -> super::Result<T>,
) -> super::Result<T> {
    let tokens = lex(source, StripShebang::No);
    let mut parser = super::Parser::new(&tokens, source, edition);
    let result = parse(&mut parser)?;
    parser.parse(TokenKind::EndOfInput)?;
    Ok(result)
}

fn parse_item(source: &str, edition: Edition) -> super::Result<ast::Item<'_>> {
    parse_via(source, edition, |this| this.parse_item())
}

fn parse_ty(source: &str, edition: Edition) -> super::Result<ast::Ty<'_>> {
    parse_via(source, edition, |this| this.parse_ty())
}

fn parse_stmt(source: &str, edition: Edition) -> super::Result<ast::Stmt<'_>> {
    parse_via(source, edition, |this| this.parse_stmt(TokenKind::EndOfInput))
}

fn parse_expr(source: &str, edition: Edition) -> super::Result<ast::Expr<'_>> {
    parse_via(source, edition, |this| this.parse_expr())
}

fn parse_pat(source: &str, edition: Edition) -> super::Result<ast::Pat<'_>> {
    parse_via(source, edition, |this| this.parse_pat(super::pat::OrPolicy::Allowed))
}

#[test]
fn file_empty() {
    assert_matches!(
        parse_file("", Rust2015),
        Ok(ast::File { attrs: deref!([]), items: deref!([]), span: _ })
    );
}

#[test]
fn expr_double_borrow_and_double_borrow() {
    assert_matches!(
        parse_expr("&&0&&&&1", Rust2015),
        Ok(ast::Expr::BinOp(
            ast::BinOp::And,
            deref!(ast::Expr::Borrow(
                ast::Mutability::Not,
                deref!(ast::Expr::Borrow(ast::Mutability::Not, _))
            )),
            deref!(ast::Expr::Borrow(
                ast::Mutability::Not,
                deref!(ast::Expr::Borrow(ast::Mutability::Not, _))
            ))
        ))
    );
}

#[test]
fn expr_or_nullary_closure() {
    assert_matches!(
        parse_expr("()||||()", Rust2015),
        Ok(ast::Expr::BinOp(
            ast::BinOp::Or,
            deref!(ast::Expr::Tup(deref!([]))),
            deref!(ast::Expr::Closure(deref!(ast::ClosureExpr {
                params: deref!([]),
                ret_ty: None,
                body: ast::Expr::Tup(deref!([]))
            })))
        ))
    );
}

// Unstable feature: `mut_ref` <https://github.com/rust-lang/rust/issues/123076>.
#[test]
fn pat_mut_ref_mut() {
    assert_matches!(
        parse_pat("mut ref mut x", Rust2015),
        Ok(ast::Pat::Ident(ast::IdentPat {
            mut_: ast::Mutability::Mut,
            by_ref: ast::ByRef::Yes(ast::Mutability::Mut),
            ident: "x"
        }))
    );
}

#[test]
fn expr_false_angle_gen_args() {
    assert_matches!(
        parse_expr("f<i32>()", Rust2015),
        // FIXME: We should report sth. like OpCannotBeChained(Level::Compare) instead
        //        since we have {`<`, `>`} here, not {`>`, `>`}.
        Err(ParseError::OpCannotBeChained(deref!("Gt"))),
    );

    assert_matches!(
        parse_expr("f<i32>", Rust2015),
        // FIXME: Same here.
        Err(ParseError::OpCannotBeChained(deref!("Gt"))),
    );
}

#[test]
fn pat_false_angle_gen_args() {
    assert_matches!(
        parse_pat("Some<i32>(0)", Rust2015),
        Err(ParseError::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _))
    );
}

#[test]
fn expr_angle_gen_args() {
    assert_matches!(
        parse_expr("f::<i32>()", Rust2015),
        Ok(ast::Expr::Call(
            deref!(ast::Expr::Path(ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: deref!([ast::PathSeg {
                        ident: "f",
                        args: Some(ast::GenericArgs::Angle(deref!([
                            ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Path(
                                ast::ExtPath {
                                    ext: None,
                                    path: ast::Path {
                                        segs: deref!([ast::PathSeg { ident: "i32", args: None }])
                                    },
                                }
                            )))
                        ])))
                    }])
                }
            })),
            deref!([])
        ))
    );
}

#[test]
fn pat_angle_gen_args() {
    assert_matches!(
        parse_pat("Some::<i32>(0)", Rust2015),
        Ok(ast::Pat::TupleStruct(deref!(ast::TupleStructPat {
            path: ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: deref!([ast::PathSeg {
                        ident: "Some",
                        args: Some(ast::GenericArgs::Angle(deref!([
                            ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Path(
                                ast::ExtPath {
                                    ext: None,
                                    path: ast::Path {
                                        segs: deref!([ast::PathSeg { ident: "i32", args: None }]),
                                    }
                                }
                            )))
                        ])))
                    }])
                }
            },
            fields: deref!([ast::Pat::Lit(ast::Lit::Num("0"))])
        }))),
    );
}

#[test]
fn ty_angle_gen_args() {
    assert_matches!(
        parse_ty("Ty<'a, (), 0>", Rust2015),
        Ok(ast::Ty::Path(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: deref!([ast::PathSeg {
                    ident: "Ty",
                    args: Some(ast::GenericArgs::Angle(deref!([
                        ast::AngleGenericArg::Argument(ast::GenericArg::Lifetime(ast::Lifetime(
                            "'a"
                        ))),
                        ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Tup(deref!(
                            []
                        )))),
                        ast::AngleGenericArg::Argument(ast::GenericArg::Const(ast::Expr::Lit(
                            ast::Lit::Num("0")
                        ))),
                    ])))
                }])
            }
        }))
    );

    assert_matches!(parse_ty("Ty::<'a, (), 0>", Rust2015), Ok(_));
}

// While typically angle generic args have to be introduced with `::<` instead of `<`
// in exprs (and pats), the trait ref of an ext path gets treated to a "type context"
// and it's unambiguous that angle generic args are meant for the trait ref when
// encountering just `<`.
#[test]
fn expr_angle_args_in_path_ext() {
    assert_matches!(
        parse_expr("<() as TraitRef<()>>::assoc", Rust2015),
        Ok(ast::Expr::Path(ast::ExtPath {
            ext: Some(ast::PathExt {
                self_ty: ast::Ty::Tup(deref!([])),
                trait_ref: Some(ast::Path {
                    segs: deref!([ast::PathSeg {
                        ident: "TraitRef",
                        args: Some(ast::GenericArgs::Angle(deref!([
                            ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Tup(
                                deref!([])
                            )))
                        ])))
                    },])
                })
            }),
            path: ast::Path { segs: deref!([ast::PathSeg { ident: "assoc", args: None }]) }
        }))
    );
}

// This demonstrates a very odd consequence of Rust's grammar:
// Not only are parenthesized generic args permitted in expression and
// pattern position but trailing `-> $Type` is also permitted.
#[test]
fn expr_pat_paren_gen_args_arrow() {
    assert_matches!(
        parse_expr("x::()->()", Rust2015),
        Ok(ast::Expr::Path(deref!(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: deref!([ast::PathSeg {
                    ident: "x",
                    args: Some(ast::GenericArgs::Paren {
                        inputs: deref!([]),
                        output: Some(ast::Ty::Tup(deref!([]))),
                    })
                }])
            }
        })))
    );

    assert_matches!(
        parse_pat("x::()->!::X", Rust2015),
        Ok(ast::Pat::Path(deref!(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: deref!([
                    ast::PathSeg {
                        ident: "x",
                        args: Some(ast::GenericArgs::Paren {
                            inputs: deref!([]),
                            output: Some(ast::Ty::Never),
                        })
                    },
                    ast::PathSeg { ident: "X", args: None }
                ])
            }
        })))
    );
}

#[test]
fn item_macro_call_gen_args() {
    assert_matches!(
        parse_item("path::to::<>::call!();", Rust2015),
        Err(ParseError::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _))
    );

    assert_matches!(
        parse_item("path::to::call<()>!();", Rust2015),
        Err(ParseError::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _))
    );
}

#[test]
fn stmt_macro_call_gen_args() {
    assert_matches!(
        parse_stmt("path::to::<>::call::<>!();", Rust2015),
        Ok(ast::Stmt::Expr(
            ast::Expr::MacroCall(deref!(ast::MacroCall {
                path: ast::Path {
                    segs: deref!([
                        ast::PathSeg { ident: "path", args: None },
                        ast::PathSeg {
                            ident: "to",
                            args: Some(ast::GenericArgs::Angle(deref!([])))
                        },
                        ast::PathSeg {
                            ident: "call",
                            args: Some(ast::GenericArgs::Angle(deref!([])))
                        },
                    ])
                },
                bracket: ast::Bracket::Round,
                stream: deref!([]),
            })),
            ast::Semicolon::Yes
        ))
    );

    assert_matches!(parse_stmt("path::to::<>::call::()!();", Rust2015), Ok(_));
}

#[test]
fn stmts_const_item_const_block() {
    assert_matches!(
        parse_expr(
            "{
    const { }
    const fn f() {}
}",
            Rust2015
        ),
        Ok(ast::Expr::Block(deref!(ast::BlockExpr {
            attrs: deref!([]),
            stmts: deref!([
                ast::Stmt::Expr(
                    ast::Expr::ConstBlock(deref!(ast::BlockExpr {
                        attrs: deref!([]),
                        stmts: deref!([])
                    })),
                    ast::Semicolon::No
                ),
                ast::Stmt::Item(ast::Item {
                    attrs: deref!([]),
                    vis: ast::Visibility::Inherited,
                    kind: ast::ItemKind::Fn(ast::FnItem {
                        constness: ast::Constness::Const,
                        safety: ast::Safety::Inherited,
                        externness: ast::Externness::Not,
                        binder: "f",
                        ..
                    }),
                    span: _
                }),
            ])
        })))
    );
}

#[test]
fn expr_control_flow_ops_block() {
    assert_matches!(
        parse_expr("if return {}", Rust2015),
        Err(ParseError::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, span: _ },
            super::ExpectedFragment::Token(TokenKind::OpenCurlyBracket),
        ))
    );
    assert_matches!(
        parse_expr("if return {} {}", Rust2015),
        Ok(ast::Expr::If(deref!(ast::IfExpr {
            condition: ast::Expr::Return(Some(deref!(ast::Expr::Block(ast::BlockExpr {
                attrs: deref!([]),
                stmts: deref!([])
            })))),
            consequent: ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) },
            alternate: None
        })))
    );

    // FIXME: Explainer, once I have one.
    assert_matches!(
        parse_expr("if break {}", Rust2015),
        Ok(ast::Expr::If(deref!(ast::IfExpr {
            condition: ast::Expr::Break(None, None),
            consequent: ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) },
            alternate: None
        })))
    );

    assert_matches!(
        parse_expr("break {}", Rust2015),
        Ok(ast::Expr::Break(
            None,
            Some(ast::Expr::Block(ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) }))
        ))
    );

    assert_matches!(
        parse_expr("if continue {}", Rust2015),
        Ok(ast::Expr::If(deref!(ast::IfExpr {
            condition: ast::Expr::Continue,
            consequent: ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) },
            alternate: None
        })))
    );
}

// FIXME: macro_rules! in stmt pos (-> item not stmt); macro_rules! no binder == macro call
// FIXME: ops
// FIXME: structs in ifs etc.
// FIXME: almost-assoc-item-constraint due to (  )
// FIXME: ranges!! exprs, pats
// FIXME: A bunch of negative behavior tests!

#[test]
fn binding_modes() {
    assert_matches!(
        parse_file(
            "
fn main() {
    let x = ();
    let mut x = ();
    let ref x = ();
    let mut ref x = ();
    let mut ref mut x = ();
    let &(mut x) = ();
    let &(ref mut x) = ();
    let &(mut ref x) = ();
    let &(mut ref mut x) = ();
    let &mut x = ();
    let &mut mut x = ();
    let &mut ref x = ();
    let &mut ref mut x = ();
    let &mut mut ref mut x = ();
}
",
            Rust2015
        ),
        Ok(_)
    );
}

// FIXME: Expand this smoke test.
#[test]
#[ignore] // FIXME: Make it work.
fn item_modifiers() {
    assert_matches!(
        parse_file(
            r#"
pub const unsafe extern "C" fn f() {}
const unsafe extern "C" fn f() {}
unsafe extern "C" fn f() {}
extern "C" fn f() {}
fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
pub const fn f() {}
pub fn f() {}
pub const extern "C" fn f() {}
const extern "C" fn f() {}
"#,
            Rust2015
        ),
        Ok(_)
    );
}
