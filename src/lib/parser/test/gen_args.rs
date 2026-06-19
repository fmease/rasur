use super::{parse_expr, parse_item, parse_pat, parse_stmt, parse_ty, t};
use crate::{
    ast,
    edition::Edition::*,
    error::{Error, ErrorKind},
    token::{Token, TokenKind},
};

#[test]
fn false_angle_gen_args_expr() {
    t!(parse_expr, Rust2015, "f<i32>()", Err([Error { kind: ErrorKind::ChainedComparison, .. }]),);

    t!(
        parse_expr,
        Rust2015,
        "f<i32>",
        Err([
            Error { kind: ErrorKind::ChainedComparison, .. },
            Error { kind: ErrorKind::UnexpectedToken(TokenKind::EndOfInput, _), .. }
        ]),
    );
}

#[test]
fn false_angle_gen_args_pat() {
    t!(
        parse_pat,
        Rust2015,
        "Some<i32>(0)",
        Err([Error { kind: ErrorKind::UnexpectedToken(TokenKind::SingleLessThan, _), .. }])
    );
}

#[test]
fn angle_gen_args_expr() {
    t!(
        parse_expr,
        Rust2015,
        "f::<i32>()",
        Ok(ast::Expr {
            kind: ast::ExprKind::Call(
                ast::Expr {
                    kind: ast::ExprKind::Path(ast::ExtPath {
                        ext: None,
                        path: ast::Path {
                            segs: [ast::PathSeg {
                                ident: ast::Ident!("f"),
                                args: Some(ast::GenericArgs::Angle([ast::AngleGenericArg::Ty(
                                    ast::Ty::Path(ast::ExtPath {
                                        ext: None,
                                        path: ast::Path {
                                            segs: [ast::PathSeg {
                                                ident: ast::Ident!("i32"),
                                                args: None
                                            }]
                                        },
                                    })
                                )]))
                            }]
                        }
                    }),
                    ..
                },
                []
            ),
            ..
        })
    );
}

#[test]
fn angle_gen_args_pat() {
    t!(
        parse_pat,
        Rust2015,
        "Some::<i32>(0)",
        Ok(ast::Pat::TupleStruct(ast::TupleStructPat {
            path: ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: [ast::PathSeg {
                        ident: ast::Ident!("Some"),
                        args: Some(ast::GenericArgs::Angle([ast::AngleGenericArg::Ty(
                            ast::Ty::Path(ast::ExtPath {
                                ext: None,
                                path: ast::Path {
                                    segs: [ast::PathSeg { ident: ast::Ident!("i32"), args: None }],
                                }
                            })
                        )]))
                    }]
                }
            },
            fields: [ast::Pat::Lit(
                ast::Sign::None,
                ast::Lit { kind: ast::LitKind::Num, value: "0", .. },
            )]
        })),
    );
}

#[test]
fn angle_gen_args_ty() {
    t!(
        parse_ty,
        Rust2015,
        "Ty<'a, (), 0>",
        Ok(ast::Ty::Path(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: [ast::PathSeg {
                    ident: ast::Ident!("Ty"),
                    args: Some(ast::GenericArgs::Angle([
                        ast::AngleGenericArg::Lifetime(ast::Lifetime(ast::Ident!("a"))),
                        ast::AngleGenericArg::Ty(ast::Ty::Tuple([])),
                        ast::AngleGenericArg::Const(ast::Expr {
                            kind: ast::ExprKind::Lit(ast::Lit {
                                kind: ast::LitKind::Num,
                                value: "0",
                                ..
                            }),
                            ..
                        }),
                    ]))
                }]
            }
        }))
    );

    t!(parse_ty, Rust2015, "Ty::<'a, (), 0>", Ok(_)); // just a smoke test
}

// While typically angle generic args have to be introduced with `::<` instead of `<`
// in exprs (and pats), the trait ref of an ext path gets treated to a "type context"
// and it's unambiguous that angle generic args are meant for the trait ref when
// encountering just `<`.
#[test]
fn angle_args_in_path_ext_expr() {
    t!(
        parse_expr,
        Rust2015,
        "<() as TraitRef<()>>::assoc",
        Ok(ast::Expr {
            kind: ast::ExprKind::Path(ast::ExtPath {
                ext: Some(ast::PathExt {
                    self_ty: ast::Ty::Tuple([]),
                    trait_ref: Some(ast::Path {
                        segs: [ast::PathSeg {
                            ident: ast::Ident!("TraitRef"),
                            args: Some(ast::GenericArgs::Angle([ast::AngleGenericArg::Ty(
                                ast::Ty::Tuple([])
                            )]))
                        },]
                    })
                }),
                path: ast::Path {
                    segs: [ast::PathSeg { ident: ast::Ident!("assoc"), args: None }]
                }
            }),
            ..
        })
    );
}

// This demonstrates a very odd consequence of Rust's grammar:
// Not only are parenthesized generic args permitted in expression and
// pattern position but trailing `-> $Type` is also permitted.
#[test]
fn paren_gen_args_arrow_expr_or_pat() {
    t!(
        parse_expr,
        Rust2015,
        "x::()->()",
        Ok(ast::Expr {
            kind: ast::ExprKind::Path(ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: [ast::PathSeg {
                        ident: ast::Ident!("x"),
                        args: Some(ast::GenericArgs::Paren([], Some(ast::Ty::Tuple([]))))
                    }]
                }
            }),
            ..
        })
    );

    t!(
        parse_pat,
        Rust2015,
        "x::()->!::X",
        Ok(ast::Pat::Path(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: [
                    ast::PathSeg {
                        ident: ast::Ident!("x"),
                        args: Some(ast::GenericArgs::Paren([], Some(ast::Ty::Never)))
                    },
                    ast::PathSeg { ident: ast::Ident!("X"), args: None }
                ]
            }
        }))
    );
}

#[test]
fn macro_call_gen_args() {
    t!(
        parse_item,
        Rust2015,
        "path::to::<>::call!();",
        Err([Error { kind: ErrorKind::UnexpectedToken(TokenKind::SingleLessThan, _), .. }])
    );

    t!(
        parse_item,
        Rust2015,
        "path::to::call<()>!();",
        Err([Error { kind: ErrorKind::UnexpectedToken(TokenKind::SingleLessThan, _), .. }])
    );

    t!(
        parse_stmt,
        Rust2015,
        "path::to::<>::call::<>!();",
        Ok(ast::Stmt::MacroCall(ast::MacroCall {
            path: ast::Path {
                segs: [
                    ast::PathSeg { ident: ast::Ident!("path"), args: None },
                    ast::PathSeg {
                        ident: ast::Ident!("to"),
                        args: Some(ast::GenericArgs::Angle([]))
                    },
                    ast::PathSeg {
                        ident: ast::Ident!("call"),
                        args: Some(ast::GenericArgs::Angle([]))
                    },
                ]
            },
            bracket: ast::Bracket::Round,
            stream: [Token { kind: TokenKind::EndOfInput, .. }],
        }))
    );

    t!(parse_stmt, Rust2015, "path::to::<>::call::()!();", Ok(_)); // just a smoke test
}

// It's never legal to reinterpret the token `<=` as `<` followed by `=`.
// Similarly for `<<=` which should never be viewed as `<` followed by `<` or `<<`.
// issue: <https://github.com/fmease/rasur/issues/11>
#[test]
fn dont_split_less_than_equals_for_angle_bracketed_lists() {
    t!(
        parse_expr,
        Rust2015,
        "0 as u64 <= 1",
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Le,
                ast::Expr { kind: ast::ExprKind::Cast(..), .. },
                _
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "x as T <<= y",
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::BitShiftLeftAssign,
                ast::Expr { kind: ast::ExprKind::Cast(..), .. },
                _
            ),
            ..
        })
    );
}

#[test]
fn const_args() {
    // We once used to reject this by mistake!
    t!(
        parse_item,
        Rust2015,
        "struct T<const N: U = Self>;",
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(ast::StructItem {
                generics: ast::Generics {
                    params: [ast::GenericParam {
                        kind: ast::GenericParamKind::Const {
                            default: Some(ast::Expr {
                                kind: ast::ExprKind::Path(ast::ExtPath {
                                    ext: None,
                                    path: ast::Path {
                                        segs: [ast::PathSeg { ident: ast::Ident!("Self"), .. }]
                                    }
                                }),
                                ..
                            }),
                            ..
                        },
                        ..
                    }],
                    ..
                },
                ..
            }),
            ..
        })
    );
}
