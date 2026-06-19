use super::super::Fragment;
use super::{parse_expr, parse_stmt, t};
use crate::{
    ast,
    edition::Edition::*,
    error::{Error, ErrorKind},
    token::TokenKind,
};

// FIXME: Exercise `StructPolicy::Yield` (e.g., struct exprs in if conditions).

#[test]
fn levels() {
    t!(
        parse_expr,
        Rust2015,
        "if(){}else{}()",
        Ok(ast::Expr {
            kind: ast::ExprKind::Call(ast::Expr { kind: ast::ExprKind::If(..), .. }, []),
            ..
        }),
    );

    t!(
        parse_expr,
        Rust2015,
        "if(){}else{}as _",
        Ok(ast::Expr {
            kind: ast::ExprKind::Cast(
                ast::Expr { kind: ast::ExprKind::If(..), .. },
                ast::Ty::Inferred
            ),
            ..
        }),
    );

    t!(
        parse_stmt,
        Rust2015,
        "-if 0{}else{}()",
        Err([Error { kind: ErrorKind::InvalidOpAfterBoundary, .. }])
    );

    // ...however, we accept this (expr, unary op):
    t!(
        parse_expr,
        Rust2015,
        "-if 0{}else{}()",
        Ok(ast::Expr {
            kind: ast::ExprKind::UnOp(
                ast::UnOp::Neg,
                ast::Expr {
                    kind: ast::ExprKind::Call(ast::Expr { kind: ast::ExprKind::If(..), .. }, []),
                    ..
                }
            ),
            ..
        })
    );

    // ...and this this (expr stmt, binary op):
    t!(
        parse_stmt,
        Rust2015,
        "1-if 0{}else{}()",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Sub,
                    ast::Expr { kind: ast::ExprKind::Lit(_), .. },
                    ast::Expr {
                        kind: ast::ExprKind::Call(
                            ast::Expr { kind: ast::ExprKind::If(..), .. },
                            []
                        ),
                        ..
                    }
                ),
                ..
            },
            ast::Semicolon::No
        ),),
    );

    t!(
        parse_stmt,
        Rust2015,
        "&if(){}()",
        Err([Error { kind: ErrorKind::InvalidOpAfterBoundary, .. }])
    );

    t!(
        parse_stmt,
        Rust2015,
        "&&{}()",
        Err([Error { kind: ErrorKind::InvalidOpAfterBoundary, .. }])
    );

    // Ensure that index & call operators are allowed to follow boundaries
    // if "they start a new stmt" (i.e., the precedence level is initial).
    //
    // Here, the `()` is a separate stmt, a tuple expr stmt; not part of a call expr.
    t!(
        parse_expr,
        Rust2015,
        "{ if(){}() }",
        Ok(ast::Expr {
            kind: ast::ExprKind::Block(
                _,
                ast::BlockExpr {
                    stmts: [
                        ast::Stmt::Expr(ast::Expr { kind: ast::ExprKind::If(_), .. }, _),
                        ast::Stmt::Expr(ast::Expr { kind: ast::ExprKind::Tuple([]), .. }, _),
                    ]
                }
            ),
            ..
        })
    );
}

#[test]
fn attrs() {
    t!(
        parse_expr,
        Rust2015,
        "#[a]0",
        Ok(ast::Expr {
            attrs: [ast::Attr {
                style: ast::AttrStyle::Outer,
                kind: ast::AttrKind::Regular(_),
                ..
            }],
            kind: ast::ExprKind::Lit(_),
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "#[a]#[b](#[c]#[d]0)",
        Ok(ast::Expr {
            attrs: [
                ast::Attr {
                    style: ast::AttrStyle::Outer,
                    kind: ast::AttrKind::Regular(ast::Meta {
                        path: ast::Path { segs: [ast::PathSeg { ident: ast::Ident!("a"), .. }] },
                        ..
                    }),
                    ..
                },
                ast::Attr {
                    style: ast::AttrStyle::Outer,
                    kind: ast::AttrKind::Regular(ast::Meta {
                        path: ast::Path { segs: [ast::PathSeg { ident: ast::Ident!("b"), .. }] },
                        ..
                    }),
                    ..
                },
            ],
            kind: ast::ExprKind::Grouped(ast::Expr {
                attrs: [
                    ast::Attr { style: ast::AttrStyle::Outer, .. },
                    ast::Attr { style: ast::AttrStyle::Outer, .. },
                ],
                kind: ast::ExprKind::Lit(_),
            }),
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "#[a]*x",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::UnOp(..)
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/25>
    t!(
        parse_expr,
        Rust2015,
        "#[a]!x",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::UnOp(..)
        })
    );

    t!(parse_expr, Rust2015, "#[a]..", Err([Error { kind: ErrorKind::ForbiddenOuterAttrs, .. }]));

    t!(parse_expr, Rust2015, "#[a]..()", Err([Error { kind: ErrorKind::ForbiddenOuterAttrs, .. }]));

    t!(parse_expr, Rust2015, "#[a]..=_", Err([Error { kind: ErrorKind::ForbiddenOuterAttrs, .. }]));

    t!(
        parse_expr,
        Rust2015,
        "#[a]&#[b]()",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::Borrow(
                ..,
                ast::Expr {
                    attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                    kind: ast::ExprKind::Tuple(_),
                    ..
                }
            )
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "#[a]&#[b]()",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::Borrow(
                ..,
                ast::Expr {
                    attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                    kind: ast::ExprKind::Tuple(_),
                    ..
                }
            )
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/27>
    t!(
        parse_expr,
        Rust2015,
        "0..#[a]1",
        Ok(ast::Expr {
            attrs: [],
            kind: ast::ExprKind::Range(
                Some(ast::Expr { attrs: [], kind: ast::ExprKind::Lit(_), .. }),
                Some(ast::Expr {
                    attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                    kind: ast::ExprKind::Lit(_),
                    ..
                }),
                ..
            )
        })
    );

    // The attr belongs to the inner expr, not to the cast itself.
    t!(
        parse_expr,
        Rust2015,
        "#[a]()as()",
        Ok(ast::Expr {
            attrs: [],
            kind: ast::ExprKind::Cast(
                ast::Expr {
                    attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                    kind: ast::ExprKind::Tuple(_)
                },
                ..
            ),
        })
    );

    // The attr belongs to the inner left expr, not to the range itself.
    t!(
        parse_expr,
        Rust2015,
        "#[a]!0..",
        Ok(ast::Expr {
            attrs: [],
            kind: ast::ExprKind::Range(
                Some(ast::Expr {
                    attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                    kind: ast::ExprKind::UnOp(..),
                    ..
                }),
                None,
                ..
            )
        })
    );

    // The attr belongs to the outermost try op expr, not to any of the inner exprs.
    t!(
        parse_expr,
        Rust2015,
        "#[a]0??",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::Try(ast::Expr {
                attrs: [],
                kind: ast::ExprKind::Try(ast::Expr { attrs: [], kind: ast::ExprKind::Lit(_) },),
            })
        })
    );

    // The attr belongs to the (outer) call expr, not to the (inner) callee expr.
    t!(
        parse_expr,
        Rust2015,
        "#[a]f()",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::Call(ast::Expr { attrs: [], kind: ast::ExprKind::Path(_) }, [])
        })
    );

    // Here, the attr of course belongs to the inner path expr, not to the call expr itself.
    t!(
        parse_expr,
        Rust2015,
        "(#[a]f)()",
        Ok(ast::Expr {
            attrs: [],
            kind: ast::ExprKind::Call(
                ast::Expr {
                    attrs: [],
                    kind: ast::ExprKind::Grouped(ast::Expr {
                        attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                        kind: ast::ExprKind::Path(_)
                    })
                },
                []
            )
        })
    );

    // The attr belongs to the (outer) indexing expr, not to the (inner) indexed expr.
    t!(
        parse_expr,
        Rust2015,
        "#[a]f[0]",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::Index(ast::Expr { attrs: [], kind: ast::ExprKind::Path(_) }, _)
        })
    );

    // The attr belongs to the (outer) field expr, not to the (inner) path expr.
    t!(
        parse_expr,
        Rust2015,
        "#[a]x.y",
        Ok(ast::Expr {
            attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
            kind: ast::ExprKind::Field(ast::Expr { attrs: [], kind: ast::ExprKind::Path(_) }, _,)
        })
    );

    // The outer attr belongs to the (outer) match expr, not to the (inner) scrutinee expr.
    t!(
        parse_expr,
        Rust2015,
        "#[a]x.match{#![b]}",
        Ok(ast::Expr {
            attrs: [
                ast::Attr { style: ast::AttrStyle::Outer, .. },
                ast::Attr { style: ast::AttrStyle::Inner, .. },
            ],
            kind: ast::ExprKind::Match(ast::MatchExpr {
                scrutinee: ast::Expr { attrs: [], kind: ast::ExprKind::Path(_) },
                ..
            })
        })
    );

    // The attr belongs to the inner left operand expr, not to the operation itself.
    t!(
        parse_expr,
        Rust2015,
        "#[a]-0+1",
        Ok(ast::Expr {
            attrs: [],
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Add,
                ast::Expr {
                    attrs: [ast::Attr { style: ast::AttrStyle::Outer, .. }],
                    kind: ast::ExprKind::UnOp(..),
                    ..
                },
                ast::Expr { attrs: [], kind: ast::ExprKind::Lit(_), .. },
                ..
            )
        })
    );
}

#[test]
fn double_borrow_and_double_borrow() {
    t!(
        parse_expr,
        Rust2015,
        "&&0&&&&1",
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::And,
                ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ast::BorrowKind::Ref,
                        ast::Mut::No,
                        ast::Expr {
                            kind: ast::ExprKind::Borrow(ast::BorrowKind::Ref, ast::Mut::No, _),
                            ..
                        }
                    ),
                    ..
                },
                ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ast::BorrowKind::Ref,
                        ast::Mut::No,
                        ast::Expr {
                            kind: ast::ExprKind::Borrow(ast::BorrowKind::Ref, ast::Mut::No, _),
                            ..
                        },
                    ),
                    ..
                }
            ),
            ..
        }),
    );
}

#[test]
fn or_nullary_closure() {
    t!(
        parse_expr,
        Rust2015,
        "()||||()",
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Or,
                ast::Expr { kind: ast::ExprKind::Tuple([]), .. },
                ast::Expr {
                    kind: ast::ExprKind::Closure(ast::ClosureExpr {
                        bound_vars: [],
                        modifiers: _,
                        params: [],
                        ret_ty: None,
                        body: ast::Expr { kind: ast::ExprKind::Tuple([]), .. }
                    }),
                    ..
                }
            ),
            ..
        })
    );
}

#[test]
fn control_flow_ops_block() {
    t!(
        parse_expr,
        Rust2015,
        "if return {}",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::EndOfInput,
                [Fragment::Token(TokenKind::OpenCurlyBracket)],
            ),
            ..
        }])
    );
    t!(
        parse_expr,
        Rust2015,
        "if return {} {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::If(ast::IfExpr {
                condition: ast::Expr {
                    kind: ast::ExprKind::Return(Some(ast::Expr {
                        kind: ast::ExprKind::Block(None, ast::BlockExpr { stmts: [] }),
                        ..
                    })),
                    ..
                },
                consequent: ast::BlockExpr { stmts: [] },
                alternate: None
            }),
            ..
        })
    );

    // FIXME: Explainer, once I have one.
    t!(
        parse_expr,
        Rust2015,
        "if break {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::If(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Break(None, None), .. },
                consequent: ast::BlockExpr { stmts: [] },
                alternate: None
            }),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "break {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::Break(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::Block(None, ast::BlockExpr { stmts: [] }),
                    ..
                })
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "if continue {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::If(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Continue(None), .. },
                consequent: ast::BlockExpr { stmts: [], .. },
                alternate: None
            }),
            ..
        })
    );
}

#[test]
fn numeric_field() {
    t!(
        parse_expr,
        Rust2015,
        "x.0",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr { kind: ast::ExprKind::Path(_), .. },
                ast::Ident!("0"),
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "x.0 .1",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr {
                    kind: ast::ExprKind::Field(
                        ast::Expr { kind: ast::ExprKind::Path(_), .. },
                        ast::Ident!("0"),
                    ),
                    ..
                },
                ast::Ident!("1"),
            ),
            ..
        })
    );

    // Context: Like rustc we currently lex this as [Ident(`x`), NumLit(`0.1`)] since the `0.1` gets
    // bluntly interpreted as a float literal. As a result, the parser has to split the literal.
    t!(
        parse_expr,
        Rust2015,
        "x.0.1",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr {
                    kind: ast::ExprKind::Field(
                        ast::Expr { kind: ast::ExprKind::Path(_), .. },
                        ast::Ident!("0"),
                    ),
                    ..
                },
                ast::Ident!("1"),
            ),
            ..
        })
    );

    // ... same thing, just with an extra space.
    t!(
        parse_expr,
        Rust2015,
        "x. 0.1",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr {
                    kind: ast::ExprKind::Field(
                        ast::Expr { kind: ast::ExprKind::Path(_), .. },
                        ast::Ident!("0"),
                    ),
                    ..
                },
                ast::Ident!("1"),
            ),
            ..
        })
    );

    // ...here we first split the `0.` & then push `.` back
    // "onto the stack" for the callee to pick up again.
    t!(
        parse_expr,
        Rust2015,
        "x.0. 1",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr {
                    kind: ast::ExprKind::Field(
                        ast::Expr { kind: ast::ExprKind::Path(_), .. },
                        ast::Ident!("0"),
                    ),
                    ..
                },
                ast::Ident!("1"),
            ),
            ..
        })
    );

    // ...similarly, we need to split the number lit `0.1` here.
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, x.0.1)",
        Ok(ast::Expr {
            kind: ast::ExprKind::OffsetOf(
                _,
                [ast::Ident!("x"), ast::Ident!("0"), ast::Ident!("1")]
            ),
            ..
        }),
    );
}

#[test]
fn ranges() {
    t!(
        parse_expr,
        Rust2015,
        "..",
        Ok(ast::Expr { kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive), .. })
    );

    t!(
        parse_expr,
        Rust2015,
        "&..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Borrow(
                ..,
                ast::Expr {
                    kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                    ..
                }
            ),
            ..
        }),
    );

    // We once used to wrongly accept this & parse it as `(..)?`.
    t!(
        parse_expr,
        Rust2015,
        "..?",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::QuestionMark,
                [Fragment::Token(TokenKind::EndOfInput)],
            ),
            ..
        }]),
    );

    // `(!x)..`, not `!(x..)`.
    t!(
        parse_expr,
        Rust2015,
        "!x..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Not, _), .. }),
                None,
                ast::RangeExprKind::Exclusive
            ),
            ..
        })
    );

    // `(&0)..`, not `&(0..)`.
    t!(
        parse_expr,
        Rust2015,
        "&0..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr { kind: ast::ExprKind::Borrow(..), .. }),
                None,
                ast::RangeExprKind::Exclusive
            ),
            ..
        }),
    );

    // `..(-x)`, not `(..) - x`.
    t!(
        parse_expr,
        Rust2015,
        "..-x",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Neg, _), .. }),
                ast::RangeExprKind::Exclusive
            ),
            ..
        })
    );

    // `..(*x)`, not `(..)*x`. Inspired by <https://github.com/tree-sitter/tree-sitter-rust/issues/291>.
    t!(
        parse_expr,
        Rust2015,
        "..*x",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Deref, _), .. }),
                ast::RangeExprKind::Exclusive
            ),
            ..
        })
    );

    // `..(0?)`, not `(..0)?`.
    t!(
        parse_expr,
        Rust2015,
        "..0?",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr { kind: ast::ExprKind::Try(..), .. }),
                ast::RangeExprKind::Exclusive
            ),
            ..
        }),
    );

    // `(1 + 2)..(3 + 4)`, not `1 + (2..3) + 4`.
    t!(
        parse_expr,
        Rust2015,
        "1 + 2..3 + 4",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr { kind: ast::ExprKind::BinOp(ast::BinOp::Add, ..), .. }),
                Some(ast::Expr { kind: ast::ExprKind::BinOp(ast::BinOp::Add, ..), .. }),
                ast::RangeExprKind::Exclusive
            ),
            ..
        })
    );

    // While here we parse the `{}` as the right argument of the range as one would expect...
    t!(
        parse_expr,
        Rust2015,
        "..{}",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr { kind: ast::ExprKind::Block(..), .. }),
                ast::RangeExprKind::Exclusive
            ),
            ..
        })
    );

    // ...here we consider it to belong to the overarching loop construct.
    t!(
        parse_expr,
        Rust2015,
        "for _ in .. {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::ForLoop(ast::ForLoopExpr {
                head: ast::Expr {
                    kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                    ..
                },
                body: ast::BlockExpr { .. },
                ..
            }),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "..=()",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr { kind: ast::ExprKind::Tuple(_), .. }),
                ast::RangeExprKind::Inclusive
            ),
            ..
        })
    );

    // `(*x)..=0`, not `*(x..=0)`.
    t!(
        parse_expr,
        Rust2015,
        "*x..=0",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Deref, _), .. }),
                Some(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                ast::RangeExprKind::Inclusive
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "..=",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(TokenKind::EndOfInput, [Fragment::Expr]),
            ..
        }]),
    );

    t!(
        parse_expr,
        Rust2015,
        "'='..=",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(TokenKind::EndOfInput, [Fragment::Expr]),
            ..
        }]),
    );

    // Unlike the `..` case, `{}` gets interpreted as the right argument of the range.
    t!(
        parse_expr,
        Rust2015,
        "for _ in ..={} {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::ForLoop(ast::ForLoopExpr {
                head: ast::Expr {
                    kind: ast::ExprKind::Range(
                        None,
                        Some(ast::Expr { kind: ast::ExprKind::Block(..), .. }),
                        ast::RangeExprKind::Inclusive,
                    ),
                    ..
                },
                body: ast::BlockExpr { .. },
                ..
            }),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        ".. .. ..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::Range(
                        None,
                        Some(ast::Expr {
                            kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                            ..
                        }),
                        ast::RangeExprKind::Exclusive
                    ),
                    ..
                }),
                ast::RangeExprKind::Exclusive
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "..=..=..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::Range(
                        None,
                        Some(ast::Expr {
                            kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                            ..
                        }),
                        ast::RangeExprKind::Inclusive
                    ),
                    ..
                }),
                ast::RangeExprKind::Inclusive
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "0..1..2",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::DoubleDot,
                [Fragment::Token(TokenKind::EndOfInput)]
            ),
            ..
        }]),
    );

    t!(
        parse_expr,
        Rust2015,
        "0..=1..2",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::DoubleDot,
                [Fragment::Token(TokenKind::EndOfInput)]
            ),
            ..
        }]),
    );

    t!(
        parse_expr,
        Rust2015,
        "0..1..=2",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::DoubleDotEquals,
                [Fragment::Token(TokenKind::EndOfInput)]
            ),
            ..
        }]),
    );

    t!(
        parse_expr,
        Rust2015,
        "0..=1..=2",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::DoubleDotEquals,
                [Fragment::Token(TokenKind::EndOfInput)]
            ),
            ..
        }]),
    );

    t!(
        parse_stmt,
        Rust2015,
        "..if(){}else{}[0]",
        Err([Error { kind: ErrorKind::InvalidOpAfterBoundary, .. }])
    );

    t!(
        parse_stmt,
        Rust2015,
        "()..if(){}else{}[0]",
        Err([Error { kind: ErrorKind::InvalidOpAfterBoundary, .. }])
    );

    // ...for comparison, this does parse:
    t!(
        parse_expr,
        Rust2015,
        "..if(){}else{}[0]",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::Index(
                        ast::Expr { kind: ast::ExprKind::If(..), .. },
                        ast::Expr { kind: ast::ExprKind::Lit(_), .. }
                    ),
                    ..
                }),
                _
            ),
            ..
        }),
    );

    // Prefix ranges propagate operator restrictions to their operand contrary to
    // normal unary ops that unconditionally lift any such restrictions.
    t!(
        parse_stmt,
        Rust2015,
        "..{}+0",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::SinglePlus,
                [Fragment::Token(TokenKind::Semicolon)],
            ),
            ..
        }])
    );

    // ...for comparison, this does parse:
    t!(
        parse_expr,
        Rust2015,
        "..{}+0",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::BinOp(
                        ast::BinOp::Add,
                        ast::Expr { kind: ast::ExprKind::Block(..), .. },
                        ast::Expr { kind: ast::ExprKind::Lit(_), .. },
                    ),
                    ..
                }),
                _
            ),
            ..
        })
    );

    // ...so does this (unary op `-` lifts operator restrictions for its operand):
    t!(
        parse_stmt,
        Rust2015,
        "-{}+0",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    ast::Expr {
                        kind: ast::ExprKind::UnOp(
                            ast::UnOp::Neg,
                            ast::Expr { kind: ast::ExprKind::Block(..), .. }
                        ),
                        ..
                    },
                    ast::Expr { kind: ast::ExprKind::Lit(_), .. }
                ),
                ..
            },
            _
        ))
    );

    // Ranges propagate operator restrictions to their right operand contrary
    // to normal binary ops that unconditionally lift any such restrictions.
    t!(
        parse_stmt,
        Rust2015,
        "1..{}+0",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::SinglePlus,
                [Fragment::Token(TokenKind::Semicolon)],
            ),
            ..
        }])
    );

    // ...for comparison, this does parse:
    t!(
        parse_expr,
        Rust2015,
        "1..{}+0",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                Some(ast::Expr {
                    kind: ast::ExprKind::BinOp(
                        ast::BinOp::Add,
                        ast::Expr { kind: ast::ExprKind::Block(..), .. },
                        ast::Expr { kind: ast::ExprKind::Lit(_), .. },
                    ),
                    ..
                }),
                _
            ),
            ..
        })
    );

    // We once used to wrongly reject this on grounds of ranges allegedly being unchainable
    // (like comparison operators) and parse / recover it as `&(..=(0..))`.
    // However, it's actually to be accepted & interpreted as `(&(..=0))..`.
    // issue: <https://github.com/fmease/rasur/issues/17>
    t!(
        parse_expr,
        Rust2015,
        "&..=0..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ..,
                        ast::Expr {
                            kind: ast::ExprKind::Range(
                                None,
                                Some(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                                ast::RangeExprKind::Inclusive
                            ),
                            ..
                        }
                    ),
                    ..
                }),
                None,
                ast::RangeExprKind::Exclusive,
            ),
            ..
        }),
    );

    // ... this one however it to be rejected:
    t!(
        parse_expr,
        Rust2015,
        "..=0..",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::DoubleDot,
                [Fragment::Token(TokenKind::EndOfInput)],
            ),
            ..
        }]),
    );

    // We once used to wrongly parse this as `(T {}) + (..(0..))` instead of `((T {}) + (..0))..`.
    // issue: <https://github.com/fmease/rasur/issues/17>
    t!(
        parse_expr,
        Rust2015,
        "T {} + ..0..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(ast::Expr {
                    kind: ast::ExprKind::BinOp(
                        ast::BinOp::Add,
                        ast::Expr { kind: ast::ExprKind::Struct(_), .. },
                        ast::Expr { kind: ast::ExprKind::Range(None, Some(_), _), .. },
                    ),
                    ..
                }),
                None,
                _
            ),
            ..
        })
    );

    // For the longest time we used to wrongly parse this as
    // `return (x + ((..).y))` instead of `(return (x + (..))).y`.
    // Inspired by <https://github.com/rust-lang/rust/pull/142476#discussion_r2159721125>.
    t!(
        parse_expr,
        Rust2015,
        "return x + .. .y",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr {
                    kind: ast::ExprKind::Return(Some(ast::Expr {
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Add,
                            ast::Expr { kind: ast::ExprKind::Path(_), .. },
                            ast::Expr { kind: ast::ExprKind::Range(None, None, _), .. },
                        ),
                        ..
                    })),
                    ..
                },
                ast::Ident!("y"),
            ),
            ..
        }),
    );

    // Replacing `..` with a lower expr like `0` makes it get parsed more like one would expect:
    t!(
        parse_expr,
        Rust2015,
        "return x + 0 .y",
        Ok(ast::Expr {
            kind: ast::ExprKind::Return(Some(ast::Expr {
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    ast::Expr { kind: ast::ExprKind::Path(_), .. },
                    ast::Expr {
                        kind: ast::ExprKind::Field(
                            ast::Expr { kind: ast::ExprKind::Lit(_), .. },
                            ast::Ident!("y")
                        ),
                        ..
                    },
                ),
                ..
            })),
            ..
        }),
    );

    // For the longest time we used to wrongly parse this as
    // `return ((!(..)).f)` instead of `(return (!(..))).f`.
    t!(
        parse_expr,
        Rust2015,
        "return !.. .f",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr {
                    kind: ast::ExprKind::Return(Some(ast::Expr {
                        kind: ast::ExprKind::UnOp(
                            ast::UnOp::Not,
                            ast::Expr { kind: ast::ExprKind::Range(None, None, _), .. }
                        ),
                        ..
                    })),
                    ..
                },
                ast::Ident!("f")
            ),
            ..
        })
    );

    // For the longest time, we used to wrongly accept this.
    t!(
        parse_expr,
        Rust2015,
        "1 + .. .y",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::SingleDot,
                [Fragment::Token(TokenKind::EndOfInput)]
            ),
            ..
        }])
    );

    // For the longest time, we used to wrongly accept this.
    t!(
        parse_expr,
        Rust2015,
        "1 * .. ?",
        Err([Error {
            kind: ErrorKind::UnexpectedToken(
                TokenKind::QuestionMark,
                [Fragment::Token(TokenKind::EndOfInput)]
            ),
            ..
        }])
    );
}

// `for<` doesn't necessarily begin a closure expr with a binder.
// FIXME: Also add test for `for<()>::AssocTy in () {}`.
// FIXME: However, `for <Ty>::AssocTy in () {}` should actually get rejected b/c
//        it doesn't parse as a closure with binder.
// FIXME: Also add a labeled for where we don't need disambig
//        (`'a: for <Ty>::AssocTy {} in () {}` is valid)
// FIXME: Also add `impl <$ty>::$segs {}`
#[test]
fn qualified_struct_pat_in_for_loop() {
    t!(
        parse_expr,
        Rust2015,
        "for<Ty as Trait>::AssocTy {} in () {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::ForLoop(ast::ForLoopExpr {
                pat: ast::Pat::Struct(ast::StructPat {
                    path: ast::ExtPath {
                        ext: Some(ast::PathExt {
                            self_ty: ast::Ty::Path(ast::ExtPath {
                                ext: None,
                                path: ast::Path {
                                    segs: [ast::PathSeg { ident: ast::Ident!("Ty"), args: None }]
                                },
                            }),
                            trait_ref: Some(ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("Trait"), args: None }]
                            })
                        }),
                        path: ast::Path {
                            segs: [ast::PathSeg { ident: ast::Ident!("AssocTy"), args: None }]
                        }
                    },
                    fields: [],
                    rest: false
                }),
                ..
            }),
            ..
        })
    );
}

#[test]
fn struct_policy() {
    // We once used to wrongly reject this because we interpreted the `{}` as belonging to
    // the `Struct` (as part of a struct expr) since we didn't propagate the struct policy
    // through the closure expr.
    //
    // Inspired by <https://www.reddit.com/r/rust/comments/1pbbx5a/comment/nrq89xi>.
    t!(
        parse_expr,
        Rust2015,
        "while || Struct {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::WhileLoop(ast::WhileLoopExpr {
                condition: ast::Expr {
                    kind: ast::ExprKind::Closure(ast::ClosureExpr {
                        body: ast::Expr {
                            kind: ast::ExprKind::Path(ast::ExtPath {
                                ext: None,
                                path: ast::Path {
                                    segs: [ast::PathSeg { ident: ast::Ident!("Struct"), .. }]
                                }
                            }),
                            ..
                        },
                        ..
                    }),
                    ..
                },
                body: ast::BlockExpr { stmts: [] },
                ..
            }),
            ..
        })
    );
}

// Distinguishing between items and exprs (in stmt ctxts) is quite involved since
// they share quite a number of prefixes / modifier combinations.
#[test]
fn expr_modifiers_in_stmt_ctxt() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    t!(
        parse_expr,
        Rust2024, // for `async` and `gen`
        r#"{
|| {};
|_| {};
| | {};
{};
use || {};
unsafe {};
static || {};
static |_| {};
static use || {};
static move || {};
static move | | {};
static gen use |_| {}; // [***]
static async || {}; // [***]
static async use | | {}; // [***]
move || {};
move |_| {};
gen || {}; // [***]
gen |_| {}; // [***]
gen {};
gen use || {}; // [***]
gen use {}; // [***]
gen move || {}; // [***]
gen move |_| {}; // [***]
for<> || {};
for<> |_| {};
for<> use || {};
for<> use |_| {};
for<> move || {};
for<> move |_| {};
for<> gen || {};
for<> gen |_| {};
for<> gen move || {};
for<> gen move |_| {};
for<> const || {};
for<> const |_| {};
for<> const static async gen move | | {};
for<> const move || {};
for<> const move |_| {};
for<> const gen || {}; // [***]
for<> const gen |_| {}; // [***]
for<> const gen move || {}; // [***]
for<> const gen move |_| {}; // [***]
for<> const async || {}; // [***]
for<> const async |_| {}; // [***]
for<> const async gen || {}; // [***]
for<> const async gen |_| {}; // [***]
for<> const async gen move || {}; // [***]
for<> const async gen move |_| {}; // [***]
for<> async || {};
for<> async |_| {};
for<> async use | | {};
for<> async move || {};
for<> async move |_| {};
for<> async gen || {};
for<> async gen |_| {};
for<> async gen move || {};
for<> async gen move |_| {};
const || {};
const |_| {};
const {};
const use || {};
const static || {};
const static move | | {};
const static async gen use | | {};
const static async gen move || {};
const move || {};
const move |_| {};
const gen || {}; // [***]
const gen |_| {}; // [***]
const gen move || {}; // [***]
const gen move |_| {}; // [***]
const async || {}; // [***]
const async |_| {}; // [***]
const async gen || {}; // [***]
const async gen |_| {}; // [***]
const async gen move || {}; // [***]
const async gen move |_| {}; // [***]
async || {};
async |_| {};
async {};
async use || {};
async use {};
async move || {};
async move |_| {};
async move {};
async gen || {}; // [***]
async gen |_| {}; // [***]
async gen {};
async gen use || {}; // [***]
async gen use {};
async gen move || {}; // [***]
async gen move |_| {}; // [***]
async gen move {};
}"#,
        Ok(_) // just a smoke test
    );
}

// rustc accepts a tiny amount more of these expr modifiers if not in a stmt ctxt.
#[test]
fn expr_modifiers_in_expr_ctxt() {
    // NOTE: Test cases marked `[+++]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    t!(
        parse_expr,
        Rust2024, // for `async` and `gen`
        r#"{
(|| {});
(|_| {});
(| | {});
({});
(use || {});
(unsafe {});
(static || {});
(static |_| {});
(static use || {});
(static move || {});
(static move | | {});
(static gen use |_| {});
(static async || {});
(static async use | | {});
(move || {});
(move |_| {});
(gen || {}); // [+++]
(gen |_| {}); // [+++]
(gen {}); // [+++]
(gen use || {}); // [+++]
(gen use {}); // [+++]
(gen move || {}); // [+++]
(gen move |_| {}); // [+++]
(for<> || {});
(for<> |_| {});
(for<> use || {});
(for<> use |_| {});
(for<> move || {});
(for<> move |_| {});
(for<> gen || {});
(for<> gen |_| {});
(for<> gen move || {});
(for<> gen move |_| {});
(for<> const || {});
(for<> const |_| {});
(for<> const static async gen move | | {});
(for<> const move || {});
(for<> const move |_| {});
(for<> const gen || {}); // [+++]
(for<> const gen |_| {}); // [+++]
(for<> const gen move || {}); // [+++]
(for<> const gen move |_| {}); // [+++]
(for<> const async || {}); // [+++]
(for<> const async |_| {}); // [+++]
(for<> const async gen || {}); // [+++]
(for<> const async gen |_| {}); // [+++]
(for<> const async gen move || {}); // [+++]
(for<> const async gen move |_| {}); // [+++]
(for<> async || {});
(for<> async |_| {});
(for<> async use | | {});
(for<> async move || {});
(for<> async move |_| {});
(for<> async gen || {});
(for<> async gen |_| {});
(for<> async gen move || {});
(for<> async gen move |_| {});
(const || {});
(const |_| {});
(const {});
(const use || {});
(const static || {});
(const static move | | {});
(const static async gen use | | {});
(const static async gen move || {});
(const move || {});
(const move |_| {});
(const gen || {}); // [+++]
(const gen |_| {}); // [+++]
(const gen move || {}); // [+++]
(const gen move |_| {}); // [+++]
(const async || {}); // [+++]
(const async |_| {}); // [+++]
(const async gen || {}); // [+++]
(const async gen |_| {}); // [+++]
(const async gen move || {}); // [+++]
(const async gen move |_| {}); // [+++]
(async || {});
(async |_| {});
(async {});
(async use || {});
(async use {});
(async move || {});
(async move |_| {});
(async move {});
(async gen || {});
(async gen |_| {});
(async gen {});
(async gen use || {});
(async gen use {});
(async gen move || {});
(async gen move |_| {});
(async gen move {});
}"#,
        Ok(_) // just a smoke test
    );
}
