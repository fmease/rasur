use super::{ExpectedFragment, Parser};
use crate::{
    Edition::{self, *},
    ast,
    error::{Buffer as ErrorBuffer, Error, UnchainableExprOp},
    lexer::{StripFrontmatter, StripShebang, lex},
    normalizer::{Normalized, normalize},
    token::{Token, TokenKind},
};
use deref as r;
use std::assert_matches;

type Result<T, E = Vec<Error>> = std::result::Result<T, E>;

macro n($source:expr) {
    normalize($source).as_ref()
}

fn parse_file(source: Normalized<&str>, edition: Edition) -> Result<ast::File<'_>> {
    let mut errors = ErrorBuffer::Hold(Vec::new());
    let file = lex(source, edition, StripShebang::Yes, StripFrontmatter::Yes, &mut errors);
    let file = super::parse(&file, source, edition, &mut errors);
    match errors.non_empty() {
        Some(errors) => Err(errors),
        None => Ok(file.unwrap()),
    }
}

fn parse_via<'src, T>(
    source: Normalized<&'src str>,
    edition: Edition,
    parse: impl FnOnce(&mut super::Parser<'_, '_, 'src>) -> super::Result<T>,
) -> Result<T> {
    let mut errors = ErrorBuffer::Hold(Vec::new());
    let file = lex(source, edition, StripShebang::No, StripFrontmatter::No, &mut errors);
    let mut p = Parser::new(&file.tokens, source, edition, &mut errors);
    let result = parse(&mut p).and_then(|r| {
        p.parse(TokenKind::EndOfInput)?;
        Ok(r)
    });
    match errors.non_empty() {
        Some(errors) => Err(errors),
        None => Ok(result.map_err(drop).unwrap()),
    }
}

fn parse_item(source: Normalized<&str>, edition: Edition) -> Result<ast::Item<'_>> {
    parse_via(source, edition, |this| this.parse_item(super::item::ItemCx::Boring))
}

fn parse_ty(source: Normalized<&str>, edition: Edition) -> Result<ast::Ty<'_>> {
    parse_via(source, edition, |this| this.parse_ty())
}

fn parse_stmt(source: Normalized<&str>, edition: Edition) -> Result<ast::Stmt<'_>> {
    parse_via(source, edition, |this| this.parse_stmt(TokenKind::EndOfInput))
}

fn parse_expr(source: Normalized<&str>, edition: Edition) -> Result<ast::Expr<'_>> {
    parse_via(source, edition, |this| this.parse_expr())
}

fn parse_pat(source: Normalized<&str>, edition: Edition) -> Result<ast::Pat<'_>> {
    parse_via(source, edition, |this| this.parse_pat(super::pat::OrPolicy::Allowed))
}

#[test]
fn file_empty() {
    assert_matches!(
        parse_file(n!(""), Rust2015),
        Ok(ast::File { shebang: None, frontmatter: None, attrs: r!([]), items: r!([]), span: _ })
    );
}

// We only permit ASCII spaces and tabs in (the padding of) frontmatter infostrings & trailers.
// However, due to CRLF→LF normalization, we automatically also permit CR before the line break.
#[test]
fn frontmatter_crlf() {
    // See also <https://github.com/fmease/rasur/issues/15>.

    assert_matches!(
        parse_file(n!("---\t\r\n---\t\r\n"), Rust2015),
        Ok(ast::File { shebang: None, frontmatter: Some("---\t\n---"), .. })
    );
}

#[test]
fn frontmatter_cr() {
    // CR isn't "horizontal whitespace" and therefore forbidden inside infostrings.
    assert_matches!(
        parse_file(n!("--- \r \n---"), Rust2015),
        Err(r!([Error::InvalidFrontmatterInfostring(_)])),
    );

    // CR isn't "horizontal whitespace" and therefore forbidden inside trailers.
    assert_matches!(
        parse_file(n!("---\n--- \r "), Rust2015),
        Err(r!([Error::InvalidFrontmatterTrailer(_)]))
    );

    // "Stray" CRs inside the frontmatter body are explicitly forbidden.
    assert_matches!(
        parse_file(n!("---\n(\r)\n---"), Rust2015),
        Err(r!([Error::InvalidScalarInFrontmatterBody(_)]))
    );
}

#[test]
fn tuple_struct_field_visibility() {
    assert_matches!(
        parse_item(n!("struct T(pub([i32; 2]));"), Rust2015),
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Public,
                    ty: ast::Ty::Grouped(r!(ast::Ty::Array(..))),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    assert_matches!(
        parse_item(n!("struct T(pub(crate)[i32]);"), Rust2015),
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Restricted(ast::Path {
                        segs: r!([ast::PathSeg { ident: ast::Ident!("crate"), .. }])
                    }),
                    ty: ast::Ty::Slice(_),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    assert_matches!(
        parse_item(n!("struct T(pub(self)&());"), Rust2015),
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Restricted(ast::Path {
                        segs: r!([ast::PathSeg { ident: ast::Ident!("self"), .. }])
                    }),
                    ty: ast::Ty::Ref(_),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/21>
    assert_matches!(
        parse_item(n!("struct T(pub(super::U));"), Rust2015),
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Public,
                    ty: ast::Ty::Grouped(r!(ast::Ty::Path(ast::ExtPath {
                        ext: None,
                        path: ast::Path {
                            segs: r!([
                                ast::PathSeg { ident: ast::Ident!("super"), .. },
                                ast::PathSeg { ident: ast::Ident!("U"), .. },
                            ])
                        }
                    }))),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    assert_matches!(
        parse_item(n!("struct T(pub(super::U)impl);"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Impl, .. }, _)])),
    );

    assert_matches!(
        parse_item(n!("struct T(pub(in super::U)!);"), Rust2015),
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Restricted(ast::Path {
                        segs: r!([
                            ast::PathSeg { ident: ast::Ident!("super"), .. },
                            ast::PathSeg { ident: ast::Ident!("U"), .. },
                        ])
                    }),
                    ty: ast::Ty::Never,
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    assert_matches!(
        parse_item(n!("struct T(pub);"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CloseRoundBracket, .. },
            ExpectedFragment::Ty
        )]))
    );
}

#[test]
fn expr_attrs() {
    assert_matches!(
        parse_expr(n!("#[a]0"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, kind: ast::AttrKind::Normal(_) }]),
            kind: ast::ExprKind::Lit(_),
        })
    );

    assert_matches!(
        parse_expr(n!("#[a]#[b](#[c]#[d]0)"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([
                ast::Attr {
                    style: ast::AttrStyle::Outer,
                    kind: ast::AttrKind::Normal(ast::NormalAttr {
                        path: ast::Path {
                            segs: r!([ast::PathSeg { ident: ast::Ident!("a"), .. }])
                        },
                        ..
                    })
                },
                ast::Attr {
                    style: ast::AttrStyle::Outer,
                    kind: ast::AttrKind::Normal(ast::NormalAttr {
                        path: ast::Path {
                            segs: r!([ast::PathSeg { ident: ast::Ident!("b"), .. }])
                        },
                        ..
                    })
                },
            ]),
            kind: ast::ExprKind::Grouped(r!(ast::Expr {
                attrs: r!([
                    ast::Attr { style: ast::AttrStyle::Outer, .. },
                    ast::Attr { style: ast::AttrStyle::Outer, .. },
                ]),
                kind: ast::ExprKind::Lit(_),
            })),
        })
    );

    assert_matches!(
        parse_expr(n!("#[a]*x"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::UnOp(..)
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/25>
    assert_matches!(
        parse_expr(n!("#[a]!x"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::UnOp(..)
        })
    );

    assert_matches!(parse_expr(n!("#[a].."), Rust2015), Err(r!([Error::ForbiddenOuterAttrs])),);

    assert_matches!(parse_expr(n!("#[a]..()"), Rust2015), Err(r!([Error::ForbiddenOuterAttrs])),);

    assert_matches!(parse_expr(n!("#[a]..=_"), Rust2015), Err(r!([Error::ForbiddenOuterAttrs])),);

    assert_matches!(
        parse_expr(n!("#[a]&#[b]()"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Borrow(
                ..,
                r!(ast::Expr {
                    attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                    kind: ast::ExprKind::Tuple(_),
                    ..
                })
            )
        })
    );

    assert_matches!(
        parse_expr(n!("#[a]&#[b]()"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Borrow(
                ..,
                r!(ast::Expr {
                    attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                    kind: ast::ExprKind::Tuple(_),
                    ..
                })
            )
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/27>
    assert_matches!(
        parse_expr(n!("0..#[a]1"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([]),
            kind: ast::ExprKind::Range(
                Some(r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Lit(_), .. })),
                Some(r!(ast::Expr {
                    attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                    kind: ast::ExprKind::Lit(_),
                    ..
                })),
                ..
            )
        })
    );

    // The attr belongs to the inner expr, not to the cast itself.
    assert_matches!(
        parse_expr(n!("#[a]()as()"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([]),
            kind: ast::ExprKind::Cast(
                r!(ast::Expr {
                    attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                    kind: ast::ExprKind::Tuple(_)
                }),
                ..
            ),
        })
    );

    // The attr belongs to the inner left expr, not to the range itself.
    assert_matches!(
        parse_expr(n!("#[a]!0.."), Rust2015),
        Ok(ast::Expr {
            attrs: r!([]),
            kind: ast::ExprKind::Range(
                Some(r!(ast::Expr {
                    attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                    kind: ast::ExprKind::UnOp(..),
                    ..
                })),
                None,
                ..
            )
        })
    );

    // The attr belongs to the outermost try op expr, not to any of the inner exprs.
    assert_matches!(
        parse_expr(n!("#[a]0??"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Try(r!(ast::Expr {
                attrs: r!([]),
                kind: ast::ExprKind::Try(r!(ast::Expr {
                    attrs: r!([]),
                    kind: ast::ExprKind::Lit(_)
                }),),
            }))
        })
    );

    // The attr belongs to the (outer) call expr, not to the (inner) callee expr.
    assert_matches!(
        parse_expr(n!("#[a]f()"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Call(
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) }),
                r!([])
            )
        })
    );

    // Here, the attr of course belongs to the inner path expr, not to the call expr itself.
    assert_matches!(
        parse_expr(n!("(#[a]f)()"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([]),
            kind: ast::ExprKind::Call(
                r!(ast::Expr {
                    attrs: r!([]),
                    kind: ast::ExprKind::Grouped(r!(ast::Expr {
                        attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                        kind: ast::ExprKind::Path(_)
                    }))
                }),
                r!([])
            )
        })
    );

    // The attr belongs to the (outer) indexing expr, not to the (inner) indexed expr.
    assert_matches!(
        parse_expr(n!("#[a]f[0]"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Index(
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) }),
                _
            )
        })
    );

    // The attr belongs to the (outer) field expr, not to the (inner) path expr.
    assert_matches!(
        parse_expr(n!("#[a]x.y"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Field(
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) }),
                _,
            )
        })
    );

    // The outer attr belongs to the (outer) match expr, not to the (inner) scrutinee expr.
    assert_matches!(
        parse_expr(n!("#[a]x.match{#![b]}"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([
                ast::Attr { style: ast::AttrStyle::Outer, .. },
                ast::Attr { style: ast::AttrStyle::Inner, .. },
            ]),
            kind: ast::ExprKind::Match(r!(ast::MatchExpr {
                scrutinee: ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) },
                ..
            }))
        })
    );

    // The attr belongs to the inner left operand expr, not to the operation itself.
    assert_matches!(
        parse_expr(n!("#[a]-0+1"), Rust2015),
        Ok(ast::Expr {
            attrs: r!([]),
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Add,
                r!(ast::Expr {
                    attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
                    kind: ast::ExprKind::UnOp(..),
                    ..
                }),
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Lit(_), .. }),
                ..
            )
        })
    );
}

#[test]
fn double_borrow_and_double_borrow_expr() {
    assert_matches!(
        parse_expr(n!("&&0&&&&1"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::And,
                r!(ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ast::BorrowKind::Ref,
                        ast::Mutability::Not,
                        r!(ast::Expr {
                            kind: ast::ExprKind::Borrow(
                                ast::BorrowKind::Ref,
                                ast::Mutability::Not,
                                _
                            ),
                            ..
                        })
                    ),
                    ..
                }),
                r!(ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ast::BorrowKind::Ref,
                        ast::Mutability::Not,
                        r!(ast::Expr {
                            kind: ast::ExprKind::Borrow(
                                ast::BorrowKind::Ref,
                                ast::Mutability::Not,
                                _
                            ),
                            ..
                        }),
                    ),
                    ..
                })
            ),
            ..
        }),
    );
}

#[test]
fn or_nullary_closure_expr() {
    assert_matches!(
        parse_expr(n!("()||||()"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Or,
                r!(ast::Expr { kind: ast::ExprKind::Tuple(r!([])), .. }),
                r!(ast::Expr {
                    kind: ast::ExprKind::Closure(r!(ast::ClosureExpr {
                        bound_vars: r!([]),
                        modifiers: _,
                        params: r!([]),
                        ret_ty: None,
                        body: ast::Expr { kind: ast::ExprKind::Tuple(r!([])), .. }
                    })),
                    ..
                })
            ),
            ..
        })
    );
}

#[test]
fn mut_ref_mut_pat() {
    assert_matches!(
        parse_pat(n!("mut ref mut x"), Rust2015),
        Ok(ast::Pat::Binding(ast::BindingPat {
            mut_: ast::Mutability::Mut,
            by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mutability::Mut),
            binder: ast::Ident!("x"),
            pat: None,
        }))
    );
}

#[test]
fn false_angle_gen_args_expr() {
    assert_matches!(
        parse_expr(n!("f<i32>()"), Rust2015),
        Err(r!([Error::UnchainableExprOp(UnchainableExprOp::Compare, _)])),
    );

    assert_matches!(
        parse_expr(n!("f<i32>"), Rust2015),
        Err(r!([
            Error::UnchainableExprOp(UnchainableExprOp::Compare, _),
            Error::UnexpectedToken(Token { kind: TokenKind::EndOfInput, span: _ }, _)
        ])),
    );
}

#[test]
fn false_angle_gen_args_pat() {
    assert_matches!(
        parse_pat(n!("Some<i32>(0)"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _)]))
    );
}

#[test]
fn angle_gen_args_expr() {
    assert_matches!(
        parse_expr(n!("f::<i32>()"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Call(
                r!(ast::Expr {
                    kind: ast::ExprKind::Path(ast::ExtPath {
                        ext: None,
                        path: ast::Path {
                            segs: r!([ast::PathSeg {
                                ident: ast::Ident!("f"),
                                args: Some(ast::GenericArgs::Angle(r!([
                                    ast::AngleGenericArg::Argument(ast::GenericArg::Ty(
                                        ast::Ty::Path(ast::ExtPath {
                                            ext: None,
                                            path: ast::Path {
                                                segs: r!([ast::PathSeg {
                                                    ident: ast::Ident!("i32"),
                                                    args: None
                                                }])
                                            },
                                        })
                                    ))
                                ])))
                            }])
                        }
                    }),
                    ..
                }),
                r!([])
            ),
            ..
        })
    );
}

#[test]
fn angle_gen_args_pat() {
    assert_matches!(
        parse_pat(n!("Some::<i32>(0)"), Rust2015),
        Ok(ast::Pat::TupleStruct(r!(ast::TupleStructPat {
            path: ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: r!([ast::PathSeg {
                        ident: ast::Ident!("Some"),
                        args: Some(ast::GenericArgs::Angle(r!([ast::AngleGenericArg::Argument(
                            ast::GenericArg::Ty(ast::Ty::Path(ast::ExtPath {
                                ext: None,
                                path: ast::Path {
                                    segs: r!([ast::PathSeg {
                                        ident: ast::Ident!("i32"),
                                        args: None
                                    }]),
                                }
                            }))
                        )])))
                    }])
                }
            },
            fields: r!([ast::Pat::Lit(ast::Sign::None, ast::Lit::Num("0"))])
        }))),
    );
}

#[test]
fn angle_gen_args_ty() {
    assert_matches!(
        parse_ty(n!("Ty<'a, (), 0>"), Rust2015),
        Ok(ast::Ty::Path(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: r!([ast::PathSeg {
                    ident: ast::Ident!("Ty"),
                    args: Some(ast::GenericArgs::Angle(r!([
                        ast::AngleGenericArg::Argument(ast::GenericArg::Lifetime(ast::Ident!(
                            "'a"
                        ))),
                        ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Tuple(r!([])))),
                        ast::AngleGenericArg::Argument(ast::GenericArg::Const(ast::Expr {
                            kind: ast::ExprKind::Lit(ast::Lit::Num("0")),
                            ..
                        })),
                    ])))
                }])
            }
        }))
    );

    assert_matches!(parse_ty(n!("Ty::<'a, (), 0>"), Rust2015), Ok(_)); // just a smoke test
}

// While typically angle generic args have to be introduced with `::<` instead of `<`
// in exprs (and pats), the trait ref of an ext path gets treated to a "type context"
// and it's unambiguous that angle generic args are meant for the trait ref when
// encountering just `<`.
#[test]
fn angle_args_in_path_ext_expr() {
    assert_matches!(
        parse_expr(n!("<() as TraitRef<()>>::assoc"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Path(ast::ExtPath {
                ext: Some(ast::PathExt {
                    self_ty: ast::Ty::Tuple(r!([])),
                    trait_ref: Some(ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("TraitRef"),
                            args: Some(ast::GenericArgs::Angle(r!([
                                ast::AngleGenericArg::Argument(ast::GenericArg::Ty(
                                    ast::Ty::Tuple(r!([]))
                                ))
                            ])))
                        },])
                    })
                }),
                path: ast::Path {
                    segs: r!([ast::PathSeg { ident: ast::Ident!("assoc"), args: None }])
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
    assert_matches!(
        parse_expr(n!("x::()->()"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Path(r!(ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: r!([ast::PathSeg {
                        ident: ast::Ident!("x"),
                        args: Some(ast::GenericArgs::Paren {
                            inputs: r!([]),
                            output: Some(ast::Ty::Tuple(r!([]))),
                        })
                    }])
                }
            })),
            ..
        })
    );

    assert_matches!(
        parse_pat(n!("x::()->!::X"), Rust2015),
        Ok(ast::Pat::Path(r!(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: r!([
                    ast::PathSeg {
                        ident: ast::Ident!("x"),
                        args: Some(ast::GenericArgs::Paren {
                            inputs: r!([]),
                            output: Some(ast::Ty::Never),
                        })
                    },
                    ast::PathSeg { ident: ast::Ident!("X"), args: None }
                ])
            }
        })))
    );
}

#[test]
fn macro_call_item_gen_args() {
    assert_matches!(
        parse_item(n!("path::to::<>::call!();"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _)]))
    );

    assert_matches!(
        parse_item(n!("path::to::call<()>!();"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _)]))
    );
}

#[test]
fn macro_call_stmt_gen_args() {
    assert_matches!(
        parse_stmt(n!("path::to::<>::call::<>!();"), Rust2015),
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::MacroCall(r!(ast::MacroCall {
                    path: ast::Path {
                        segs: r!([
                            ast::PathSeg { ident: ast::Ident!("path"), args: None },
                            ast::PathSeg {
                                ident: ast::Ident!("to"),
                                args: Some(ast::GenericArgs::Angle(r!([])))
                            },
                            ast::PathSeg {
                                ident: ast::Ident!("call"),
                                args: Some(ast::GenericArgs::Angle(r!([])))
                            },
                        ])
                    },
                    bracket: ast::Bracket::Round,
                    stream: r!([]),
                })),
                ..
            },
            ast::Semicolon::Yes
        ))
    );

    assert_matches!(parse_stmt(n!("path::to::<>::call::()!();"), Rust2015), Ok(_)); // just a smoke test
}

#[test]
fn const_block_const_item_modifier() {
    assert_matches!(
        parse_expr(
            n!("{
    const {}
    const fn f() {}
}"),
            Rust2015
        ),
        Ok(ast::Expr {
            kind: ast::ExprKind::Block(
                None,
                r!(ast::BlockExpr {
                    attrs: r!([]),
                    stmts: r!([
                        ast::Stmt::Expr(
                            ast::Expr {
                                kind: ast::ExprKind::SpecialBlock(
                                    ast::SpecialBlockKind::Const,
                                    r!(ast::BlockExpr { attrs: r!([]), stmts: r!([]) })
                                ),
                                ..
                            },
                            ast::Semicolon::No
                        ),
                        ast::Stmt::Item(ast::Item {
                            attrs: r!([]),
                            vis: ast::Visibility::Inherited,
                            kind: ast::ItemKind::Fn(ast::FnItem {
                                modifiers: ast::FnItemModifiers {
                                    constness: ast::Constness::Const,
                                    ..
                                },
                                binder: ast::Ident!("f"),
                                ..
                            }),
                            span: _
                        }),
                    ])
                })
            ),
            ..
        })
    );

    assert_matches!(
        parse_file(
            n!("
    const {}
    const fn f() {}
"),
            Rust2015
        ),
        Ok(ast::File {
            items: r!([
                ast::Item {
                    kind: ast::ItemKind::ConstBlock(r!(ast::ConstBlockItem {
                        body: ast::BlockExpr { attrs: r!([]), stmts: r!([]) }
                    })),
                    ..
                },
                ast::Item {
                    attrs: r!([]),
                    vis: ast::Visibility::Inherited,
                    kind: ast::ItemKind::Fn(ast::FnItem {
                        modifiers: ast::FnItemModifiers { constness: ast::Constness::Const, .. },
                        binder: ast::Ident!("f"),
                        ..
                    }),
                    span: _
                },
            ]),
            ..
        })
    );
}

#[test]
fn control_flow_ops_block_expr() {
    assert_matches!(
        parse_expr(n!("if return {}"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, span: _ },
            ExpectedFragment::Token(TokenKind::OpenCurlyBracket),
        )]))
    );
    assert_matches!(
        parse_expr(n!("if return {} {}"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::If(r!(ast::IfExpr {
                condition: ast::Expr {
                    kind: ast::ExprKind::Return(Some(r!(ast::Expr {
                        kind: ast::ExprKind::Block(
                            None,
                            ast::BlockExpr { attrs: r!([]), stmts: r!([]) }
                        ),
                        ..
                    }))),
                    ..
                },
                consequent: ast::BlockExpr { attrs: r!([]), stmts: r!([]) },
                alternate: None
            })),
            ..
        })
    );

    // FIXME: Explainer, once I have one.
    assert_matches!(
        parse_expr(n!("if break {}"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::If(r!(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Break(None, None), .. },
                consequent: ast::BlockExpr { attrs: r!([]), stmts: r!([]) },
                alternate: None
            })),
            ..
        })
    );

    assert_matches!(
        parse_expr(n!("break {}"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Break(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::Block(
                        None,
                        ast::BlockExpr { attrs: r!([]), stmts: r!([]) }
                    ),
                    ..
                })
            ),
            ..
        })
    );

    assert_matches!(
        parse_expr(n!("if continue {}"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::If(r!(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Continue(None), .. },
                consequent: ast::BlockExpr { stmts: r!([]), .. },
                alternate: None
            })),
            ..
        })
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
fn qualified_struct_pat_in_for_loop_expr() {
    assert_matches!(
        parse_expr(n!("for<Ty as Trait>::AssocTy {} in () {}"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::ForLoop(r!(ast::ForLoopExpr {
                pat: ast::Pat::Struct(ast::StructPat {
                    path: ast::ExtPath {
                        ext: Some(ast::PathExt {
                            self_ty: ast::Ty::Path(ast::ExtPath {
                                ext: None,
                                path: ast::Path {
                                    segs: r!([ast::PathSeg {
                                        ident: ast::Ident!("Ty"),
                                        args: None
                                    }])
                                },
                            }),
                            trait_ref: Some(ast::Path {
                                segs: r!([ast::PathSeg {
                                    ident: ast::Ident!("Trait"),
                                    args: None
                                }])
                            })
                        }),
                        path: ast::Path {
                            segs: r!([ast::PathSeg { ident: ast::Ident!("AssocTy"), args: None }])
                        }
                    },
                    fields: r!([]),
                    rest: false
                }),
                ..
            })),
            ..
        })
    );
}

// It's never legal to reinterpret the token `<=` as `<` followed by `=`.
// Similarly for `<<=` which should never be viewed as `<` followed by `<` or `<<`.
// issue: <https://github.com/fmease/rasur/issues/11>
#[test]
fn dont_split_less_than_equals_for_angle_bracketed_lists() {
    assert_matches!(
        parse_expr(n!("0 as u64 <= 1"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Le,
                r!(ast::Expr { kind: ast::ExprKind::Cast(..), .. }),
                _
            ),
            ..
        })
    );

    assert_matches!(
        parse_expr(n!("x as T <<= y"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Assign(ast::AssignOp::BitShiftLeft),
                r!(ast::Expr { kind: ast::ExprKind::Cast(..), .. }),
                _
            ),
            ..
        })
    );
}

// FIXME: More extensively test receivers & fn params! Below are just temporary smoke tests.
#[test]
fn method_receivers() {
    assert_matches!(parse_item(n!("fn f(&self);"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&mut self);"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(mut self);"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&'a self);"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&'a mut self);"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&'a pin mut self);"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&pin const self);"), Rust2015), Ok(_));

    // issue: <https://github.com/fmease/rasur/issues/18>
    assert_matches!(parse_item(n!("fn f(self::T: ());"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&self::T: ());"), Rust2015), Ok(_));
    assert_matches!(parse_item(n!("fn f(&mut self::T: ());"), Rust2015), Ok(_));
}

#[test]
fn bare_trait_object_tys() {
    assert_matches!(
        parse_ty(n!("A+"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    assert_matches!(parse_ty(n!("Hold<A+>"), Rust2015), Ok(_));

    assert_matches!(
        parse_ty(n!("(A)+"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    // It's easy to accidentally accept the following code while trying to support the form above.
    assert_matches!(
        parse_ty(n!("(A+)+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SinglePlus, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )])),
    );

    assert_matches!(
        parse_ty(n!("?A"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { polarity: ast::BoundPolarity::Maybe, .. },
                ..
            }])
        ))
    );

    // MB: `?` is the only trait bound modifier that also "formally begins a type".
    //     `const`, `[const]`, `async` all don't.
    assert_matches!(parse_ty(n!("Hold<?A>"), Rust2015), Ok(_));

    assert_matches!(
        parse_ty(n!("(?A)+"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { polarity: ast::BoundPolarity::Maybe, .. },
                ..
            }])
        ))
    );

    assert_matches!(
        parse_ty(n!("const A"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { constness: ast::BoundConstness::Always, .. },
                ..
            }])
        ))
    );

    // See comment further up.
    assert_matches!(
        parse_ty(n!("Hold<const A>"), Rust2015),
        // The diagnostic could be better (we're expecting `Hold<const { … }>` at this point).
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CommonIdent, .. },
            ExpectedFragment::Token(TokenKind::OpenCurlyBracket),
        )]))
    );

    assert_matches!(
        parse_ty(n!("(const A)+"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { constness: ast::BoundConstness::Always, .. },
                ..
            }])
        ))
    );

    // This is also a bug upstream, see also <https://github.com/rust-lang/rust/issues/146122>.
    assert_matches!(
        parse_ty(n!("[const] A"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CloseSquareBracket, .. },
            ExpectedFragment::Bound
        )])),
    );

    assert_matches!(
        parse_ty(n!("async A"), Rust2018),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { asyncness: ast::BoundAsyncness::Always, .. },
                ..
            }])
        ))
    );

    // See comment further up.
    assert_matches!(
        parse_ty(n!("Hold<async A>"), Rust2018),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Async, .. }, _)]))
    );

    assert_matches!(
        parse_ty(n!("for<>A"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    assert_matches!(parse_ty(n!("Hold<for<>A>"), Rust2015), Ok(_));

    assert_matches!(
        parse_ty(n!("(for<>A)+"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    // It's easy to accidentally accept the following code while trying to support the form above.
    assert_matches!(
        parse_ty(n!("(for<>A+)+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SinglePlus, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )])),
    );

    assert_matches!(
        parse_ty(n!("for<>'a"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::TickedIdent, .. },
            ExpectedFragment::PathSegIdent
        )])),
    );

    assert_matches!(
        parse_ty(n!("for<>'a+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::TickedIdent, .. },
            ExpectedFragment::PathSegIdent
        )])),
    );

    assert_matches!(
        parse_ty(n!("'a+"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Outlives(_)])))
    );

    assert_matches!(parse_ty(n!("Hold<'a+>"), Rust2015), Ok(_));

    assert_matches!(parse_ty(n!("'a"), Rust2015), Err(r!([Error::LifetimeObjectTyWithoutPlus(_)])));

    // It makes sense to reject this since you can't parenthesize lifetimes in "normal" bounds either.
    assert_matches!(
        parse_ty(n!("('a)+"), Rust2015),
        Err(r!([
            Error::LifetimeObjectTyWithoutPlus(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::SinglePlus, .. },
                ExpectedFragment::Token(TokenKind::EndOfInput)
            )
        ]))
    );

    // issue: <https://github.com/fmease/rasur/issues/20>
    assert_matches!(
        parse_ty(n!("use<>"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Use(_)])))
    );

    // Indeed, even though you can't parenthesize precise-capturing lists
    // in "normal" bounds, you can do so in bare trait object type bounds.
    // If find it a bit janky. Might report upstream.
    assert_matches!(
        parse_ty(n!("(use<>)+"), Rust2015),
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Use(_)])))
    );

    // It's easy to accidentally accept the following code while trying to support the form above.
    assert_matches!(
        parse_ty(n!("(use<>+)+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SinglePlus, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )]))
    );

    assert_matches!(
        parse_ty(n!("Hold<use<>>"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Use, .. }, _)])),
    );

    assert_matches!(
        parse_ty(n!("A + B"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait { .. }, ast::Bound::Trait { .. }])
        ))
    );

    assert_matches!(
        parse_ty(n!("&A + B"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    assert_matches!(
        parse_ty(n!("&for<>A + B"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    assert_matches!(
        parse_ty(n!("*const A + B"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    assert_matches!(
        parse_ty(n!("&A + B"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    assert_matches!(
        parse_ty(n!("fn() -> A + B"), Rust2015),
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    // Like `dyn (Fn() -> A) + B`, not like `dyn Fn() -> (dyn A + B)`.
    assert_matches!(
        parse_ty(n!("Fn() -> A + B"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([
                ast::Bound::Trait {
                    path: ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("Fn"),
                            args: Some(ast::GenericArgs::Paren {
                                inputs: r!([]),
                                output: Some(ast::Ty::Path(ast::ExtPath {
                                    ext: None,
                                    path: ast::Path {
                                        segs: r!([ast::PathSeg { ident: ast::Ident!("A"), .. }])
                                    }
                                }))
                            })
                        }])
                    },
                    ..
                },
                ast::Bound::Trait {
                    path: ast::Path { segs: r!([ast::PathSeg { ident: ast::Ident!("B"), .. }]) },
                    ..
                }
            ]),
        )),
    );

    // Similarly
    assert_matches!(
        parse_ty(n!("Fn() -> (A) + B"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait { .. }, ast::Bound::Trait { .. }]),
        )),
    );

    // This is considered legal what I find slightly odd, see also my long comment in the type parser.
    // Normally, bare lifetimes aren't allowed in type position. At least, they need to be followed by
    // a `+` to count as a bare trait object type. However, below, the `+` doesn't actually "belong"
    // to the lifetime bound, it belongs to the parent bound list.
    assert_matches!(
        parse_ty(n!("Fn() -> 'a + A"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([
                ast::Bound::Trait {
                    path: ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("Fn"),
                            args: Some(ast::GenericArgs::Paren {
                                inputs: r!([]),
                                output: Some(ast::Ty::DynTrait(
                                    ast::DynKind::Bare,
                                    r!([ast::Bound::Outlives(_)])
                                )),
                            })
                        }])
                    },
                    ..
                },
                ast::Bound::Trait { .. }
            ]),
        )),
    );

    // The same happens here, too, in our impl but on the surface the `+` could truly belong
    // to either bare trait object type (still, it doesn't get rejected as ambiguous).
    assert_matches!(
        parse_ty(n!("Fn() -> 'a+"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                path: ast::Path {
                    segs: r!([ast::PathSeg {
                        ident: ast::Ident!("Fn"),
                        args: Some(ast::GenericArgs::Paren {
                            inputs: r!([]),
                            output: Some(ast::Ty::DynTrait(
                                ast::DynKind::Bare,
                                r!([ast::Bound::Outlives(_)])
                            )),
                        })
                    }])
                },
                ..
            },]),
        ))
    );

    // issue: <https://github.com/fmease/rasur/issues/23>
    assert_matches!(
        parse_expr(n!("0 as A + 1 as B"), Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Add,
                r!(ast::Expr {
                    kind: ast::ExprKind::Cast(
                        r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                        r!(ast::Ty::Path(..))
                    ),
                    ..
                }),
                r!(ast::Expr {
                    kind: ast::ExprKind::Cast(
                        r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                        r!(ast::Ty::Path(..))
                    ),
                    ..
                }),
            ),
            ..
        })
    );

    assert_matches!(
        parse_expr(n!("0 as for<> A+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );

    assert_matches!(
        parse_expr(n!("0 as 'a+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );

    assert_matches!(
        parse_expr(n!("0 as const A+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );

    assert_matches!(
        parse_expr(n!("0 as use<>+"), Rust2015),
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );
}

#[test]
fn ambiguous_plus() {
    assert_matches!(parse_ty(n!("&dyn A + B"), Rust2015), Err(r!([Error::AmbiguousPlus(_)])),);

    assert_matches!(parse_ty(n!("&dyn A+"), Rust2015), Err(r!([Error::AmbiguousPlus(_)])),);

    assert_matches!(parse_ty(n!("&impl A + B"), Rust2015), Err(r!([Error::AmbiguousPlus(_)])));

    assert_matches!(parse_ty(n!("&impl A+"), Rust2015), Err(r!([Error::AmbiguousPlus(_)])));

    assert_matches!(parse_ty(n!("F() -> dyn A + B"), Rust2015), Err(r!([Error::AmbiguousPlus(_)])));

    assert_matches!(
        parse_ty(n!("F() -> impl A + B"), Rust2015),
        Err(r!([Error::AmbiguousPlus(_)]))
    );

    assert_matches!(
        parse_ty(n!("dyn F() -> impl A+"), Rust2015),
        Err(r!([Error::AmbiguousPlus(_)]))
    );

    assert_matches!(
        parse_ty(n!("impl F() -> dyn A+"), Rust2015),
        Err(r!([Error::AmbiguousPlus(_)]))
    );

    // Indeed, this is not (to be) flagged as ambiguous.
    // I wonder if it's an oversight or intentional?
    assert_matches!(
        parse_ty(n!("impl F() -> for<> A + B"), Rust2015),
        Ok(ast::Ty::ImplTrait(r!([
            ast::Bound::Trait {
                path: ast::Path {
                    segs: r!([ast::PathSeg {
                        ident: ast::Ident!("F"),
                        args: Some(ast::GenericArgs::Paren {
                            inputs: r!([]),
                            output: Some(ast::Ty::DynTrait(
                                ast::DynKind::Bare,
                                r!([ast::Bound::Trait { .. }])
                            )),
                        })
                    }])
                },
                ..
            },
            ast::Bound::Trait { .. }
        ])))
    );

    // ... after all, you could hypothetically parse it like this:
    assert_matches!(
        parse_ty(n!("impl F() -> (for<> A + B)"), Rust2015),
        Ok(ast::Ty::ImplTrait(r!([ast::Bound::Trait {
            path: ast::Path {
                segs: r!([ast::PathSeg {
                    ident: ast::Ident!("F"),
                    args: Some(ast::GenericArgs::Paren {
                        inputs: r!([]),
                        output: Some(ast::Ty::Grouped(ast::Ty::DynTrait(
                            ast::DynKind::Bare,
                            r!([ast::Bound::Trait { .. }, ast::Bound::Trait { .. }])
                        ))),
                    })
                }])
            },
            ..
        },])))
    );

    // Not ambiguous (counterexample).
    assert_matches!(
        parse_ty(n!("F() -> fn() -> A + B"), Rust2015),
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([
                ast::Bound::Trait {
                    path: ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("F"),
                            args: Some(ast::GenericArgs::Paren {
                                inputs: r!([]),
                                output: Some(ast::Ty::FnPtr(..)),
                            })
                        }])
                    },
                    ..
                },
                ast::Bound::Trait { .. }
            ])
        ))
    );
}

// FIXME: macro_rules! in stmt pos (-> item not stmt); macro_rules! no binder == macro call
// FIXME: ops
// FIXME: structs in ifs etc.
// FIXME: almost-assoc-item-constraint due to (  )
// FIXME: ranges!! exprs, pats
// FIXME: A bunch of negative behavior tests!
// FIXME: Add stmt `{ 0 } + 0` error, stmt `&{ 0 } + 0` ok but stmt `..{ 0 } + 0` err! etc.
//        More: `0 + { 0 } + 0` OK. stmt `{ 0 } || 0` err.

#[test]
fn binding_modes() {
    assert_matches!(
        parse_file(
            n!("
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
"),
            Rust2015
        ),
        Ok(_) // just a smoke test
    );
}

#[test]
fn item_modifiers_in_item_ctxt() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    assert_matches!(
        parse_file(
            n!(r#"
async extern fn f() {}
async fn f() {}
async gen fn f() {}
async gen safe fn f() {}
async gen unsafe fn f() {}
async safe extern fn f() {}
async safe fn f() {}
async unsafe extern fn f() {}
async unsafe fn f() {}
auto trait Trait {}
const F: () = ();
const async fn f() {}
const async gen safe extern "C" fn f() {}
const async gen safe fn f() {}
const async safe extern fn f() {}
const async safe fn f() {}
const async unsafe extern fn f() {}
const async unsafe fn f() {}
const auto trait Trait {}
const auto: () = (); // [!]
const extern "C" fn f() {}
const extern fn f() {}
const gen fn f() {}
const impl !Trait for () {}
const impl () {}
const impl Trait for () {}
const safe extern fn f() {} // [***]
const safe fn f() {} // [***]
const safe: () = (); // [!]
const trait Trait {}
const unsafe auto trait Trait {} // [***]
const unsafe extern "C" fn f() {}
const unsafe impl Trait for () {} // [***]
const unsafe trait Trait {} // [***]
default const F: ();
default fn f();
default type T;
extern "C" fn f() {}
extern "C" {}
extern crate krate;
extern fn f() {}
extern {}
final fn f();
final type T;
fn f() {}
fn wrap() { safe fn f() {} } // [***]
gen extern fn f() {}
gen fn f() {}
gen unsafe fn f() {}
impl !Trait for () {}
impl Trait for () {}
impl const Trait for () {}
pub const extern "C" fn f() {}
pub const fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
pub default const async gen unsafe extern "C" fn f();
pub final const async gen unsafe extern "C" fn f();
pub fn f() {}
reuse const impl Trait for () {}
reuse const unsafe impl !Trait for () {} // [***]
reuse f;
reuse impl Trait for () {}
reuse unsafe impl Trait for () {}
safe extern "C" fn f() {}
safe extern fn f() {}
safe fn f() {}
safe static X: ();
static safe: ();
trait Trait {}
type const F: ();
type const safe: (); // [!]
unsafe auto trait Trait {}
unsafe extern "C" fn f() {}
unsafe extern "C" {}
unsafe extern {}
unsafe fn f() {}
unsafe impl Trait for () {}
unsafe impl const !Trait for () {}
unsafe impl const Trait for () {}
unsafe mod m;
unsafe static X: ();
unsafe trait Trait {}
use f;
use {self::*, self::{}};
"#),
            Rust2024 // for `async` and `gen`
        ),
        Ok(_) // just a smoke test
    );
}

#[test]
fn item_modifiers_in_stmt_ctxt() {
    // FIXME: Re-audit:
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    // NOTE: Commented-out "test cases" marked `[???]` don't get accepted
    //       by either rustc or rasur but I feel like they should be
    //       supported "logically" or for consistency.

    assert_matches!(
        parse_expr(
            n!(r#"{
async extern fn f() {}
async fn f() {}
async gen fn f() {}
async gen safe fn f() {}
async gen unsafe fn f() {}
async safe extern fn f() {}
async safe fn f() {}
async unsafe extern fn f() {}
async unsafe fn f() {}
auto trait Trait {}
const F: () = ();
const async fn f() {}
const async gen safe extern "C" fn f() {}
const async gen safe fn f() {}
const async safe extern fn f() {}
const async safe fn f() {}
const async unsafe extern fn f() {}
const async unsafe fn f() {}
const auto trait Trait {}
const auto: () = (); // [!]
const extern "C" fn f() {}
const extern fn f() {}
const gen fn f() {}
const impl !Trait for () {}
const impl () {}
const impl Trait for () {}
const safe extern fn f() {} // [***]
const safe fn f() {} // [***]
const safe: () = (); // [!]
const trait Trait {}
const unsafe auto trait Trait {} // [***]
const unsafe extern "C" fn f() {}
const unsafe impl Trait for () {} // [***]
const unsafe trait Trait {} // [***]
// default const F: (); // [???]
// default fn f(); // [???]
// default type T; // [???]
extern "C" fn f() {}
extern "C" {}
extern crate krate;
extern fn f() {}
extern {}
final fn f(); // issue: <https://github.com/fmease/rasur/issues/26>
final type T;
fn f() {}
fn wrap() { safe fn f() {} } // [***]
gen extern fn f() {}
gen fn f() {}
gen unsafe fn f() {}
impl !Trait for () {}
impl Trait for () {}
impl const Trait for () {}
pub const extern "C" fn f() {}
pub const fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
// pub default const async gen unsafe extern "C" fn f(); // [???]
pub final const async gen unsafe extern "C" fn f();
pub fn f() {}
reuse const impl Trait for () {}
reuse const unsafe impl !Trait for () {} // [***]
reuse f;
reuse impl Trait for () {}
reuse unsafe impl Trait for () {}
safe extern "C" fn f() {}
safe extern fn f() {}
safe fn f() {}
safe static X: ();
static safe: ();
trait Trait {}
type const F: ();
type const safe: (); // [!]
unsafe auto trait Trait {}
unsafe extern "C" fn f() {}
unsafe extern "C" {}
unsafe extern {}
unsafe fn f() {}
unsafe impl Trait for () {}
unsafe impl const !Trait for () {}
unsafe impl const Trait for () {}
unsafe mod m;
unsafe static X: ();
unsafe trait Trait {}
use f;
use {self::*, self::{}};
}"#),
            Rust2024 // for `async` and `gen`
        ),
        Ok(_) // just a smoke test
    );
}

#[test]
fn ty_modifiers() {
    assert_matches!(
        parse_ty(
            n!(r##"(
fn(),
for<'a> unsafe fn(),
for<> fn(),
for<> safe fn(),
for<T> unsafe extern fn(),
safe extern fn(),
safe fn(),
unsafe extern fn(),
unsafe extern r#"raw"# fn(),
unsafe fn(),
)"##),
            Rust2015
        ),
        Ok(_) // just a smoke test
    );
}

// Distinguishing between items and exprs (in stmt ctxts) is quite involved since
// they share quite a number of prefixes / modifier combinations.
#[test]
fn expr_modifiers_in_stmt_ctxt() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    assert_matches!(
        parse_expr(
            n!(r#"{
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
move || {};
move |_| {};
gen || {}; // [***]
gen |_| {}; // [***]
gen {};
gen use || {}; // [***]
gen use {};
gen static use |_| {}; // [***]
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
for<> const async gen static move | | {}; // [***]
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
const async gen static use | | {}; // [***]
const async gen move || {}; // [***]
const async gen move |_| {}; // [***]
async || {};
async |_| {};
async {};
async use || {};
async use {};
async static use | | {}; // [***]
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
}"#),
            Rust2024 // for `async` and `gen`
        ),
        Ok(_) // just a smoke test
    );
}

// rustc accepts a tiny amount more of these expr modifiers if not in a stmt ctxt.
#[test]
fn expr_modifiers_in_expr_ctxt() {
    // NOTE: Test cases marked `[+++]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    assert_matches!(
        parse_expr(
            n!(r#"{
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
(move || {});
(move |_| {});
(gen || {}); // [+++]
(gen |_| {}); // [+++]
(gen {}); // [+++]
(gen use || {}); // [+++]
(gen use {}); // [+++]
(gen static use |_| {}); // [+++]
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
(for<> const async gen static move | | {}); // [+++]
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
(const async gen static use | | {}); // [+++]
(const async gen move || {}); // [+++]
(const async gen move |_| {}); // [+++]
(async || {});
(async |_| {});
(async {});
(async use || {});
(async use {});
(async static use | | {}); // [+++]
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
}"#),
            Rust2024 // for `async` and `gen`
        ),
        Ok(_) // just a smoke test
    );
}

#[test]
fn trait_bounds() {
    // See also <https://github.com/fmease/rasur/issues/16>.

    assert_matches!(
        parse_ty(
            n!("(
impl !Trait,
impl (Trait),
impl (for<> Trait),
impl (for<> const async Trait),
impl ?Trait,
impl Trait,
impl [const] Trait,
impl [const] async Trait,
impl async Trait,
impl const Trait,
impl for<> Trait,
impl for<> const async Trait,
impl ~const Trait,
impl ~const async Trait,
)"),
            Rust2018 // for `async`
        ),
        Ok(_)
    ); // just a smoke test

    assert_matches!(
        parse_file(
            n!("
fn f<T: !Trait>();
fn f<T: (Trait)>();
fn f<T: (for<> Trait)>();
fn f<T: (for<> const async Trait)>();
fn f<T: ?Trait>();
fn f<T: Trait>();
fn f<T: [const] Trait>();
fn f<T: [const] async Trait>();
fn f<T: async Trait>();
fn f<T: const Trait>();
fn f<T: for<> Trait>();
fn f<T: for<> const async Trait>();
fn f<T: ~const Trait>();
fn f<T: ~const async Trait>();
"),
            Rust2018 // for `async`
        ),
        Ok(_)
    ); // just a smoke test
}
