use super::super::ExpectedFragment;
use super::{parse_expr, parse_file, parse_ty, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};
use deref as r;

#[test]
fn bare_trait_object_tys() {
    t!(
        parse_ty,
        Rust2015,
        "A+",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    t!(parse_ty, Rust2015, "Hold<A+>", Ok(_));

    t!(
        parse_ty,
        Rust2015,
        "(A)+",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    // It's easy to accidentally accept the following code while trying to support the form above.
    t!(
        parse_ty,
        Rust2015,
        "(A+)+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SinglePlus, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )])),
    );

    t!(
        parse_ty,
        Rust2015,
        "?A",
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
    t!(parse_ty, Rust2015, "Hold<?A>", Ok(_));

    t!(
        parse_ty,
        Rust2015,
        "(?A)+",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { polarity: ast::BoundPolarity::Maybe, .. },
                ..
            }])
        ))
    );

    t!(
        parse_ty,
        Rust2015,
        "const A",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { constness: ast::BoundConstness::Always, .. },
                ..
            }])
        ))
    );

    // See comment further up.
    t!(
        parse_ty,
        Rust2015,
        "Hold<const A>",
        // The diagnostic could be better (we're expecting `Hold<const { … }>` at this point).
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CommonIdent, .. },
            ExpectedFragment::Token(TokenKind::OpenCurlyBracket),
        )]))
    );

    t!(
        parse_ty,
        Rust2015,
        "(const A)+",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { constness: ast::BoundConstness::Always, .. },
                ..
            }])
        ))
    );

    // This is also a bug upstream, see also <https://github.com/rust-lang/rust/issues/146122>.
    t!(
        parse_ty,
        Rust2015,
        "[const] A",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CloseSquareBracket, .. },
            ExpectedFragment::Bound
        )])),
    );

    t!(
        parse_ty,
        Rust2018,
        "async A",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                modifiers: ast::TraitBoundModifiers { asyncness: ast::BoundAsyncness::Always, .. },
                ..
            }])
        ))
    );

    // See comment further up.
    t!(
        parse_ty,
        Rust2018,
        "Hold<async A>",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Async, .. }, _)]))
    );

    t!(
        parse_ty,
        Rust2015,
        "for<>A",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    t!(parse_ty, Rust2015, "Hold<for<>A>", Ok(_));

    t!(
        parse_ty,
        Rust2015,
        "(for<>A)+",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Trait { .. }])))
    );

    // It's easy to accidentally accept the following code while trying to support the form above.
    t!(
        parse_ty,
        Rust2015,
        "(for<>A+)+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SinglePlus, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )])),
    );

    t!(
        parse_ty,
        Rust2015,
        "for<>'a",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::TickedIdent, .. },
            ExpectedFragment::PathSegIdent
        )])),
    );

    t!(
        parse_ty,
        Rust2015,
        "for<>'a+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::TickedIdent, .. },
            ExpectedFragment::PathSegIdent
        )])),
    );

    t!(
        parse_ty,
        Rust2015,
        "'a+",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Outlives(_)])))
    );

    t!(parse_ty, Rust2015, "Hold<'a+>", Ok(_));

    t!(parse_ty, Rust2015, "'a", Err(r!([Error::LifetimeObjectTyWithoutPlus(_)])));

    // It makes sense to reject this since you can't parenthesize lifetimes in "normal" bounds either.
    t!(
        parse_ty,
        Rust2015,
        "('a)+",
        Err(r!([
            Error::LifetimeObjectTyWithoutPlus(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::SinglePlus, .. },
                ExpectedFragment::Token(TokenKind::EndOfInput)
            )
        ]))
    );

    // issue: <https://github.com/fmease/rasur/issues/20>
    t!(
        parse_ty,
        Rust2015,
        "use<>",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Use(_)])))
    );

    // Indeed, even though you can't parenthesize precise-capturing lists
    // in "normal" bounds, you can do so in bare trait object type bounds.
    // If find it a bit janky. Might report upstream.
    t!(
        parse_ty,
        Rust2015,
        "(use<>)+",
        Ok(ast::Ty::DynTrait(ast::DynKind::Bare, r!([ast::Bound::Use(_)])))
    );

    // It's easy to accidentally accept the following code while trying to support the form above.
    t!(
        parse_ty,
        Rust2015,
        "(use<>+)+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SinglePlus, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )]))
    );

    t!(
        parse_ty,
        Rust2015,
        "Hold<use<>>",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Use, .. }, _)])),
    );

    t!(
        parse_ty,
        Rust2015,
        "A + B",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait { .. }, ast::Bound::Trait { .. }])
        ))
    );

    t!(
        parse_ty,
        Rust2015,
        "&A + B",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    t!(
        parse_ty,
        Rust2015,
        "&for<>A + B",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    t!(
        parse_ty,
        Rust2015,
        "*const A + B",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    t!(
        parse_ty,
        Rust2015,
        "&A + B",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    t!(
        parse_ty,
        Rust2015,
        "fn() -> A + B",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SinglePlus, .. }, _)])),
    );

    // Like `dyn (Fn() -> A) + B`, not like `dyn Fn() -> (dyn A + B)`.
    t!(
        parse_ty,
        Rust2015,
        "Fn() -> A + B",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([
                ast::Bound::Trait {
                    path: ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("Fn"),
                            args: Some(ast::GenericArgs::Paren(
                                r!([]),
                                Some(ast::Ty::Path(ast::ExtPath {
                                    ext: None,
                                    path: ast::Path {
                                        segs: r!([ast::PathSeg { ident: ast::Ident!("A"), .. }])
                                    }
                                }))
                            ))
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
    t!(
        parse_ty,
        Rust2015,
        "Fn() -> (A) + B",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait { .. }, ast::Bound::Trait { .. }]),
        )),
    );

    // This is considered legal what I find slightly odd, see also my long comment in the type parser.
    // Normally, bare lifetimes aren't allowed in type position. At least, they need to be followed by
    // a `+` to count as a bare trait object type. However, below, the `+` doesn't actually "belong"
    // to the lifetime bound, it belongs to the parent bound list.
    t!(
        parse_ty,
        Rust2015,
        "Fn() -> 'a + A",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([
                ast::Bound::Trait {
                    path: ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("Fn"),
                            args: Some(ast::GenericArgs::Paren(
                                r!([]),
                                Some(ast::Ty::DynTrait(
                                    ast::DynKind::Bare,
                                    r!([ast::Bound::Outlives(_)])
                                )),
                            ))
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
    t!(
        parse_ty,
        Rust2015,
        "Fn() -> 'a+",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([ast::Bound::Trait {
                path: ast::Path {
                    segs: r!([ast::PathSeg {
                        ident: ast::Ident!("Fn"),
                        args: Some(ast::GenericArgs::Paren(
                            r!([]),
                            Some(ast::Ty::DynTrait(
                                ast::DynKind::Bare,
                                r!([ast::Bound::Outlives(_)])
                            )),
                        ))
                    }])
                },
                ..
            },]),
        ))
    );

    // issue: <https://github.com/fmease/rasur/issues/23>
    t!(
        parse_expr,
        Rust2015,
        "0 as A + 1 as B",
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

    t!(
        parse_expr,
        Rust2015,
        "0 as for<> A+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );

    t!(
        parse_expr,
        Rust2015,
        "0 as 'a+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );

    t!(
        parse_expr,
        Rust2015,
        "0 as const A+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );

    t!(
        parse_expr,
        Rust2015,
        "0 as use<>+",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )]))
    );
}

#[test]
fn ambiguous_plus() {
    t!(parse_ty, Rust2015, "&dyn A + B", Err(r!([Error::AmbiguousPlus(_)])),);

    t!(parse_ty, Rust2015, "&dyn A+", Err(r!([Error::AmbiguousPlus(_)])),);

    t!(parse_ty, Rust2015, "&impl A + B", Err(r!([Error::AmbiguousPlus(_)])));

    t!(parse_ty, Rust2015, "&impl A+", Err(r!([Error::AmbiguousPlus(_)])));

    t!(parse_ty, Rust2015, "F() -> dyn A + B", Err(r!([Error::AmbiguousPlus(_)])));

    t!(parse_ty, Rust2015, "F() -> impl A + B", Err(r!([Error::AmbiguousPlus(_)])));

    t!(parse_ty, Rust2015, "dyn F() -> impl A+", Err(r!([Error::AmbiguousPlus(_)])));

    t!(parse_ty, Rust2015, "impl F() -> dyn A+", Err(r!([Error::AmbiguousPlus(_)])));

    // Indeed, this is not (to be) flagged as ambiguous.
    // I wonder if it's an oversight or intentional?
    t!(
        parse_ty,
        Rust2015,
        "impl F() -> for<> A + B",
        Ok(ast::Ty::ImplTrait(r!([
            ast::Bound::Trait {
                path: ast::Path {
                    segs: r!([ast::PathSeg {
                        ident: ast::Ident!("F"),
                        args: Some(ast::GenericArgs::Paren(
                            r!([]),
                            Some(ast::Ty::DynTrait(
                                ast::DynKind::Bare,
                                r!([ast::Bound::Trait { .. }])
                            )),
                        ))
                    }])
                },
                ..
            },
            ast::Bound::Trait { .. }
        ])))
    );

    // ... after all, you could hypothetically parse it like this:
    t!(
        parse_ty,
        Rust2015,
        "impl F() -> (for<> A + B)",
        Ok(ast::Ty::ImplTrait(r!([ast::Bound::Trait {
            path: ast::Path {
                segs: r!([ast::PathSeg {
                    ident: ast::Ident!("F"),
                    args: Some(ast::GenericArgs::Paren(
                        r!([]),
                        Some(ast::Ty::Grouped(ast::Ty::DynTrait(
                            ast::DynKind::Bare,
                            r!([ast::Bound::Trait { .. }, ast::Bound::Trait { .. }])
                        ))),
                    ))
                }])
            },
            ..
        },])))
    );

    // Not ambiguous (counterexample).
    t!(
        parse_ty,
        Rust2015,
        "F() -> fn() -> A + B",
        Ok(ast::Ty::DynTrait(
            ast::DynKind::Bare,
            r!([
                ast::Bound::Trait {
                    path: ast::Path {
                        segs: r!([ast::PathSeg {
                            ident: ast::Ident!("F"),
                            args: Some(ast::GenericArgs::Paren(r!([]), Some(ast::Ty::FnPtr(..))))
                        }])
                    },
                    ..
                },
                ast::Bound::Trait { .. }
            ])
        ))
    );
}

#[test]
fn ty_modifiers() {
    t!(
        parse_ty,
        Rust2015,
        r##"(
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
)"##,
        Ok(_) // just a smoke test
    );
}

#[test]
fn trait_bounds() {
    // See also <https://github.com/fmease/rasur/issues/16>.

    t!(
        parse_ty,
        Rust2018, // for `async`
        "(
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
)",
        Ok(_) // just a smoke test
    );

    t!(
        parse_file,
        Rust2018, // for `async`
        "
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
",
        Ok(_) // just a smoke test
    );
}
