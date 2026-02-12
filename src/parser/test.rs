use crate::{
    Edition::{self, *},
    ast,
    error::{Buffer as ErrorBuffer, Error, UnchainableExprOp},
    lexer::{StripFrontmatter, StripShebang, lex},
    token::{Token, TokenKind},
};
use std::assert_matches::assert_matches;

// FIXME: Replace these tests with golden tests of ~debug fmt'ed nodes (custom fmt, derived, compact)

type Result<T, E = Vec<Error>> = std::result::Result<T, E>;

fn parse_file(source: &str, edition: Edition) -> Result<ast::File<'_>, ()> {
    let mut errors = ErrorBuffer::Void;
    let file = lex(source, edition, StripShebang::Yes, StripFrontmatter::Yes, &mut errors);
    let file = super::parse(file, source, edition, &mut errors);
    file.map_err(drop)
}

// FIXME: make the impl nicer
fn parse_via<'src, T>(
    source: &'src str,
    edition: Edition,
    parse: impl FnOnce(&mut super::Parser<'_, '_, 'src>) -> super::Result<T>,
) -> Result<T> {
    let mut errors = ErrorBuffer::Hold(Vec::new());
    let file = lex(source, edition, StripShebang::No, StripFrontmatter::No, &mut errors);
    let mut p = super::Parser::new(&file.tokens, source, edition, &mut errors);
    let result = parse(&mut p).and_then(|r| {
        p.parse(TokenKind::EndOfInput)?;
        Ok(r)
    });
    match errors.non_empty() {
        Some(errors) => Err(errors),
        None => Ok(result.unwrap_or_else(|_| unreachable!())),
    }
}

fn parse_item(source: &str, edition: Edition) -> Result<ast::Item<'_>> {
    parse_via(source, edition, |this| this.parse_item(super::item::ItemCx::Boring))
}

fn parse_ty(source: &str, edition: Edition) -> Result<ast::Ty<'_>> {
    parse_via(source, edition, |this| this.parse_ty())
}

fn parse_stmt(source: &str, edition: Edition) -> Result<ast::Stmt<'_>> {
    parse_via(source, edition, |this| this.parse_stmt(TokenKind::EndOfInput))
}

fn parse_expr(source: &str, edition: Edition) -> Result<ast::Expr<'_>> {
    parse_via(source, edition, |this| this.parse_expr())
}

fn parse_pat(source: &str, edition: Edition) -> Result<ast::Pat<'_>> {
    parse_via(source, edition, |this| this.parse_pat(super::pat::OrPolicy::Allowed))
}

#[test]
fn file_empty() {
    assert_matches!(
        parse_file("", Rust2015),
        Ok(ast::File {
            shebang: None,
            frontmatter: None,
            attrs: deref!([]),
            items: deref!([]),
            span: _
        })
    );
}

#[test]
fn expr_double_borrow_and_double_borrow() {
    assert_matches!(
        parse_expr("&&0&&&&1", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::And,
                deref!(ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ast::BorrowKind::Ref,
                        ast::Mutability::Not,
                        deref!(ast::Expr {
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
                deref!(ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ast::BorrowKind::Ref,
                        ast::Mutability::Not,
                        deref!(ast::Expr {
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
fn expr_or_nullary_closure() {
    assert_matches!(
        parse_expr("()||||()", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Or,
                deref!(ast::Expr { kind: ast::ExprKind::Tuple(deref!([])), .. }),
                deref!(ast::Expr {
                    kind: ast::ExprKind::Closure(deref!(ast::ClosureExpr {
                        bound_vars: deref!([]),
                        modifiers: _,
                        params: deref!([]),
                        ret_ty: None,
                        body: ast::Expr { kind: ast::ExprKind::Tuple(deref!([])), .. }
                    })),
                    ..
                })
            ),
            ..
        })
    );
}

// Unstable feature: `mut_ref` <https://github.com/rust-lang/rust/issues/123076>.
#[test]
fn pat_mut_ref_mut() {
    assert_matches!(
        parse_pat("mut ref mut x", Rust2015),
        Ok(ast::Pat::Binding(ast::BindingPat {
            mut_: ast::Mutability::Mut,
            by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mutability::Mut),
            binder: ast::Ident!("x"),
            pat: None,
        }))
    );
}

#[test]
fn expr_false_angle_gen_args() {
    assert_matches!(
        parse_expr("f<i32>()", Rust2015),
        Err(deref!([Error::UnchainableExprOp(UnchainableExprOp::Compare, _)])),
    );

    assert_matches!(
        parse_expr("f<i32>", Rust2015),
        Err(deref!([
            Error::UnchainableExprOp(UnchainableExprOp::Compare, _),
            Error::UnexpectedToken(Token { kind: TokenKind::EndOfInput, span: _ }, _)
        ])),
    );
}

#[test]
fn pat_false_angle_gen_args() {
    assert_matches!(
        parse_pat("Some<i32>(0)", Rust2015),
        Err(deref!([Error::UnexpectedToken(
            Token { kind: TokenKind::SingleLessThan, span: _ },
            _
        )]))
    );
}

#[test]
fn expr_angle_gen_args() {
    assert_matches!(
        parse_expr("f::<i32>()", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Call(
                deref!(ast::Expr {
                    kind: ast::ExprKind::Path(ast::ExtPath {
                        ext: None,
                        path: ast::Path {
                            segs: deref!([ast::PathSeg {
                                ident: ast::Ident!("f"),
                                args: Some(ast::GenericArgs::Angle(deref!([
                                    ast::AngleGenericArg::Argument(ast::GenericArg::Ty(
                                        ast::Ty::Path(ast::ExtPath {
                                            ext: None,
                                            path: ast::Path {
                                                segs: deref!([ast::PathSeg {
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
                deref!([])
            ),
            ..
        })
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
                        ident: ast::Ident!("Some"),
                        args: Some(ast::GenericArgs::Angle(deref!([
                            ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Path(
                                ast::ExtPath {
                                    ext: None,
                                    path: ast::Path {
                                        segs: deref!([ast::PathSeg {
                                            ident: ast::Ident!("i32"),
                                            args: None
                                        }]),
                                    }
                                }
                            )))
                        ])))
                    }])
                }
            },
            fields: deref!([ast::Pat::Lit(ast::Sign::None, ast::Lit::Num("0"))])
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
                    ident: ast::Ident!("Ty"),
                    args: Some(ast::GenericArgs::Angle(deref!([
                        ast::AngleGenericArg::Argument(ast::GenericArg::Lifetime(ast::Ident!(
                            "'a"
                        ))),
                        ast::AngleGenericArg::Argument(ast::GenericArg::Ty(ast::Ty::Tuple(
                            deref!([])
                        ))),
                        ast::AngleGenericArg::Argument(ast::GenericArg::Const(ast::Expr {
                            kind: ast::ExprKind::Lit(ast::Lit::Num("0")),
                            ..
                        })),
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
        Ok(ast::Expr {
            kind: ast::ExprKind::Path(ast::ExtPath {
                ext: Some(ast::PathExt {
                    self_ty: ast::Ty::Tuple(deref!([])),
                    trait_ref: Some(ast::Path {
                        segs: deref!([ast::PathSeg {
                            ident: ast::Ident!("TraitRef"),
                            args: Some(ast::GenericArgs::Angle(deref!([
                                ast::AngleGenericArg::Argument(ast::GenericArg::Ty(
                                    ast::Ty::Tuple(deref!([]))
                                ))
                            ])))
                        },])
                    })
                }),
                path: ast::Path {
                    segs: deref!([ast::PathSeg { ident: ast::Ident!("assoc"), args: None }])
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
fn expr_pat_paren_gen_args_arrow() {
    assert_matches!(
        parse_expr("x::()->()", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Path(deref!(ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: deref!([ast::PathSeg {
                        ident: ast::Ident!("x"),
                        args: Some(ast::GenericArgs::Paren {
                            inputs: deref!([]),
                            output: Some(ast::Ty::Tuple(deref!([]))),
                        })
                    }])
                }
            })),
            ..
        })
    );

    assert_matches!(
        parse_pat("x::()->!::X", Rust2015),
        Ok(ast::Pat::Path(deref!(ast::ExtPath {
            ext: None,
            path: ast::Path {
                segs: deref!([
                    ast::PathSeg {
                        ident: ast::Ident!("x"),
                        args: Some(ast::GenericArgs::Paren {
                            inputs: deref!([]),
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
fn item_macro_call_gen_args() {
    assert_matches!(
        parse_item("path::to::<>::call!();", Rust2015),
        Err(deref!([Error::UnexpectedToken(
            Token { kind: TokenKind::SingleLessThan, span: _ },
            _
        )]))
    );

    assert_matches!(
        parse_item("path::to::call<()>!();", Rust2015),
        Err(deref!([Error::UnexpectedToken(
            Token { kind: TokenKind::SingleLessThan, span: _ },
            _
        )]))
    );
}

#[test]
fn stmt_macro_call_gen_args() {
    assert_matches!(
        parse_stmt("path::to::<>::call::<>!();", Rust2015),
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::MacroCall(deref!(ast::MacroCall {
                    path: ast::Path {
                        segs: deref!([
                            ast::PathSeg { ident: ast::Ident!("path"), args: None },
                            ast::PathSeg {
                                ident: ast::Ident!("to"),
                                args: Some(ast::GenericArgs::Angle(deref!([])))
                            },
                            ast::PathSeg {
                                ident: ast::Ident!("call"),
                                args: Some(ast::GenericArgs::Angle(deref!([])))
                            },
                        ])
                    },
                    bracket: ast::Bracket::Round,
                    stream: deref!([]),
                })),
                ..
            },
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
        Ok(ast::Expr {
            kind: ast::ExprKind::Block(
                None,
                deref!(ast::BlockExpr {
                    attrs: deref!([]),
                    stmts: deref!([
                        ast::Stmt::Expr(
                            ast::Expr {
                                kind: ast::ExprKind::SpecialBlock(
                                    ast::SpecialBlockKind::Const,
                                    deref!(ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) })
                                ),
                                ..
                            },
                            ast::Semicolon::No
                        ),
                        ast::Stmt::Item(ast::Item {
                            attrs: deref!([]),
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
}

#[test]
fn expr_control_flow_ops_block() {
    assert_matches!(
        parse_expr("if return {}", Rust2015),
        Err(deref!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, span: _ },
            super::ExpectedFragment::Token(TokenKind::OpenCurlyBracket),
        )]))
    );
    assert_matches!(
        parse_expr("if return {} {}", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::If(deref!(ast::IfExpr {
                condition: ast::Expr {
                    kind: ast::ExprKind::Return(Some(deref!(ast::Expr {
                        kind: ast::ExprKind::Block(
                            None,
                            ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) }
                        ),
                        ..
                    }))),
                    ..
                },
                consequent: ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) },
                alternate: None
            })),
            ..
        })
    );

    // FIXME: Explainer, once I have one.
    assert_matches!(
        parse_expr("if break {}", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::If(deref!(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Break(None, None), .. },
                consequent: ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) },
                alternate: None
            })),
            ..
        })
    );

    assert_matches!(
        parse_expr("break {}", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::Break(
                None,
                Some(ast::Expr {
                    kind: ast::ExprKind::Block(
                        None,
                        ast::BlockExpr { attrs: deref!([]), stmts: deref!([]) }
                    ),
                    ..
                })
            ),
            ..
        })
    );

    assert_matches!(
        parse_expr("if continue {}", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::If(deref!(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Continue(None), .. },
                consequent: ast::BlockExpr { stmts: deref!([]), .. },
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
fn expr_qualified_struct_pat_in_for_loop() {
    assert_matches!(
        parse_expr("for<Ty as Trait>::AssocTy {} in () {}", Rust2015),
        Ok(ast::Expr {
            kind: ast::ExprKind::ForLoop(deref!(ast::ForLoopExpr {
                pat: ast::Pat::Struct(ast::StructPat {
                    path: ast::ExtPath {
                        ext: Some(ast::PathExt {
                            self_ty: ast::Ty::Path(ast::ExtPath {
                                ext: None,
                                path: ast::Path {
                                    segs: deref!([ast::PathSeg {
                                        ident: ast::Ident!("Ty"),
                                        args: None
                                    }])
                                },
                            }),
                            trait_ref: Some(ast::Path {
                                segs: deref!([ast::PathSeg {
                                    ident: ast::Ident!("Trait"),
                                    args: None
                                }])
                            })
                        }),
                        path: ast::Path {
                            segs: deref!([ast::PathSeg {
                                ident: ast::Ident!("AssocTy"),
                                args: None
                            }])
                        }
                    },
                    fields: deref!([]),
                    rest: false
                }),
                ..
            })),
            ..
        })
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

#[test]
fn item_modifiers() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    // FIXME: Add `type const`, `const impl`, `reuse impl`
    assert_matches!(
        parse_file(
            r#"
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
const async gen safe fn f() {}
const async gen safe extern "C" fn f() {}
const async safe extern fn f() {}
const async safe fn f() {}
const async unsafe extern fn f() {}
const async unsafe fn f() {}
const auto trait Trait {}
const auto: () = (); // [!]
const extern "C" fn f() {}
const extern fn f() {}
const gen fn f() {}
const safe extern fn f() {} // [***]
const safe fn f() {} // [***]
const safe: () = (); // [!]
const trait Trait {}
const unsafe auto trait Trait {} // [***]
const unsafe extern "C" fn f() {}
const unsafe impl Trait for () {} // [***]
const unsafe trait Trait {} // [***]
extern "C" fn f() {}
extern "C" {}
extern crate krate;
extern fn f() {}
extern {}
fn f() {}
fn wrap() { safe fn f() {} } // [***]
gen fn f() {}
gen extern fn f() {}
gen unsafe fn f() {}
impl !Trait for () {}
impl Trait for () {}
impl const Trait for () {}
pub const extern "C" fn f() {}
pub const fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
pub fn f() {}
safe extern "C" fn f() {}
safe extern fn f() {}
safe fn f() {}
safe static X: ();
static safe: ();
trait Trait {}
unsafe auto trait Trait {}
unsafe extern "C" fn f() {}
unsafe extern "C" {}
unsafe extern {}
unsafe fn f() {}
unsafe mod m;
unsafe impl Trait for () {}
unsafe impl const !Trait for () {}
unsafe impl const Trait for () {}
unsafe static X: ();
unsafe trait Trait {}
"#,
            Rust2024 // for `async` and `gen`
        ),
        Ok(_) // just a smoke test
    );
}

#[test]
fn ty_modifiers() {
    assert_matches!(
        parse_ty(
            r##"(
fn(),
for<> fn(),
unsafe fn(),
unsafe extern fn(),
unsafe extern r#"raw"# fn(),
for<'a> unsafe fn(),
for<T> unsafe extern fn(),
)"##,
            Rust2015
        ),
        Ok(_) // just a smoke test
    );
}

// FIXME: Add `static` once we support that.
#[test]
fn expr_modifiers() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    assert_matches!(
        parse_expr(
            r#"{
|| {};
|_| {};
| | {};
static || {};
static |_| {};
static move || {};
static move | | {};
move || {};
move |_| {};
gen || {}; // [***]
gen |_| {}; // [***]
gen move || {}; // [***]
gen move |_| {}; // [***]
for<> || {};
for<> |_| {};
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
for<> const async gen move || {}; // [***]
for<> const async gen move |_| {}; // [***]
for<> const async gen static move | | {}; // [***]
for<> async || {};
for<> async |_| {};
for<> async move || {};
for<> async move |_| {};
for<> async gen || {};
for<> async gen |_| {};
for<> async gen move || {};
for<> async gen move |_| {};
const || {};
const |_| {};
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
const async gen move || {}; // [***]
const async gen move |_| {}; // [***]
async || {};
async |_| {};
async move || {};
async move |_| {};
async gen || {}; // [***]
async gen |_| {}; // [***]
async gen move || {}; // [***]
async gen move |_| {}; // [***]
}"#,
            Rust2024 // for `async` and `gen`
        ),
        Ok(_) // just a smoke test
    );
}
