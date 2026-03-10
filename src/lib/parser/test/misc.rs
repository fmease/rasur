use super::{parse_expr, parse_file, parse_item, parse_pat, parse_ty, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};
use deref as r;

#[test]
fn abi_strs() {
    // To borrow our lexer terms, ABI strings have to have flavor UTF-8
    // and no suffix but they can be unguarded, guarded or raw.

    t!(
        parse_ty,
        Rust2015,
        r#"extern "ABI" fn()"#,
        Ok(ast::Ty::FnPtr(r!(ast::FnPtrTy {
            modifiers: ast::FnPtrTyModifiers {
                externness: ast::Externness::Extern(Some(r#""ABI""#)),
                ..
            },
            ..
        })))
    );

    t!(
        parse_ty,
        Rust2015,
        r#"extern r"ABI" fn()"#,
        Ok(ast::Ty::FnPtr(r!(ast::FnPtrTy {
            modifiers: ast::FnPtrTyModifiers {
                externness: ast::Externness::Extern(Some(r#"r"ABI""#)),
                ..
            },
            ..
        })))
    );

    t!(
        parse_ty,
        Rust2015,
        r##"extern r#"ABI"# fn()"##,
        Ok(ast::Ty::FnPtr(r!(ast::FnPtrTy {
            modifiers: ast::FnPtrTyModifiers {
                externness: ast::Externness::Extern(Some(r##"r#"ABI"#"##)),
                ..
            },
            ..
        })))
    );

    t!(parse_ty, Rust2015, r#"extern b"ABI" fn()"#, Err(r!([Error::InvalidAbiStr(_)])));

    t!(parse_ty, Rust2021, r#"extern c"ABI" fn()"#, Err(r!([Error::InvalidAbiStr(_)])));

    t!(parse_ty, Rust2018, r#"extern "ABI"suffix fn()"#, Err(r!([Error::AbiStrSuffix(_)])),);
}

#[test]
fn const_block_const_item_modifier() {
    t!(
        parse_expr,
        Rust2015,
        "{
    const {}
    const fn f() {}
}",
        Ok(ast::Expr {
            kind: ast::ExprKind::Block(
                None,
                r!(ast::BlockExpr {
                    stmts: r!([
                        ast::Stmt::Expr(
                            ast::Expr {
                                kind: ast::ExprKind::SpecialBlock(
                                    ast::SpecialBlockKind::Const,
                                    r!(ast::BlockExpr { stmts: r!([]) })
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

    t!(
        parse_file,
        Rust2015,
        "
    const {}
    const fn f() {}
",
        Ok(ast::File {
            items: r!([
                ast::Item {
                    kind: ast::ItemKind::ConstBlock(r!(ast::ConstBlockItem {
                        body: ast::BlockExpr { stmts: r!([]) }
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
fn builtin_syntax() {
    t!(parse_expr, Rust2015, "builtin#unknown(1 + 2 @)", Err(r!([Error::UnknownBuiltinSyntax(_)])),);

    t!(
        parse_expr,
        Rust2021,
        "builtin#unknown(1 + 2 @)",
        Err(r!([
            Error::ReservedPrefix(_),
            Error::UnexpectedToken(Token { kind: TokenKind::At, .. }, _)
        ])),
    );

    t!(
        parse_expr,
        Rust2021,
        "builtin # unknown(1 + 2 @)",
        Err(r!([Error::UnknownBuiltinSyntax(_)])),
    );

    t!(
        parse_expr,
        Rust2021,
        "builtin # type_ascribe(0,i32)",
        Ok(ast::Expr {
            kind: ast::ExprKind::Ascription(
                r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                r!(ast::Ty::Path(_))
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2021,
        "builtin # offset_of(X,0.x.y.1)",
        Ok(ast::Expr {
            kind: ast::ExprKind::OffsetOf(
                r!(ast::Ty::Path(ast::ExtPath {
                    ext: None,
                    path: ast::Path { segs: r!([ast::PathSeg { ident: ast::Ident!("X"), .. }]) }
                })),
                r!([ast::Ident!("0"), ast::Ident!("x"), ast::Ident!("y"), ast::Ident!("1"),]),
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2021,
        "builtin # wrap_binder(&0)",
        Ok(ast::Expr {
            kind: ast::ExprKind::UnsafeBinderCast(
                ast::UnsafeBinderCastKind::Wrap,
                r!(ast::Expr { .. }),
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2021,
        "builtin # unwrap_binder(x)",
        Ok(ast::Expr {
            kind: ast::ExprKind::UnsafeBinderCast(
                ast::UnsafeBinderCastKind::Unwrap,
                r!(ast::Expr { .. }),
            ),
            ..
        })
    );

    t!(parse_pat, Rust2021, "builtin # deref(0)", Ok(ast::Pat::Deref(r!(ast::Pat::Lit(..)))));
}

#[test]
fn unicode_17() {
    // See also:
    // <https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AU17%3AXID_Start%3A%5D+-+%5B%3AU16%3AXID_Start%3A%5D&g=&i=idstatus>
    // <https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AU17%3AXID_Continue%3A%5D+-+%5B%3AU16%3AXID_Continue%3A%5D+-+%5B%3AXID_Start%3A%5D&g=&i=idstatus>

    // Since Unicode 17, U+088F is included in XID_Start.
    t!(
        parse_item,
        Rust2015,
        "fn \u{88f}();",
        Ok(ast::Item {
            kind: ast::ItemKind::Fn(r!(ast::FnItem { binder: ast::Ident!("\u{88f}"), .. })),
            ..
        })
    );

    // Since Unicode 17, U+10EFB is included in XID_Continue.
    t!(
        parse_item,
        Rust2015,
        "fn f\u{10efb}();",
        Ok(ast::Item {
            kind: ast::ItemKind::Fn(r!(ast::FnItem { binder: ast::Ident!("f\u{10efb}"), .. })),
            ..
        })
    );
}

#[test]
fn ticked_idents() {
    // Ticked keywords aren't illegal per se:
    t!(
        parse_item,
        Rust2015,
        "M! { 'if }",
        Ok(ast::Item {
            kind: ast::ItemKind::MacroCall(r!(ast::MacroCall {
                stream: r!([Token { kind: TokenKind::TickedIdent, .. }]),
                ..
            })),
            ..
        })
    );

    // However as lifetimes they are (except for `'_` and `'static` of course):
    t!(parse_item, Rust2015, "type T<'if>;", Err(r!([Error::ReservedLifetime(_)])));

    // Similarly, as labels they are, too:
    t!(parse_expr, Rust2015, "'if: loop {}", Err(r!([Error::ReservedLabel(_)])));
}
