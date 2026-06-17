use super::{parse_expr, parse_file, parse_item, parse_pat, parse_ty, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    lexer::IdentKind,
    token::{Token, TokenKind},
};

#[test]
fn abi_strs() {
    // To borrow our lexer terms, ABI strings have to have flavor UTF-8
    // and no suffix but they can be unguarded, guarded or raw.

    t!(
        parse_ty,
        Rust2015,
        r#"extern "ABI" fn()"#,
        Ok(ast::Ty::FnPtr(ast::FnPtrTy {
            modifiers: ast::FnPtrTyModifiers { extern_: ast::Extern::Yes(Some(r#""ABI""#)), .. },
            ..
        }))
    );

    t!(
        parse_ty,
        Rust2015,
        r#"extern r"ABI" fn()"#,
        Ok(ast::Ty::FnPtr(ast::FnPtrTy {
            modifiers: ast::FnPtrTyModifiers { extern_: ast::Extern::Yes(Some(r#"r"ABI""#)), .. },
            ..
        }))
    );

    t!(
        parse_ty,
        Rust2015,
        r##"extern r#"ABI"# fn()"##,
        Ok(ast::Ty::FnPtr(ast::FnPtrTy {
            modifiers: ast::FnPtrTyModifiers {
                extern_: ast::Extern::Yes(Some(r##"r#"ABI"#"##)),
                ..
            },
            ..
        }))
    );

    t!(parse_ty, Rust2015, r#"extern b"ABI" fn()"#, Err([Error::InvalidAbiStr(_)]));

    t!(parse_ty, Rust2021, r#"extern c"ABI" fn()"#, Err([Error::InvalidAbiStr(_)]));

    t!(parse_ty, Rust2018, r#"extern "ABI"suffix fn()"#, Err([Error::AbiStrSuffix(_)]));
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
                ast::BlockExpr {
                    stmts: [
                        ast::Stmt::Expr(
                            ast::Expr {
                                kind: ast::ExprKind::SpecialBlock(
                                    ast::SpecialBlockKind::Const,
                                    ast::BlockExpr { stmts: [] }
                                ),
                                ..
                            },
                            ast::Semicolon::No
                        ),
                        ast::Stmt::Item(ast::Item {
                            attrs: [],
                            vis: ast::Visibility::Inherited,
                            kind: ast::ItemKind::Fn(ast::FnItem {
                                modifiers: ast::FnItemModifiers { const_: ast::Const::Yes, .. },
                                binder: ast::Ident!("f"),
                                ..
                            }),
                            span: _
                        }),
                    ]
                }
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
            items: [
                ast::Item {
                    kind: ast::ItemKind::ConstBlock(ast::ConstBlockItem {
                        body: ast::BlockExpr { stmts: [] }
                    }),
                    ..
                },
                ast::Item {
                    attrs: [],
                    vis: ast::Visibility::Inherited,
                    kind: ast::ItemKind::Fn(ast::FnItem {
                        modifiers: ast::FnItemModifiers { const_: ast::Const::Yes, .. },
                        binder: ast::Ident!("f"),
                        ..
                    }),
                    span: _
                },
            ],
            ..
        })
    );
}

#[test]
fn builtin_syntax() {
    t!(parse_expr, Rust2015, "builtin#unknown(1 + 2 @)", Err([Error::UnknownBuiltinSyntax(_)]),);

    t!(
        parse_expr,
        Rust2021,
        "builtin#unknown(1 + 2 @)",
        Err([
            Error::ReservedPrefix(_),
            Error::UnexpectedToken(Token { kind: TokenKind::At, .. }, _)
        ]),
    );

    t!(parse_expr, Rust2021, "builtin # unknown(1 + 2 @)", Err([Error::UnknownBuiltinSyntax(_)]),);

    t!(
        parse_expr,
        Rust2021,
        "builtin # type_ascribe(0,i32)",
        Ok(ast::Expr {
            kind: ast::ExprKind::Ascription(
                ast::Expr { kind: ast::ExprKind::Lit(_), .. },
                ast::Ty::Path(_)
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
                ast::Ty::Path(ast::ExtPath {
                    ext: None,
                    path: ast::Path { segs: [ast::PathSeg { ident: ast::Ident!("X"), .. }] }
                }),
                [ast::Ident!("0"), ast::Ident!("x"), ast::Ident!("y"), ast::Ident!("1"),],
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
                ast::Expr { .. },
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
                ast::Expr { .. },
            ),
            ..
        })
    );

    t!(parse_pat, Rust2021, "builtin # deref(0)", Ok(ast::Pat::Deref(ast::Pat::Lit(..))));
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
            kind: ast::ItemKind::Fn(ast::FnItem { binder: ast::Ident!("\u{88f}"), .. }),
            ..
        })
    );

    // Since Unicode 17, U+10EFB is included in XID_Continue.
    t!(
        parse_item,
        Rust2015,
        "fn f\u{10efb}();",
        Ok(ast::Item {
            kind: ast::ItemKind::Fn(ast::FnItem { binder: ast::Ident!("f\u{10efb}"), .. }),
            ..
        })
    );
}

#[test]
fn raw_idents() {
    t!(
        parse_expr,
        Rust2015,
        "r#loop {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::Struct(ast::StructExpr {
                path: ast::ExtPath {
                    ext: None,
                    path: ast::Path { segs: [ast::PathSeg { ident: ast::Ident!("loop"), .. }] }
                },
                ..
            }),
            ..
        })
    );

    // Using a macro call to demonstrate that this is a lexical error even!
    t!(
        parse_item,
        Rust2015,
        "K!(r#self r#_);",
        Err([
            Error::InvalidRawIdent(IdentKind::Normal, _),
            Error::InvalidRawIdent(IdentKind::Normal, _)
        ])
    );

    // `r#` is considered to be a malformed raw delimited string literal. That's what rustc does, too.
    // We might want to diverge from that behaviorally eventually but it's not super important.
    t!(
        parse_item,
        Rust2015,
        "K!(r#);",
        Err([Error::InvalidStrLitDelimiter(_), Error::MissingClosingDelimiters(_)])
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
            kind: ast::ItemKind::MacroCall(ast::MacroCall {
                stream: [
                    Token { kind: TokenKind::TickedIdent, .. },
                    Token { kind: TokenKind::EndOfInput, .. }
                ],
                ..
            }),
            ..
        })
    );

    // However as lifetimes they are (except for `'_` and `'static` of course):
    t!(parse_item, Rust2015, "type T<'if>;", Err([Error::ReservedLifetime(_)]));

    // Similarly, as labels they are, too:
    t!(parse_expr, Rust2015, "'if: loop {}", Err([Error::ReservedLabel(_)]));
}

#[test]
fn raw_ticked_idents() {
    t!(
        parse_item,
        Rust2021,
        "type T<'r#if>;",
        Ok(ast::Item {
            kind: ast::ItemKind::TyAlias(ast::TyAliasItem {
                generics: ast::Generics {
                    params: [ast::GenericParam {
                        kind: ast::GenericParamKind::Lifetime(_),
                        binder: ast::Ident!("if"),
                        ..
                    }],
                    ..
                },
                ..
            }),
            ..
        })
    );

    t!(parse_expr, Rust2021, "'r#if: loop {}", Ok(_));

    t!(
        parse_expr,
        Rust2018,
        "'r#if: loop {}",
        Err([Error::UnexpectedToken(Token { kind: TokenKind::Hash, .. }, _)])
    );

    // Using a macro call to demonstrate that this is a lexical error even!
    t!(
        parse_item,
        Rust2018,
        "C! { 'r#if }",
        Ok(ast::Item {
            kind: ast::ItemKind::MacroCall(ast::MacroCall {
                stream: [
                    Token { kind: TokenKind::TickedIdent, .. },
                    Token { kind: TokenKind::Hash, .. },
                    Token { kind: TokenKind::If, .. },
                    Token { kind: TokenKind::EndOfInput, .. }
                ],
                ..
            }),
            ..
        })
    );

    t!(
        parse_item,
        Rust2021,
        "type R = &'r#_ ();",
        Err([Error::InvalidRawIdent(IdentKind::Ticked, _)])
    );

    // We once used to accept this by mistake!
    // Using a macro call to demonstrate that this is a lexical error even!
    t!(
        parse_item,
        Rust2021,
        "seg!('r#self 'r#Self);",
        Err([
            Error::InvalidRawIdent(IdentKind::Ticked, _),
            Error::InvalidRawIdent(IdentKind::Ticked, _)
        ])
    );

    // We once used to accept this by mistake!
    t!(parse_item, Rust2021, "W!('r#0);", Err([Error::InvalidRawIdent(IdentKind::Ticked, _)]));

    // We once used to accept this by mistake treating it as an empty raw ticked ident!
    t!(parse_item, Rust2021, "O!('r#);", Err([Error::InvalidRawIdent(IdentKind::Ticked, _)]));
}

#[test]
fn char_lits_or_ticked_idents() {
    t!(
        parse_item,
        Rust2015,
        "M! { 'a'a }",
        Ok(ast::Item {
            kind: ast::ItemKind::MacroCall(ast::MacroCall {
                stream: [
                    Token { kind: TokenKind::CharLit, .. },
                    Token { kind: TokenKind::LitSuffix, .. },
                    Token { kind: TokenKind::EndOfInput, .. }
                ],
                ..
            }),
            ..
        })
    );

    t!(
        parse_item,
        Rust2015,
        "M! { 'a 'a }",
        Ok(ast::Item {
            kind: ast::ItemKind::MacroCall(ast::MacroCall {
                stream: [
                    Token { kind: TokenKind::TickedIdent, .. },
                    Token { kind: TokenKind::TickedIdent, .. },
                    Token { kind: TokenKind::EndOfInput, .. }
                ],
                ..
            }),
            ..
        })
    );

    t!(parse_item, Rust2015, "M! { '?a'a }", Err([Error::MultiScalarCharLit(_)]));

    t!(
        parse_item,
        Rust2015,
        "M! { 'a?'a }",
        Ok(ast::Item {
            kind: ast::ItemKind::MacroCall(ast::MacroCall {
                stream: [
                    Token { kind: TokenKind::TickedIdent, .. },
                    Token { kind: TokenKind::QuestionMark, .. },
                    Token { kind: TokenKind::TickedIdent, .. },
                    Token { kind: TokenKind::EndOfInput, .. }
                ],
                ..
            }),
            ..
        })
    );

    // We once used to accept this by mistake treating it as a "legal" multi-scalar char lit!
    t!(
        parse_item,
        Rust2021,
        "W!('r#');",
        Err([
            Error::InvalidRawIdent(IdentKind::Ticked, _),
            Error::UnterminatedCharLit(_),
            Error::MissingClosingDelimiters(_),
        ])
    );

    // We once used to accidentally accept this & lex it as two consecutive ticked idents.
    t!(parse_item, Rust2021, "M! { 'r#a'a }", Err([Error::TickFollowingRawTickedIdent(_)]));

    // We once used to accidentally accept this & lex it as two consecutive ticked idents.
    t!(parse_item, Rust2021, "M! { 'r#a'r#a }", Err([Error::TickFollowingRawTickedIdent(_)]));
}
