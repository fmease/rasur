use super::super::Fragment;
use super::{parse_expr, parse_pat, parse_ty, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};

#[test]
fn suffixes_invalid_places() {
    t!(
        parse_expr,
        Rust2015,
        "compound.0suffix",
        Err([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            [Fragment::Token(TokenKind::EndOfInput)],
        )])
    );

    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0suffix)",
        Err([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            [Fragment::Token(TokenKind::SingleDot)],
        )])
    );

    t!(
        parse_ty,
        Rust2015,
        "builtin#field_of(T, 0suffix)",
        Err([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            [Fragment::Token(TokenKind::SingleDot)],
        )])
    );

    t!(
        parse_expr,
        Rust2015,
        "Compound { 0suffix: 0 }",
        Err([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            [Fragment::Token(TokenKind::SingleColon)],
        )])
    );

    t!(
        parse_pat,
        Rust2015,
        "Compound { 0suffix: 0 }",
        Err([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            [Fragment::Token(TokenKind::SingleColon)],
        )])
    );

    t!(
        parse_pat,
        Rust2015,
        "Compound { 0suffix }",
        Err([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            [Fragment::Token(TokenKind::SingleColon)],
        )])
    );
}

#[test]
fn exponents_invalid_places() {
    // In field exprs, "exponents" in the numeric identifier are legal...
    t!(
        parse_expr,
        Rust2015,
        "compound.0e1",
        Ok(ast::Expr { kind: ast::ExprKind::Field(_, ast::Ident!("0e1")), .. }),
    );
    t!(
        parse_expr,
        Rust2015,
        "compound.0.1e2", // exercise float lit splitting
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                ast::Expr { kind: ast::ExprKind::Field(_, ast::Ident!("0")), .. },
                ast::Ident!("1e2")
            ),
            ..
        }),
    );

    // ...unless the "exponent" contains an explicit sign:
    t!(parse_expr, Rust2015, "compound.0e+1", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_expr, Rust2015, "compound.0e-1", Err([Error::InvalidNumericIdent(_)]));
    t!(
        parse_expr,
        Rust2015,
        "compound.0.1e+2", // exercise float lit splitting
        Err([Error::InvalidNumericIdent(_)])
    );
    t!(
        parse_expr,
        Rust2015,
        "compound.0. 1e-2", // exercise float lit splitting
        Err([Error::InvalidNumericIdent(_)])
    );

    // Similarly, in OffsetOf/FieldOf exprs, "exponents" in the numeric are legal...
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0e1)",
        Ok(ast::Expr { kind: ast::ExprKind::OffsetOf(_, [ast::Ident!("0e1")]), .. }),
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0.1e2)", // exercise float lit splitting
        Ok(ast::Expr {
            kind: ast::ExprKind::OffsetOf(_, [ast::Ident!("0"), ast::Ident!("1e2")]),
            ..
        }),
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0. 1e2)", // exercise float lit splitting
        Ok(ast::Expr {
            kind: ast::ExprKind::OffsetOf(_, [ast::Ident!("0"), ast::Ident!("1e2")]),
            ..
        }),
    );

    // ...unless the "exponent" contains an explicit sign:
    t!(parse_expr, Rust2015, "builtin#offset_of(T, 0e+1)", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_expr, Rust2015, "builtin#offset_of(T, 0e-1)", Err([Error::InvalidNumericIdent(_)]));
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0.1e+2)", // exercise float lit splitting
        Err([Error::InvalidNumericIdent(_)])
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0. 1e-2)", // exercise float lit splitting
        Err([Error::InvalidNumericIdent(_)])
    );

    // In stark contrast, in struct exprs & pats  "exponents" are outright forbidden
    // regardless of whether they have an explicit sign or not:

    t!(parse_expr, Rust2015, "Compound { 0e1: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_expr, Rust2015, "Compound { 0e-1: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_pat, Rust2015, "Compound { 0e1: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_pat, Rust2015, "Compound { 0e+1: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(
        parse_pat,
        Rust2015,
        "Compound { 0e1 }",
        Err([
            Error::InvalidNumericIdent(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::CloseCurlyBracket, .. },
                [Fragment::Token(TokenKind::SingleColon)]
            )
        ])
    );
    t!(
        parse_pat,
        Rust2015,
        "Compound { 0e+1 }",
        Err([
            Error::InvalidNumericIdent(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::CloseCurlyBracket, .. },
                [Fragment::Token(TokenKind::SingleColon)]
            )
        ])
    );
    t!(
        parse_pat,
        Rust2015,
        "Compound { 0e-1 }",
        Err([
            Error::InvalidNumericIdent(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::CloseCurlyBracket, .. },
                [Fragment::Token(TokenKind::SingleColon)]
            )
        ])
    );
}

#[test]
fn fractional_part_invalid_places() {
    // We lex `0.0` and `0.` as a single token, a number literal.
    // However, in the cases below we require integer literals.
    // The parser needs to inspect the literal itself to detect this.

    t!(parse_expr, Rust2015, "Compound { 0.0: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_expr, Rust2015, "Compound { 0.: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_pat, Rust2015, "Compound { 0.0: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(parse_pat, Rust2015, "Compound { 0.: 0 }", Err([Error::InvalidNumericIdent(_)]));
    t!(
        parse_pat,
        Rust2015,
        "Compound { 0.0 }",
        Err([
            Error::InvalidNumericIdent(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::CloseCurlyBracket, .. },
                [Fragment::Token(TokenKind::SingleColon)]
            )
        ])
    );
    t!(
        parse_pat,
        Rust2015,
        "Compound { 0. }",
        Err([
            Error::InvalidNumericIdent(_),
            Error::UnexpectedToken(
                Token { kind: TokenKind::CloseCurlyBracket, .. },
                [Fragment::Token(TokenKind::SingleColon)]
            )
        ])
    );
}
