use super::super::ExpectedFragment;
use super::{parse_expr, parse_pat, parse_ty, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};
use deref as r;

#[test]
fn num_lit_suffixes_invalid_places() {
    t!(
        parse_expr,
        Rust2015,
        "compound.0suffix",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )]))
    );

    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0suffix)",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            ExpectedFragment::Token(TokenKind::SingleDot),
        )]))
    );

    t!(
        parse_ty,
        Rust2015,
        "builtin#field_of(T, 0suffix)",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            ExpectedFragment::Token(TokenKind::SingleDot),
        )]))
    );

    t!(
        parse_expr,
        Rust2015,
        "Compound { 0suffix: 0 }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            ExpectedFragment::Token(TokenKind::SingleColon),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "Compound { 0suffix: 0 }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            ExpectedFragment::Token(TokenKind::Comma),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "Compound { 0suffix }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::LitSuffix, .. },
            ExpectedFragment::Token(TokenKind::Comma),
        )]))
    );
}

#[test]
fn num_lit_exponents_invalid_places() {
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
                r!(ast::Expr { kind: ast::ExprKind::Field(_, ast::Ident!("0")), .. }),
                ast::Ident!("1e2")
            ),
            ..
        }),
    );

    // ...unless the "exponent" contains an explicit sign:
    t!(parse_expr, Rust2015, "compound.0e+1", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_expr, Rust2015, "compound.0e-1", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(
        parse_expr,
        Rust2015,
        "compound.0.1e+2", // exercise float lit splitting
        Err(r!([Error::InvalidNumericIdent(_)]))
    );
    t!(
        parse_expr,
        Rust2015,
        "compound.0. 1e-2", // exercise float lit splitting
        Err(r!([Error::InvalidNumericIdent(_)]))
    );

    // Similarly, in OffsetOf/FieldOf exprs, "exponents" in the numeric are legal...
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0e1)",
        Ok(ast::Expr { kind: ast::ExprKind::OffsetOf(_, r!([ast::Ident!("0e1")])), .. }),
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0.1e2)", // exercise float lit splitting
        Ok(ast::Expr {
            kind: ast::ExprKind::OffsetOf(_, r!([ast::Ident!("0"), ast::Ident!("1e2")])),
            ..
        }),
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0. 1e2)", // exercise float lit splitting
        Ok(ast::Expr {
            kind: ast::ExprKind::OffsetOf(_, r!([ast::Ident!("0"), ast::Ident!("1e2")])),
            ..
        }),
    );

    // ...unless the "exponent" contains an explicit sign:
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0e+1)",
        Err(r!([Error::InvalidNumericIdent(_)]))
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0e-1)",
        Err(r!([Error::InvalidNumericIdent(_)]))
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0.1e+2)", // exercise float lit splitting
        Err(r!([Error::InvalidNumericIdent(_)]))
    );
    t!(
        parse_expr,
        Rust2015,
        "builtin#offset_of(T, 0. 1e-2)", // exercise float lit splitting
        Err(r!([Error::InvalidNumericIdent(_)]))
    );

    // In stark contrast, in struct exprs & pats  "exponents" are outright forbidden
    // regardless of whether they have an explicit sign or not:

    t!(parse_expr, Rust2015, "Compound { 0e1: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_expr, Rust2015, "Compound { 0e-1: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0e1: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0e+1: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0e1 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0e+1 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0e-1 }", Err(r!([Error::InvalidNumericIdent(_)])));
}

#[test]
fn num_lit_fractional_part_invalid_places() {
    // We lex `0.0` and `0.` as a single token, a number literal.
    // However, in the cases below we require integer literals.
    // The parser needs to inspect the literal itself to detect this.

    t!(parse_expr, Rust2015, "Compound { 0.0: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_expr, Rust2015, "Compound { 0.: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0.0: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0.: 0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0.0 }", Err(r!([Error::InvalidNumericIdent(_)])));
    t!(parse_pat, Rust2015, "Compound { 0. }", Err(r!([Error::InvalidNumericIdent(_)])));
}
