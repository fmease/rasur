use super::super::Fragment;
use super::{parse_file, parse_pat, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};
use deref as r;

#[test]
fn mut_ref_mut() {
    t!(
        parse_pat,
        Rust2015,
        "mut ref mut x",
        Ok(ast::Pat::Binding(r!(ast::BindingPat {
            mut_: ast::Mut::Yes,
            by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mut::Yes),
            binder: ast::Ident!("x"),
            pat: None,
        })))
    );
}

#[test]
fn binding_modes() {
    t!(
        parse_file,
        Rust2015,
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
        Ok(_) // just a smoke test
    );
}

#[test]
fn pseudo_field_binding_mode_box() {
    // issue: <https://github.com/fmease/rasur/issues/19>

    t!(
        parse_pat,
        Rust2015,
        "X { box mut ref mut x }",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            fields: r!([ast::StructPatField {
                attrs: _,
                binder: None,
                body: ast::Pat::Box(r!(ast::Pat::Binding(r!(ast::BindingPat {
                    mut_: ast::Mut::Yes,
                    by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mut::Yes),
                    binder: ast::Ident!("x"),
                    pat: None,
                }))))
            }]),
            ..
        })))
    );
}

#[test]
fn structs() {
    t!(
        parse_pat,
        Rust2015,
        "S {}",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            path: ast::ExtPath {
                ext: None,
                path: ast::Path {
                    segs: r!([ast::PathSeg { ident: ast::Ident!("S"), args: None }])
                }
            },
            fields: r!([]),
            rest: false
        })))
    );

    t!(
        parse_pat,
        Rust2015,
        "S { .. }",
        Ok(ast::Pat::Struct(r!(ast::StructPat { fields: r!([]), rest: true, .. })))
    );

    t!(
        parse_pat,
        Rust2015,
        "S { f: x @ Some(_) }",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            fields: r!([ast::StructPatField {
                attrs: r!([]),
                binder: Some(ast::Ident!("f")),
                body: ast::Pat::Binding(_),
            }]),
            ..
        })))
    );

    t!(
        parse_pat,
        Rust2015,
        "S { f: .. }",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            fields: r!([ast::StructPatField {
                binder: Some(ast::Ident!("f")),
                body: ast::Pat::Rest,
                ..
            }]),
            rest: false,
            ..
        })))
    );

    t!(
        parse_pat,
        Rust2015,
        "S { f }",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            fields: r!([ast::StructPatField {
                binder: None,
                body: ast::Pat::Binding(r!(ast::BindingPat {
                    mut_: ast::Mut::No,
                    by_ref: ast::ByRef::No,
                    binder: ast::Ident!("f"),
                    pat: None,
                })),
                ..
            }]),
            rest: false,
            ..
        })))
    );

    t!(
        parse_pat,
        Rust2015,
        "S { _ }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::Underscore, .. },
            r!([Fragment::Token(TokenKind::CommonIdent), Fragment::Token(TokenKind::NumLit)])
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "S { #[a] f: _ }",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            fields: r!([ast::StructPatField {
                attrs: r!([ast::Attr {
                    style: ast::AttrStyle::Outer,
                    kind: ast::AttrKind::Regular(_),
                    ..
                }]),
                binder: Some(ast::Ident!("f")),
                body: ast::Pat::Wildcard(ast::WildcardKind::Normal),
            }]),
            ..
        })))
    );

    // Context: rustc once used to accept this accidentally:
    t!(
        parse_pat,
        Rust2015,
        "S { #[a] .. }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDot, .. },
            r!([Fragment::Token(TokenKind::CommonIdent), Fragment::Token(TokenKind::NumLit)])
        )]))
    );

    // No numeric identifier shorthands:
    t!(
        parse_pat,
        Rust2015,
        "S { 0 }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CloseCurlyBracket, .. },
            r!([Fragment::Token(TokenKind::SingleColon)])
        )]))
    );

    // If a "modifier" is present, the explicit form is forbidden:
    for modifier in ["box", "mut", "ref"] {
        t!(
            parse_pat,
            Rust2015,
            &format!("S {{ {modifier} f: _ }}"),
            Err(r!([Error::UnexpectedToken(
                Token { kind: TokenKind::SingleColon, .. },
                r!([Fragment::Token(TokenKind::Comma)])
            )]))
        );
    }

    // There are no numeric identifier shorthands and the presence of modifiers requires shorthand.
    // Consequently, modifiers are incompatible with numeric identifiers:
    t!(
        parse_pat,
        Rust2015,
        "S { ref 0 }",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::NumLit, .. },
            r!([Fragment::Token(TokenKind::CommonIdent)])
        )]))
    );
}

#[test]
fn ranges_and_rest() {
    // Not a range but a rest pattern.
    t!(parse_pat, Rust2015, "..", Ok(ast::Pat::Rest));

    t!(
        parse_pat,
        Rust2015,
        "..1",
        Ok(ast::Pat::Range(
            None,
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::None,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", suffix: None })
            ))),
            ast::RangePatKind::Exclusive
        ))
    );

    t!(
        parse_pat,
        Rust2015,
        "-1..1",
        Ok(ast::Pat::Range(
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::Neg,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", .. })
            ))),
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::None,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", .. })
            ))),
            ast::RangePatKind::Exclusive
        ))
    );

    t!(
        parse_pat,
        Rust2015,
        "X..",
        Ok(ast::Pat::Range(
            Some(r!(ast::RangePatBound::Path(ast::ExtPath {
                ext: None,
                path: ast::Path { segs: r!([ast::PathSeg { ident: ast::Ident!("X"), .. }]) }
            }))),
            None,
            ast::RangePatKind::Exclusive
        ))
    );

    // Inclusive ranges need an explicit upper bound:

    t!(
        parse_pat,
        Rust2015,
        "..=",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            r!([Fragment::Lit, Fragment::ExtPath]),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "0..=",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            r!([Fragment::Lit, Fragment::ExtPath]),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "..=1",
        Ok(ast::Pat::Range(
            None,
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::None,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", suffix: None })
            ))),
            ast::RangePatKind::Inclusive { legacy: false },
        ))
    );

    t!(
        parse_pat,
        Rust2015,
        "-1..=1",
        Ok(ast::Pat::Range(
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::Neg,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", .. })
            ))),
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::None,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", .. })
            ))),
            ast::RangePatKind::Inclusive { legacy: false },
        ))
    );

    // Legacy ranges need an explict lower bound:

    t!(
        parse_pat,
        Rust2015,
        "...",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::TripleDot, .. },
            r!([Fragment::Pat]),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "...1",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::TripleDot, .. },
            r!([Fragment::Pat]),
        )]))
    );

    // Of course, them being inclusive, they need an explicit upper bound:
    t!(
        parse_pat,
        Rust2015,
        "X...",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            r!([Fragment::Lit, Fragment::ExtPath]),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "-1...1",
        Ok(ast::Pat::Range(
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::Neg,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", .. })
            ))),
            Some(r!(ast::RangePatBound::Lit(
                ast::Sign::None,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "1", .. })
            ))),
            ast::RangePatKind::Inclusive { legacy: true },
        ))
    );

    // We once used to incorrectly accept this.
    t!(
        parse_pat,
        Rust2015,
        "&5..10",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDot, .. },
            r!([Fragment::Token(TokenKind::EndOfInput)])
        )]))
    );

    // We once used to incorrectly accept this.
    t!(
        parse_pat,
        Rust2015,
        "&..10",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::NumLit, .. },
            r!([Fragment::Token(TokenKind::EndOfInput)]),
        )]))
    );

    // We once used to incorrectly accept this.
    t!(
        parse_pat,
        Rust2015,
        "&5..=10",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDotEquals, .. },
            r!([Fragment::Token(TokenKind::EndOfInput)])
        )]))
    );

    // We once used to incorrectly accept this.
    t!(
        parse_pat,
        Rust2015,
        "&..=10",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDotEquals, .. },
            r!([Fragment::Pat]),
        )]))
    );

    // Contrary to the non-legacy ranges, this is indeed allowed!
    // The expr analog `&5..=10` actually gets parsed as `(&5)..=10`
    // whereas this gets interpreted as `&(5...10)`.
    t!(
        parse_pat,
        Rust2015,
        "&5...10",
        Ok(ast::Pat::Borrow(
            ..,
            r!(ast::Pat::Range(Some(_), Some(_), ast::RangePatKind::Inclusive { legacy: true }))
        )),
    );

    // The snippets below are legal because the `..` isn't
    // interpreted as a range but as a rest pattern.

    t!(parse_pat, Rust2015, "&..", Ok(ast::Pat::Borrow(.., r!(ast::Pat::Rest))));

    t!(parse_pat, Rust2015, "box ..", Ok(ast::Pat::Box(r!(ast::Pat::Rest))));

    t!(
        parse_pat,
        Rust2015,
        "5..=&10",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::SingleAmpersand, .. },
            r!([Fragment::Lit, Fragment::ExtPath]),
        )]))
    );

    // Leading bar.
    t!(parse_pat, Rust2015, "|..1", Ok(ast::Pat::Range(..)));

    // We once used to wrongly parse this as `Grouped(Rest)`.
    // Inspired by <https://www.reddit.com/r/rust/comments/1pbbx5a/comment/nrqkwto>.
    t!(parse_pat, Rust2015, "(..)", Ok(ast::Pat::Tuple(r!([(r!([]), ast::Pat::Rest)]))));
}

#[test]
fn guards() {
    // Guards aren't allowed at the top-level.
    t!(
        parse_pat,
        Rust2015,
        "0 if true",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::If, .. },
            r!([Fragment::Token(TokenKind::EndOfInput)]),
        )]))
    );

    t!(
        parse_pat,
        Rust2015,
        "(0 if true)",
        Ok(ast::Pat::Grouped(r!(ast::Pat::Guarded(
            r!(ast::Pat::Lit(..)),
            r!(ast::Expr { kind: ast::ExprKind::Lit(..), .. })
        ))))
    );

    t!(
        parse_pat,
        Rust2015,
        "(x if true)",
        Ok(ast::Pat::Grouped(r!(ast::Pat::Guarded(
            r!(ast::Pat::Binding(..)),
            r!(ast::Expr { kind: ast::ExprKind::Lit(..), .. })
        ))))
    );

    // We once used to accept this due to us treating `if` as a normal operator.
    t!(
        parse_pat,
        Rust2015,
        "(0 if true if true)",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::If, .. },
            r!([Fragment::Token(TokenKind::Comma)])
        )]))
    );

    // Obviously, `(&0) if true` over `&(0 if true)`.
    // Demonstrates that guards can't be a "lower pattern" but need to be an operator
    // (that isn't allowed repeat similar to range exprs) since `&` is a prefix op.
    t!(
        parse_pat,
        Rust2015,
        "(&0 if true)",
        Ok(ast::Pat::Grouped(r!(ast::Pat::Guarded(
            r!(ast::Pat::Borrow(.., r!(ast::Pat::Lit(..)))),
            _
        ))))
    );

    // Obviously, `(..1) if true` over `..(1 if true)`.
    t!(
        parse_pat,
        Rust2015,
        "(..1 if true)",
        Ok(ast::Pat::Grouped(r!(ast::Pat::Guarded(
            r!(ast::Pat::Range(None, Some(ast::RangePatBound::Lit(..)), _)),
            _
        ))))
    );

    t!(
        parse_pat,
        Rust2015,
        "(0.. if true)",
        Ok(ast::Pat::Grouped(r!(ast::Pat::Guarded(
            r!(ast::Pat::Range(Some(ast::RangePatBound::Lit(..)), None, _)),
            _
        ))))
    );

    // Obviously, `(box 0) if true` over `box (0 if true)`.
    // At the time of writing, `box` isn't a prefix op but a just lower pat unlike `&`.
    // Guards don't need to be binops due to `box` but due to `&`.
    t!(
        parse_pat,
        Rust2015,
        "(box 0 if true)",
        Ok(ast::Pat::Grouped(r!(ast::Pat::Guarded(r!(ast::Pat::Box(r!(ast::Pat::Lit(..)))), _))))
    );
}
