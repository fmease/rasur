use super::{ExpectedFragment, Parser};
use crate::{
    ast,
    edition::Edition::{self, *},
    error::{Buffer as ErrorBuffer, Error, InvalidScalarPlace},
    lexer::{self, lex},
    span::{ByteIndex, Spanned},
    token::{Token, TokenKind},
};
use deref as r;
use normalizer::{Normalized, normalize};

// NOTE: We're not using implicit deref patterns at the moment since rust-analyzer
//       can't handle them yet and would color the entirely red. Use `r!(…)` for now.

type Result<T, E = Vec<Error>> = std::result::Result<T, E>;

mod normalizer {
    use std::borrow::Cow;

    pub(super) fn normalize(source: &str) -> Normalized<Cow<'_, str>> {
        Normalized { raw: crate::lexer::normalize(source) }
    }

    #[derive(Clone, Copy)]
    pub(super) struct Normalized<T> {
        raw: T,
    }

    impl<T> Normalized<T> {
        pub(super) fn into_inner(self) -> T {
            self.raw
        }
    }

    impl Normalized<Cow<'_, str>> {
        pub(super) fn as_ref(&self) -> Normalized<&str> {
            Normalized { raw: &self.raw }
        }
    }
}

fn parse_file(source: Normalized<&str>, edition: Edition) -> Result<ast::File<'_>> {
    let source = source.into_inner();
    let errors = ErrorBuffer::default();

    let mut offset = ByteIndex::default();
    let shebang = lexer::strip_shebang(source, &mut offset, edition);
    let frontmatter = lexer::strip_frontmatter(source, &mut offset, &errors);

    let tokens = lex(source, offset, edition, &errors);
    let file = super::parse(tokens, shebang, frontmatter, source, edition, &errors);

    if let errors = errors.into_inner()
        && !errors.is_empty()
    {
        return Err(errors);
    }

    Ok(file.unwrap())
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

fn parse_via<'src, T>(
    source: Normalized<&'src str>,
    edition: Edition,
    parse: impl FnOnce(&mut super::Parser<'_, '_, 'src>) -> super::Result<T>,
) -> Result<T> {
    let source = source.into_inner();
    let errors = ErrorBuffer::default();

    let tokens = lex(source, ByteIndex::default(), edition, &errors);
    let tokens = super::prepare(tokens);
    let mut p = Parser::new(&tokens, source, edition, &errors);

    let node = parse(&mut p).and_then(|r| {
        p.parse(TokenKind::EndOfInput)?;
        Ok(r)
    });

    if let errors = errors.into_inner()
        && !errors.is_empty()
    {
        return Err(errors);
    }

    Ok(node.unwrap())
}

macro_rules! t {
    ($parse:ident, $edition:ident, $source:literal, $ast:pat $(,)?) => {
        match $parse(normalize($source).as_ref(), $edition) {
            $ast => {}
            ast => panic!("{:?}: {} != {:#?}", $source, stringify!($ast), ast),
        }
    };
}

#[test]
fn file_empty() {
    t!(
        parse_file,
        Rust2015,
        "",
        Ok(ast::File { shebang: None, frontmatter: None, attrs: r!([]), items: r!([]), span: _ })
    );
}

// We only permit ASCII spaces and tabs in (the padding of) frontmatter infostrings & trailers.
// However, due to CRLF→LF normalization, we automatically also permit CR before the line break.
#[test]
fn frontmatter_crlf() {
    // See also <https://github.com/fmease/rasur/issues/15>.

    t!(
        parse_file,
        Rust2015,
        "---\t\r\n---\t\r\n",
        Ok(ast::File {
            shebang: None,
            frontmatter: Some(ast::Frontmatter {
                infostring: Spanned { bare: "", .. },
                content: Spanned { bare: "", .. },
                ..
            }),
            ..
        })
    );
}

#[test]
fn frontmatter_cr() {
    // CR isn't "horizontal whitespace" and therefore forbidden inside infostrings.
    t!(parse_file, Rust2015, "--- \r \n---", Err(r!([Error::InvalidFrontmatterInfostring(_)])),);

    // CR isn't "horizontal whitespace" and therefore forbidden inside trailers.
    t!(parse_file, Rust2015, "---\n--- \r ", Err(r!([Error::InvalidFrontmatterTrailer(_)])));

    // "Stray" CRs inside the frontmatter body are explicitly forbidden.
    t!(
        parse_file,
        Rust2015,
        "---\n(\r)\n---",
        Err(r!([Error::InvalidScalar('\r', InvalidScalarPlace::FrontmatterBody, _)]))
    );
}

#[test]
fn tuple_struct_field_visibility() {
    t!(
        parse_item,
        Rust2015,
        "struct T(pub([i32; 2]));",
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

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(crate)[i32]);",
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

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(self)&());",
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
    t!(
        parse_item,
        Rust2015,
        "struct T(pub(super::U));",
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

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(super::U)impl);",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Impl, .. }, _)])),
    );

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(in super::U)!);",
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

    t!(
        parse_item,
        Rust2015,
        "struct T(pub);",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CloseRoundBracket, .. },
            ExpectedFragment::Ty
        )]))
    );
}

#[test]
fn range_exprs() {
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
                r!(ast::Expr {
                    kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                    ..
                })
            ),
            ..
        }),
    );

    // We once used to wrongly accept this & parse it as `(..)?`.
    t!(
        parse_expr,
        Rust2015,
        "..?",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::QuestionMark, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )])),
    );

    // `(!x)..`, not `!(x..)`.
    t!(
        parse_expr,
        Rust2015,
        "!x..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(r!(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Not, _), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::Borrow(..), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Neg, _), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Deref, _), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::Try(..), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::BinOp(ast::BinOp::Add, ..), .. })),
                Some(r!(ast::Expr { kind: ast::ExprKind::BinOp(ast::BinOp::Add, ..), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::Block(..), .. })),
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
            kind: ast::ExprKind::ForLoop(r!(ast::ForLoopExpr {
                head: ast::Expr {
                    kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                    ..
                },
                body: ast::BlockExpr { .. },
                ..
            })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::Tuple(_), .. })),
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
                Some(r!(ast::Expr { kind: ast::ExprKind::UnOp(ast::UnOp::Deref, _), .. })),
                Some(r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. })),
                ast::RangeExprKind::Inclusive
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "..=",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )])),
    );

    t!(
        parse_expr,
        Rust2015,
        "'='..=",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, .. },
            ExpectedFragment::Expr
        )])),
    );

    // Unlike the `..` case, `{}` gets interpreted as the right argument of the range.
    t!(
        parse_expr,
        Rust2015,
        "for _ in ..={} {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::ForLoop(r!(ast::ForLoopExpr {
                head: ast::Expr {
                    kind: ast::ExprKind::Range(
                        None,
                        Some(r!(ast::Expr { kind: ast::ExprKind::Block(..), .. })),
                        ast::RangeExprKind::Inclusive,
                    ),
                    ..
                },
                body: ast::BlockExpr { .. },
                ..
            })),
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
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::Range(
                        None,
                        Some(r!(ast::Expr {
                            kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                            ..
                        })),
                        ast::RangeExprKind::Exclusive
                    ),
                    ..
                })),
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
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::Range(
                        None,
                        Some(r!(ast::Expr {
                            kind: ast::ExprKind::Range(None, None, ast::RangeExprKind::Exclusive),
                            ..
                        })),
                        ast::RangeExprKind::Inclusive
                    ),
                    ..
                })),
                ast::RangeExprKind::Inclusive
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "0..1..2",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDot, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput)
        )])),
    );

    t!(
        parse_expr,
        Rust2015,
        "0..=1..2",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDot, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput)
        )])),
    );

    t!(
        parse_expr,
        Rust2015,
        "0..1..=2",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDotEquals, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput)
        )])),
    );

    t!(
        parse_expr,
        Rust2015,
        "0..=1..=2",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDotEquals, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput)
        )])),
    );

    // FIXME
    #[cfg(false)]
    t!(
        parse_stmt,
        Rust2015,
        "..if(){}else{}[0]",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::OpenSquareBracket, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )]))
    );

    // FIXME
    #[cfg(false)]
    t!(
        parse_stmt,
        Rust2015,
        "()..if(){}else{}[0]",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::OpenSquareBracket, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )]))
    );

    // ...for comparison, this does parse:
    t!(
        parse_expr,
        Rust2015,
        "..if(){}else{}[0]",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::Index(
                        r!(ast::Expr { kind: ast::ExprKind::If(..), .. }),
                        r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. })
                    ),
                    ..
                })),
                _
            ),
            ..
        }),
    );

    // FIXME
    #[cfg(false)]
    t!(parse_stmt, Rust2015, "..{}+0", Err(_));

    // ...for comparison, this does parse:
    t!(
        parse_expr,
        Rust2015,
        "..{}+0",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                None,
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::BinOp(
                        ast::BinOp::Add,
                        r!(ast::Expr { kind: ast::ExprKind::Block(..), .. }),
                        r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                    ),
                    ..
                })),
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
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::Borrow(
                        ..,
                        r!(ast::Expr {
                            kind: ast::ExprKind::Range(
                                None,
                                Some(r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. })),
                                ast::RangeExprKind::Inclusive
                            ),
                            ..
                        })
                    ),
                    ..
                })),
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
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::DoubleDot, .. },
            ExpectedFragment::Token(TokenKind::EndOfInput),
        )])),
    );

    // We once used to wrongly parse this as `(T {}) + (..(0..))` instead of `((T {}) + (..0))..`.
    // issue: <https://github.com/fmease/rasur/issues/17>
    t!(
        parse_expr,
        Rust2015,
        "T {} + ..0..",
        Ok(ast::Expr {
            kind: ast::ExprKind::Range(
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::BinOp(
                        ast::BinOp::Add,
                        r!(ast::Expr { kind: ast::ExprKind::Struct(_), .. }),
                        r!(ast::Expr { kind: ast::ExprKind::Range(None, Some(_), _), .. }),
                    ),
                    ..
                })),
                None,
                _
            ),
            ..
        })
    );

    // FIXME: We currently wrongly parse this as `return (x + ((..).y))` instead of `(return (x + (..))).y`.
    // Inspired by <https://github.com/rust-lang/rust/pull/142476#discussion_r2159721125>.
    #[cfg(false)]
    t!(
        parse_expr,
        Rust2015,
        "return x + .. .y",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                r!(ast::Expr {
                    kind: ast::ExprKind::Return(Some(r!(ast::Expr {
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Add,
                            r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
                            r!(ast::Expr { kind: ast::ExprKind::Range(None, None, _), .. }),
                        ),
                        ..
                    }))),
                    ..
                }),
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
            kind: ast::ExprKind::Return(Some(r!(ast::Expr {
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
                    r!(ast::Expr {
                        kind: ast::ExprKind::Field(
                            r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                            ast::Ident!("y")
                        ),
                        ..
                    }),
                ),
                ..
            }))),
            ..
        }),
    );
}

#[test]
fn expr_levels() {
    t!(
        parse_expr,
        Rust2015,
        "if(){}else{}()",
        Ok(ast::Expr {
            kind: ast::ExprKind::Call(r!(ast::Expr { kind: ast::ExprKind::If(..), .. }), r!([])),
            ..
        }),
    );

    t!(
        parse_expr,
        Rust2015,
        "if(){}else{}as _",
        Ok(ast::Expr {
            kind: ast::ExprKind::Cast(
                r!(ast::Expr { kind: ast::ExprKind::If(..), .. }),
                r!(ast::Ty::Inferred)
            ),
            ..
        }),
    );

    // FIXME
    #[cfg(false)]
    t!(
        parse_expr,
        Rust2015,
        "-if 0{}else{}()",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::OpenRoundBracket, .. }, _)])),
    );

    // ...however, we accept this:
    t!(
        parse_expr,
        Rust2015,
        "1-if 0{}else{}()",
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Sub,
                r!(ast::Expr { kind: ast::ExprKind::Lit(_), .. }),
                r!(ast::Expr {
                    kind: ast::ExprKind::Call(
                        r!(ast::Expr { kind: ast::ExprKind::If(..), .. }),
                        r!([])
                    ),
                    ..
                })
            ),
            ..
        }),
    );
}

#[test]
fn expr_attrs() {
    t!(
        parse_expr,
        Rust2015,
        "#[a]0",
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, kind: ast::AttrKind::Normal(_) }]),
            kind: ast::ExprKind::Lit(_),
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "#[a]#[b](#[c]#[d]0)",
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

    t!(
        parse_expr,
        Rust2015,
        "#[a]*x",
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::UnOp(..)
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/25>
    t!(
        parse_expr,
        Rust2015,
        "#[a]!x",
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::UnOp(..)
        })
    );

    t!(parse_expr, Rust2015, "#[a]..", Err(r!([Error::ForbiddenOuterAttrs])),);

    t!(parse_expr, Rust2015, "#[a]..()", Err(r!([Error::ForbiddenOuterAttrs])),);

    t!(parse_expr, Rust2015, "#[a]..=_", Err(r!([Error::ForbiddenOuterAttrs])),);

    t!(
        parse_expr,
        Rust2015,
        "#[a]&#[b]()",
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

    t!(
        parse_expr,
        Rust2015,
        "#[a]&#[b]()",
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
    t!(
        parse_expr,
        Rust2015,
        "0..#[a]1",
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
    t!(
        parse_expr,
        Rust2015,
        "#[a]()as()",
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
    t!(
        parse_expr,
        Rust2015,
        "#[a]!0..",
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
    t!(
        parse_expr,
        Rust2015,
        "#[a]0??",
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
    t!(
        parse_expr,
        Rust2015,
        "#[a]f()",
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Call(
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) }),
                r!([])
            )
        })
    );

    // Here, the attr of course belongs to the inner path expr, not to the call expr itself.
    t!(
        parse_expr,
        Rust2015,
        "(#[a]f)()",
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
    t!(
        parse_expr,
        Rust2015,
        "#[a]f[0]",
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Index(
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) }),
                _
            )
        })
    );

    // The attr belongs to the (outer) field expr, not to the (inner) path expr.
    t!(
        parse_expr,
        Rust2015,
        "#[a]x.y",
        Ok(ast::Expr {
            attrs: r!([ast::Attr { style: ast::AttrStyle::Outer, .. }]),
            kind: ast::ExprKind::Field(
                r!(ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) }),
                _,
            )
        })
    );

    // The outer attr belongs to the (outer) match expr, not to the (inner) scrutinee expr.
    t!(
        parse_expr,
        Rust2015,
        "#[a]x.match{#![b]}",
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
    t!(
        parse_expr,
        Rust2015,
        "#[a]-0+1",
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

    // This used to trigger a debug assertion.
    t!(
        parse_stmt,
        Rust2015,
        "#[a]match x{#![b]}",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                attrs: r!([
                    ast::Attr { style: ast::AttrStyle::Outer, .. },
                    ast::Attr { style: ast::AttrStyle::Inner, .. },
                ]),
                kind: ast::ExprKind::Match(r!(ast::MatchExpr {
                    scrutinee: ast::Expr { attrs: r!([]), kind: ast::ExprKind::Path(_) },
                    ..
                }))
            },
            _
        ))
    );
}

#[test]
fn double_borrow_and_double_borrow_expr() {
    t!(
        parse_expr,
        Rust2015,
        "&&0&&&&1",
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
    t!(
        parse_expr,
        Rust2015,
        "()||||()",
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

#[test]
fn mut_ref_mut_pat() {
    t!(
        parse_pat,
        Rust2015,
        "mut ref mut x",
        Ok(ast::Pat::Binding(r!(ast::BindingPat {
            mut_: ast::Mutability::Mut,
            by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mutability::Mut),
            binder: ast::Ident!("x"),
            pat: None,
        })))
    );
}

#[test]
fn false_angle_gen_args_expr() {
    t!(parse_expr, Rust2015, "f<i32>()", Err(r!([Error::ChainedComparison(_)])),);

    t!(
        parse_expr,
        Rust2015,
        "f<i32>",
        Err(r!([
            Error::ChainedComparison(_),
            Error::UnexpectedToken(Token { kind: TokenKind::EndOfInput, span: _ }, _)
        ])),
    );
}

#[test]
fn false_angle_gen_args_pat() {
    t!(
        parse_pat,
        Rust2015,
        "Some<i32>(0)",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _)]))
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
    t!(
        parse_pat,
        Rust2015,
        "Some::<i32>(0)",
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
            fields: r!([ast::Pat::Lit(
                ast::Sign::None,
                r!(ast::Lit { kind: ast::LitKind::Num, value: "0", .. }),
            )])
        }))),
    );
}

#[test]
fn angle_gen_args_ty() {
    t!(
        parse_ty,
        Rust2015,
        "Ty<'a, (), 0>",
        Ok(ast::Ty::Path(r!(ast::ExtPath {
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
                            kind: ast::ExprKind::Lit(r!(ast::Lit {
                                kind: ast::LitKind::Num,
                                value: "0",
                                ..
                            })),
                            ..
                        })),
                    ])))
                }])
            }
        })))
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
            kind: ast::ExprKind::Path(r!(ast::ExtPath {
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
            })),
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

    t!(
        parse_pat,
        Rust2015,
        "x::()->!::X",
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
    t!(
        parse_item,
        Rust2015,
        "path::to::<>::call!();",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _)]))
    );

    t!(
        parse_item,
        Rust2015,
        "path::to::call<()>!();",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::SingleLessThan, span: _ }, _)]))
    );
}

#[test]
fn macro_call_stmt_gen_args() {
    t!(
        parse_stmt,
        Rust2015,
        "path::to::<>::call::<>!();",
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

    t!(parse_stmt, Rust2015, "path::to::<>::call::()!();", Ok(_)); // just a smoke test
}

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
fn control_flow_ops_block_expr() {
    t!(
        parse_expr,
        Rust2015,
        "if return {}",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::EndOfInput, span: _ },
            ExpectedFragment::Token(TokenKind::OpenCurlyBracket),
        )]))
    );
    t!(
        parse_expr,
        Rust2015,
        "if return {} {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::If(r!(ast::IfExpr {
                condition: ast::Expr {
                    kind: ast::ExprKind::Return(Some(r!(ast::Expr {
                        kind: ast::ExprKind::Block(None, ast::BlockExpr { stmts: r!([]) }),
                        ..
                    }))),
                    ..
                },
                consequent: ast::BlockExpr { stmts: r!([]) },
                alternate: None
            })),
            ..
        })
    );

    // FIXME: Explainer, once I have one.
    t!(
        parse_expr,
        Rust2015,
        "if break {}",
        Ok(ast::Expr {
            kind: ast::ExprKind::If(r!(ast::IfExpr {
                condition: ast::Expr { kind: ast::ExprKind::Break(None, None), .. },
                consequent: ast::BlockExpr { stmts: r!([]) },
                alternate: None
            })),
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
                Some(r!(ast::Expr {
                    kind: ast::ExprKind::Block(None, ast::BlockExpr { stmts: r!([]) }),
                    ..
                }))
            ),
            ..
        })
    );

    t!(
        parse_expr,
        Rust2015,
        "if continue {}",
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
    t!(
        parse_expr,
        Rust2015,
        "for<Ty as Trait>::AssocTy {} in () {}",
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
    t!(
        parse_expr,
        Rust2015,
        "0 as u64 <= 1",
        Ok(ast::Expr {
            kind: ast::ExprKind::BinOp(
                ast::BinOp::Le,
                r!(ast::Expr { kind: ast::ExprKind::Cast(..), .. }),
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
    t!(parse_item, Rust2015, "fn f(&self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&'a self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&'a mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&'a pin mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&pin const self);", Ok(_));

    // issue: <https://github.com/fmease/rasur/issues/18>
    t!(parse_item, Rust2015, "fn f(self::T: ());", Ok(_));
    t!(parse_item, Rust2015, "fn f(&self::T: ());", Ok(_));
    t!(parse_item, Rust2015, "fn f(&mut self::T: ());", Ok(_));
}

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
    t!(
        parse_ty,
        Rust2015,
        "impl F() -> (for<> A + B)",
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

#[test]
fn numeric_field_exprs() {
    t!(
        parse_expr,
        Rust2015,
        "x.0",
        Ok(ast::Expr {
            kind: ast::ExprKind::Field(
                r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
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
                r!(ast::Expr {
                    kind: ast::ExprKind::Field(
                        r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
                        ast::Ident!("0"),
                    ),
                    ..
                }),
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
                r!(ast::Expr {
                    kind: ast::ExprKind::Field(
                        r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
                        ast::Ident!("0"),
                    ),
                    ..
                }),
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
                r!(ast::Expr {
                    kind: ast::ExprKind::Field(
                        r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
                        ast::Ident!("0"),
                    ),
                    ..
                }),
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
                r!(ast::Expr {
                    kind: ast::ExprKind::Field(
                        r!(ast::Expr { kind: ast::ExprKind::Path(_), .. }),
                        ast::Ident!("0"),
                    ),
                    ..
                }),
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
                r!([ast::Ident!("x"), ast::Ident!("0"), ast::Ident!("1")])
            ),
            ..
        }),
    );
}

// FIXME: macro_rules! in stmt pos (-> item not stmt); macro_rules! no binder == macro call
// FIXME: ops
// FIXME: structs in ifs etc.
// FIXME: almost-assoc-item-constraint due to (  )
// FIXME: exprs, pats
// FIXME: A bunch of negative behavior tests!
// FIXME: Add stmt `{ 0 } + 0` error, stmt `&{ 0 } + 0` ok but stmt `..{ 0 } + 0` err! etc.
//        More: `0 + { 0 } + 0` OK. stmt `{ 0 } || 0` err.

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
                    mut_: ast::Mutability::Mut,
                    by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mutability::Mut),
                    binder: ast::Ident!("x"),
                    pat: None,
                }))))
            }]),
            ..
        })))
    );
}

#[test]
fn item_modifiers_in_item_ctxt() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    t!(
        parse_file,
        Rust2024, // for `async` and `gen`
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
auto impl(crate) trait Trait {}
auto impl(in crate) trait Trait {}
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
"#,
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

    t!(
        parse_expr,
        Rust2024, // for `async` and `gen`
        r#"{
async extern fn f() {}
async fn f() {}
async gen fn f() {}
async gen safe fn f() {}
async gen unsafe fn f() {}
async safe extern fn f() {}
async safe fn f() {}
async unsafe extern fn f() {}
async unsafe fn f() {}
auto impl(crate) trait Trait {}
auto impl(in crate) trait Trait {}
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
}"#,
        Ok(_) // just a smoke test
    );

    // Make sure that we don't consider these weak / context-dependent keywords as item modifiers:
    t!(
        parse_stmt,
        Rust2015,
        "auto as _",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::Cast(
                    r!(ast::Expr {
                        kind: ast::ExprKind::Path(r!(ast::ExtPath {
                            ext: None,
                            path: ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("auto"), .. }]
                            }
                        })),
                        ..
                    }),
                    _
                ),
                ..
            },
            _
        ))
    );

    t!(
        parse_stmt,
        Rust2015,
        "default as _",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::Cast(
                    r!(ast::Expr {
                        kind: ast::ExprKind::Path(r!(ast::ExtPath {
                            ext: None,
                            path: ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("default"), .. }]
                            }
                        })),
                        ..
                    }),
                    _
                ),
                ..
            },
            _
        ))
    );

    t!(
        parse_stmt,
        Rust2015,
        "safe as _",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::Cast(
                    r!(ast::Expr {
                        kind: ast::ExprKind::Path(r!(ast::ExtPath {
                            ext: None,
                            path: ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("safe"), .. }]
                            }
                        })),
                        ..
                    }),
                    _
                ),
                ..
            },
            _
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
fn delegation() {
    // FIXME: This is just a smoke test, convert to proper tests.
    // See also <https://github.com/fmease/rasur/issues/30>

    t!(parse_item, Rust2015, "reuse it;", Ok(_));
    t!(parse_item, Rust2015, "reuse self;", Ok(_));
    t!(parse_item, Rust2015, "reuse path::<>::to::<_>::something::();", Ok(_));
    t!(parse_item, Rust2015, "reuse it as that;", Ok(_));
    t!(parse_item, Rust2015, "reuse it::*;", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{};", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{f, g, h};", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{f as f, g as g};", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{self, super, crate};", Ok(_));
    t!(parse_item, Rust2015, "reuse it {}", Ok(_));
    t!(parse_item, Rust2015, "reuse it { 1 + 2 * 3}", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{} {}", Ok(_));
    t!(parse_item, Rust2015, "reuse <()>::it;", Ok(_));
    t!(parse_item, Rust2015, "reuse <() as Trait>::it;", Ok(_));

    // Contrary to its sibling, the use-item, these are not accepted:
    t!(parse_item, Rust2015, "reuse *;", Err(_));
    t!(parse_item, Rust2015, "reuse {};", Err(_));
    t!(parse_item, Rust2015, "reuse ::it;", Err(_));
    t!(parse_item, Rust2015, "reuse it as _;", Err(_));
    t!(parse_item, Rust2015, "reuse it::{*};", Err(_));
    t!(parse_item, Rust2015, "reuse it::{f::g::h};", Err(_));
    t!(parse_item, Rust2015, "reuse it::{f::{g::{h}}};", Err(_));

    // Some other invalid forms:
    t!(parse_item, Rust2015, "reuse it<i32>;", Err(_));
    t!(parse_item, Rust2015, "reuse it::f<i32>;", Err(_));
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
