use super::{ExpectedFragment, Parser, Result, TokenKind, error::ParseError, one_of};
use crate::ast;
use std::cmp::Ordering;

impl<'src> Parser<'_, 'src> {
    /// Parse a pattern.
    ///
    /// <!-- FIXME: Add EBNF section back in -->
    pub(super) fn parse_pat(&mut self, ors: OrPolicy) -> Result<ast::Pat<'src>> {
        // NOTE: To be kept in sync with `Self::begins_pat`.

        self.parse_pat_at_level(Level::Initial, ors)
    }

    // FIXME: Unused policy.
    fn begins_pat(&self, _ors: OrPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_pat`.

        // NOTE: We intentionally skip SinglePipe because leading pipes are only permitted
        //       at the top-level.
        match self.token.kind {
            | TokenKind::DoubleAmpersand
            | TokenKind::DoubleDot
            | TokenKind::DoubleDotEquals
            | TokenKind::False
            | TokenKind::Mut
            | TokenKind::NumLit
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::Ref
            | TokenKind::SingleAmpersand
            | TokenKind::StrLit
            | TokenKind::True
            | TokenKind::Underscore => return true,
            _ => {}
        }

        if self.begins_ext_path() {
            return true;
        }

        false
    }

    // FIXME: Optional (top-level) leading pipe unless OrPolicy::Forbidden
    //        OrPolicy might not cut it, it's definitely not a prefix op.
    //        Tho not sure. Think about cases like `..|0` and `..0|0`. How
    //        should they be parsed?
    fn parse_pat_at_level(&mut self, level: Level, ors: OrPolicy) -> Result<ast::Pat<'src>> {
        let op = match self.token.kind {
            // FIXME: SingleHyphen
            TokenKind::SingleAmpersand => Some(Op::SingleBorrow),
            TokenKind::DoubleAmpersand => Some(Op::DoubleBorrow),
            TokenKind::DoubleDot => Some(Op::RangeExclusive),
            TokenKind::DoubleDotEquals => {
                Some(Op::RangeInclusive(ast::RangeInclusivePatKind::Normal))
            }
            // `...` isn't included here because it has to have an explicit lower bound.
            _ => None,
        };
        let mut left = if let Some(op) = op {
            self.advance();
            self.fin_parse_prefix_op_pat(op, ors)
        } else {
            self.parse_lower_pat()
        }?;

        loop {
            let op = match self.token.kind {
                // FIXME: Do we need to care about DoublePipe in some way?
                TokenKind::SinglePipe if let OrPolicy::Allowed = ors => Op::Or,
                TokenKind::DoubleDot => Op::RangeExclusive,
                TokenKind::DoubleDotEquals => {
                    Op::RangeInclusive(ast::RangeInclusivePatKind::Normal)
                }
                TokenKind::TripleDot => Op::RangeInclusive(ast::RangeInclusivePatKind::Legacy),
                _ => break,
            };

            let left_level = op.left_level().unwrap();
            match left_level.cmp(&level) {
                Ordering::Less => break,
                // FIXME: Don't use Debug repr of op, use surface-language symbol.
                Ordering::Equal => return Err(ParseError::OpCannotBeChained(format!("{op:?}"))),
                Ordering::Greater => {}
            }
            self.advance();

            left = self.fin_parse_op_pat(op, left, ors)?;
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_pat(&mut self, op: Op, ors: OrPolicy) -> Result<ast::Pat<'src>> {
        let right_level = op.right_level().unwrap();

        match op {
            Op::SingleBorrow => self.fin_parse_borrow_pat(right_level, ors),
            Op::DoubleBorrow => {
                let borrow = self.fin_parse_borrow_pat(right_level, ors)?;
                Ok(ast::Pat::Borrow(ast::Mutability::Not, Box::new(borrow)))
            }
            Op::RangeInclusive(kind) => {
                self.fin_parse_range_inclusive_pat(kind, None, right_level, ors)
            }
            Op::RangeExclusive => self.fin_parse_range_exclusive_pat(None, right_level, ors),
            _ => unreachable!(),
        }
    }

    fn fin_parse_op_pat(
        &mut self,
        op: Op,
        left: ast::Pat<'src>,
        ors: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        match op {
            Op::Or => {
                let right = self.parse_pat_at_level(op.right_level().unwrap(), ors)?;
                Ok(ast::Pat::Or(Box::new(left), Box::new(right)))
            }
            Op::RangeExclusive => self.fin_parse_range_exclusive_pat(
                Some(Box::new(left)),
                op.right_level().unwrap(),
                ors,
            ),
            Op::RangeInclusive(kind) => self.fin_parse_range_inclusive_pat(
                kind,
                Some(Box::new(left)),
                op.right_level().unwrap(),
                ors,
            ),
            _ => unreachable!(),
        }
    }

    fn fin_parse_borrow_pat(
        &mut self,
        right_level: Level,
        ors: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        let mut_ = self.parse_mutability();
        let pat = self.parse_pat_at_level(right_level, ors)?;
        Ok(ast::Pat::Borrow(mut_, Box::new(pat)))
    }

    fn fin_parse_range_exclusive_pat(
        &mut self,
        left: Option<Box<ast::Pat<'src>>>,
        right_level: Level,
        ors: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        // FIXME: "begins_pat_at(right_level)"?
        let right =
            self.begins_pat(ors).then(|| self.parse_pat_at_level(right_level, ors)).transpose()?;
        Ok(ast::Pat::Range(left, right.map(Box::new), ast::RangePatKind::Exclusive))
    }

    fn fin_parse_range_inclusive_pat(
        &mut self,
        kind: ast::RangeInclusivePatKind,
        left: Option<Box<ast::Pat<'src>>>,
        right_level: Level,
        ors: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        let right = self.parse_pat_at_level(right_level, ors)?;
        Ok(ast::Pat::Range(left, Some(Box::new(right)), ast::RangePatKind::Inclusive(kind)))
    }

    fn parse_lower_pat(&mut self) -> Result<ast::Pat<'src>> {
        match self.token.kind {
            TokenKind::False => {
                self.advance();
                return Ok(ast::Pat::Lit(ast::Lit::Bool(false)));
            }
            TokenKind::Mut => {
                self.advance();
                // FIXME: Use the Keyword API
                return match self.as_ident(self.token) {
                    Some("ref") => {
                        self.advance();
                        self.fin_parse_by_ref_ident_pat(ast::Mutability::Mut)
                    }
                    Some(ident) if self.ident_is_common(ident) => {
                        self.advance();
                        Ok(ast::Pat::Ident(ast::IdentPat {
                            mut_: ast::Mutability::Mut,
                            by_ref: ast::ByRef::No,
                            ident,
                        }))
                    }
                    _ => Err(ParseError::UnexpectedToken(
                        self.token,
                        one_of![ExpectedFragment::Raw("ref"), ExpectedFragment::CommonIdent],
                    )),
                };
            }
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::Pat::Lit(ast::Lit::Num(lit)));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_grouped_or_tuple(
                    |this| this.parse_pat(OrPolicy::Allowed),
                    ast::Pat::Grouped,
                    ast::Pat::Tuple,
                );
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let elems = self.fin_parse_delim_seq(
                    TokenKind::CloseSquareBracket,
                    TokenKind::Comma,
                    |this| this.parse_pat(OrPolicy::Allowed),
                )?;
                return Ok(ast::Pat::Slice(elems));
            }
            TokenKind::Ref => {
                self.advance();
                return self.fin_parse_by_ref_ident_pat(ast::Mutability::Not);
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::Pat::Lit(ast::Lit::Str(lit)));
            }
            TokenKind::True => {
                self.advance();
                return Ok(ast::Pat::Lit(ast::Lit::Bool(true)));
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::Pat::Wildcard);
            }
            _ => {}
        }
        if self.begins_ext_path() {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;

            match self.token.kind {
                TokenKind::SingleBang => {
                    let ast::ExtPath { ext: None, path } = path else {
                        return Err(ParseError::TyRelMacroCall);
                    };
                    let (bracket, stream) = self.parse_delimited_token_stream()?;
                    return Ok(ast::Pat::MacroCall(ast::MacroCall { path, bracket, stream }));
                }
                TokenKind::OpenRoundBracket => {
                    self.advance();
                    let fields = self.fin_parse_delim_seq(
                        TokenKind::CloseRoundBracket,
                        TokenKind::Comma,
                        |this| this.parse_pat(OrPolicy::Allowed),
                    )?;
                    return Ok(ast::Pat::TupleStruct(Box::new(ast::TupleStructPat {
                        path,
                        fields,
                    })));
                }
                TokenKind::OpenCurlyBracket => {
                    self.advance();
                    const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                    const SEPARATOR: TokenKind = TokenKind::Comma;
                    let mut fields = Vec::new();
                    let mut rest = false;

                    while !self.consume(DELIMITER) {
                        if self.consume(TokenKind::DoubleDot) {
                            rest = true;
                            self.parse(DELIMITER)?;
                            break;
                        }

                        // FIXME: NumLit fields
                        // FIXME:
                        // FIXME: Parse mut? ref? mut? (a following `":" Pat` is not permitted)
                        let binder = self.parse_common_ident()?;
                        let body = self
                            .consume(TokenKind::SingleColon)
                            .then(|| self.parse_pat(OrPolicy::Allowed))
                            .transpose()?;
                        fields.push(ast::StructPatField { binder, body });

                        if self.token.kind != DELIMITER {
                            self.parse(SEPARATOR)?;
                        }
                    }

                    return Ok(ast::Pat::Struct(Box::new(ast::StructPat { path, fields, rest })));
                }
                _ => {}
            }

            return Ok(match path {
                ast::ExtPath {
                    ext: None,
                    path: ast::Path { segs: deref!([ast::PathSeg { ident, args: None }]) },
                } => ast::Pat::Ident(ast::IdentPat {
                    by_ref: ast::ByRef::No,
                    mut_: ast::Mutability::Not,
                    ident,
                }),
                _ => ast::Pat::Path(Box::new(path)),
            });
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Pat))
    }

    fn fin_parse_by_ref_ident_pat(&mut self, mut_: ast::Mutability) -> Result<ast::Pat<'src>> {
        let ref_mut = self.parse_mutability();
        let ident = self.parse_common_ident()?;
        Ok(ast::Pat::Ident(ast::IdentPat { by_ref: ast::ByRef::Yes(ref_mut), mut_, ident }))
    }
}

#[derive(Clone, Copy)]
pub(super) enum OrPolicy {
    Allowed,
    Forbidden,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Level {
    Initial,
    Range,
    OrLeft,
    OrRight,
    Prefix,
}

#[derive(Clone, Copy, Debug)]
enum Op {
    DoubleBorrow,
    Or,
    RangeExclusive,
    RangeInclusive(ast::RangeInclusivePatKind),
    SingleBorrow,
}

impl Op {
    fn left_level(self) -> Option<Level> {
        Some(match self {
            Self::Or => Level::OrLeft,
            Self::RangeExclusive | Self::RangeInclusive(_) => Level::Range,
            Self::SingleBorrow | Self::DoubleBorrow => return None,
        })
    }

    fn right_level(self) -> Option<Level> {
        Some(match self {
            Self::Or => Level::OrRight,
            Self::RangeExclusive | Self::RangeInclusive(_) => Level::Range,
            Self::SingleBorrow | Self::DoubleBorrow => Level::Prefix,
        })
    }
}
