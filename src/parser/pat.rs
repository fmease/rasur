use super::{ExpectedFragment, Parser, Result, TokenKind, error::ParseError, one_of};
use crate::ast;
use std::cmp::Ordering;

impl<'src> Parser<'_, 'src> {
    /// Parse a pattern.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    pub(super) fn parse_pat(&mut self, o_policy: OrPolicy) -> Result<ast::Pat<'src>> {
        if let OrPolicy::Allowed = o_policy {
            self.consume(TokenKind::SinglePipe);
        }

        self.parse_pat_at_level(Level::Initial, o_policy)
    }

    fn parse_pat_at_level(&mut self, level: Level, o_policy: OrPolicy) -> Result<ast::Pat<'src>> {
        // Negation (of literals) is handled in `Self::parse_lower_pat` instead!
        let op = match self.token.kind {
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
            self.fin_parse_prefix_op_pat(op, o_policy)
        } else {
            self.parse_lower_pat()
        }?;

        loop {
            let op = match self.token.kind {
                // FIXME: Do we need to care about DoublePipe in some way?
                TokenKind::SinglePipe if let OrPolicy::Allowed = o_policy => Op::Or,
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

            left = self.fin_parse_op_pat(op, left, o_policy)?;
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_pat(&mut self, op: Op, o_policy: OrPolicy) -> Result<ast::Pat<'src>> {
        let right_level = op.right_level().unwrap();

        match op {
            Op::DoubleBorrow => {
                let borrow = self.fin_parse_borrow_pat(right_level, o_policy)?;
                Ok(ast::Pat::Borrow(ast::Mutability::Not, Box::new(borrow)))
            }
            Op::SingleBorrow => self.fin_parse_borrow_pat(right_level, o_policy),
            Op::RangeExclusive => self.fin_parse_range_exclusive_pat(None),
            Op::RangeInclusive(kind) => self.fin_parse_range_inclusive_pat(kind, None),
            _ => unreachable!(),
        }
    }

    fn fin_parse_op_pat(
        &mut self,
        op: Op,
        left: ast::Pat<'src>,
        o_policy: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        match op {
            Op::Or => {
                let right = self.parse_pat_at_level(op.right_level().unwrap(), o_policy)?;
                Ok(ast::Pat::Or(Box::new(left), Box::new(right)))
            }
            Op::RangeExclusive => self.fin_parse_range_exclusive_pat(Some(Box::new(left))),
            Op::RangeInclusive(kind) => {
                self.fin_parse_range_inclusive_pat(kind, Some(Box::new(left)))
            }
            _ => unreachable!(),
        }
    }

    fn fin_parse_borrow_pat(
        &mut self,
        right_level: Level,
        o_policy: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        let mut_ = self.parse_mutability();
        let pat = self.parse_pat_at_level(right_level, o_policy)?;
        Ok(ast::Pat::Borrow(mut_, Box::new(pat)))
    }

    fn fin_parse_range_exclusive_pat(
        &mut self,
        left: Option<Box<ast::Pat<'src>>>,
    ) -> Result<ast::Pat<'src>> {
        let right =
            self.begins_range_pat_bound().then(|| self.parse_range_pat_bound()).transpose()?;
        Ok(ast::Pat::Range(left, right.map(Box::new), ast::RangePatKind::Exclusive))
    }

    fn fin_parse_range_inclusive_pat(
        &mut self,
        kind: ast::RangeInclusivePatKind,
        left: Option<Box<ast::Pat<'src>>>,
    ) -> Result<ast::Pat<'src>> {
        let right = self.parse_range_pat_bound()?;
        Ok(ast::Pat::Range(left, Some(Box::new(right)), ast::RangePatKind::Inclusive(kind)))
    }

    fn begins_range_pat_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_range_pat_bound`.

        match self.token.kind {
            | TokenKind::CharLit
            | TokenKind::False
            | TokenKind::NumLit
            | TokenKind::SingleHyphen
            | TokenKind::StrLit
            | TokenKind::True => true,
            _ => self.begins_ext_path(),
        }
    }

    fn parse_range_pat_bound(&mut self) -> Result<ast::Pat<'src>> {
        // NOTE: To be kept in sync with `Self::begins_range_pat_bound`.

        if let Some(lit) = self.parse_opt_lit_pat()? {
            return Ok(lit);
        }
        if self.begins_ext_path() {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;
            return Ok(ast::Pat::Path(Box::new(path)));
        }

        Err(ParseError::UnexpectedToken(
            self.token,
            one_of![ExpectedFragment::Literal, ExpectedFragment::ExtPath],
        ))
    }

    fn parse_lower_pat(&mut self) -> Result<ast::Pat<'src>> {
        if let Some(lit) = self.parse_opt_lit_pat()? {
            return Ok(lit);
        }

        match self.token.kind {
            TokenKind::Box => {
                self.advance();
                return Ok(ast::Pat::Box(Box::new(self.parse_pat(OrPolicy::Forbidden)?)));
            }
            TokenKind::Mut => {
                self.advance();
                return match self.token.kind {
                    TokenKind::Ref => {
                        self.advance();
                        self.fin_parse_by_ref_ident_pat(ast::Mutability::Mut)
                    }
                    TokenKind::CommonIdent => {
                        let ident = self.source(self.token.span);
                        self.advance();
                        Ok(ast::Pat::Binding(ast::BindingPat {
                            mut_: ast::Mutability::Mut,
                            by_ref: ast::ByRef::No,
                            ident,
                        }))
                    }
                    _ => Err(ParseError::UnexpectedToken(
                        self.token,
                        one_of![TokenKind::Ref, ExpectedFragment::CommonIdent],
                    )),
                };
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
            TokenKind::SingleBang => {
                self.advance();
                return Ok(ast::Pat::Never);
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::Pat::Wildcard(ast::WildcardKind::Normal));
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

                    self.advance();
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

                        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;

                        let mut_ = self.parse_mutability();
                        let by_ref = if self.consume(TokenKind::Ref) {
                            ast::ByRef::Yes(self.parse_mutability())
                        } else {
                            ast::ByRef::No
                        };

                        let (binder, _) = self.parse_common_ident_or(TokenKind::NumLit)?;
                        // NOTE: Indeed, contrary to struct exprs, shorthand numeric fields are
                        //       syntactically permitted in struct pats.
                        let body = if let ast::Mutability::Not = mut_
                            && let ast::ByRef::No = by_ref
                            && self.consume(TokenKind::SingleColon)
                        {
                            Some(self.parse_pat(OrPolicy::Allowed)?)
                        } else {
                            None
                        };

                        fields.push(ast::StructPatField { attrs, mut_, by_ref, binder, body });

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
                } => ast::Pat::Binding(ast::BindingPat {
                    mut_: ast::Mutability::Not,
                    by_ref: ast::ByRef::No,
                    ident,
                }),
                _ => ast::Pat::Path(Box::new(path)),
            });
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Pat))
    }

    fn parse_opt_lit_pat(&mut self) -> Result<Option<ast::Pat<'src>>> {
        let sign =
            if self.consume(TokenKind::SingleHyphen) { ast::Sign::Neg } else { ast::Sign::None };

        let lit = match self.token.kind {
            TokenKind::CharLit => {
                let lit = self.source(self.token.span);
                self.advance();
                Some(ast::Lit::Char(lit))
            }
            TokenKind::False => {
                self.advance();
                Some(ast::Lit::Bool(false))
            }
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                Some(ast::Lit::Num(lit))
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                Some(ast::Lit::Str(lit))
            }
            TokenKind::True => {
                self.advance();
                Some(ast::Lit::Bool(true))
            }
            _ => None,
        };

        if let Some(lit) = lit {
            return Ok(Some(ast::Pat::Lit(sign, lit)));
        }
        if let ast::Sign::Neg = sign {
            return Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Literal));
        }

        Ok(None)
    }

    fn fin_parse_by_ref_ident_pat(&mut self, mut_: ast::Mutability) -> Result<ast::Pat<'src>> {
        let ref_mut = self.parse_mutability();
        let ident = self.parse_common_ident()?;
        Ok(ast::Pat::Binding(ast::BindingPat { by_ref: ast::ByRef::Yes(ref_mut), mut_, ident }))
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
