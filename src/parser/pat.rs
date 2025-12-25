use super::{ExpectedFragment, Parser, Result, TokenKind, error::Error, one_of};
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
        // Negation and ranges aren't handled here since they don't operate on general patterns
        // but on literals and range bounds, respectively.
        let op = match self.token.kind {
            TokenKind::SingleAmpersand => Some(Op::SingleBorrow),
            TokenKind::DoubleAmpersand => Some(Op::DoubleBorrow),
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
                TokenKind::SinglePipe if let OrPolicy::Allowed = o_policy => Op::Or,
                _ => break,
            };

            let left_level = op.left_level().unwrap();
            match left_level.cmp(&level) {
                Ordering::Less => break,
                // FIXME: Don't use Debug repr of op, use surface-language symbol.
                Ordering::Equal => return self.fatal(Error::OpCannotBeChained(format!("{op:?}"))),
                Ordering::Greater => {}
            }
            self.advance();

            left = self.fin_parse_suffix_op_pat(op, left, o_policy)?;
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_pat(&mut self, op: Op, o_policy: OrPolicy) -> Result<ast::Pat<'src>> {
        let right_level = op.right_level().unwrap();

        match op {
            Op::DoubleBorrow => {
                let borrow = self.fin_parse_borrow_pat(right_level, o_policy)?;
                Ok(ast::Pat::Borrow(ast::BorrowKind::Ref, ast::Mutability::Not, Box::new(borrow)))
            }
            Op::SingleBorrow => self.fin_parse_borrow_pat(right_level, o_policy),
            _ => unreachable!(),
        }
    }

    fn fin_parse_suffix_op_pat(
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
            _ => unreachable!(),
        }
    }

    fn fin_parse_borrow_pat(
        &mut self,
        right_level: Level,
        o_policy: OrPolicy,
    ) -> Result<ast::Pat<'src>> {
        let (kind, mut_) = self.parse_borrow_kind_and_mutability();
        let pat = self.parse_pat_at_level(right_level, o_policy)?;
        Ok(ast::Pat::Borrow(kind, mut_, Box::new(pat)))
    }

    fn fin_parse_range_exclusive_pat(
        &mut self,
        left: Option<ast::RangePatBound<'src>>,
    ) -> Result<ast::Pat<'src>> {
        let right =
            self.begins_range_pat_bound().then(|| self.parse_range_pat_bound()).transpose()?;
        Ok(ast::Pat::Range(left, right, ast::RangePatKind::Exclusive))
    }

    fn fin_parse_range_inclusive_pat(
        &mut self,
        kind: ast::RangeInclusivePatKind,
        left: Option<ast::RangePatBound<'src>>,
    ) -> Result<ast::Pat<'src>> {
        let right = self.parse_range_pat_bound()?;
        Ok(ast::Pat::Range(left, Some(right), ast::RangePatKind::Inclusive(kind)))
    }

    fn parse_range_pat_bound(&mut self) -> Result<ast::RangePatBound<'src>> {
        // NOTE: To be kept in sync with `Self::begins_range_pat_bound`.

        if let Some((sign, lit)) = self.opt_parse_negatable_lit()? {
            Ok(ast::RangePatBound::Lit(sign, lit))
        } else if self.begins_ext_path(self.token) {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;
            Ok(ast::RangePatBound::Path(path))
        } else {
            self.fatal(Error::UnexpectedToken(
                self.token,
                one_of![ExpectedFragment::Literal, ExpectedFragment::ExtPath],
            ))
        }
    }

    fn begins_range_pat_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_range_pat_bound`.

        self.begins_negatable_lit() || self.begins_ext_path(self.token)
    }

    fn parse_lower_pat(&mut self) -> Result<ast::Pat<'src>> {
        // `TripleDot` isn't included here as the corresponding range has to be bounded on the left.
        match self.token.kind {
            TokenKind::DoubleDot => {
                self.advance();
                return self.fin_parse_range_exclusive_pat(None);
            }
            TokenKind::DoubleDotEquals => {
                self.advance();
                return self
                    .fin_parse_range_inclusive_pat(ast::RangeInclusivePatKind::Normal, None);
            }
            _ => {}
        }

        if let Some((sign, lit)) = self.opt_parse_negatable_lit()? {
            return match self.token.kind {
                TokenKind::DoubleDot => {
                    self.advance();
                    self.fin_parse_range_exclusive_pat(Some(ast::RangePatBound::Lit(sign, lit)))
                }
                TokenKind::DoubleDotEquals => {
                    self.advance();
                    self.fin_parse_range_inclusive_pat(
                        ast::RangeInclusivePatKind::Normal,
                        Some(ast::RangePatBound::Lit(sign, lit)),
                    )
                }
                TokenKind::TripleDot => {
                    self.advance();
                    self.fin_parse_range_inclusive_pat(
                        ast::RangeInclusivePatKind::Legacy,
                        Some(ast::RangePatBound::Lit(sign, lit)),
                    )
                }
                _ => Ok(ast::Pat::Lit(sign, lit)),
            };
        }

        match (self.parse_mutability(), self.parse_by_ref()) {
            (ast::Mutability::Not, ast::ByRef::No) => {}
            (mut_, by_ref) => {
                let binder = self.parse_common_ident()?;
                return self.fin_parse_binding_pat(mut_, by_ref, binder);
            }
        }

        match self.token.kind {
            TokenKind::Box => {
                self.advance();
                return Ok(ast::Pat::Box(Box::new(self.parse_pat(OrPolicy::Forbidden)?)));
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

        if self.begins_ext_path(self.token) {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;

            match self.token.kind {
                TokenKind::DoubleDot => {
                    self.advance();
                    return self
                        .fin_parse_range_exclusive_pat(Some(ast::RangePatBound::Path(path)));
                }
                TokenKind::DoubleDotEquals => {
                    self.advance();
                    return self.fin_parse_range_inclusive_pat(
                        ast::RangeInclusivePatKind::Normal,
                        Some(ast::RangePatBound::Path(path)),
                    );
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
                        let by_ref = self.parse_by_ref();

                        // NOTE: Shorthand numeric fields are syntactically permitted in
                        //       struct pats contrary to struct exprs.
                        let (binder, _) = self.parse_common_ident_or(TokenKind::NumLit)?;
                        let body = if let (ast::Mutability::Not, ast::ByRef::No) = (mut_, by_ref)
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
                TokenKind::SingleBang => {
                    let ast::ExtPath { ext: None, path } = path else {
                        return self.fatal(Error::TyRelMacroCall);
                    };

                    self.advance();
                    let (bracket, stream) = self.parse_delimited_token_stream()?;

                    return Ok(ast::Pat::MacroCall(Box::new(ast::MacroCall {
                        path,
                        bracket,
                        stream,
                    })));
                }
                TokenKind::TripleDot => {
                    self.advance();
                    return self.fin_parse_range_inclusive_pat(
                        ast::RangeInclusivePatKind::Legacy,
                        Some(ast::RangePatBound::Path(path)),
                    );
                }
                _ => {}
            }

            return match path {
                ast::ExtPath {
                    ext: None,
                    path: ast::Path { segs: deref!([ast::PathSeg { ident, args: None }]) },
                } => self.fin_parse_binding_pat(ast::Mutability::Not, ast::ByRef::No, ident),
                _ => Ok(ast::Pat::Path(Box::new(path))),
            };
        }

        self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::Pat))
    }

    fn fin_parse_binding_pat(
        &mut self,
        mut_: ast::Mutability,
        by_ref: ast::ByRef,
        binder: ast::Ident<'src>,
    ) -> Result<ast::Pat<'src>> {
        let pat = self
            .consume(TokenKind::At)
            .then(|| self.parse_pat(OrPolicy::Forbidden).map(Box::new))
            .transpose()?;
        Ok(ast::Pat::Binding(Box::new(ast::BindingPat { mut_, by_ref, binder, pat })))
    }

    fn parse_by_ref(&mut self) -> ast::ByRef {
        if self.consume(TokenKind::Ref) {
            let (kind, mut_) = self.parse_borrow_kind_and_mutability();
            ast::ByRef::Yes(kind, mut_)
        } else {
            ast::ByRef::No
        }
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
    OrLeft,
    OrRight,
    Prefix,
}

#[derive(Clone, Copy, Debug)]
enum Op {
    DoubleBorrow,
    Or,
    SingleBorrow,
}

impl Op {
    fn left_level(self) -> Option<Level> {
        Some(match self {
            Self::Or => Level::OrLeft,
            Self::SingleBorrow | Self::DoubleBorrow => return None,
        })
    }

    fn right_level(self) -> Option<Level> {
        Some(match self {
            Self::Or => Level::OrRight,
            Self::SingleBorrow | Self::DoubleBorrow => Level::Prefix,
        })
    }
}
