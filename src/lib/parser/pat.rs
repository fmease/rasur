use super::{
    Fragment, Result, TokenKind,
    common::ExpInNumIdentPolicy,
    frags,
    weak::{self, Weak as _},
};
use crate::{ast, error::Error, feature::Feature};

impl<'src> super::Parser<'_, '_, 'src> {
    /// Parse a pattern.
    // FIXME: Experiment with turning ranges and negation back into true operators.
    //        Might not be feasible / worth it.
    pub(super) fn parse_pat(&mut self, o_policy: OrPolicy) -> Result<ast::Pat<'src>> {
        self.parse_pat_where(o_policy, NonLegacyRangePolicy::Parse, GuardPolicy::Yield)
    }

    fn parse_pat_where(
        &mut self,
        o_policy: OrPolicy,
        r_policy: NonLegacyRangePolicy,
        g_policy: GuardPolicy,
    ) -> Result<ast::Pat<'src>> {
        if let OrPolicy::Parse = o_policy {
            _ = self.consume(TokenKind::SinglePipe);
        }

        self.parse_pat_at_level(Level::Initial, o_policy, r_policy, g_policy)
    }

    fn parse_pat_at_level(
        &mut self,
        level: Level,
        o_policy: OrPolicy,
        r_policy: NonLegacyRangePolicy,
        g_policy: GuardPolicy,
    ) -> Result<ast::Pat<'src>> {
        let mut left = if let Some(op) = self.token.kind.as_prefix_pat_op() {
            self.advance();
            self.fin_parse_prefix_op_pat(op, o_policy, g_policy)
        } else {
            self.parse_lower_pat(r_policy)
        }?;

        while let Some(op) = self.token.kind.as_infix_or_postfix_pat_op(o_policy, g_policy) {
            if op.left_level().unwrap() <= level {
                break;
            }

            self.advance();

            left =
                self.fin_parse_infix_or_postfix_op_pat(op, left, o_policy, r_policy, g_policy)?;

            if let Op::Guard = op {
                break;
            }
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_pat(
        &mut self,
        op: Op,
        o_policy: OrPolicy,
        g_policy: GuardPolicy,
    ) -> Result<ast::Pat<'src>> {
        let right_level = op.right_level().unwrap();

        match op {
            Op::DoubleBorrow => {
                let borrow = self.fin_parse_borrow_pat(right_level, o_policy, g_policy)?;
                Ok(ast::Pat::Borrow(ast::BorrowKind::Ref, ast::Mut::No, Box::new(borrow)))
            }
            Op::SingleBorrow => self.fin_parse_borrow_pat(right_level, o_policy, g_policy),
            _ => unreachable!(),
        }
    }

    fn fin_parse_infix_or_postfix_op_pat(
        &mut self,
        op: Op,
        left: ast::Pat<'src>,
        o_policy: OrPolicy,
        r_policy: NonLegacyRangePolicy,
        g_policy: GuardPolicy,
    ) -> Result<ast::Pat<'src>> {
        match op {
            Op::Guard => {
                let guard = self.parse_expr()?;
                Ok(ast::Pat::Guarded(Box::new(left), Box::new(guard)))
            }
            Op::Or => {
                let right = self.parse_pat_at_level(
                    op.right_level().unwrap(),
                    o_policy,
                    r_policy,
                    g_policy,
                )?;
                Ok(ast::Pat::Or(Box::new(left), Box::new(right)))
            }
            _ => unreachable!(),
        }
    }

    fn fin_parse_borrow_pat(
        &mut self,
        right_level: Level,
        o_policy: OrPolicy,
        g_policy: GuardPolicy,
    ) -> Result<ast::Pat<'src>> {
        let (kind, mut_) = self.parse_borrow_kind_and_mutability();
        let pat =
            self.parse_pat_at_level(right_level, o_policy, NonLegacyRangePolicy::Yield, g_policy)?;
        Ok(ast::Pat::Borrow(kind, mut_, Box::new(pat)))
    }

    fn parse_lower_pat(&mut self, r_policy: NonLegacyRangePolicy) -> Result<ast::Pat<'src>> {
        let start = self.token.span;

        // `TripleDot` isn't included here as the corresponding range has to be bounded on the left.
        match self.token.kind {
            TokenKind::DoubleDot => {
                self.advance();
                return self.fin_parse_range_or_rest_pat(
                    DoubleDotKind::RestOrExclusiveRange(r_policy),
                    None,
                );
            }
            TokenKind::DoubleDotEquals if let NonLegacyRangePolicy::Parse = r_policy => {
                self.advance();
                return self.fin_parse_range_or_rest_pat(
                    DoubleDotKind::InclusiveRange { legacy: false },
                    None,
                );
            }
            _ => {}
        }

        if let Some((sign, lit)) = self.opt_parse_negatable_lit()? {
            return match self.token.kind {
                TokenKind::DoubleDot if let NonLegacyRangePolicy::Parse = r_policy => {
                    self.advance();
                    self.fin_parse_range_or_rest_pat(
                        DoubleDotKind::ExclusiveRange,
                        Some(ast::RangePatBound::Lit(sign, lit)),
                    )
                }
                TokenKind::DoubleDotEquals if let NonLegacyRangePolicy::Parse = r_policy => {
                    self.advance();
                    self.fin_parse_range_or_rest_pat(
                        DoubleDotKind::InclusiveRange { legacy: false },
                        Some(ast::RangePatBound::Lit(sign, lit)),
                    )
                }
                TokenKind::TripleDot => {
                    self.advance();
                    self.fin_parse_range_or_rest_pat(
                        DoubleDotKind::InclusiveRange { legacy: true },
                        Some(ast::RangePatBound::Lit(sign, lit)),
                    )
                }
                _ => Ok(ast::Pat::Lit(sign, lit)),
            };
        }

        match self.parse_mut_by_ref() {
            (ast::Mut::No, ast::ByRef::No) => {}
            (mut_, by_ref) => {
                let binder = self.parse_common_ident()?;
                return self.fin_parse_binding_pat(mut_, by_ref, binder);
            }
        }

        match self.token.kind {
            // FIXME: Should this be a prefix op? Then "OrPolicy::Yield" would come for free.
            TokenKind::Box => {
                self.feature(Feature::box_patterns, self.token.span);
                self.advance();
                return Ok(ast::Pat::Box(Box::new(self.parse_pat_where(
                    OrPolicy::Yield,
                    NonLegacyRangePolicy::Yield,
                    GuardPolicy::Yield,
                )?)));
            }
            TokenKind::CommonIdent if self.check(weak::Builtin) => {
                self.advance();
                return self.fin_parse_builtin_syntax(
                    start,
                    ast::Pat::Error,
                    |this, name| match name {
                        weak::Deref::STR => {
                            let pat = this.parse_pat(OrPolicy::Parse)?;
                            this.parse(TokenKind::CloseRoundBracket)?;
                            Ok(Some(ast::Pat::Deref(Box::new(pat))))
                        }
                        _ => Ok(None),
                    },
                );
            }
            TokenKind::OpenRoundBracket => {
                self.advance();

                let mut pats = Vec::new();

                const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
                const SEPARATOR: TokenKind = TokenKind::Comma;
                while !self.consume(DELIMITER) {
                    let pat = self.parse_pat_where(
                        OrPolicy::Parse,
                        NonLegacyRangePolicy::Parse,
                        GuardPolicy::Parse,
                    )?;

                    if self.token.kind == DELIMITER {
                        if pats.is_empty() && !matches!(pat, ast::Pat::Rest) {
                            // This is actually a grouped node, not a tuple.
                            self.advance();
                            return Ok(ast::Pat::Grouped(Box::new(pat)));
                        }
                    } else {
                        self.parse(SEPARATOR)?;
                    }

                    pats.push(pat);
                }

                return Ok(ast::Pat::Tuple(pats));
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let elems = self.fin_parse_delim_seq(
                    TokenKind::CloseSquareBracket,
                    TokenKind::Comma,
                    |this| {
                        this.parse_pat_where(
                            OrPolicy::Parse,
                            NonLegacyRangePolicy::Parse,
                            GuardPolicy::Parse,
                        )
                    },
                )?;
                return Ok(ast::Pat::Slice(elems));
            }
            TokenKind::SingleBang => {
                self.feature(Feature::never_patterns, self.token.span);
                self.advance();
                return Ok(ast::Pat::Never);
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::Pat::Wildcard(ast::WildcardKind::Normal));
            }
            _ => {}
        }

        if self.begins_ext_path(0) {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;

            match self.token.kind {
                TokenKind::DoubleDot if let NonLegacyRangePolicy::Parse = r_policy => {
                    self.advance();
                    return self.fin_parse_range_or_rest_pat(
                        DoubleDotKind::ExclusiveRange,
                        Some(ast::RangePatBound::Path(path)),
                    );
                }
                TokenKind::DoubleDotEquals if let NonLegacyRangePolicy::Parse = r_policy => {
                    self.advance();
                    return self.fin_parse_range_or_rest_pat(
                        DoubleDotKind::InclusiveRange { legacy: false },
                        Some(ast::RangePatBound::Path(path)),
                    );
                }
                TokenKind::OpenCurlyBracket => {
                    self.advance();

                    if path.ext.is_some() {
                        self.feature_no_span_fixme(Feature::more_qualified_paths);
                    }

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

                        let boxed = if let span = self.token.span
                            && self.consume(TokenKind::Box)
                        {
                            self.feature(Feature::box_patterns, span);
                            Boxed::Yes
                        } else {
                            Boxed::No
                        };
                        let (mut_, by_ref) = self.parse_mut_by_ref();

                        let unmarked = matches!(
                            (boxed, mut_, by_ref),
                            (Boxed::No, ast::Mut::No, ast::ByRef::No)
                        );

                        let (binder, numeric) = if unmarked {
                            self.parse_common_ident_or(TokenKind::NumLit)?
                        } else {
                            (self.parse_common_ident()?, false)
                        };
                        if numeric {
                            self.validate_numeric_ident(binder, ExpInNumIdentPolicy::Reject);
                        }

                        let (binder, body) = if unmarked
                            && self.consume_or_parse(TokenKind::SingleColon, !numeric)?
                        {
                            let body = self.parse_pat_where(
                                OrPolicy::Parse,
                                NonLegacyRangePolicy::Parse,
                                GuardPolicy::Parse,
                            )?;
                            (Some(binder), body)
                        } else {
                            let body = ast::Pat::Binding(Box::new(ast::BindingPat {
                                mut_,
                                by_ref,
                                binder,
                                pat: None,
                            }));
                            let body = match boxed {
                                Boxed::Yes => ast::Pat::Box(Box::new(body)),
                                Boxed::No => body,
                            };

                            (None, body)
                        };

                        fields.push(ast::StructPatField { attrs, binder, body });

                        if self.token.kind != DELIMITER {
                            self.parse(SEPARATOR)?;
                        }

                        #[derive(Clone, Copy)]
                        enum Boxed {
                            Yes,
                            No,
                        }
                    }

                    return Ok(ast::Pat::Struct(Box::new(ast::StructPat { path, fields, rest })));
                }
                TokenKind::OpenRoundBracket => {
                    self.advance();

                    let fields = self.fin_parse_delim_seq(
                        TokenKind::CloseRoundBracket,
                        TokenKind::Comma,
                        |this| {
                            this.parse_pat_where(
                                OrPolicy::Parse,
                                NonLegacyRangePolicy::Parse,
                                GuardPolicy::Parse,
                            )
                        },
                    )?;
                    return Ok(ast::Pat::TupleStruct(Box::new(ast::TupleStructPat {
                        path,
                        fields,
                    })));
                }
                TokenKind::SingleBang => {
                    if path.ext.is_some() {
                        self.error(Error::TyRelMacroCall(start.until(self.token.span)));
                    }

                    self.advance();
                    let (bracket, stream) = self.parse_delimited_token_stream()?;

                    return Ok(ast::Pat::MacroCall(Box::new(ast::MacroCall {
                        path: path.path,
                        bracket,
                        stream,
                    })));
                }
                TokenKind::TripleDot => {
                    self.advance();
                    return self.fin_parse_range_or_rest_pat(
                        DoubleDotKind::InclusiveRange { legacy: true },
                        Some(ast::RangePatBound::Path(path)),
                    );
                }
                _ => {}
            }

            return match path {
                ast::ExtPath {
                    ext: None,
                    path: ast::Path { segs: deref!([ast::PathSeg { ident, args: None }]) },
                } => self.fin_parse_binding_pat(ast::Mut::No, ast::ByRef::No, ident),
                _ => Ok(ast::Pat::Path(Box::new(path))),
            };
        }

        self.fatal(Error::UnexpectedToken(self.token, frags![Fragment::Pat]))
    }

    fn fin_parse_range_or_rest_pat(
        &mut self,
        kind: DoubleDotKind,
        left: Option<ast::RangePatBound<'src>>,
    ) -> Result<ast::Pat<'src>> {
        let right =
            if !matches!(kind, DoubleDotKind::RestOrExclusiveRange(NonLegacyRangePolicy::Yield))
                && (matches!(kind, DoubleDotKind::InclusiveRange { .. })
                    || self.begins_range_pat_bound())
            {
                Some(self.parse_range_pat_bound()?)
            } else {
                None
            };

        let kind = match kind {
            DoubleDotKind::ExclusiveRange | DoubleDotKind::RestOrExclusiveRange(_) => {
                if left.is_none() && right.is_none() {
                    return Ok(ast::Pat::Rest);
                }

                ast::RangePatKind::Exclusive
            }
            DoubleDotKind::InclusiveRange { legacy } => ast::RangePatKind::Inclusive { legacy },
        };

        Ok(ast::Pat::Range(left.map(Box::new), right.map(Box::new), kind))
    }

    fn parse_range_pat_bound(&mut self) -> Result<ast::RangePatBound<'src>> {
        // NOTE: To be kept in sync with `Self::begins_range_pat_bound`.

        if let Some((sign, lit)) = self.opt_parse_negatable_lit()? {
            Ok(ast::RangePatBound::Lit(sign, lit))
        } else if self.begins_ext_path(0) {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;
            Ok(ast::RangePatBound::Path(path))
        } else {
            self.fatal(Error::UnexpectedToken(self.token, frags![Fragment::Lit, Fragment::ExtPath]))
        }
    }

    fn begins_range_pat_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_range_pat_bound`.

        self.begins_negatable_lit() || self.begins_ext_path(0)
    }

    fn fin_parse_binding_pat(
        &mut self,
        mut_: ast::Mut,
        by_ref: ast::ByRef,
        binder: ast::Ident<'src>,
    ) -> Result<ast::Pat<'src>> {
        let pat = self
            .consume(TokenKind::At)
            .then(|| self.parse_pat(OrPolicy::Yield).map(Box::new))
            .transpose()?;
        Ok(ast::Pat::Binding(Box::new(ast::BindingPat { mut_, by_ref, binder, pat })))
    }

    fn parse_mut_by_ref(&mut self) -> (ast::Mut, ast::ByRef) {
        let start = self.token.span;
        let mut_ = self.parse_mut();

        let by_ref = if self.consume(TokenKind::Ref) {
            let (kind, mut_) = self.parse_borrow_kind_and_mutability();
            ast::ByRef::Yes(kind, mut_)
        } else {
            ast::ByRef::No
        };

        if let ast::Mut::Yes = mut_
            && let ast::ByRef::Yes(..) = by_ref
        {
            self.feature(Feature::mut_ref, start);
        }

        (mut_, by_ref)
    }
}

#[derive(Clone, Copy)]
pub(super) enum OrPolicy {
    Parse,
    Yield,
}

#[derive(Clone, Copy)]
enum NonLegacyRangePolicy {
    Parse,
    Yield,
}

#[derive(Clone, Copy)]
enum GuardPolicy {
    Parse,
    Yield,
}

enum DoubleDotKind {
    ExclusiveRange,
    InclusiveRange { legacy: bool },
    RestOrExclusiveRange(NonLegacyRangePolicy),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Level {
    Initial,
    Guard,
    OrLeft,
    OrRight,
    Prefix,
}

// Negation and ranges aren't included here since they don't operate on
// general patterns but on literals and range bounds, respectively.
#[derive(Clone, Copy, Debug)]
enum Op {
    DoubleBorrow,
    Guard,
    Or,
    SingleBorrow,
}

impl Op {
    fn left_level(self) -> Option<Level> {
        Some(match self {
            Self::Guard => Level::Guard,
            Self::Or => Level::OrLeft,
            Self::SingleBorrow | Self::DoubleBorrow => return None,
        })
    }

    fn right_level(self) -> Option<Level> {
        Some(match self {
            Self::Guard => return None,
            Self::Or => Level::OrRight,
            Self::SingleBorrow | Self::DoubleBorrow => Level::Prefix,
        })
    }
}

impl TokenKind {
    fn as_prefix_pat_op(self) -> Option<Op> {
        Some(match self {
            Self::SingleAmpersand => Op::SingleBorrow,
            Self::DoubleAmpersand => Op::DoubleBorrow,
            _ => return None,
        })
    }

    fn as_infix_or_postfix_pat_op(self, o_policy: OrPolicy, g_policy: GuardPolicy) -> Option<Op> {
        Some(match self {
            // FIXME: Register feature w/o triggering on stable match guards.
            // FEATURE: `guard_patterns` <https://github.com/rust-lang/rust/issues/129967>
            Self::If if let GuardPolicy::Parse = g_policy => Op::Guard,
            Self::SinglePipe if let OrPolicy::Parse = o_policy => Op::Or,
            _ => return None,
        })
    }
}
