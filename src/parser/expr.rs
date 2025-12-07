use super::{
    ExpectedFragment, Parser, Result, TokenKind, error::ParseError, ident::RAW, one_of,
    pat::OrPolicy, path::GenericArgsMode,
};
use crate::ast;
use std::{cmp::Ordering, mem};

impl<'src> Parser<'_, 'src> {
    /// Parse an expression.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    pub(crate) fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_where(StructPolicy::Allowed, LetPolicy::Forbidden, OpPolicy::Allowed)
    }

    pub(crate) fn parse_expr_where(
        &mut self,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        o_policy: OpPolicy,
    ) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_at_level(Level::Initial, s_policy, l_policy, o_policy)
    }

    pub(super) fn begins_expr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_expr`.

        // `TokenKind::Let` isn't included here because let-exprs are but an impl detail.
        match self.token.kind {
            | TokenKind::Async
            | TokenKind::Break
            | TokenKind::CharLit
            | TokenKind::Const
            | TokenKind::Continue
            | TokenKind::DoubleAmpersand
            | TokenKind::DoubleDot
            | TokenKind::DoubleDotEquals
            | TokenKind::DoublePipe
            | TokenKind::False
            | TokenKind::For
            | TokenKind::Gen
            | TokenKind::If
            | TokenKind::Loop
            | TokenKind::Match
            | TokenKind::Move
            | TokenKind::NumLit
            | TokenKind::OpenCurlyBracket
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::Return
            | TokenKind::SingleAmpersand
            | TokenKind::SingleAsterisk
            | TokenKind::SingleBang
            | TokenKind::SingleHyphen
            | TokenKind::SinglePipe
            | TokenKind::StrLit
            | TokenKind::True
            | TokenKind::Try
            | TokenKind::Underscore
            | TokenKind::Unsafe
            | TokenKind::While => return true,
            _ => {}
        }

        if self.begins_ext_path() {
            return true;
        }

        false
    }

    fn parse_expr_at_level(
        &mut self,
        level: Level,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        o_policy: OpPolicy,
    ) -> Result<ast::Expr<'src>> {
        let op = match self.token.kind {
            TokenKind::SingleHyphen => Some(Op::Neg),
            TokenKind::SingleBang => Some(Op::Not),
            TokenKind::SingleAsterisk => Some(Op::Deref),
            TokenKind::SingleAmpersand => Some(Op::SingleBorrow),
            TokenKind::DoubleAmpersand => Some(Op::DoubleBorrow),
            TokenKind::DoubleDot => Some(Op::RangeExclusive),
            TokenKind::DoubleDotEquals => Some(Op::RangeInclusive),
            _ => None,
        };
        let mut left = if let Some(op) = op {
            self.advance();
            self.fin_parse_prefix_op_expr(op, s_policy)
        } else {
            self.parse_lower_expr(s_policy, l_policy)
        }?;

        loop {
            let op = match self.token.kind {
                TokenKind::AmpersandEquals => Op::BitAndAssign,
                TokenKind::As => Op::Cast,
                TokenKind::AsteriskEquals => Op::MulAssign,
                TokenKind::BangEquals => Op::Ne,
                TokenKind::CaretEquals => Op::BitXorAssign,
                TokenKind::DoubleAmpersand => Op::And,
                TokenKind::DoubleDot => Op::RangeExclusive,
                TokenKind::DoubleDotEquals => Op::RangeInclusive,
                TokenKind::DoubleEquals => Op::Eq,
                TokenKind::DoubleGreaterThan => Op::BitShiftRight,
                TokenKind::DoubleGreaterThanEquals => Op::BitShiftRightAssign,
                TokenKind::DoubleLessThan => Op::BitShiftLeft,
                TokenKind::DoubleLessThanEquals => Op::BitShiftLeftAssign,
                TokenKind::DoublePipe => Op::Or,
                TokenKind::GreaterThanEquals => Op::Ge,
                TokenKind::HypenEquals => Op::SubAssign,
                TokenKind::LessThanEquals => Op::Le,
                TokenKind::OpenRoundBracket => Op::Call,
                TokenKind::OpenSquareBracket => Op::Index,
                TokenKind::PercentEquals => Op::RemAssign,
                TokenKind::PipeEquals => Op::BitOrAssign,
                TokenKind::PlusEquals => Op::AddAssign,
                TokenKind::QuestionMark => Op::Try,
                TokenKind::SingleAmpersand => Op::BitAnd,
                TokenKind::SingleAsterisk => Op::Mul,
                TokenKind::SingleCaret => Op::BitXor,
                TokenKind::SingleDot => Op::Project,
                TokenKind::SingleEquals => Op::Assign,
                TokenKind::SingleGreaterThan => Op::Gt,
                TokenKind::SingleHyphen => Op::Sub,
                TokenKind::SingleLessThan => Op::Lt,
                TokenKind::SinglePercent => Op::Rem,
                TokenKind::SinglePipe => Op::BitOr,
                TokenKind::SinglePlus => Op::Add,
                TokenKind::SingleSlash => Op::Div,
                TokenKind::SlashEquals => Op::DivAssign,
                _ => break,
            };

            if let OpPolicy::Restricted(rule) = o_policy
                && left.kind.is_boundary(rule)
                && !op.overrules_boundary()
            {
                break;
            }

            let left_level = op.left_level().unwrap();
            match left_level.cmp(&level) {
                Ordering::Less => break,
                // FIXME: Don't use Debug repr of op, use surface-language symbol.
                Ordering::Equal => return Err(ParseError::OpCannotBeChained(format!("{op:?}"))),
                Ordering::Greater => {}
            }
            self.advance();

            left = self.fin_parse_op_expr(op, left, s_policy)?;
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_expr(
        &mut self,
        op: Op,
        s_policy: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let right_level = op.right_level().unwrap();

        let ast_op = match op {
            Op::Neg => ast::UnOp::Neg,
            Op::Not => ast::UnOp::Not,
            Op::Deref => ast::UnOp::Deref,
            Op::SingleBorrow => {
                return self.fin_parse_borrow_expr(right_level, s_policy);
            }
            Op::DoubleBorrow => {
                let borrow = self.fin_parse_borrow_expr(right_level, s_policy)?;
                return Ok(ast::ExprKind::Borrow(
                    ast::BorrowKind::Ref,
                    ast::Mutability::Not,
                    Box::new(borrow),
                )
                .into());
            }
            Op::RangeInclusive => {
                return self.fin_parse_range_inclusive_expr(None, right_level, s_policy);
            }
            Op::RangeExclusive => {
                return self.fin_parse_range_exclusive_expr(None, right_level, s_policy);
            }
            _ => unreachable!(),
        };

        let right = self.parse_expr_at_level(
            right_level,
            s_policy,
            LetPolicy::Forbidden,
            OpPolicy::Allowed,
        )?;
        Ok(ast::ExprKind::UnOp(ast_op, Box::new(right)).into())
    }

    fn fin_parse_op_expr(
        &mut self,
        op: Op,
        left: ast::Expr<'src>,
        s_policy: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let ast_op = match op {
            Op::Add => ast::BinOp::Add,
            Op::AddAssign => ast::BinOp::AddAssign,
            Op::And => ast::BinOp::And,
            Op::Assign => ast::BinOp::Assign,
            Op::BitAnd => ast::BinOp::BitAnd,
            Op::BitAndAssign => ast::BinOp::BitAndAssign,
            Op::BitOr => ast::BinOp::BitOr,
            Op::BitOrAssign => ast::BinOp::BitOrAssign,
            Op::BitShiftLeft => ast::BinOp::BitShiftLeft,
            Op::BitShiftLeftAssign => ast::BinOp::BitShiftLeftAssign,
            Op::BitShiftRight => ast::BinOp::BitShiftRight,
            Op::BitShiftRightAssign => ast::BinOp::BitShiftRightAssign,
            Op::BitXor => ast::BinOp::BitXor,
            Op::BitXorAssign => ast::BinOp::BitXorAssign,
            Op::Call => {
                let args = self.fin_parse_fn_args()?;
                return Ok(ast::ExprKind::Call(Box::new(left), args).into());
            }
            Op::Cast => {
                let ty = self.parse_ty()?;
                return Ok(ast::ExprKind::Cast(Box::new(left), Box::new(ty)).into());
            }
            Op::Div => ast::BinOp::Div,
            Op::DivAssign => ast::BinOp::DivAssign,
            Op::Eq => ast::BinOp::Eq,
            Op::Project => {
                return self.fin_parse_projection_expr(left);
            }
            Op::Ge => ast::BinOp::Ge,
            Op::Gt => ast::BinOp::Gt,
            Op::Index => {
                let index = self.parse_expr()?;
                self.parse(TokenKind::CloseSquareBracket)?;
                return Ok(ast::ExprKind::Index(Box::new(left), Box::new(index)).into());
            }
            Op::Le => ast::BinOp::Le,
            Op::Lt => ast::BinOp::Lt,
            Op::Mul => ast::BinOp::Mul,
            Op::MulAssign => ast::BinOp::MulAssign,
            Op::Ne => ast::BinOp::Ne,
            Op::Or => ast::BinOp::Or,
            Op::RangeExclusive => {
                return self.fin_parse_range_exclusive_expr(
                    Some(Box::new(left)),
                    op.right_level().unwrap(),
                    s_policy,
                );
            }
            Op::RangeInclusive => {
                return self.fin_parse_range_inclusive_expr(
                    Some(Box::new(left)),
                    op.right_level().unwrap(),
                    s_policy,
                );
            }
            Op::Rem => ast::BinOp::Rem,
            Op::RemAssign => ast::BinOp::RemAssign,
            Op::Sub => ast::BinOp::Sub,
            Op::SubAssign => ast::BinOp::SubAssign,
            Op::Try => return Ok(ast::ExprKind::Try(Box::new(left)).into()),
            _ => unreachable!(),
        };

        let right = self.parse_expr_at_level(
            op.right_level().unwrap(),
            s_policy,
            LetPolicy::Forbidden,
            OpPolicy::Allowed,
        )?;
        Ok(ast::ExprKind::BinOp(ast_op, Box::new(left), Box::new(right)).into())
    }

    fn fin_parse_projection_expr(&mut self, left: ast::Expr<'src>) -> Result<ast::Expr<'src>> {
        let numeric = match self.token.kind {
            TokenKind::Await => {
                self.advance();
                return Ok(ast::ExprKind::Await(Box::new(left)).into());
            }
            TokenKind::CommonIdent => false,
            // FIXME: TokenKind::Match // postfix match
            TokenKind::NumLit => true,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.token,
                    one_of![TokenKind::Await, TokenKind::CommonIdent, TokenKind::NumLit],
                ));
            }
        };

        let ident = self.source(self.token.span);
        self.advance();

        if !numeric {
            let gen_args_start = self.token.span;
            let gen_args = ast::ObligatorilyDisambiguatedGenericArgs::parse(self)?;

            if self.consume(TokenKind::OpenRoundBracket) {
                let args = self.fin_parse_fn_args()?;
                return Ok(ast::ExprKind::MethodCall(Box::new(ast::MethodCallExpr {
                    receiver: left,
                    seg: ast::PathSeg { ident, args: gen_args },
                    args,
                }))
                .into());
            } else if gen_args.is_some() {
                return Err(ParseError::GenericArgsOnFieldExpr(
                    gen_args_start.until(self.token.span),
                ));
            }
        }

        Ok(ast::ExprKind::Field(Box::new(left), ident).into())
    }

    fn fin_parse_fn_args(&mut self) -> Result<Vec<ast::Expr<'src>>> {
        self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            this.parse_expr()
        })
    }

    fn fin_parse_borrow_expr(
        &mut self,
        right_level: Level,
        s_policy: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let (kind, mut_) = if self.token.kind == TokenKind::CommonIdent
            && self.is_common_ident(RAW)
            && let Some(mut_) = self.look_ahead(1, |t| match t.kind {
                TokenKind::Mut => Some(ast::Mutability::Mut),
                TokenKind::Const => Some(ast::Mutability::Not),
                _ => None,
            }) {
            self.advance();
            self.advance();
            (ast::BorrowKind::Raw, mut_)
        } else {
            (ast::BorrowKind::Ref, self.parse_mutability())
        };

        let expr = self.parse_expr_at_level(
            right_level,
            s_policy,
            LetPolicy::Forbidden,
            OpPolicy::Allowed,
        )?;
        Ok(ast::ExprKind::Borrow(kind, mut_, Box::new(expr)).into())
    }

    // FIXME: We're accepting `..{ 0 } + 0` as an expr stmt even though we shouldn't.
    //        Passing along the OpSet doesn't help because we presumably break before
    //        the `+` when parsing the RHS of the range because `..` has a lower level
    //        compared to `+`. Thus we yield to the parent which presumably checks if
    //        `..{ 0 }` is "complete" which it isn't of course, so it accepts the `+`.
    //
    //        I know that in rustc, ranges aren't really parsed via a level / precedence
    //        system but ... ad hoc? I don't dare to read its code. I wonder if we
    //        should just parse the RHS with the initial level or sth like that?
    //
    //        We currently also parse `return x + .. .field` incorrectly likely due
    //        to similar reasons.
    fn fin_parse_range_exclusive_expr(
        &mut self,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        s_policy: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        // FIXME: "begins_expr_at(right_level)"?
        let right = self
            .begins_expr()
            .then(|| {
                self.parse_expr_at_level(
                    right_level,
                    s_policy,
                    LetPolicy::Forbidden,
                    OpPolicy::Allowed,
                )
            })
            .transpose()?;
        Ok(ast::ExprKind::Range(left, right.map(Box::new), ast::RangeExprKind::Exclusive).into())
    }

    // FIXME: See large comment above.
    fn fin_parse_range_inclusive_expr(
        &mut self,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        s_policy: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let right = self.parse_expr_at_level(
            right_level,
            s_policy,
            LetPolicy::Forbidden,
            OpPolicy::Allowed,
        )?;
        return Ok(ast::ExprKind::Range(
            left,
            Some(Box::new(right)),
            ast::RangeExprKind::Inclusive,
        )
        .into());
    }

    fn parse_lower_expr(
        &mut self,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
    ) -> Result<ast::Expr<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        let kind = self.parse_lower_expr_kind(s_policy, l_policy)?;
        Ok(ast::Expr { attrs, kind })
    }

    #[expect(clippy::too_many_lines)]
    fn parse_lower_expr_kind(
        &mut self,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
    ) -> Result<ast::ExprKind<'src>> {
        let start = self.token.span;

        // FIXME: Provide more targeted diagnostics if the qualifiers don't make sense.
        match self.parse_expr_qualifiers()?.as_mut_slice() {
            [] => {}
            [qualifiers @ .., Qualifier::OpenCurlyBracket] => {
                if let [Qualifier::Async | Qualifier::Gen, ..] = qualifiers {
                    let (asyncness, qualifiers) = Qualifier::strip_async(qualifiers);
                    let (genness, qualifiers) = Qualifier::strip_gen(qualifiers);
                    let (mode, qualifiers) = Qualifier::strip_move(qualifiers);
                    if !qualifiers.is_empty() {
                        return Err(ParseError::InvalidExprPrefix(start.until(self.token.span)));
                    }
                    let block = self.fin_parse_block_expr()?;
                    let kind = match (asyncness, genness) {
                        (ast::Asyncness::Async, ast::Genness::Gen) => ast::GenBlockKind::AsyncGen,
                        (ast::Asyncness::Async, ast::Genness::Not) => ast::GenBlockKind::Async,
                        (ast::Asyncness::Not, ast::Genness::Gen) => ast::GenBlockKind::Gen,
                        (ast::Asyncness::Not, ast::Genness::Not) => unreachable!(),
                    };
                    return Ok(ast::ExprKind::GenBlock(kind, mode, Box::new(block)));
                }

                // FIXME: This won't cut it once we support labeled blocks since
                //        only bare blocks can be labeled.
                let kind = match qualifiers {
                    [] => ast::BlockKind::Bare,
                    [Qualifier::Const] => ast::BlockKind::Const,
                    [Qualifier::Try] => ast::BlockKind::Try,
                    [Qualifier::Unsafe] => ast::BlockKind::Unsafe,
                    _ => return Err(ParseError::InvalidExprPrefix(start.until(self.token.span))),
                };
                return Ok(ast::ExprKind::Block(kind, Box::new(self.fin_parse_block_expr()?)));
            }
            [qualifiers @ .., Qualifier::Pipe] => {
                let mut modifiers = ast::ClosureExprModifiers::default();

                let (bound_vars, mut qualifiers) = match qualifiers {
                    [Qualifier::For(bound_vars), qualifiers @ ..] => {
                        (mem::take(bound_vars), &*qualifiers)
                    }
                    _ => (Vec::new(), &*qualifiers),
                };
                (modifiers.constness, qualifiers) = match qualifiers {
                    [Qualifier::Const, qualifiers @ ..] => (ast::Constness::Const, qualifiers),
                    _ => (ast::Constness::Not, qualifiers),
                };
                (modifiers.asyncness, qualifiers) = Qualifier::strip_async(qualifiers);
                (modifiers.genness, qualifiers) = Qualifier::strip_gen(qualifiers);
                (modifiers.mode, qualifiers) = Qualifier::strip_move(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidExprPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_closure_expr(bound_vars, modifiers);
            }
            _ => return Err(ParseError::InvalidExprPrefix(start.until(self.token.span))),
        }

        match self.token.kind {
            TokenKind::Break => {
                self.advance();
                let label = self.parse_lifetime()?.map(|ast::Lifetime(label)| label);
                let expr = if (self.token.kind != TokenKind::OpenCurlyBracket
                    || s_policy == StructPolicy::Allowed)
                    && self.begins_expr()
                {
                    // NOTE: Re. StructPolicy::Allowed -- yes, indeed!
                    //       Add test where the break is inside an if!
                    let expr = self.parse_expr()?;
                    Some(Box::new(expr))
                } else {
                    None
                };
                return Ok(ast::ExprKind::Break(label, expr));
            }
            TokenKind::CharLit => {
                let lit = self.source(self.token.span);
                self.advance();
                // FIXME: Validate that the char lit only contains one scalar.
                return Ok(ast::ExprKind::Lit(ast::Lit::Char(lit)));
            }
            TokenKind::Continue => {
                self.advance();
                // FIXME: Parse optional label.
                return Ok(ast::ExprKind::Continue);
            }
            TokenKind::False => {
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Bool(false)));
            }
            TokenKind::For => {
                self.advance();
                let pat = self.parse_pat(OrPolicy::Allowed)?;
                self.parse(TokenKind::In)?;
                let head = self.parse_expr_where(
                    StructPolicy::Forbidden,
                    LetPolicy::Forbidden,
                    OpPolicy::Allowed,
                )?;
                let body = self.parse_block_expr()?;
                return Ok(ast::ExprKind::ForLoop(Box::new(ast::ForLoopExpr { pat, head, body })));
            }
            TokenKind::If => {
                self.advance();
                return self.fin_parse_if_expr();
            }
            TokenKind::Let if let LetPolicy::Allowed = l_policy => {
                self.advance();
                let pat = self.parse_pat(OrPolicy::Allowed)?;
                self.parse(TokenKind::SingleEquals)?;
                // FIXME: This prolly parses `if let _ = true && true` with wrong precedence.
                let expr =
                    self.parse_expr_where(s_policy, LetPolicy::Forbidden, OpPolicy::Allowed)?;
                return Ok(ast::ExprKind::Let(Box::new(ast::LetExpr { pat, body: expr })));
            }
            TokenKind::Loop => {
                self.advance();
                return Ok(ast::ExprKind::Loop(Box::new(self.parse_block_expr()?)));
            }
            TokenKind::Match => {
                self.advance();

                let scrutinee = self.parse_expr_where(
                    StructPolicy::Forbidden,
                    LetPolicy::Forbidden,
                    OpPolicy::Allowed,
                )?;
                let mut arms = Vec::new();

                self.parse(TokenKind::OpenCurlyBracket)?;

                const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                const SEPARATOR: TokenKind = TokenKind::Comma;
                while !self.consume(DELIMITER) {
                    let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
                    let pat = self.parse_pat(OrPolicy::Allowed)?;
                    let guard = self
                        .consume(TokenKind::If)
                        .then(|| {
                            self.parse_expr_where(
                                StructPolicy::Allowed,
                                LetPolicy::Allowed,
                                OpPolicy::Allowed,
                            )
                        })
                        .transpose()?;
                    self.parse(TokenKind::WideArrow)?;

                    let rule = ast::CurlyBracketedMacroCallIsBoundary::No;

                    let body = self.parse_expr_where(
                        StructPolicy::Allowed,
                        LetPolicy::Forbidden,
                        OpPolicy::Restricted(rule),
                    )?;

                    if self.token.kind == DELIMITER || body.kind.is_boundary(rule) {
                        self.consume(SEPARATOR);
                    } else {
                        self.parse(SEPARATOR)?;
                    }

                    arms.push(ast::MatchArm { attrs, pat, guard, body });
                }

                return Ok(ast::ExprKind::Match(Box::new(ast::MatchExpr { scrutinee, arms })));
            }
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Num(lit)));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_grouped_or_tuple(
                    |this| this.parse_expr(),
                    |expr| ast::ExprKind::Grouped(expr),
                    |exprs| ast::ExprKind::Tuple(exprs),
                );
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let mut elems = Vec::new();

                while !self.consume(TokenKind::CloseSquareBracket) {
                    let elem = self.parse_expr()?;

                    if elems.is_empty() && self.consume(TokenKind::Semicolon) {
                        let count = self.parse_expr()?;
                        self.parse(TokenKind::CloseSquareBracket)?;

                        return Ok(ast::ExprKind::Repeat(Box::new(elem), Box::new(count)));
                    }

                    elems.push(elem);

                    if self.token.kind != TokenKind::CloseSquareBracket {
                        self.parse(TokenKind::Comma)?;
                    }
                }

                return Ok(ast::ExprKind::Array(elems));
            }
            TokenKind::Return => {
                self.advance();
                // NOTE: Re. StructPolicy::Allowed -- yes, indeed!
                //       Add test where the break is inside an if!
                let expr =
                    self.begins_expr().then(|| self.parse_expr().map(Box::new)).transpose()?;
                return Ok(ast::ExprKind::Return(expr));
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Str(lit)));
            }
            TokenKind::True => {
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Bool(true)));
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::ExprKind::Wildcard);
            }
            TokenKind::While => {
                self.advance();
                let condition = self.parse_expr_where(
                    StructPolicy::Forbidden,
                    LetPolicy::Allowed,
                    OpPolicy::Allowed,
                )?;
                let body = self.parse_block_expr()?;
                return Ok(ast::ExprKind::While(Box::new(ast::WhileExpr { condition, body })));
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

                    return Ok(ast::ExprKind::MacroCall(Box::new(ast::MacroCall {
                        path,
                        bracket,
                        stream,
                    })));
                }
                TokenKind::OpenCurlyBracket if let StructPolicy::Allowed = s_policy => {
                    self.advance();

                    const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                    const SEPARATOR: TokenKind = TokenKind::Comma;
                    let mut fields = Vec::new();
                    let mut base = None;

                    while !self.consume(DELIMITER) {
                        if self.consume(TokenKind::DoubleDot) {
                            base = Some(if self.token.kind != DELIMITER {
                                Some(self.parse_expr()?)
                            } else {
                                None
                            });
                            self.parse(DELIMITER)?;
                            break;
                        }

                        let (binder, numeric) = self.parse_common_ident_or(TokenKind::NumLit)?;
                        let body = if self.consume_or_parse(TokenKind::SingleColon, !numeric)? {
                            Some(self.parse_expr()?)
                        } else {
                            None
                        };

                        fields.push(ast::StructExprField { binder, body });

                        if self.token.kind != DELIMITER {
                            self.parse(SEPARATOR)?;
                        }
                    }

                    return Ok(ast::ExprKind::Struct(Box::new(ast::StructExpr {
                        path,
                        fields,
                        base,
                    })));
                }
                _ => {}
            }

            return Ok(ast::ExprKind::Path(Box::new(path)));
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Expr))
    }

    fn parse_expr_qualifiers(&mut self) -> Result<Vec<Qualifier<'src>>> {
        // FIXME: Should we also accept+split `|=` and `||=` for diagnostic purposes?

        let mut qualifiers = Vec::new();

        loop {
            let qualifier = match self.token.kind {
                TokenKind::Async => Qualifier::Async,
                TokenKind::Const => Qualifier::Const,
                TokenKind::DoublePipe => {
                    self.modify_in_place(TokenKind::SinglePipe);
                    qualifiers.push(Qualifier::Pipe);
                    break;
                }
                TokenKind::For if self.pick_generic_param_list_over_ext_path(1) => {
                    self.advance();
                    qualifiers.push(Qualifier::For(self.parse_generic_param_list()?));
                    continue;
                }
                TokenKind::Gen => Qualifier::Gen,
                TokenKind::Move => Qualifier::Move,
                TokenKind::OpenCurlyBracket => {
                    self.advance();
                    qualifiers.push(Qualifier::OpenCurlyBracket);
                    break;
                }
                TokenKind::SinglePipe => {
                    self.advance();
                    qualifiers.push(Qualifier::Pipe);
                    break;
                }
                TokenKind::Try => Qualifier::Try,
                TokenKind::Unsafe => Qualifier::Unsafe,
                _ => break,
            };
            self.advance();
            qualifiers.push(qualifier);
        }

        Ok(qualifiers)
    }

    pub(super) fn parse_block_expr(&mut self) -> Result<ast::BlockExpr<'src>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        self.fin_parse_block_expr()
    }

    /// Finish parsing a block expression assuming the leading `{` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Block_Expr ::= "{" Attrs⟨Inner⟩* Stmt* "}"
    /// ```
    pub(super) fn fin_parse_block_expr(&mut self) -> Result<ast::BlockExpr<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let mut stmts = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
        while !self.consume(DELIMITER) {
            stmts.push(self.parse_stmt(DELIMITER)?);
        }

        Ok(ast::BlockExpr { attrs, stmts })
    }

    fn fin_parse_closure_expr(
        &mut self,
        bound_vars: Vec<ast::GenericParam<'src>>,
        modifiers: ast::ClosureExprModifiers,
    ) -> Result<ast::ExprKind<'src>> {
        // FIXME: Maybe reuse parse_fn_param_list smh?
        let params = self.fin_parse_delim_seq(TokenKind::SinglePipe, TokenKind::Comma, |this| {
            let pat = this.parse_pat(OrPolicy::Forbidden)?;
            let ty = this.consume(TokenKind::SingleColon).then(|| this.parse_ty()).transpose()?;

            Ok(ast::ClosureParam { pat, ty })
        })?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;

        let body = if ret_ty.is_some() {
            ast::ExprKind::Block(ast::BlockKind::Bare, Box::new(self.parse_block_expr()?)).into()
        } else {
            self.parse_expr()?
        };

        Ok(ast::ExprKind::Closure(Box::new(ast::ClosureExpr {
            bound_vars,
            modifiers,
            params,
            ret_ty,
            body,
        })))
    }

    fn fin_parse_if_expr(&mut self) -> Result<ast::ExprKind<'src>> {
        let condition =
            self.parse_expr_where(StructPolicy::Forbidden, LetPolicy::Allowed, OpPolicy::Allowed)?;
        let consequent = self.parse_block_expr()?;

        let alternate = if self.consume(TokenKind::Else) {
            Some(ast::Expr {
                attrs: Vec::new(),
                kind: match self.token.kind {
                    TokenKind::OpenCurlyBracket => {
                        self.advance();
                        ast::ExprKind::Block(
                            ast::BlockKind::Bare,
                            Box::new(self.fin_parse_block_expr()?),
                        )
                    }
                    TokenKind::If => {
                        self.advance();
                        self.fin_parse_if_expr()?
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            self.token,
                            one_of![TokenKind::OpenCurlyBracket, TokenKind::If],
                        ));
                    }
                },
            })
        } else {
            None
        };

        Ok(ast::ExprKind::If(Box::new(ast::IfExpr { condition, consequent, alternate })))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum StructPolicy {
    Allowed,
    Forbidden,
}

#[derive(Clone, Copy)]
pub(crate) enum LetPolicy {
    Allowed,
    Forbidden,
}

#[derive(Clone, Copy)]
pub(crate) enum OpPolicy {
    Allowed,
    Restricted(ast::CurlyBracketedMacroCallIsBoundary),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Op {
    Add,
    AddAssign,
    And,
    Assign,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitShiftLeft,
    BitShiftLeftAssign,
    BitShiftRight,
    BitShiftRightAssign,
    BitXor,
    BitXorAssign,
    Call,
    Cast,
    Deref,
    Div,
    DivAssign,
    DoubleBorrow,
    Eq,
    Project,
    Ge,
    Gt,
    Index,
    Le,
    Lt,
    Mul,
    MulAssign,
    Ne,
    Neg,
    Not,
    Or,
    RangeExclusive,
    RangeInclusive,
    Rem,
    RemAssign,
    SingleBorrow,
    Sub,
    SubAssign,
    Try,
}

impl Op {
    fn left_level(self) -> Option<Level> {
        Some(match self {
            Self::Add | Self::Sub => Level::SumLeft,
            Self::And => Level::AndLeft,
            | Self::AddAssign
            | Self::Assign
            | Self::BitAndAssign
            | Self::BitOrAssign
            | Self::BitShiftLeftAssign
            | Self::BitShiftRightAssign
            | Self::BitXorAssign
            | Self::DivAssign
            | Self::MulAssign
            | Self::RemAssign
            | Self::SubAssign => Level::AssignLeft,
            Self::BitAnd => Level::BitAndLeft,
            Self::BitOr => Level::BitOrLeft,
            Self::BitShiftLeft | Self::BitShiftRight => Level::BitShiftLeft,
            Self::BitXor => Level::BitXorLeft,
            Self::Call | Self::Index => Level::Call,
            Self::Cast => Level::Cast,
            Self::Deref | Self::Neg | Self::Not | Self::SingleBorrow | Self::DoubleBorrow => {
                return None;
            }
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => Level::Compare,
            Self::Project => Level::Project,
            Self::Mul | Self::Div | Self::Rem => Level::ProductLeft,
            Self::Or => Level::OrLeft,
            Self::RangeInclusive | Self::RangeExclusive => Level::Range,
            Self::Try => Level::Try,
        })
    }

    fn right_level(self) -> Option<Level> {
        Some(match self {
            Self::Add | Self::Sub => Level::SumRight,
            Self::And => Level::AndRight,
            | Self::AddAssign
            | Self::Assign
            | Self::BitAndAssign
            | Self::BitOrAssign
            | Self::BitShiftLeftAssign
            | Self::BitShiftRightAssign
            | Self::BitXorAssign
            | Self::DivAssign
            | Self::MulAssign
            | Self::RemAssign
            | Self::SubAssign => Level::AssignRight,
            Self::BitAnd => Level::BitAndRight,
            Self::BitOr => Level::BitOrRight,
            Self::BitShiftLeft | Self::BitShiftRight => Level::BitShiftRight,
            Self::BitXor => Level::BitXorRight,
            Self::Call | Self::Cast | Self::Project | Self::Index | Self::Try => return None,
            Self::Deref | Self::Neg | Self::Not | Self::SingleBorrow | Self::DoubleBorrow => {
                Level::Prefix
            }
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => Level::Compare,
            Self::Mul | Self::Div | Self::Rem => Level::ProductRight,
            Self::Or => Level::OrRight,
            Self::RangeInclusive | Self::RangeExclusive => Level::Range,
        })
    }

    fn overrules_boundary(self) -> bool {
        matches!(self, Self::Project | Self::Try)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Level {
    Initial,
    AssignRight,
    AssignLeft,
    Range,
    OrLeft,
    OrRight,
    AndLeft,
    AndRight,
    Compare,
    BitOrLeft,
    BitOrRight,
    BitXorLeft,
    BitXorRight,
    BitAndLeft,
    BitAndRight,
    BitShiftLeft,
    BitShiftRight,
    SumLeft,
    SumRight,
    ProductLeft,
    ProductRight,
    Cast,
    Prefix,
    Try,
    Call,
    Project,
}

enum Qualifier<'src> {
    Async,
    Const,
    For(Vec<ast::GenericParam<'src>>),
    Gen,
    Move,
    OpenCurlyBracket,
    Pipe,
    Try,
    Unsafe,
}

impl Qualifier<'_> {
    fn strip_async(qualifiers: &[Self]) -> (ast::Asyncness, &[Self]) {
        match qualifiers {
            [Self::Async, qualifiers @ ..] => (ast::Asyncness::Async, qualifiers),
            _ => (ast::Asyncness::Not, qualifiers),
        }
    }

    fn strip_gen(qualifiers: &[Self]) -> (ast::Genness, &[Self]) {
        match qualifiers {
            [Self::Gen, qualifiers @ ..] => (ast::Genness::Gen, qualifiers),
            _ => (ast::Genness::Not, qualifiers),
        }
    }

    fn strip_move(qualifiers: &[Self]) -> (ast::CaptureMode, &[Self]) {
        match qualifiers {
            [Self::Move, qualifiers @ ..] => (ast::CaptureMode::Move, qualifiers),
            _ => (ast::CaptureMode::Ref, qualifiers),
        }
    }
}
