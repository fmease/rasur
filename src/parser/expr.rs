use super::{
    ExpectedFragment, Parser, Result, TokenKind, error::ParseError, one_of, pat::OrPolicy,
    path::GenericArgsMode,
};
use crate::ast;
use std::cmp::Ordering;

impl<'src> Parser<'_, 'src> {
    /// Parse an expression.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    pub(super) fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_at_level(Level::Initial, StructPolicy::Allowed, LetPolicy::Forbidden)
    }

    fn parse_expr_where(
        &mut self,
        structs: StructPolicy,
        lets: LetPolicy,
    ) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_at_level(Level::Initial, structs, lets)
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
        structs: StructPolicy,
        lets: LetPolicy,
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
            self.fin_parse_prefix_op_expr(op, structs)
        } else {
            self.parse_lower_expr(structs, lets)
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
                TokenKind::SingleDot => Op::Field,
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

            let left_level = op.left_level().unwrap();
            match left_level.cmp(&level) {
                Ordering::Less => break,
                // FIXME: Don't use Debug repr of op, use surface-language symbol.
                Ordering::Equal => return Err(ParseError::OpCannotBeChained(format!("{op:?}"))),
                Ordering::Greater => {}
            }
            self.advance();

            left = self.fin_parse_op_expr(op, left, structs)?;
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_expr(
        &mut self,
        op: Op,
        structs: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let right_level = op.right_level().unwrap();

        let ast_op = match op {
            Op::Neg => ast::UnOp::Neg,
            Op::Not => ast::UnOp::Not,
            Op::Deref => ast::UnOp::Deref,
            Op::SingleBorrow => {
                return self.fin_parse_borrow_expr(right_level, structs);
            }
            Op::DoubleBorrow => {
                let borrow = self.fin_parse_borrow_expr(right_level, structs)?;
                return Ok(ast::ExprKind::Borrow(ast::Mutability::Not, Box::new(borrow)).into());
            }
            Op::RangeInclusive => {
                return self.fin_parse_range_inclusive_expr(None, right_level, structs);
            }
            Op::RangeExclusive => {
                return self.fin_parse_range_exclusive_expr(None, right_level, structs);
            }
            _ => unreachable!(),
        };

        let right = self.parse_expr_at_level(right_level, structs, LetPolicy::Forbidden)?;
        Ok(ast::ExprKind::UnOp(ast_op, Box::new(right)).into())
    }

    fn fin_parse_op_expr(
        &mut self,
        op: Op,
        left: ast::Expr<'src>,
        structs: StructPolicy,
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
            Op::Field => {
                return self.fin_parse_field_or_method_call_expr(left);
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
                    structs,
                );
            }
            Op::RangeInclusive => {
                return self.fin_parse_range_inclusive_expr(
                    Some(Box::new(left)),
                    op.right_level().unwrap(),
                    structs,
                );
            }
            Op::Rem => ast::BinOp::Rem,
            Op::RemAssign => ast::BinOp::RemAssign,
            Op::Sub => ast::BinOp::Sub,
            Op::SubAssign => ast::BinOp::SubAssign,
            Op::Try => return Ok(ast::ExprKind::Try(Box::new(left)).into()),
            _ => unreachable!(),
        };

        let right =
            self.parse_expr_at_level(op.right_level().unwrap(), structs, LetPolicy::Forbidden)?;
        Ok(ast::ExprKind::BinOp(ast_op, Box::new(left), Box::new(right)).into())
    }

    fn fin_parse_field_or_method_call_expr(
        &mut self,
        left: ast::Expr<'src>,
    ) -> Result<ast::Expr<'src>> {
        match self.token.kind {
            TokenKind::NumLit => {
                let ident = self.source(self.token.span);
                self.advance();
                Ok(ast::ExprKind::Field(Box::new(left), ident).into())
            }
            _ if let Some(ident) = self.as_common_ident(self.token) => {
                self.advance();
                let gen_args_start = self.token.span;
                let gen_args = ast::ObligatorilyDisambiguatedGenericArgs::parse(self)?;
                Ok(if self.consume(TokenKind::OpenRoundBracket) {
                    let args = self.fin_parse_fn_args()?;
                    ast::ExprKind::MethodCall(Box::new(ast::MethodCallExpr {
                        receiver: left,
                        seg: ast::PathSeg { ident, args: gen_args },
                        args,
                    }))
                } else if gen_args.is_some() {
                    return Err(ParseError::GenericArgsOnFieldExpr(
                        gen_args_start.until(self.token.span),
                    ));
                } else {
                    ast::ExprKind::Field(Box::new(left), ident)
                }
                .into())
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.token,
                    one_of![ExpectedFragment::CommonIdent, TokenKind::NumLit],
                ));
            }
        }
    }

    fn fin_parse_fn_args(&mut self) -> Result<Vec<ast::Expr<'src>>> {
        self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            this.parse_expr()
        })
    }

    fn fin_parse_borrow_expr(
        &mut self,
        right_level: Level,
        structs: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let mut_ = self.parse_mutability();
        let expr = self.parse_expr_at_level(right_level, structs, LetPolicy::Forbidden)?;
        Ok(ast::ExprKind::Borrow(mut_, Box::new(expr)).into())
    }

    fn fin_parse_range_exclusive_expr(
        &mut self,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        structs: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        // FIXME: "begins_expr_at(right_level)"?
        let right = self
            .begins_expr()
            .then(|| self.parse_expr_at_level(right_level, structs, LetPolicy::Forbidden))
            .transpose()?;
        Ok(ast::ExprKind::Range(left, right.map(Box::new), ast::RangeExprKind::Exclusive).into())
    }

    fn fin_parse_range_inclusive_expr(
        &mut self,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        structs: StructPolicy,
    ) -> Result<ast::Expr<'src>> {
        let right = self.parse_expr_at_level(right_level, structs, LetPolicy::Forbidden)?;
        return Ok(ast::ExprKind::Range(
            left,
            Some(Box::new(right)),
            ast::RangeExprKind::Inclusive,
        )
        .into());
    }

    fn parse_lower_expr(
        &mut self,
        structs: StructPolicy,
        lets: LetPolicy,
    ) -> Result<ast::Expr<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        let kind = self.parse_lower_expr_kind(structs, lets)?;
        Ok(ast::Expr { attrs, kind })
    }

    #[expect(clippy::too_many_lines)]
    fn parse_lower_expr_kind(
        &mut self,
        structs: StructPolicy,
        lets: LetPolicy,
    ) -> Result<ast::ExprKind<'src>> {
        match self.token.kind {
            // FIXME: Also support async move? closures.
            TokenKind::Async => {
                self.advance();
                let gen_ = self.consume(TokenKind::Gen);
                return Ok(ast::ExprKind::Block(
                    match gen_ {
                        true => ast::BlockKind::AsyncGen,
                        false => ast::BlockKind::Async,
                    },
                    Box::new(self.parse_block_expr()?),
                ));
            }
            TokenKind::Break => {
                self.advance();
                let label = self.parse_common_lifetime()?.map(|ast::Lifetime(label)| label);
                let expr = if (self.token.kind != TokenKind::OpenCurlyBracket
                    || structs == StructPolicy::Allowed)
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
            TokenKind::Const => {
                self.advance();
                return Ok(ast::ExprKind::Block(
                    ast::BlockKind::Const,
                    Box::new(self.parse_block_expr()?),
                ));
            }
            TokenKind::Continue => {
                self.advance();
                // FIXME: Parse optional label.
                return Ok(ast::ExprKind::Continue);
            }
            TokenKind::DoublePipe => {
                self.modify_in_place(TokenKind::SinglePipe);
                return self.fin_parse_closure_expr(ast::ClosureKind::Normal);
            }
            TokenKind::False => {
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Bool(false)));
            }
            TokenKind::Gen => {
                self.advance();
                return Ok(ast::ExprKind::Block(
                    ast::BlockKind::Gen,
                    Box::new(self.parse_block_expr()?),
                ));
            }
            // FIXME: Also support closure expr with binder.
            TokenKind::For => {
                self.advance();
                let pat = self.parse_pat(OrPolicy::Allowed)?;
                self.parse(TokenKind::In)?;
                let expr = self.parse_expr_where(StructPolicy::Forbidden, LetPolicy::Forbidden)?;
                let body = self.parse_block_expr()?;
                return Ok(ast::ExprKind::ForLoop(Box::new(ast::ForLoopExpr {
                    pat,
                    head: expr,
                    body,
                })));
            }
            TokenKind::If => {
                self.advance();

                let condition =
                    self.parse_expr_where(StructPolicy::Forbidden, LetPolicy::Allowed)?;
                let consequent = self.parse_block_expr()?;

                let alternate = if self.consume(TokenKind::Else) {
                    let (TokenKind::If | TokenKind::OpenCurlyBracket) = self.token.kind else {
                        return Err(ParseError::UnexpectedToken(
                            self.token,
                            one_of![TokenKind::OpenCurlyBracket, TokenKind::If],
                        ));
                    };

                    // FIXME: Think about this again. StructPolicy::Allowed?
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                return Ok(ast::ExprKind::If(Box::new(ast::IfExpr {
                    condition,
                    consequent,
                    alternate,
                })));
            }
            TokenKind::Let if let LetPolicy::Allowed = lets => {
                self.advance();
                let pat = self.parse_pat(OrPolicy::Allowed)?;
                self.parse(TokenKind::SingleEquals)?;
                // FIXME: This prolly parses `if let _ = true && true` with wrong precedence.
                let expr = self.parse_expr_where(structs, LetPolicy::Forbidden)?;
                return Ok(ast::ExprKind::Let(Box::new(ast::LetExpr { pat, body: expr })));
            }
            TokenKind::Loop => {
                self.advance();
                return Ok(ast::ExprKind::Loop(Box::new(self.parse_block_expr()?)));
            }
            TokenKind::Match => {
                self.advance();

                let scrutinee =
                    self.parse_expr_where(StructPolicy::Forbidden, LetPolicy::Forbidden)?;
                let mut arms = Vec::new();

                self.parse(TokenKind::OpenCurlyBracket)?;

                const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                while !self.consume(DELIMITER) {
                    let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
                    let pat = self.parse_pat(OrPolicy::Allowed)?;
                    self.parse(TokenKind::WideArrow)?;

                    let body = self.parse_expr()?;

                    if self.token.kind == DELIMITER || !body.kind.needs_comma_as_match_arm_body() {
                        self.consume(TokenKind::Comma);
                    } else {
                        self.parse(TokenKind::Comma)?;
                    }

                    arms.push(ast::MatchArm { attrs, pat, body });
                }

                return Ok(ast::ExprKind::Match(Box::new(ast::MatchExpr { scrutinee, arms })));
            }
            TokenKind::Move => {
                self.advance();
                // FIXME: Hack. Create+use `parse(TokenPrefix::Pipe)`
                if self.token.kind == TokenKind::DoublePipe {
                    self.modify_in_place(TokenKind::SinglePipe);
                } else {
                    self.parse(TokenKind::SinglePipe)?;
                }
                return self.fin_parse_closure_expr(ast::ClosureKind::Move);
            }
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Num(lit)));
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return Ok(ast::ExprKind::Block(
                    ast::BlockKind::Bare,
                    Box::new(self.fin_parse_block_expr()?),
                ));
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
            TokenKind::SinglePipe => {
                self.advance();
                return self.fin_parse_closure_expr(ast::ClosureKind::Normal);
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
            TokenKind::Try => {
                self.advance();
                return Ok(ast::ExprKind::Block(
                    ast::BlockKind::Try,
                    Box::new(self.parse_block_expr()?),
                ));
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::ExprKind::Wildcard);
            }
            TokenKind::Unsafe => {
                self.advance();
                return Ok(ast::ExprKind::Block(
                    ast::BlockKind::Unsafe,
                    Box::new(self.parse_block_expr()?),
                ));
            }
            TokenKind::While => {
                self.advance();
                let condition =
                    self.parse_expr_where(StructPolicy::Forbidden, LetPolicy::Allowed)?;
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
                TokenKind::OpenCurlyBracket if let StructPolicy::Allowed = structs => {
                    self.advance();

                    let fields = self.fin_parse_delim_seq(
                        TokenKind::CloseCurlyBracket,
                        TokenKind::Comma,
                        |this| {
                            // FIXME: NumLit fields
                            let binder = this.parse_common_ident()?;
                            let body = this
                                .consume(TokenKind::SingleColon)
                                .then(|| this.parse_expr())
                                .transpose()?;
                            // FIXME: rest / base
                            Ok(ast::StructExprField { binder, body })
                        },
                    )?;

                    return Ok(ast::ExprKind::Struct(Box::new(ast::StructExpr { path, fields })));
                }
                _ => {}
            }

            return Ok(ast::ExprKind::Path(Box::new(path)));
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Expr))
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

    fn fin_parse_closure_expr(&mut self, kind: ast::ClosureKind) -> Result<ast::ExprKind<'src>> {
        // FIXME: Maybe reuse parse_fn_params smh?
        let params = self.fin_parse_delim_seq(TokenKind::SinglePipe, TokenKind::Comma, |this| {
            let pat = this.parse_pat(OrPolicy::Forbidden)?;
            let ty = this.consume(TokenKind::SingleColon).then(|| this.parse_ty()).transpose()?;

            Ok(ast::ClosureParam { pat, ty })
        })?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;

        let body = match ret_ty {
            Some(_) => {
                ast::ExprKind::Block(ast::BlockKind::Bare, Box::new(self.parse_block_expr()?))
                    .into()
            }
            None => self.parse_expr()?,
        };

        Ok(ast::ExprKind::Closure(Box::new(ast::ClosureExpr { kind, params, ret_ty, body })))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StructPolicy {
    Allowed,
    Forbidden,
}

#[derive(Clone, Copy)]
enum LetPolicy {
    Allowed,
    Forbidden,
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
    Field,
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
            Self::Field => Level::Project,
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
            Self::Call | Self::Cast | Self::Field | Self::Index | Self::Try => return None,
            Self::Deref | Self::Neg | Self::Not | Self::SingleBorrow | Self::DoubleBorrow => {
                Level::Prefix
            }
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => Level::Compare,
            Self::Mul | Self::Div | Self::Rem => Level::ProductRight,
            Self::Or => Level::OrRight,
            Self::RangeInclusive | Self::RangeExclusive => Level::Range,
        })
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
