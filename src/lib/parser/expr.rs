use super::{
    Fragment, Result, TokenKind, TokenPrefix,
    common::ExpInNumIdentPolicy,
    frags,
    pat::OrPolicy,
    path::GenericArgsMode,
    ty::PlusPolicy,
    weak::{self, Weak as _},
};
use crate::{ast, edition::Edition, error::ErrorKind, feature::Feature, span::Span};
use std::mem;

impl<'src> super::Parser<'_, '_, 'src> {
    /// Parse an expression.
    pub(super) fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_where(StructPolicy::Parse, LetPolicy::YieldOrReject, OpPolicy::Parse)
    }

    pub(super) fn parse_expr_where(
        &mut self,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        o_policy: OpPolicy,
    ) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        let start = self.token.span;
        let expr = self.parse_expr_at_level(Level::Initial, s_policy, l_policy, o_policy)?;
        let span = self.prev_token().map_or(start, |token| start.to(token.span));
        self.validate_let_chain(&expr, span, l_policy);

        Ok(expr)
    }

    pub(super) fn begins_expr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_expr`.

        // `TokenKind::Let` isn't included here because let-exprs are but an impl detail.
        match self.token.kind {
            | TokenKind::Async
            | TokenKind::Become
            | TokenKind::Break
            | TokenKind::CharLit
            | TokenKind::Const
            | TokenKind::Continue
            | TokenKind::Do
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
            | TokenKind::Static
            | TokenKind::StrLit
            | TokenKind::TickedIdent
            | TokenKind::True
            | TokenKind::Try
            | TokenKind::Underscore
            | TokenKind::Use
            | TokenKind::Unsafe
            | TokenKind::While
            | TokenKind::Yield => true,
            _ => self.begins_ext_path(0) || self.begins_outer_attr(),
        }
    }

    fn parse_expr_at_level(
        &mut self,
        level: Level,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        o_policy: OpPolicy,
    ) -> Result<ast::Expr<'src>> {
        let mut h_policy = HigherPostfixOpPolicy::Parse;

        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;

        let mut left = if let Some(op) = self.token.kind.as_prefix_expr_op() {
            h_policy = HigherPostfixOpPolicy::Yield;
            self.advance();

            let left = self.fin_parse_prefix_op_expr(op, s_policy, o_policy, attrs)?;

            if let Op::Range(_) = op {
                return Ok(left);
            }

            left
        } else {
            self.parse_lower_expr(s_policy, l_policy, attrs)?
        };

        while let Some(op) = self.token.kind.as_infix_or_postfix_expr_op() {
            if let OpPolicy::YieldOnBoundary(rule) = o_policy
                && left.kind.is_boundary(rule)
                && !op.overrules_boundary()
            {
                if let Op::Call | Op::Index = op
                    && level != Level::Initial
                {
                    self.error(ErrorKind::InvalidOpAfterBoundary, self.token.span);
                    return Err(());
                }

                break;
            }

            let left_level = op.left_level().unwrap();
            if left_level <= level {
                break;
            }

            if let Level::Compare = left_level
                && let ast::ExprKind::BinOp(ast::CompareOp!(), ..) = left.kind
            {
                self.error(ErrorKind::ChainedComparison, self.token.span);
            }

            if let Op::Call | Op::Dot | Op::Index | Op::Try = op {
                if let HigherPostfixOpPolicy::Yield = h_policy {
                    break;
                }
                if let ast::ExprKind::Cast(..) = left.kind {
                    self.error(ErrorKind::InvalidOpAfterCast, self.token.span);
                }
            } else {
                h_policy = HigherPostfixOpPolicy::Yield;
            }

            self.advance();

            left =
                self.fin_parse_infix_or_postfix_op_expr(op, left, s_policy, l_policy, o_policy)?;

            if let Op::Range(_) = op {
                break;
            }
        }

        Ok(left)
    }

    fn fin_parse_prefix_op_expr(
        &mut self,
        op: Op,
        s_policy: StructPolicy,
        o_policy: OpPolicy,
        attrs: Vec<ast::Attr<'src>>,
    ) -> Result<ast::Expr<'src>> {
        let right_level = op.right_level().unwrap();

        match op {
            Op::DoubleBorrow => {
                let expr =
                    self.fin_parse_borrow_expr(right_level, s_policy, o_policy, Vec::new())?;
                let kind =
                    ast::ExprKind::Borrow(ast::BorrowKind::Ref, ast::Mut::No, Box::new(expr));
                Ok(ast::Expr { attrs, kind })
            }
            Op::Range(kind) => {
                self.fin_parse_range_expr(kind, None, right_level, s_policy, o_policy, attrs)
            }
            Op::SingleBorrow => self.fin_parse_borrow_expr(right_level, s_policy, o_policy, attrs),
            Op::UnOp(op) => {
                let right = self.parse_expr_at_level(
                    right_level,
                    s_policy,
                    LetPolicy::YieldOrReject,
                    o_policy,
                )?;

                Ok(ast::Expr { attrs, kind: ast::ExprKind::UnOp(op, Box::new(right)) })
            }
            _ => unreachable!(),
        }
    }

    fn fin_parse_infix_or_postfix_op_expr(
        &mut self,
        op: Op,
        mut left: ast::Expr<'src>,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        o_policy: OpPolicy,
    ) -> Result<ast::Expr<'src>> {
        let right_level = op.right_level();

        match op {
            Op::BinOp(op) => {
                let l_policy = match op {
                    ast::BinOp::And => l_policy,
                    _ => LetPolicy::YieldOrReject,
                };

                let right = self.parse_expr_at_level(
                    right_level.unwrap(),
                    s_policy,
                    l_policy,
                    OpPolicy::Parse,
                )?;

                Ok(ast::ExprKind::BinOp(op, Box::new(left), Box::new(right)).into())
            }
            Op::Call => {
                let attrs = mem::take(&mut left.attrs);
                let args = self.fin_parse_fn_args()?;
                Ok(ast::Expr { attrs, kind: ast::ExprKind::Call(Box::new(left), args) })
            }
            Op::Cast => {
                let ty = self.parse_ty_where(PlusPolicy::Yield)?;
                Ok(ast::ExprKind::Cast(Box::new(left), Box::new(ty)).into())
            }
            Op::Dot => self.fin_parse_dot_expr(left),
            Op::Index => {
                let attrs = mem::take(&mut left.attrs);
                let index = self.parse_expr()?;
                self.parse(TokenKind::CloseSquareBracket)?;
                let kind = ast::ExprKind::Index(Box::new(left), Box::new(index));
                Ok(ast::Expr { attrs, kind })
            }
            Op::Range(kind) => self.fin_parse_range_expr(
                kind,
                Some(Box::new(left)),
                right_level.unwrap(),
                s_policy,
                o_policy,
                Vec::new(),
            ),
            Op::Try => {
                let attrs = mem::take(&mut left.attrs);
                Ok(ast::Expr { attrs, kind: ast::ExprKind::Try(Box::new(left)) })
            }
            _ => unreachable!(),
        }
    }

    fn fin_parse_dot_expr(&mut self, mut left: ast::Expr<'src>) -> Result<ast::Expr<'src>> {
        let mut attrs = mem::take(&mut left.attrs);

        let numeric = match self.token.kind {
            TokenKind::Await => {
                self.advance();
                return Ok(ast::Expr { attrs, kind: ast::ExprKind::Await(Box::new(left)) });
            }
            TokenKind::CommonIdent => false,
            TokenKind::Match => {
                self.feature(Feature::postfix_match, self.token.span);
                self.advance();
                let kind = self.fin_parse_match_expr(left, ast::MatchKind::Postfix, &mut attrs)?;
                return Ok(ast::Expr { attrs, kind });
            }
            TokenKind::NumLit => true,
            TokenKind::Use => {
                self.feature(Feature::ergonomic_clones, self.token.span);
                self.advance();
                return Ok(ast::Expr { attrs, kind: ast::ExprKind::Use(Box::new(left)) });
            }
            TokenKind::Yield => {
                self.feature(Feature::yield_expr, self.token.span);
                self.advance();
                let kind = ast::ExprKind::Yield(ast::YieldExpr::Postfix(Box::new(left)));
                return Ok(ast::Expr { attrs, kind });
            }
            _ => {
                self.unexpected(
                    self.token,
                    frags![
                        TokenKind::Await,
                        TokenKind::CommonIdent,
                        TokenKind::Match,
                        TokenKind::NumLit,
                        TokenKind::Use,
                        TokenKind::Yield
                    ],
                );
                return Err(());
            }
        };

        let (ident, extra) = self.split_float_lit();

        if !numeric {
            let gen_args_start = self.token.span;
            let gen_args = ast::ObligatorilyDisambiguatedGenericArgs::parse(self)?;

            if self.consume(TokenKind::OpenRoundBracket) {
                let fn_args = self.fin_parse_fn_args()?;
                let kind = ast::ExprKind::MethodCall(Box::new(ast::MethodCallExpr {
                    receiver: left,
                    seg: ast::PathSeg { ident, args: gen_args },
                    args: fn_args,
                }));
                return Ok(ast::Expr { attrs, kind });
            }
            if gen_args.is_some() {
                self.error(
                    ErrorKind::GenericArgsOnFieldExpr,
                    gen_args_start.until(self.token.span),
                );
            }
        }

        left = ast::Expr { attrs, kind: ast::ExprKind::Field(Box::new(left), ident) };

        if let Some(ident) = extra {
            let attrs = mem::take(&mut left.attrs);
            left = ast::Expr { attrs, kind: ast::ExprKind::Field(Box::new(left), ident) };
        }

        Ok(left)
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
        o_policy: OpPolicy,
        attrs: Vec<ast::Attr<'src>>,
    ) -> Result<ast::Expr<'src>> {
        let (kind, mut_) = self.parse_borrow_kind_and_mutability();
        let expr =
            self.parse_expr_at_level(right_level, s_policy, LetPolicy::YieldOrReject, o_policy)?;

        Ok(ast::Expr { attrs, kind: ast::ExprKind::Borrow(kind, mut_, Box::new(expr)) })
    }

    fn fin_parse_range_expr(
        &mut self,
        kind: ast::RangeExprKind,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        s_policy: StructPolicy,
        o_policy: OpPolicy,
        attrs: Vec<ast::Attr<'src>>,
    ) -> Result<ast::Expr<'src>> {
        if left.is_none()
            && let Some(attr) = attrs.first()
        {
            let span = attr.span.to(attrs.last().unwrap().span);
            self.error(ErrorKind::ForbiddenOuterAttrs, span);
        }

        let right = if matches!(kind, ast::RangeExprKind::Inclusive)
            || (s_policy == StructPolicy::Parse || self.token.kind != TokenKind::OpenCurlyBracket)
                // NB: Indeed, we plain out ignore the level here.
                && self.begins_expr()
        {
            Some(self.parse_expr_at_level(
                right_level,
                s_policy,
                LetPolicy::YieldOrReject,
                o_policy,
            )?)
        } else {
            None
        };

        Ok(ast::Expr { attrs, kind: ast::ExprKind::Range(left, right.map(Box::new), kind) })
    }

    fn parse_lower_expr(
        &mut self,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        mut attrs: Vec<ast::Attr<'src>>,
    ) -> Result<ast::Expr<'src>> {
        let kind = self.parse_lower_expr_kind(s_policy, l_policy, &mut attrs)?;
        Ok(ast::Expr { attrs, kind })
    }

    #[expect(clippy::too_many_lines)]
    fn parse_lower_expr_kind(
        &mut self,
        s_policy: StructPolicy,
        l_policy: LetPolicy,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ExprKind<'src>> {
        let start = self.token.span;

        if let label @ Some(_) = self.parse_label() {
            self.parse(TokenKind::SingleColon)?;

            return match self.token.kind {
                TokenKind::For => {
                    self.advance();
                    self.fin_parse_for_loop_expr(label, attrs)
                }
                TokenKind::Loop => {
                    self.advance();
                    self.fin_parse_loop_expr(label, attrs)
                }
                TokenKind::OpenCurlyBracket => {
                    self.advance();
                    let block = self.fin_parse_block_expr(AttrPolicy::Parse(attrs))?;
                    return Ok(ast::ExprKind::Block(label, Box::new(block)));
                }
                TokenKind::While => {
                    self.advance();
                    self.fin_parse_while_loop_expr(label, attrs)
                }
                _ => {
                    self.unexpected(
                        self.token,
                        frags![
                            TokenKind::For,
                            TokenKind::Loop,
                            TokenKind::OpenCurlyBracket,
                            TokenKind::While
                        ],
                    );
                    return Err(());
                }
            };
        }

        // FIXME: Provide more targeted diagnostics if the qualifiers don't make sense.
        match self.parse_expr_qualifiers()?.as_mut_slice() {
            [] => {}
            [qualifiers @ .., Qualifier::OpenCurlyBracket] => {
                if let [Qualifier::Async | Qualifier::Gen(_), ..] = qualifiers {
                    let (async_, qualifiers) = Qualifier::strip_async(qualifiers);
                    let (gen_, qualifiers) = Qualifier::strip_gen(qualifiers, self);
                    let (mode, qualifiers) = Qualifier::strip_capture_mode(qualifiers, self);
                    if !qualifiers.is_empty() {
                        self.error(ErrorKind::InvalidExprPrefix, start.until(self.token.span));
                    }
                    let block = self.fin_parse_block_expr(AttrPolicy::Parse(attrs))?;
                    let kind = match (async_, gen_) {
                        (ast::Async::Yes, ast::Gen::Yes) => ast::GenBlockKind::AsyncGen,
                        (ast::Async::Yes, ast::Gen::No) => ast::GenBlockKind::Async,
                        (ast::Async::No, ast::Gen::Yes) => ast::GenBlockKind::Gen,
                        (ast::Async::No, ast::Gen::No) => unreachable!(),
                    };
                    return Ok(ast::ExprKind::GenBlock(kind, mode, Box::new(block)));
                }

                let (kind, qualifiers) = match qualifiers {
                    [qualifiers @ .., Qualifier::Const(_)] => {
                        (Some(ast::SpecialBlockKind::Const), qualifiers)
                    }
                    [qualifiers @ .., Qualifier::Try(span, ty)] => {
                        self.feature(
                            if ty.is_some() {
                                Feature::try_blocks_heterogeneous
                            } else {
                                Feature::try_blocks
                            },
                            *span,
                        );
                        (Some(ast::SpecialBlockKind::Try(mem::take(ty))), qualifiers)
                    }
                    [qualifiers @ .., Qualifier::Unsafe] => {
                        (Some(ast::SpecialBlockKind::Unsafe), qualifiers)
                    }
                    _ => (None, qualifiers),
                };
                if !qualifiers.is_empty() {
                    self.error(ErrorKind::InvalidExprPrefix, start.until(self.token.span));
                }
                let block = self.fin_parse_block_expr(AttrPolicy::Parse(attrs))?;
                return Ok(match kind {
                    None => ast::ExprKind::Block(None, Box::new(block)),
                    Some(kind) => ast::ExprKind::SpecialBlock(kind, Box::new(block)),
                });
            }
            [qualifiers @ .., Qualifier::Pipe] => {
                let mut modifiers = ast::ClosureExprModifiers::default();

                let (bound_vars, mut qualifiers) = match qualifiers {
                    [Qualifier::ForBinder(span, bound_vars), qualifiers @ ..] => {
                        self.feature(Feature::closure_lifetime_binder, *span);
                        (mem::take(bound_vars), &*qualifiers)
                    }
                    _ => (Vec::new(), &*qualifiers),
                };
                (modifiers.const_, qualifiers) = match qualifiers {
                    [Qualifier::Const(span), qualifiers @ ..] => {
                        self.feature(Feature::const_closures, *span);
                        (ast::Const::Yes, qualifiers)
                    }
                    _ => (ast::Const::No, qualifiers),
                };
                (modifiers.static_, qualifiers) = match qualifiers {
                    [Qualifier::Static(span), qualifiers @ ..] => {
                        self.feature(Feature::coroutines, *span);
                        (ast::Static::Yes, qualifiers)
                    }
                    _ => (ast::Static::No, qualifiers),
                };
                (modifiers.async_, qualifiers) = Qualifier::strip_async(qualifiers);
                (modifiers.gen_, qualifiers) = Qualifier::strip_gen(qualifiers, self);
                (modifiers.mode, qualifiers) = Qualifier::strip_capture_mode(qualifiers, self);
                if !qualifiers.is_empty() {
                    self.error(ErrorKind::InvalidExprPrefix, start.until(self.token.span));
                }

                return self.fin_parse_closure_expr(bound_vars, modifiers, s_policy);
            }
            [Qualifier::Move(span)] => {
                self.feature(Feature::move_expr, *span);
                self.parse(TokenKind::OpenRoundBracket)?;
                let expr = self.parse_expr()?;
                self.parse(TokenKind::CloseRoundBracket)?;
                return Ok(ast::ExprKind::Move(Box::new(expr)));
            }
            _ => {
                self.error(ErrorKind::InvalidExprPrefix, start.until(self.token.span));
                return Err(());
            }
        }

        match self.token.kind {
            TokenKind::Become => {
                self.feature(Feature::explicit_tail_calls, self.token.span);
                self.advance();
                return Ok(ast::ExprKind::Become(Box::new(self.parse_expr()?)));
            }
            TokenKind::Break => {
                self.advance();
                let label = self.parse_label();
                let expr = if (self.token.kind != TokenKind::OpenCurlyBracket
                    || s_policy == StructPolicy::Parse)
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
            TokenKind::CharLit => return Ok(self.fin_parse_lit_expr(ast::LitKind::Char)),
            TokenKind::CommonIdent if self.check(weak::Builtin) => {
                self.advance();
                return self.fin_parse_builtin_expr(start);
            }
            TokenKind::Continue => {
                self.advance();
                return Ok(ast::ExprKind::Continue(self.parse_label()));
            }
            TokenKind::Do if self.matches(weak::Yeet, self.peek(1)) => {
                self.feature(Feature::yeet_expr, self.token.span);
                self.advance();
                self.advance();
                let expr =
                    self.begins_expr().then(|| self.parse_expr().map(Box::new)).transpose()?;
                return Ok(ast::ExprKind::Yeet(expr));
            }
            TokenKind::False => return Ok(self.fin_parse_lit_expr(ast::LitKind::Bool)),
            TokenKind::For => {
                self.advance();
                return self.fin_parse_for_loop_expr(None, attrs);
            }
            TokenKind::If => {
                self.advance();
                return self.fin_parse_if_expr();
            }
            TokenKind::Let if let LetPolicy::Parse(_) = l_policy => {
                self.advance();
                let pat = self.parse_pat(OrPolicy::Parse)?;
                self.parse(TokenKind::SingleEquals)?;
                let body = self.parse_expr_at_level(
                    Level::AndRight,
                    s_policy,
                    LetPolicy::YieldOrReject,
                    OpPolicy::Parse,
                )?;
                return Ok(ast::ExprKind::Let(Box::new(ast::LetExpr { pat, body })));
            }
            TokenKind::Loop => {
                self.advance();
                return self.fin_parse_loop_expr(None, attrs);
            }
            TokenKind::Match => {
                self.advance();

                let scrutinee = self.parse_expr_where(
                    StructPolicy::Yield,
                    LetPolicy::YieldOrReject,
                    OpPolicy::Parse,
                )?;

                return self.fin_parse_match_expr(scrutinee, ast::MatchKind::Prefix, attrs);
            }
            TokenKind::NumLit => return Ok(self.fin_parse_lit_expr(ast::LitKind::Num)),
            TokenKind::OpenRoundBracket => {
                self.advance();

                let mut exprs = Vec::new();

                const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
                const SEPARATOR: TokenKind = TokenKind::Comma;
                while !self.consume(DELIMITER) {
                    let expr = self.parse_expr()?;

                    if self.token.kind == DELIMITER {
                        if exprs.is_empty() {
                            // This is actually a grouped node, not a tuple.
                            self.advance();
                            return Ok(ast::ExprKind::Grouped(Box::new(expr)));
                        }
                    } else {
                        self.parse(SEPARATOR)?;
                    }

                    exprs.push(expr);
                }

                return Ok(ast::ExprKind::Tuple(exprs));
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
                // FIXME: Add test where the break is inside an if!
                let expr =
                    self.begins_expr().then(|| self.parse_expr().map(Box::new)).transpose()?;
                return Ok(ast::ExprKind::Return(expr));
            }
            TokenKind::StrLit => return Ok(self.fin_parse_lit_expr(ast::LitKind::Str)),
            TokenKind::True => return Ok(self.fin_parse_lit_expr(ast::LitKind::Bool)),
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::ExprKind::Wildcard);
            }
            TokenKind::While => {
                self.advance();
                return self.fin_parse_while_loop_expr(None, attrs);
            }
            TokenKind::Yield => {
                self.feature(Feature::yield_expr, self.token.span);
                self.advance();
                let expr =
                    self.begins_expr().then(|| self.parse_expr().map(Box::new)).transpose()?;
                return Ok(ast::ExprKind::Yield(ast::YieldExpr::Prefix(expr)));
            }
            _ => {}
        }

        if self.begins_ext_path(0) {
            let path = self.parse_ext_path::<ast::ObligatorilyDisambiguatedGenericArgs>()?;

            match self.token.kind {
                TokenKind::SingleBang => {
                    if path.ext.is_some() {
                        self.error(ErrorKind::TyRelMacroCall, start.until(self.token.span));
                    }

                    self.advance();
                    let (bracket, stream) = self.parse_delimited_token_stream()?;

                    return Ok(ast::ExprKind::MacroCall(Box::new(ast::MacroCall {
                        path: path.path,
                        bracket,
                        stream,
                    })));
                }
                TokenKind::OpenCurlyBracket if let StructPolicy::Parse = s_policy => {
                    self.advance();

                    if path.ext.is_some() {
                        self.feature_no_span_fixme(Feature::more_qualified_paths);
                    }

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

                        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;

                        let (binder, numeric) = self.parse_common_ident_or(TokenKind::NumLit)?;
                        if numeric {
                            self.validate_numeric_ident(binder, ExpInNumIdentPolicy::Reject);
                        }

                        let body = if self.consume_or_parse(TokenKind::SingleColon, !numeric)? {
                            Some(self.parse_expr()?)
                        } else {
                            None
                        };

                        fields.push(ast::StructExprField { attrs, binder, body });

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

        self.unexpected(self.token, frags![Fragment::Expr]);
        Err(())
    }

    fn parse_expr_qualifiers(&mut self) -> Result<Vec<Qualifier<'src>>> {
        // FIXME: Should we also accept+split `|=` and `||=` for diagnostic purposes?

        let mut qualifiers = Vec::new();

        loop {
            let qualifier = match self.token.kind {
                TokenKind::Async => Qualifier::Async,
                TokenKind::Const => Qualifier::Const(self.token.span),
                TokenKind::DoublePipe => {
                    self.parse_unchecked(TokenPrefix::Pipe);
                    qualifiers.push(Qualifier::Pipe);
                    break;
                }
                TokenKind::For if self.pick_generic_param_list_over_ext_path(1) => {
                    let span = self.token.span;
                    self.advance();
                    self.parse(TokenPrefix::LessThan)?;
                    let bound_vars = self.fin_parse_generic_param_list()?;
                    // FIXME: The span should also include the list of parameters.
                    qualifiers.push(Qualifier::ForBinder(span, bound_vars));
                    continue;
                }
                TokenKind::Gen => Qualifier::Gen(self.token.span),
                TokenKind::Move => Qualifier::Move(self.token.span),
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
                TokenKind::Static => Qualifier::Static(self.token.span),
                TokenKind::Try => {
                    let span = self.token.span;
                    self.advance();
                    let ty = self
                        .consume(weak::Bikeshed)
                        .then(|| self.parse_ty().map(Box::new))
                        .transpose()?;
                    // FIXME: The span should also include the type if present.
                    qualifiers.push(Qualifier::Try(span, ty));
                    continue;
                }
                TokenKind::Unsafe => Qualifier::Unsafe,
                TokenKind::Use => Qualifier::Use(self.token.span),
                _ => break,
            };
            self.advance();
            qualifiers.push(qualifier);
        }

        Ok(qualifiers)
    }

    fn fin_parse_lit_expr(&mut self, kind: ast::LitKind) -> ast::ExprKind<'src> {
        ast::ExprKind::Lit(Box::new(self.fin_parse_lit(kind)))
    }

    pub(super) fn parse_block_expr(
        &mut self,
        a_policy: AttrPolicy<'_, 'src>,
    ) -> Result<ast::BlockExpr<'src>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        self.fin_parse_block_expr(a_policy)
    }

    /// Finish parsing a block expression assuming the leading `{` has already been parsed.
    pub(super) fn fin_parse_block_expr(
        &mut self,
        a_policy: AttrPolicy<'_, 'src>,
    ) -> Result<ast::BlockExpr<'src>> {
        let (attrs, reject) = match a_policy {
            AttrPolicy::Parse(attrs) => (attrs, false),
            AttrPolicy::Reject => (&mut Vec::new(), true),
        };
        self.parse_attrs_into(ast::AttrStyle::Inner, attrs)?;
        if reject && let Some(attr) = attrs.first() {
            let span = attr.span.to(attrs.last().unwrap().span);
            self.error(ErrorKind::ForbiddenInnerAttrs, span);
        }

        let mut stmts = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
        while !self.consume(DELIMITER) {
            stmts.push(self.parse_stmt(DELIMITER)?);
        }

        Ok(ast::BlockExpr { stmts })
    }

    fn fin_parse_closure_expr(
        &mut self,
        bound_vars: Vec<ast::GenericParam<'src>>,
        modifiers: ast::ClosureExprModifiers,
        s_policy: StructPolicy,
    ) -> Result<ast::ExprKind<'src>> {
        let params = self.fin_parse_delim_seq(TokenPrefix::Pipe, TokenKind::Comma, |this| {
            let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
            let pat = this.parse_pat(OrPolicy::Yield)?;
            let ty = this.consume(TokenKind::SingleColon).then(|| this.parse_ty()).transpose()?;

            Ok(ast::ClosureParam { attrs, pat, ty })
        })?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;

        let body = if ret_ty.is_some() {
            let mut attrs = Vec::new();
            let block = self.parse_block_expr(AttrPolicy::Parse(&mut attrs))?;
            ast::Expr { attrs, kind: ast::ExprKind::Block(None, Box::new(block)) }
        } else {
            self.parse_expr_where(s_policy, LetPolicy::YieldOrReject, OpPolicy::Parse)?
        };

        Ok(ast::ExprKind::Closure(Box::new(ast::ClosureExpr {
            bound_vars,
            modifiers,
            params,
            ret_ty,
            body,
        })))
    }

    fn fin_parse_for_loop_expr(
        &mut self,
        label: Option<ast::Ident<'src>>,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ExprKind<'src>> {
        let await_ = if let span = self.token.span
            && self.consume(TokenKind::Await)
        {
            self.feature(Feature::async_for_loop, span);
            ast::Await::Yes
        } else {
            ast::Await::No
        };
        let pat = self.parse_pat(OrPolicy::Parse)?;
        self.parse(TokenKind::In)?;
        let head =
            self.parse_expr_where(StructPolicy::Yield, LetPolicy::YieldOrReject, OpPolicy::Parse)?;
        let body = self.parse_block_expr(AttrPolicy::Parse(attrs))?;

        Ok(ast::ExprKind::ForLoop(Box::new(ast::ForLoopExpr { label, await_, pat, head, body })))
    }

    fn fin_parse_if_expr(&mut self) -> Result<ast::ExprKind<'src>> {
        let condition = self.parse_expr_where(
            StructPolicy::Yield,
            LetPolicy::Parse(LetAllowance::AtTopLevelOnlyPriorTo2024),
            OpPolicy::Parse,
        )?;
        let consequent = self.parse_block_expr(AttrPolicy::Reject)?;

        let alternate = if self.consume(TokenKind::Else) {
            Some(ast::Expr {
                attrs: Vec::new(),
                kind: match self.token.kind {
                    TokenKind::OpenCurlyBracket => {
                        self.advance();
                        let block = self.fin_parse_block_expr(AttrPolicy::Reject)?;
                        ast::ExprKind::Block(None, Box::new(block))
                    }
                    TokenKind::If => {
                        self.advance();
                        self.fin_parse_if_expr()?
                    }
                    _ => {
                        self.unexpected(
                            self.token,
                            frags![TokenKind::OpenCurlyBracket, TokenKind::If],
                        );
                        return Err(());
                    }
                },
            })
        } else {
            None
        };

        Ok(ast::ExprKind::If(Box::new(ast::IfExpr { condition, consequent, alternate })))
    }

    fn fin_parse_loop_expr(
        &mut self,
        label: Option<ast::Ident<'src>>,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ExprKind<'src>> {
        Ok(ast::ExprKind::Loop(label, Box::new(self.parse_block_expr(AttrPolicy::Parse(attrs))?)))
    }

    fn fin_parse_match_expr(
        &mut self,
        scrutinee: ast::Expr<'src>,
        kind: ast::MatchKind,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ExprKind<'src>> {
        self.parse(TokenKind::OpenCurlyBracket)?;

        self.parse_attrs_into(ast::AttrStyle::Inner, attrs)?;

        let mut arms = Vec::new();
        const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
        const SEPARATOR: TokenKind = TokenKind::Comma;
        while !self.consume(DELIMITER) {
            let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
            let pat = self.parse_pat(OrPolicy::Parse)?;

            let (pat, guard) = match pat {
                ast::Pat::Grouped(span, ast::Pat::Guarded(pat, guard)) => {
                    self.error(ErrorKind::ParenthesizedGuardedPatInMatch, span);

                    (*pat, Some(*guard))
                }
                _ if self.consume(TokenKind::If) => (
                    pat,
                    Some(self.parse_expr_where(
                        StructPolicy::Parse,
                        LetPolicy::Parse(LetAllowance::Unconditional),
                        OpPolicy::Parse,
                    )?),
                ),
                _ => (pat, None),
            };

            let rule = ast::CurlyBracketedMacroCallIsBoundary::No;

            // NOTE: I'm really unhappy about the existence of this complex condition and
            //       would like to see it gone as soon as possible.
            //       Reported upstream: <https://github.com/rust-lang/rust/issues/153134>.
            let is_body_optional = self.token.kind == DELIMITER
                || guard.is_some()
                || pat.contains_never_or_macro_call();

            let body = self
                .consume_or_parse(TokenKind::WideArrow, is_body_optional)?
                .then(|| {
                    self.parse_expr_where(
                        StructPolicy::Parse,
                        LetPolicy::YieldOrReject,
                        OpPolicy::YieldOnBoundary(rule),
                    )
                })
                .transpose()?;

            if body.is_none() {
                self.feature_no_span_fixme(Feature::never_patterns);
            }

            self.consume_or_parse(
                SEPARATOR,
                self.token.kind == DELIMITER
                    || body.as_ref().is_some_and(|body| body.kind.is_boundary(rule)),
            )?;

            arms.push(ast::MatchArm { attrs, pat, guard, body });
        }

        Ok(ast::ExprKind::Match(Box::new(ast::MatchExpr { kind, scrutinee, arms })))
    }

    fn fin_parse_while_loop_expr(
        &mut self,
        label: Option<ast::Ident<'src>>,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ExprKind<'src>> {
        let condition = self.parse_expr_where(
            StructPolicy::Yield,
            LetPolicy::Parse(LetAllowance::AtTopLevelOnlyPriorTo2024),
            OpPolicy::Parse,
        )?;
        let body = self.parse_block_expr(AttrPolicy::Parse(attrs))?;

        Ok(ast::ExprKind::WhileLoop(Box::new(ast::WhileLoopExpr { label, condition, body })))
    }

    fn fin_parse_builtin_expr(&mut self, start: Span) -> Result<ast::ExprKind<'src>> {
        self.fin_parse_builtin_syntax(start, ast::ExprKind::Error, |this, name| {
            Ok(match name {
                weak::OffsetOf::STR => {
                    let ty = this.parse_ty()?;
                    this.parse(TokenKind::Comma)?;
                    let fields = this.fin_parse_delimited_field_seq()?;

                    Some(ast::ExprKind::OffsetOf(Box::new(ty), fields))
                }
                weak::TypeAscribe::STR => {
                    let expr = this.parse_expr()?;
                    this.parse(TokenKind::Comma)?;
                    let ty = this.parse_ty()?;
                    this.parse(TokenKind::CloseRoundBracket)?;
                    Some(ast::ExprKind::Ascription(Box::new(expr), Box::new(ty)))
                }
                weak::UnwrapBinder::STR => {
                    let expr = this.parse_expr()?;
                    this.parse(TokenKind::CloseRoundBracket)?;
                    Some(ast::ExprKind::UnsafeBinderCast(
                        ast::UnsafeBinderCastKind::Unwrap,
                        Box::new(expr),
                    ))
                }
                weak::WrapBinder::STR => {
                    let expr = this.parse_expr()?;
                    this.parse(TokenKind::CloseRoundBracket)?;
                    Some(ast::ExprKind::UnsafeBinderCast(
                        ast::UnsafeBinderCastKind::Wrap,
                        Box::new(expr),
                    ))
                }
                _ => None,
            })
        })
    }

    fn validate_let_chain(&self, expr: &ast::Expr<'src>, span: Span, l_policy: LetPolicy) {
        // If the let policy was forbidden, we would've already failed while (actually) parsing.
        if let LetPolicy::Parse(_) = l_policy
            && !self.is_valid_let_chain(expr, true, l_policy)
        {
            // FIXME: Fake an UnexpectedToken(Let|&&|.., Fragment::Expr) in the
            // relevant cases for uniformity with the corresp. parser diagnostic.
            self.error(ErrorKind::InvalidLetChain, span);
        }
    }

    fn is_valid_let_chain(&self, expr: &ast::Expr<'src>, root: bool, l_policy: LetPolicy) -> bool {
        // We only check the cases that weren't already covered by the parser.

        match &expr.kind {
            ast::ExprKind::Let(_) => match l_policy {
                LetPolicy::Parse(LetAllowance::Unconditional) => true,
                LetPolicy::Parse(LetAllowance::AtTopLevelOnlyPriorTo2024) => {
                    root || self.edition >= Edition::Rust2024
                }
                LetPolicy::YieldOrReject => false,
            },
            ast::ExprKind::BinOp(ast::BinOp::And, left, right) => {
                self.is_valid_let_chain(left, false, l_policy)
                    && self.is_valid_let_chain(right, false, l_policy)
            }
            ast::ExprKind::BinOp(ast::BinOp::Or | ast::AssignOp!(), left, right) => {
                self.is_valid_let_chain(left, false, LetPolicy::YieldOrReject)
                    && self.is_valid_let_chain(right, false, LetPolicy::YieldOrReject)
            }
            ast::ExprKind::Range(Some(left), _right, _) => {
                self.is_valid_let_chain(left, false, LetPolicy::YieldOrReject)
            }
            _ => true,
        }
    }

    /// Optionally parse a label.
    fn parse_label(&mut self) -> Option<ast::Ident<'src>> {
        self.parse_ticked_ident(
            |kind| matches!(kind, TokenKind::CommonIdent),
            ErrorKind::ReservedLabel,
        )
    }
}

impl ast::Pat<'_> {
    fn contains_never_or_macro_call(&self) -> bool {
        match self {
            Self::Never | Self::MacroCall(_) => true,
            | Self::Binding(_)
            | Self::Error(_)
            | Self::Lit(..)
            | Self::Path(_)
            | Self::Range(..)
            | Self::Rest
            | Self::Wildcard(_) => false,
            | Self::Borrow(.., pat)
            | Self::Deref(pat)
            | Self::Grouped(_, pat)
            | Self::Guarded(pat, _)
            | Self::Or(_, pat) => pat.contains_never_or_macro_call(),
            Self::Slice(pats) | Self::Tuple(pats) => {
                pats.iter().any(Self::contains_never_or_macro_call)
            }
            Self::Struct(pat) => {
                pat.fields.iter().any(|field| field.body.contains_never_or_macro_call())
            }
            Self::TupleStruct(pat) => pat.fields.iter().any(Self::contains_never_or_macro_call),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum StructPolicy {
    Parse,
    Yield,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum LetPolicy {
    #[expect(private_interfaces)] // nobody should use this variant outside of this module
    Parse(LetAllowance),
    YieldOrReject,
}

#[derive(Clone, Copy, Debug)]
enum LetAllowance {
    Unconditional,
    AtTopLevelOnlyPriorTo2024,
}

#[derive(Clone, Copy)]
pub(crate) enum OpPolicy {
    Parse,
    YieldOnBoundary(ast::CurlyBracketedMacroCallIsBoundary),
}

enum HigherPostfixOpPolicy {
    Parse,
    Yield,
}

pub(crate) enum AttrPolicy<'a, 'src> {
    Parse(&'a mut Vec<ast::Attr<'src>>),
    Reject,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Op {
    BinOp(ast::BinOp),
    Call,
    Cast,
    Dot,
    DoubleBorrow,
    Index,
    Range(ast::RangeExprKind),
    SingleBorrow,
    Try,
    UnOp(ast::UnOp),
}

impl Op {
    fn left_level(self) -> Option<Level> {
        Some(match self {
            Self::BinOp(op) => op.left_level(),
            Self::Call | Self::Index => Level::Call,
            Self::Cast => Level::Cast,
            Self::UnOp(_) | Self::SingleBorrow | Self::DoubleBorrow => return None,
            Self::Dot => Level::Dot,
            Self::Range(_) => Level::Range,
            Self::Try => Level::Try,
        })
    }

    fn right_level(self) -> Option<Level> {
        Some(match self {
            Self::BinOp(op) => op.right_level(),
            Self::Call | Self::Cast | Self::Dot | Self::Index | Self::Try => return None,
            Self::UnOp(_) | Self::SingleBorrow | Self::DoubleBorrow => Level::Prefix,
            Self::Range(_) => Level::Range,
        })
    }

    fn overrules_boundary(self) -> bool {
        matches!(self, Self::Dot | Self::Try)
    }
}

impl ast::BinOp {
    fn left_level(self) -> Level {
        match self {
            Self::Add | Self::Sub => Level::SumLeft,
            Self::And => Level::AndLeft,
            Self::BitAnd => Level::BitAndLeft,
            Self::BitOr => Level::BitOrLeft,
            Self::BitShiftLeft | Self::BitShiftRight => Level::BitShiftLeft,
            Self::BitXor => Level::BitXorLeft,
            Self::Mul | Self::Div | Self::Rem => Level::ProductLeft,
            Self::Or => Level::OrLeft,
            ast::AssignOp!() => Level::AssignLeft,
            ast::CompareOp!() => Level::Compare,
        }
    }

    fn right_level(self) -> Level {
        match self {
            Self::Add | Self::Sub => Level::SumRight,
            Self::And => Level::AndRight,
            Self::BitAnd => Level::BitAndRight,
            Self::BitOr => Level::BitOrRight,
            Self::BitShiftLeft | Self::BitShiftRight => Level::BitShiftRight,
            Self::BitXor => Level::BitXorRight,
            Self::Mul | Self::Div | Self::Rem => Level::ProductRight,
            Self::Or => Level::OrRight,
            ast::AssignOp!() => Level::AssignRight,
            ast::CompareOp!() => Level::Compare,
        }
    }
}

impl TokenKind {
    fn as_prefix_expr_op(self) -> Option<Op> {
        Some(match self {
            Self::DoubleAmpersand => Op::DoubleBorrow,
            Self::DoubleDot => Op::Range(ast::RangeExprKind::Exclusive),
            Self::DoubleDotEquals => Op::Range(ast::RangeExprKind::Inclusive),
            Self::SingleAmpersand => Op::SingleBorrow,
            Self::SingleAsterisk => Op::UnOp(ast::UnOp::Deref),
            Self::SingleBang => Op::UnOp(ast::UnOp::Not),
            Self::SingleHyphen => Op::UnOp(ast::UnOp::Neg),
            _ => return None,
        })
    }

    fn as_infix_or_postfix_expr_op(self) -> Option<Op> {
        Some(match self {
            Self::AmpersandEquals => Op::BinOp(ast::BinOp::BitAndAssign),
            Self::As => Op::Cast,
            Self::AsteriskEquals => Op::BinOp(ast::BinOp::MulAssign),
            Self::BangEquals => Op::BinOp(ast::BinOp::Ne),
            Self::CaretEquals => Op::BinOp(ast::BinOp::BitXorAssign),
            Self::DoubleAmpersand => Op::BinOp(ast::BinOp::And),
            Self::DoubleDot => Op::Range(ast::RangeExprKind::Exclusive),
            Self::DoubleDotEquals => Op::Range(ast::RangeExprKind::Inclusive),
            Self::DoubleEquals => Op::BinOp(ast::BinOp::Eq),
            Self::DoubleGreaterThan => Op::BinOp(ast::BinOp::BitShiftRight),
            Self::DoubleGreaterThanEquals => Op::BinOp(ast::BinOp::BitShiftRightAssign),
            Self::DoubleLessThan => Op::BinOp(ast::BinOp::BitShiftLeft),
            Self::DoubleLessThanEquals => Op::BinOp(ast::BinOp::BitShiftLeftAssign),
            Self::DoublePipe => Op::BinOp(ast::BinOp::Or),
            Self::GreaterThanEquals => Op::BinOp(ast::BinOp::Ge),
            Self::HypenEquals => Op::BinOp(ast::BinOp::SubAssign),
            Self::LessThanEquals => Op::BinOp(ast::BinOp::Le),
            Self::OpenRoundBracket => Op::Call,
            Self::OpenSquareBracket => Op::Index,
            Self::PercentEquals => Op::BinOp(ast::BinOp::RemAssign),
            Self::PipeEquals => Op::BinOp(ast::BinOp::BitOrAssign),
            Self::PlusEquals => Op::BinOp(ast::BinOp::AddAssign),
            Self::QuestionMark => Op::Try,
            Self::SingleAmpersand => Op::BinOp(ast::BinOp::BitAnd),
            Self::SingleAsterisk => Op::BinOp(ast::BinOp::Mul),
            Self::SingleCaret => Op::BinOp(ast::BinOp::BitXor),
            Self::SingleDot => Op::Dot,
            Self::SingleEquals => Op::BinOp(ast::BinOp::Assign),
            Self::SingleGreaterThan => Op::BinOp(ast::BinOp::Gt),
            Self::SingleHyphen => Op::BinOp(ast::BinOp::Sub),
            Self::SingleLessThan => Op::BinOp(ast::BinOp::Lt),
            Self::SinglePercent => Op::BinOp(ast::BinOp::Rem),
            Self::SinglePipe => Op::BinOp(ast::BinOp::BitOr),
            Self::SinglePlus => Op::BinOp(ast::BinOp::Add),
            Self::SingleSlash => Op::BinOp(ast::BinOp::Div),
            Self::SlashEquals => Op::BinOp(ast::BinOp::DivAssign),
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
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
    Dot,
}

enum Qualifier<'src> {
    Async,
    Const(Span),
    ForBinder(Span, Vec<ast::GenericParam<'src>>),
    Gen(Span),
    Move(Span),
    OpenCurlyBracket,
    Pipe,
    Static(Span),
    Try(Span, Option<Box<ast::Ty<'src>>>),
    Unsafe,
    Use(Span),
}

impl Qualifier<'_> {
    fn strip_async(qualifiers: &[Self]) -> (ast::Async, &[Self]) {
        match qualifiers {
            [Self::Async, qualifiers @ ..] => (ast::Async::Yes, qualifiers),
            _ => (ast::Async::No, qualifiers),
        }
    }

    fn strip_gen<'q>(
        qualifiers: &'q [Self],
        p: &super::Parser<'_, '_, '_>,
    ) -> (ast::Gen, &'q [Self]) {
        match qualifiers {
            [Self::Gen(span), qualifiers @ ..] => {
                p.feature(Feature::gen_blocks, *span);
                (ast::Gen::Yes, qualifiers)
            }
            _ => (ast::Gen::No, qualifiers),
        }
    }

    fn strip_capture_mode<'q>(
        qualifiers: &'q [Self],
        p: &super::Parser<'_, '_, '_>,
    ) -> (ast::CaptureMode, &'q [Self]) {
        match qualifiers {
            [Self::Move(_), qualifiers @ ..] => (ast::CaptureMode::Move, qualifiers),
            [Self::Use(span), qualifiers @ ..] => {
                p.feature(Feature::ergonomic_clones, *span);
                (ast::CaptureMode::Use, qualifiers)
            }
            _ => (ast::CaptureMode::Ref, qualifiers),
        }
    }
}
