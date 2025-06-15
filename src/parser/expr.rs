use super::{ExpectedFragment, Ident, ParseError, Parser, Result, TokenKind, one_of};
use crate::ast;
use std::cmp::Ordering;

impl<'src> Parser<'src> {
    /// Parse an expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Expr ::=
    ///     | Path
    ///     | Macro_Call
    ///     | Wildcard_Expr
    ///     | Bool_Expr
    ///     | If_Expr
    ///     | Loop_Expr
    ///     | Match_Expr
    ///     | While_Expr
    ///     | #Num_Lit
    ///     | #Str_Lit
    ///     | Borrow_Expr
    ///     | Block_Expr
    ///     | Paren_Or_Tuple_Expr
    /// Wildcard_Expr ::= "_"
    /// Bool_Expr ::= "false" | "true"
    /// If_Expr ::= "if" Expr__ Block_Expr ("else" (Block_Expr | If_Expr))?
    /// Loop_Expr ::= "loop" Block_Expr
    /// # FIXME: Doesn't include trailing-block logic
    /// Match_Expr ::= "match" Expr__ "{" (Pat "=>" Expr ("," | >"}"))* "}"
    /// While_Expr ::= "while" Expr__ "Block_Expr
    /// Borrow_Expr ::= "&" "mut"? Expr
    /// Paren_Or_Tuple_Expr ::= "(" (Expr ("," | >")"))* ")"
    /// ```
    pub(super) fn parse_expr(&mut self, policy: StructLitPolicy) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_at(Level::Initial, policy)
    }

    pub(super) fn begins_expr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_expr`.

        if self.begins_path() {
            return true;
        }

        let token = self.token();
        match token.kind {
            #[rustfmt::skip]
            TokenKind::Ident => {
                matches!(
                    self.source(token.span),
                    | "_" | "const" | "continue" | "break" | "false" | "if"
                    | "loop" | "match" | "return" | "true" | "while" | "unsafe"
                )
            }
            | TokenKind::SingleHyphen
            | TokenKind::SingleBang
            | TokenKind::Asterisk
            | TokenKind::SingleAmpersand
            | TokenKind::DoubleAmpersand
            | TokenKind::SinglePipe
            | TokenKind::DoublePipe
            | TokenKind::DoubleDot
            | TokenKind::DoubleDotEquals
            | TokenKind::NumLit
            | TokenKind::StrLit
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenCurlyBracket => true,
            _ => false,
        }
    }

    fn parse_expr_at(&mut self, level: Level, policy: StructLitPolicy) -> Result<ast::Expr<'src>> {
        let token = self.token();
        let op = match token.kind {
            // FIXME: Support DoubleHypen (double negated)
            TokenKind::SingleHyphen => Some(Op::Neg),
            TokenKind::SingleBang => Some(Op::Not),
            TokenKind::Asterisk => Some(Op::Deref),
            // FIXME: Also DoubleAmpersand, we want to support `&&1&&&&1` :)
            TokenKind::SingleAmpersand => Some(Op::Borrow),
            TokenKind::DoubleDot => Some(Op::RangeExclusive),
            TokenKind::DoubleDotEquals => Some(Op::RangeInclusive),
            _ => None,
        };
        let mut left = if let Some(op) = op {
            self.advance();
            self.fin_parse_prefix_op(op, policy)
        } else {
            self.parse_lower_expr(policy)
        }?;

        loop {
            let token = self.token();
            let op = match token.kind {
                TokenKind::SingleAmpersand => Op::BitAnd,
                TokenKind::Asterisk => Op::Mul,
                TokenKind::BangEquals => Op::Ne,
                TokenKind::Caret => Op::BitXor,
                TokenKind::SingleDot => Op::Field,
                TokenKind::DoubleDot => Op::RangeExclusive,
                TokenKind::DoubleDotEquals => Op::RangeInclusive,
                TokenKind::DoubleAmpersand => Op::And,
                TokenKind::DoubleEquals => Op::Eq,
                TokenKind::DoublePipe => Op::Or,
                TokenKind::SingleEquals => Op::Assign,
                TokenKind::GreaterThan => Op::Gt,
                TokenKind::GreaterThanEquals => Op::Ge,
                TokenKind::SingleHyphen => Op::Sub,
                TokenKind::LessThan => Op::Lt,
                TokenKind::LessThanEquals => Op::Le,
                TokenKind::Percent => Op::Rem,
                TokenKind::SinglePipe => Op::BitOr,
                TokenKind::Plus => Op::Add,
                TokenKind::Slash => Op::Div,
                TokenKind::OpenRoundBracket => Op::Call,
                TokenKind::OpenSquareBracket => Op::Index,
                TokenKind::QuestionMark => Op::Try,
                TokenKind::Ident if let "as" = self.source(token.span) => Op::Cast,
                _ => break,
            };

            let left_level = op.left_level().unwrap();
            match left_level.cmp(&level) {
                Ordering::Less => break,
                Ordering::Equal => return Err(ParseError::OpCannotBeChained(op)),
                Ordering::Greater => {}
            }
            self.advance();

            left = self.fin_parse_op(op, left, policy)?;
        }

        Ok(left)
    }

    fn fin_parse_prefix_op(&mut self, op: Op, policy: StructLitPolicy) -> Result<ast::Expr<'src>> {
        let right_level = op.right_level().unwrap();

        let ast_op = match op {
            Op::Neg => ast::UnOp::Neg,
            Op::Not => ast::UnOp::Not,
            Op::Deref => ast::UnOp::Deref,
            Op::Borrow => {
                let mut_ = self.parse_mutability();
                let expr = self.parse_expr_at(right_level, policy)?;
                return Ok(ast::Expr::Borrow(mut_, Box::new(expr)));
            }
            Op::RangeInclusive => {
                return self.fin_parse_range_exclusive(None, right_level, policy);
            }
            Op::RangeExclusive => {
                return self.fin_parse_range_exclusive(None, right_level, policy);
            }
            _ => unreachable!(),
        };

        let right = self.parse_expr_at(right_level, policy)?;
        Ok(ast::Expr::UnOp(ast_op, Box::new(right)))
    }

    fn fin_parse_op(
        &mut self,
        op: Op,
        left: ast::Expr<'src>,
        policy: StructLitPolicy,
    ) -> Result<ast::Expr<'src>> {
        let ast_op = match op {
            Op::Add => ast::BinOp::Add,
            Op::And => ast::BinOp::And,
            Op::Assign => ast::BinOp::Assign,
            Op::BitAnd => ast::BinOp::BitAnd,
            Op::BitOr => ast::BinOp::BitOr,
            Op::BitXor => ast::BinOp::BitXor,
            Op::Call => {
                let args = self.parse_delimited_sequence(
                    TokenKind::CloseRoundBracket,
                    TokenKind::Comma,
                    |this| this.parse_expr(StructLitPolicy::Allowed),
                )?;
                return Ok(ast::Expr::Call(Box::new(left), args));
            }
            Op::Cast => {
                let ty = self.parse_ty()?;
                return Ok(ast::Expr::Cast(Box::new(left), Box::new(ty)));
            }
            Op::Div => ast::BinOp::Div,
            Op::Eq => ast::BinOp::Eq,
            Op::Field => {
                let token = self.token();
                let ident = match token.kind {
                    TokenKind::NumLit => self.source(token.span),
                    _ if let Some(ident) = self.as_common_ident(token) => ident,
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            token,
                            one_of![ExpectedFragment::CommonIdent, TokenKind::NumLit],
                        ));
                    }
                };
                self.advance();
                return Ok(ast::Expr::Field(Box::new(left), ident));
            }
            Op::Ge => ast::BinOp::Ge,
            Op::Gt => ast::BinOp::Gt,
            Op::Index => {
                let index = self.parse_expr(StructLitPolicy::Allowed)?;
                self.parse(TokenKind::CloseSquareBracket)?;
                return Ok(ast::Expr::Index(Box::new(left), Box::new(index)));
            }
            Op::Le => ast::BinOp::Le,
            Op::Lt => ast::BinOp::Lt,
            Op::Mul => ast::BinOp::Mul,
            Op::Ne => ast::BinOp::Ne,
            Op::Or => ast::BinOp::Or,
            Op::RangeExclusive => {
                return self.fin_parse_range_exclusive(
                    Some(Box::new(left)),
                    op.right_level().unwrap(),
                    policy,
                );
            }
            Op::RangeInclusive => {
                return self.fin_parse_range_inclusive(
                    Some(Box::new(left)),
                    op.right_level().unwrap(),
                    policy,
                );
            }
            Op::Rem => ast::BinOp::Rem,
            Op::Sub => ast::BinOp::Sub,
            Op::Try => return Ok(ast::Expr::Try(Box::new(left))),
            _ => unreachable!(),
        };

        let right = self.parse_expr_at(op.right_level().unwrap(), policy)?;
        Ok(ast::Expr::BinOp(ast_op, Box::new(left), Box::new(right)))
    }

    fn fin_parse_range_exclusive(
        &mut self,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        policy: StructLitPolicy,
    ) -> Result<ast::Expr<'src>> {
        // FIXME: "begins_expr_at(right_level)"?
        let right =
            self.begins_expr().then(|| self.parse_expr_at(right_level, policy)).transpose()?;
        Ok(ast::Expr::Range(left, right.map(Box::new), ast::RangeKind::Exclusive))
    }

    fn fin_parse_range_inclusive(
        &mut self,
        left: Option<Box<ast::Expr<'src>>>,
        right_level: Level,
        policy: StructLitPolicy,
    ) -> Result<ast::Expr<'src>> {
        let right = self.parse_expr_at(right_level, policy)?;
        return Ok(ast::Expr::Range(left, Some(Box::new(right)), ast::RangeKind::Inclusive));
    }

    #[expect(clippy::too_many_lines)]
    fn parse_lower_expr(&mut self, policy: StructLitPolicy) -> Result<ast::Expr<'src>> {
        let token = self.token();
        match token.kind {
            TokenKind::Ident => match self.source(token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Expr::Wildcard);
                }
                "break" => {
                    self.advance();
                    let label = self.consume_lifetime()?.map(|ast::Lifetime(label)| label);
                    let expr = self
                        .begins_expr()
                        // NOTE: Yes indeed, allowed! Plz add test for this!
                        .then(|| self.parse_expr(StructLitPolicy::Allowed).map(Box::new))
                        .transpose()?;
                    return Ok(ast::Expr::Break(label, expr));
                }
                "const" => {
                    self.advance();
                    return Ok(ast::Expr::ConstBlock(Box::new(self.parse_block_expr()?)));
                }
                "continue" => {
                    self.advance();
                    return Ok(ast::Expr::Continue);
                }
                "false" => {
                    self.advance();
                    return Ok(ast::Expr::BoolLit(false));
                }
                "if" => {
                    self.advance();

                    // FIXME: Permit let-exprs
                    let condition = self.parse_expr(StructLitPolicy::Forbidden)?;
                    let consequent = self.parse_block_expr()?;

                    let alternate = if self.consume(Ident("else")) {
                        let token = self.token();
                        match token.kind {
                            TokenKind::OpenCurlyBracket => {}
                            TokenKind::Ident if let "if" = self.source(token.span) => {}
                            _ => {
                                return Err(ParseError::UnexpectedToken(
                                    token,
                                    one_of![
                                        TokenKind::OpenCurlyBracket,
                                        ExpectedFragment::Raw("if")
                                    ],
                                ));
                            }
                        }

                        // FIXME: Think about this again. Allowed?
                        Some(self.parse_expr(StructLitPolicy::Allowed)?)
                    } else {
                        None
                    };

                    return Ok(ast::Expr::If(Box::new(ast::IfExpr {
                        condition,
                        consequent,
                        alternate,
                    })));
                }
                "loop" => {
                    self.advance();
                    return Ok(ast::Expr::Loop(Box::new(self.parse_block_expr()?)));
                }
                "match" => {
                    self.advance();

                    let scrutinee = self.parse_expr(StructLitPolicy::Forbidden)?;
                    let mut arms = Vec::new();

                    self.parse(TokenKind::OpenCurlyBracket)?;

                    const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                    while !self.consume(DELIMITER) {
                        let pat = self.parse_pat()?;
                        self.parse(TokenKind::WideArrow)?;

                        let body = self.parse_expr(StructLitPolicy::Allowed)?;

                        if body.has_trailing_block(ast::TrailingBlockMode::Match)
                            || self.token().kind == DELIMITER
                        {
                            self.consume(TokenKind::Comma);
                        } else {
                            self.parse(TokenKind::Comma)?;
                        }

                        arms.push(ast::MatchArm { pat, body });
                    }

                    return Ok(ast::Expr::Match(Box::new(ast::MatchExpr { scrutinee, arms })));
                }
                "return" => {
                    self.advance();
                    let expr = self
                        .begins_expr()
                        // NOTE: Yes indeed. Add test.
                        .then(|| self.parse_expr(StructLitPolicy::Allowed).map(Box::new))
                        .transpose()?;
                    return Ok(ast::Expr::Return(expr));
                }
                "true" => {
                    self.advance();
                    return Ok(ast::Expr::BoolLit(true));
                }
                "while" => {
                    self.advance();
                    // FIXME: Permit let-exprs
                    let condition = self.parse_expr(StructLitPolicy::Forbidden)?;
                    let body = self.parse_block_expr()?;
                    return Ok(ast::Expr::While(Box::new(ast::WhileExpr { condition, body })));
                }
                "unsafe" => {
                    self.advance();
                    return Ok(ast::Expr::UnsafeBlock(Box::new(self.parse_block_expr()?)));
                }
                _ => {}
            },
            TokenKind::NumLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Expr::NumLit(lit));
            }
            TokenKind::StrLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Expr::StrLit(lit));
            }
            TokenKind::SinglePipe => {
                self.advance();
                // FIXME: Maybe reuse parse_fn_params smh?
                let params = self.parse_delimited_sequence(
                    TokenKind::SinglePipe,
                    TokenKind::Comma,
                    |this| {
                        let pat = this.parse_pat()?;
                        let ty = this
                            .consume(TokenKind::SingleColon)
                            .then(|| this.parse_ty())
                            .transpose()?;

                        Ok(ast::ClosureParam { pat, ty })
                    },
                )?;
                return self.fin_parse_closure_expr(params);
            }
            TokenKind::DoublePipe => {
                self.advance();
                return self.fin_parse_closure_expr(Vec::new());
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return Ok(ast::Expr::Block(Box::new(self.fin_parse_block_expr()?)));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_grouped_or_tuple(
                    |this| this.parse_expr(StructLitPolicy::Allowed),
                    ast::Expr::Grouped,
                    ast::Expr::Tup,
                );
            }
            _ => {}
        }

        if self.begins_ext_path() {
            let path = self.parse_ext_path::<ast::GenericArgsPolicy::DisambiguatedOnly>()?;

            let token = self.token();
            match token.kind {
                TokenKind::SingleBang => {
                    let ast::ExtPath { self_ty: None, path } = path else {
                        return Err(ParseError::TyRelMacroCall);
                    };

                    self.advance();
                    let (bracket, stream) = self.parse_delimited_token_stream()?;

                    // FIXME: Proper error

                    return Ok(ast::Expr::MacroCall(Box::new(ast::MacroCall {
                        path,
                        bracket,
                        stream,
                    })));
                }
                TokenKind::OpenCurlyBracket if let StructLitPolicy::Allowed = policy => {
                    self.advance();

                    // FIXME: NumLit fields
                    let fields = self.parse_delimited_sequence(
                        TokenKind::CloseCurlyBracket,
                        TokenKind::Comma,
                        |this| {
                            let ident = this.parse_common_ident()?;
                            this.parse(TokenKind::SingleColon)?;
                            let expr = this.parse_expr(StructLitPolicy::Allowed)?;
                            Ok(ast::StructLitField { ident, expr })
                        },
                    )?;

                    return Ok(ast::Expr::StructLit(Box::new(ast::StructLit { path, fields })));
                }
                _ => {}
            }

            return Ok(ast::Expr::Path(Box::new(path)));
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Expr))
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
        params: Vec<ast::ClosureParam<'src>>,
    ) -> Result<ast::Expr<'src>> {
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;

        let body = match ret_ty {
            Some(_) => ast::Expr::Block(Box::new(self.parse_block_expr()?)),
            None => self.parse_expr(StructLitPolicy::Allowed)?,
        };

        Ok(ast::Expr::Closure(Box::new(ast::ClosureExpr { params, ret_ty, body })))
    }
}

#[derive(Clone, Copy)]
pub(super) enum StructLitPolicy {
    Allowed,
    Forbidden,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Op {
    Add,
    And,
    Assign,
    BitAnd,
    BitOr,
    BitXor,
    Borrow,
    Call,
    Cast,
    Deref,
    Div,
    Eq,
    Field,
    Ge,
    Gt,
    Index,
    Le,
    Lt,
    Mul,
    Ne,
    Neg,
    Not,
    Or,
    RangeExclusive,
    RangeInclusive,
    Rem,
    Sub,
    Try,
}

impl Op {
    fn left_level(self) -> Option<Level> {
        Some(match self {
            Self::Deref | Self::Neg | Self::Not | Self::Borrow => return None,
            Self::Add | Self::Sub => Level::SumLeft,
            Self::And => Level::AndLeft,
            Self::Assign => Level::AssignLeft,
            Self::BitAnd => Level::BitAndLeft,
            Self::BitOr => Level::BitOrLeft,
            Self::BitXor => Level::BitXorLeft,
            Self::Call | Self::Index => Level::Call,
            Self::Cast => Level::Cast,
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
            Self::Assign => Level::AssignRight,
            Self::BitAnd => Level::BitAndRight,
            Self::BitOr => Level::BitOrRight,
            Self::BitXor => Level::BitXorRight,
            Self::Call | Self::Cast | Self::Field | Self::Index | Self::Try => return None,
            Self::Deref | Self::Neg | Self::Not | Self::Borrow => Level::Prefix,
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
    #[expect(dead_code)] // FIXME
    BitShiftLeft,
    #[expect(dead_code)] // FIXME
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
