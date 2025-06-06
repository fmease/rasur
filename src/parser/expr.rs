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
            | TokenKind::Hyphen
            | TokenKind::Bang
            | TokenKind::Asterisk
            | TokenKind::Ampersand
            | TokenKind::DoubleAmpersand
            | TokenKind::NumLit
            | TokenKind::StrLit
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenCurlyBracket => true,
            _ => false,
        }
    }

    fn parse_expr_at(&mut self, level: Level, policy: StructLitPolicy) -> Result<ast::Expr<'src>> {
        let token = self.token();
        let left = match token.kind {
            TokenKind::Hyphen => ast::UnOp::Neg.into(),
            TokenKind::Bang => ast::UnOp::Not.into(),
            TokenKind::Asterisk => ast::UnOp::Deref.into(),
            // FIXME: Smh. consider DoubleAmpersand, too. However are we allowed to commit
            //        early, namely before "the level check"? E.g., `&&1&&&&1`
            TokenKind::Ampersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let expr = self.parse_expr_at(Level::Prefix, policy)?;
                ast::Expr::Borrow(mut_, Box::new(expr)).into()
            }
            _ => self.parse_lower_expr(policy)?.into(),
        };
        let mut left = match left {
            OpOrExpr::UnOp(op) => {
                self.advance();
                let (_, Some(right_level)) = op.level() else { unreachable!() }; // FIXME: unreachable??
                let right = self.parse_expr_at(right_level, policy)?;
                ast::Expr::UnOp(op, Box::new(right))
            }
            OpOrExpr::Expr(expr) => expr,
        };

        loop {
            let token = self.token();
            let op = match token.kind {
                TokenKind::Ampersand => ast::BinOp::BitAnd.into(),
                TokenKind::Asterisk => ast::BinOp::Mul.into(),
                TokenKind::BangEquals => ast::BinOp::Ne.into(),
                TokenKind::Caret => ast::BinOp::BitXor.into(),
                TokenKind::Dot => PostfixOp::Project.into(),
                TokenKind::DoubleAmpersand => ast::BinOp::And.into(),
                TokenKind::Equals => ast::BinOp::Assign.into(),
                TokenKind::DoubleEquals => ast::BinOp::Eq.into(),
                TokenKind::DoublePipe => ast::BinOp::Or.into(),
                TokenKind::GreaterThan => ast::BinOp::Gt.into(),
                TokenKind::GreaterThanEquals => ast::BinOp::Ge.into(),
                TokenKind::Hyphen => ast::BinOp::Sub.into(),
                TokenKind::LessThan => ast::BinOp::Lt.into(),
                TokenKind::LessThanEquals => ast::BinOp::Le.into(),
                TokenKind::OpenRoundBracket => PostfixOp::Call.into(),
                TokenKind::OpenSquareBracket => PostfixOp::Index.into(),
                TokenKind::Percent => ast::BinOp::Rem.into(),
                TokenKind::Pipe => ast::BinOp::BitOr.into(),
                TokenKind::Plus => ast::BinOp::Add.into(),
                TokenKind::QuestionMark => PostfixOp::Try.into(),
                TokenKind::Slash => ast::BinOp::Div.into(),
                TokenKind::Ident if let "as" = self.source(token.span) => PostfixOp::Cast.into(),
                _ => break,
            };

            match op {
                InfixOrPostfixOp::Infix(op) => {
                    let (left_level, right_level) = op.levels();
                    match left_level.cmp(&level) {
                        Ordering::Less => break,
                        Ordering::Equal => return Err(ParseError::OpCannotBeChained(op)),
                        Ordering::Greater => {}
                    }
                    self.advance();

                    let right = self.parse_expr_at(right_level, policy)?;
                    left = ast::Expr::BinOp(op, Box::new(left), Box::new(right));
                }
                InfixOrPostfixOp::Postfix(op) => {
                    let left_level = op.level();
                    if left_level < level {
                        break;
                    }
                    self.advance();

                    left = self.fin_parse_postfix_op(op, left)?;
                }
            }

            enum InfixOrPostfixOp {
                Infix(ast::BinOp),
                Postfix(PostfixOp),
            }

            impl From<ast::BinOp> for InfixOrPostfixOp {
                fn from(op: ast::BinOp) -> Self {
                    Self::Infix(op)
                }
            }

            impl From<PostfixOp> for InfixOrPostfixOp {
                fn from(op: PostfixOp) -> Self {
                    Self::Postfix(op)
                }
            }
        }

        enum OpOrExpr<'src> {
            UnOp(ast::UnOp),
            Expr(ast::Expr<'src>),
        }

        impl<'src> From<ast::Expr<'src>> for OpOrExpr<'src> {
            fn from(expr: ast::Expr<'src>) -> Self {
                Self::Expr(expr)
            }
        }

        impl<'src> From<ast::UnOp> for OpOrExpr<'src> {
            fn from(op: ast::UnOp) -> Self {
                Self::UnOp(op)
            }
        }

        Ok(left)
    }

    fn fin_parse_postfix_op(
        &mut self,
        op: PostfixOp,
        left: ast::Expr<'src>,
    ) -> Result<ast::Expr<'src>> {
        Ok(match op {
            PostfixOp::Cast => {
                let ty = self.parse_ty()?;
                ast::Expr::Cast(Box::new(left), Box::new(ty))
            }
            PostfixOp::Try => ast::Expr::UnOp(ast::UnOp::Try, Box::new(left)),
            PostfixOp::Call => {
                let args = self.parse_delimited_sequence(
                    TokenKind::CloseRoundBracket,
                    TokenKind::Comma,
                    |this| this.parse_expr(StructLitPolicy::Allowed),
                )?;
                ast::Expr::Call(Box::new(left), args)
            }
            PostfixOp::Index => {
                let index = self.parse_expr(StructLitPolicy::Allowed)?;
                self.parse(TokenKind::CloseSquareBracket)?;
                ast::Expr::Index(Box::new(left), Box::new(index))
            }
            PostfixOp::Project => {
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
                ast::Expr::Field(Box::new(left), ident)
            }
        })
    }

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
                    let label = self.consume_lifetime().map(|ast::Lifetime(label)| label);
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
                        };

                        arms.push(ast::MatchArm { pat, body })
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

        if self.begins_path() {
            let path = self.parse_path::<ast::GenericArgsPolicy::DisambiguatedOnly>()?;

            let token = self.token();
            match token.kind {
                TokenKind::Bang => {
                    self.advance();
                    let (bracket, stream) = self.parse_delimited_token_stream()?;
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
                            this.parse(TokenKind::Colon)?;
                            let expr = this.parse_expr(StructLitPolicy::Allowed)?;
                            Ok(ast::StructLitField { ident, expr })
                        },
                    )?;

                    return Ok(ast::Expr::StructLit(Box::new(ast::StructLit { path, fields })));
                }
                _ => {}
            }

            return Ok(ast::Expr::Path(path));
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
}

#[derive(Clone, Copy)]
pub(super) enum StructLitPolicy {
    Allowed,
    Forbidden,
}

impl ast::UnOp {
    fn level(self) -> (Option<Level>, Option<Level>) {
        match self {
            Self::Deref | Self::Neg | Self::Not => (None, Some(Level::Prefix)),
            // FIXME: unreachable
            Self::Try => (Some(Level::Try), None),
        }
    }
}

impl ast::BinOp {
    fn levels(self) -> (Level, Level) {
        match self {
            Self::Assign => (Level::AssignLeft, Level::AssignRight),
            Self::Or => (Level::OrLeft, Level::OrRight),
            Self::And => (Level::AndLeft, Level::AndRight),
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => {
                (Level::Compare, Level::Compare)
            }
            Self::BitOr => (Level::BitOrLeft, Level::BitOrRight),
            Self::BitXor => (Level::BitXorLeft, Level::BitXorRight),
            Self::BitAnd => (Level::BitAndLeft, Level::BitAndRight),
            Self::Add | Self::Sub => (Level::SumLeft, Level::SumRight),
            Self::Mul | Self::Div | Self::Rem => (Level::ProductLeft, Level::ProductRight),
        }
    }
}

#[derive(Clone, Copy)]
enum PostfixOp {
    Cast,
    Try,
    Call,
    Index,
    Project,
}

impl PostfixOp {
    fn level(self) -> Level {
        match self {
            Self::Cast => Level::Cast,
            Self::Try => Level::Try,
            Self::Call | Self::Index => Level::Call,
            Self::Project => Level::Project,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Level {
    Initial,
    // FIXME: Jump
    AssignRight,
    AssignLeft,
    // FIXME: Range
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
