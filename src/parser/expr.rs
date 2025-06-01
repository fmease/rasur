use super::{ExpectedFragment, Ident, ParseError, Parser, Result, TokenKind, one_of};
use crate::ast;

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
    pub(super) fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        self.parse_expr_at(Level::Initial)
    }

    pub(super) fn begins_expr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_expr`.

        if self.begins_path() {
            return true;
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => {
                matches!(self.source(token.span), "_" | "false" | "if" | "match" | "true" | "while")
            }
            | TokenKind::Hyphen
            | TokenKind::Bang
            | TokenKind::Asterisk
            | TokenKind::Ampersand
            | TokenKind::NumLit
            | TokenKind::StrLit
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenCurlyBracket => true,
            _ => false,
        }
    }

    fn parse_expr_at(&mut self, level: Level) -> Result<ast::Expr<'src>> {
        let token = self.token();
        let left = match token.kind {
            TokenKind::Hyphen => ast::UnOp::Neg.into(),
            TokenKind::Bang => ast::UnOp::Not.into(),
            TokenKind::Asterisk => ast::UnOp::Deref.into(),
            TokenKind::Ampersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let expr = self.parse_expr_at(Level::NegNotDerefBorrow)?;
                ast::Expr::Borrow(mut_, Box::new(expr)).into()
            }
            _ => self.parse_lower_expr()?.into(),
        };
        let mut left = match left {
            UnOpOrExpr::UnOp(op) => {
                self.advance();
                let (_, Some(right_level)) = op.level() else { unreachable!() }; // FIXME: unreachable??
                let right = self.parse_expr_at(right_level)?;
                ast::Expr::UnOp(op, Box::new(right))
            }
            UnOpOrExpr::Expr(expr) => expr,
        };

        loop {
            let token = self.token();
            let op = match token.kind {
                TokenKind::Plus => ast::BinOp::Add,
                TokenKind::Hyphen => ast::BinOp::Sub,
                TokenKind::Asterisk => ast::BinOp::Mul,
                TokenKind::Slash => ast::BinOp::Div,
                TokenKind::Percent => ast::BinOp::Rem,
                TokenKind::Caret => ast::BinOp::BitXor,
                TokenKind::Ampersand if self.is_glued_to(token, TokenKind::Ampersand) => {
                    ast::BinOp::And
                }
                TokenKind::Ampersand => ast::BinOp::BitAnd,
                TokenKind::Pipe if self.is_glued_to(token, TokenKind::Pipe) => ast::BinOp::Or,
                TokenKind::Pipe => ast::BinOp::BitOr,
                TokenKind::QuestionMark => {
                    let op = ast::UnOp::Try;
                    let (Some(left_level), _) = op.level() else { unreachable!() }; // FIXME: unreachable??
                    if left_level < level {
                        break;
                    }

                    self.advance();

                    left = ast::Expr::UnOp(op, Box::new(left));

                    continue;
                }
                _ => break,
            };

            let (left_level, right_level) = op.levels();
            if left_level < level {
                break;
            }

            for _ in 0..op.length() {
                self.advance();
            }

            let right = self.parse_expr_at(right_level)?;

            left = ast::Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        enum UnOpOrExpr<'src> {
            UnOp(ast::UnOp),
            Expr(ast::Expr<'src>),
        }

        impl<'src> From<ast::Expr<'src>> for UnOpOrExpr<'src> {
            fn from(expr: ast::Expr<'src>) -> Self {
                Self::Expr(expr)
            }
        }

        impl<'src> From<ast::UnOp> for UnOpOrExpr<'src> {
            fn from(op: ast::UnOp) -> Self {
                Self::UnOp(op)
            }
        }

        Ok(left)
    }

    fn parse_lower_expr(&mut self) -> Result<ast::Expr<'src>> {
        if self.begins_path() {
            let path = self.parse_path::<ast::GenericArgsPolicy::DisambiguatedOnly>()?;

            if self.consume(TokenKind::Bang) {
                let (bracket, stream) = self.parse_delimited_token_stream()?;
                return Ok(ast::Expr::MacroCall(ast::MacroCall { path, bracket, stream }));
            }

            return Ok(ast::Expr::Path(path));
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => match self.source(token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Expr::Wildcard);
                }
                "false" => {
                    self.advance();
                    return Ok(ast::Expr::BoolLit(false));
                }
                "if" => {
                    self.advance();

                    // FIXME: Add Restriction::StructLit
                    // FIXME: Permit let-exprs
                    let condition = self.parse_expr()?;
                    self.parse(TokenKind::OpenCurlyBracket)?;
                    let consequent = self.fin_parse_block_expr()?;

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

                        Some(self.parse_expr()?)
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

                    self.parse(TokenKind::OpenCurlyBracket)?;
                    let body = self.fin_parse_block_expr()?;

                    return Ok(ast::Expr::Loop(Box::new(body)));
                }
                "match" => {
                    self.advance();

                    // FIXME: Add Restriction::StructLit
                    let scrutinee = self.parse_expr()?;
                    let mut arms = Vec::new();

                    self.parse(TokenKind::OpenCurlyBracket)?;

                    const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                    while !self.consume(DELIMITER) {
                        let pat = self.parse_pat()?;
                        self.parse(TokenKind::WideArrow)?;

                        let body = self.parse_expr()?;

                        if body.has_trailing_block(ast::TrailingBlockMode::Match)
                            || self.token().kind == DELIMITER
                        {
                            self.consume(TokenKind::Comma);
                        } else {
                            self.parse(TokenKind::Comma)?;
                        };

                        arms.push(ast::MatchArm { pat, body })
                    }

                    return Ok(ast::Expr::Match { scrutinee: Box::new(scrutinee), arms });
                }
                "true" => {
                    self.advance();
                    return Ok(ast::Expr::BoolLit(true));
                }
                "while" => {
                    self.advance();
                    // FIXME: Add Restriction::StructLit
                    // FIXME: Permit let-exprs
                    let condition = self.parse_expr()?;
                    self.parse(TokenKind::OpenCurlyBracket)?;
                    let body = self.fin_parse_block_expr()?;

                    return Ok(ast::Expr::While {
                        condition: Box::new(condition),
                        body: Box::new(body),
                    });
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
                return self.fin_parse_block_expr();
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_group_or_tuple(
                    Self::parse_expr,
                    ast::Expr::Group,
                    ast::Expr::Tup,
                );
            }
            _ => {}
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Expr))
    }

    /// Finish parsing a block expression assuming the leading `{` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Block_Expr ::= "{" Attrs⟨Inner⟩* Stmt* "}"
    /// ```
    pub(super) fn fin_parse_block_expr(&mut self) -> Result<ast::Expr<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let mut stmts = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
        while !self.consume(DELIMITER) {
            stmts.push(self.parse_stmt(DELIMITER)?);
        }

        Ok(ast::Expr::Block(Box::new(ast::BlockExpr { attrs, stmts })))
    }
}

impl ast::UnOp {
    fn level(self) -> (Option<Level>, Option<Level>) {
        match self {
            Self::Deref | Self::Neg | Self::Not => (None, Some(Level::NegNotDerefBorrow)),
            Self::Try => (Some(Level::Try), None),
        }
    }
}

impl ast::BinOp {
    fn levels(self) -> (Level, Level) {
        match self {
            Self::Or => (Level::OrLeft, Level::OrRight),
            Self::And => (Level::AndLeft, Level::AndRight),
            Self::BitOr => (Level::BitOrLeft, Level::BitOrRight),
            Self::BitXor => (Level::BitXorLeft, Level::BitXorRight),
            Self::BitAnd => (Level::BitAndLeft, Level::BitAndRight),
            Self::Add | Self::Sub => (Level::AddSubLeft, Level::AddSubRight),
            Self::Mul | Self::Div | Self::Rem => (Level::MulDivRemLeft, Level::MulDivRemRight),
        }
    }

    fn length(self) -> usize {
        match self {
            Self::And | Self::Or => 2,
            | Self::Add
            | Self::BitAnd
            | Self::BitOr
            | Self::BitXor
            | Self::Div
            | Self::Mul
            | Self::Rem
            | Self::Sub => 1,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(dead_code)] // FIXME
enum Level {
    Initial,
    OrLeft,
    OrRight,
    AndLeft,
    AndRight,
    CmpLeft,
    CmpRight,
    BitOrLeft,
    BitOrRight,
    BitXorLeft,
    BitXorRight,
    BitAndLeft,
    BitAndRight,
    BitShiftLeft,
    BitShiftRight,
    AddSubLeft,
    AddSubRight,
    MulDivRemLeft,
    MulDivRemRight,
    Cast,
    NegNotDerefBorrow,
    Try,
}
