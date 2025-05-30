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
            TokenKind::Ampersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let expr = self.parse_expr()?;
                return Ok(ast::Expr::Borrow(mut_, Box::new(expr)));
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return self.fin_parse_block_expr();
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_parenthesized_or_tuple(Self::parse_expr, ast::Expr::Tup);
            }
            _ => {}
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Expr))
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
            TokenKind::NumLit
            | TokenKind::StrLit
            | TokenKind::Ampersand
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenCurlyBracket => true,
            _ => false,
        }
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
