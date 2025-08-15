use super::{
    ExpectedFragment, MacroCallPolicy, ParseError, Parser, Result, TokenKind, pat::OrPolicy,
};
use crate::ast;

impl<'src> Parser<'_, 'src> {
    /// Parse a statement.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Stmt ::=
    ///     | Item\Macro_Call
    ///     | Let_Stmt
    ///     | Expr ";" // FIXME: Not entirely factual
    ///     | ";"
    /// Let_Stmt ::= "let" Pat (":" Ty) ("=" Expr) ";"
    /// ```
    // NOTE: Contrary to rustc and syn, at the time of writing we represent "macro stmts" as
    //       "macro expr stmts". I think the difference only matters if we were to perform
    //       macro expansion.
    pub(super) fn parse_stmt(&mut self, delimiter: TokenKind) -> Result<ast::Stmt<'src>> {
        // FIXME: Outer attrs on let stmt
        if self.begins_item(MacroCallPolicy::Forbidden) {
            return Ok(ast::Stmt::Item(self.parse_item()?));
        }

        if self.consume_ident_if("let") {
            let pat = self.parse_pat(OrPolicy::Forbidden)?;
            let ty = self.consume(TokenKind::SingleColon).then(|| self.parse_ty()).transpose()?;
            let body =
                self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
            self.parse(TokenKind::Semicolon)?;
            return Ok(ast::Stmt::Let(ast::LetStmt { pat, ty, body }));
        }

        if self.begins_expr() {
            let expr = self.parse_expr()?;
            // FIXME: Should we replace the delimiter check with some sort of `begins_stmt` check?
            let semi = if expr.has_trailing_block(ast::TrailingBlockMode::Normal)
                || self.token.kind == delimiter
            {
                match self.consume(TokenKind::Semicolon) {
                    true => ast::Semicolon::Yes,
                    false => ast::Semicolon::No,
                }
            } else {
                self.parse(TokenKind::Semicolon)?;
                ast::Semicolon::Yes
            };
            return Ok(ast::Stmt::Expr(expr, semi));
        }

        match self.token.kind {
            TokenKind::Semicolon => {
                self.advance();
                Ok(ast::Stmt::Empty)
            }
            _ => Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Stmt)),
        }
    }
}
