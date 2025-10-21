use super::{
    ExpectedFragment, MacroCallPolicy, Parser, Result, TokenKind, error::ParseError,
    keyword::Keyword, pat::OrPolicy,
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
    // FIXME: Try to get rid of param `delimiter`.
    pub(super) fn parse_stmt(&mut self, delimiter: TokenKind) -> Result<ast::Stmt<'src>> {
        // FIXME: Outer attrs on let stmt
        if self.begins_item(MacroCallPolicy::Forbidden) {
            return Ok(ast::Stmt::Item(self.parse_item()?));
        }

        if self.consume(Keyword::Let) {
            let pat = self.parse_pat(OrPolicy::Forbidden)?;
            let ty = self.consume(TokenKind::SingleColon).then(|| self.parse_ty()).transpose()?;
            // FIXME: Proper diagnostic for the !else_may_follow case.
            let body = if self.consume(TokenKind::SingleEquals) {
                let body = self.parse_expr()?;
                let alternate = if let Some(Keyword::Else) = self.as_keyword(self.token)
                    && else_may_follow(&body)
                {
                    self.advance();
                    Some(self.parse_block_expr()?)
                } else {
                    None
                };
                Some((body, alternate))
            } else {
                None
            };
            // FIXME: Should mention `else`, too, where applicable.
            self.parse(TokenKind::Semicolon)?;
            return Ok(ast::Stmt::Let(ast::LetStmt { pat, ty, body }));
        }

        if self.begins_expr() {
            let expr = self.parse_expr()?;
            // FIXME: Should we replace the delimiter check with some sort of `begins_stmt` check?
            let semi = if self.token.kind == delimiter || !expr.needs_semicolon_as_stmt() {
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

fn else_may_follow(expr: &ast::Expr<'_>) -> bool {
    match expr {
        | ast::Expr::Array(_)
        | ast::Expr::Call(..)
        | ast::Expr::Cast(..)
        | ast::Expr::Continue
        | ast::Expr::Field(..)
        | ast::Expr::Grouped(_)
        | ast::Expr::Index(..)
        | ast::Expr::Lit(_)
        | ast::Expr::MethodCall(_)
        | ast::Expr::Path(_)
        | ast::Expr::Repeat(..)
        | ast::Expr::Try(_)
        | ast::Expr::Tup(_)
        | ast::Expr::Wildcard => true,
        | ast::Expr::BinOp(ast::BinOp::And | ast::BinOp::Or, ..)
        | ast::Expr::Block(..)
        | ast::Expr::ForLoop(_)
        | ast::Expr::If(_)
        | ast::Expr::Loop(_)
        | ast::Expr::Match(_)
        | ast::Expr::Struct(_)
        | ast::Expr::While(_) => false,
        ast::Expr::MacroCall(call) => match call.bracket {
            ast::Bracket::Round | ast::Bracket::Square => true,
            ast::Bracket::Curly => false,
        },
        ast::Expr::BinOp(.., expr) | ast::Expr::Borrow(_, expr) | ast::Expr::UnOp(_, expr) => {
            else_may_follow(expr)
        }
        ast::Expr::Closure(expr) => else_may_follow(&expr.body),
        ast::Expr::Let(expr) => else_may_follow(&expr.body),
        ast::Expr::Break(_, expr) | ast::Expr::Range(_, expr, _) | ast::Expr::Return(expr) => {
            expr.as_ref().is_none_or(|expr| else_may_follow(expr))
        }
    }
}
