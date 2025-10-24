use super::{
    ExpectedFragment, MacroCallPolicy, Parser, Result, TokenKind, error::ParseError, pat::OrPolicy,
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

        if self.consume(TokenKind::Let) {
            let pat = self.parse_pat(OrPolicy::Forbidden)?;
            let ty = self.consume(TokenKind::SingleColon).then(|| self.parse_ty()).transpose()?;
            // FIXME: Proper diagnostic for the !else_may_follow case.
            let body = if self.consume(TokenKind::SingleEquals) {
                let body = self.parse_expr()?;
                let alternate = if let TokenKind::Else = self.token.kind
                    && else_may_follow(&body.kind)
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
            let semi = if self.token.kind == delimiter || !expr.kind.needs_semicolon_as_stmt() {
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

fn else_may_follow(expr: &ast::ExprKind<'_>) -> bool {
    match expr {
        | ast::ExprKind::Array(_)
        | ast::ExprKind::Call(..)
        | ast::ExprKind::Cast(..)
        | ast::ExprKind::Continue
        | ast::ExprKind::Field(..)
        | ast::ExprKind::Grouped(_)
        | ast::ExprKind::Index(..)
        | ast::ExprKind::Lit(_)
        | ast::ExprKind::MethodCall(_)
        | ast::ExprKind::Path(_)
        | ast::ExprKind::Repeat(..)
        | ast::ExprKind::Try(_)
        | ast::ExprKind::Tuple(_)
        | ast::ExprKind::Wildcard => true,
        | ast::ExprKind::BinOp(ast::BinOp::And | ast::BinOp::Or, ..)
        | ast::ExprKind::Block(..)
        | ast::ExprKind::ForLoop(_)
        | ast::ExprKind::If(_)
        | ast::ExprKind::Loop(_)
        | ast::ExprKind::Match(_)
        | ast::ExprKind::Struct(_)
        | ast::ExprKind::While(_) => false,
        | ast::ExprKind::MacroCall(call) => match call.bracket {
            ast::Bracket::Round | ast::Bracket::Square => true,
            ast::Bracket::Curly => false,
        },
        | ast::ExprKind::BinOp(.., expr)
        | ast::ExprKind::Borrow(_, expr)
        | ast::ExprKind::UnOp(_, expr) => else_may_follow(&expr.kind),
        | ast::ExprKind::Closure(expr) => else_may_follow(&expr.body.kind),
        | ast::ExprKind::Let(expr) => else_may_follow(&expr.body.kind),
        | ast::ExprKind::Break(_, expr)
        | ast::ExprKind::Range(_, expr, _)
        | ast::ExprKind::Return(expr) => {
            expr.as_ref().is_none_or(|expr| else_may_follow(&expr.kind))
        }
    }
}
