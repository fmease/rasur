use super::{ExpectedFragment, ParseError, Parser, Result, TokenKind};
use crate::ast;

impl<'src> Parser<'src> {
    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Pat ::=
    ///     | Path
    ///     | Macro_Call
    ///     | Wildcard_Pat
    ///     | #Num_Lit
    ///     | #Str_Lit
    ///     | Borrow_Pat
    ///     | Paren_Or_Tup_Pat
    /// Wildcard_Pat ::= "_"
    /// Borrow_Pat ::= "&" "mut"? Pat
    /// Paren_Or_Tup_Pat ::= "(" (Pat ("," | >")"))* ")"
    /// ```
    pub(super) fn parse_pat(&mut self) -> Result<ast::Pat<'src>> {
        if self.begins_path() {
            let path = self.parse_path::<ast::GenericArgsPolicy::DisambiguatedOnly>()?;

            if self.consume(TokenKind::Bang) {
                let (bracket, stream) = self.parse_delimited_token_stream()?;
                return Ok(ast::Pat::MacroCall(ast::MacroCall { path, bracket, stream }));
            }

            return Ok(ast::Pat::Path(path));
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => match self.source(token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Pat::Wildcard);
                }
                _ => {}
            },
            TokenKind::NumLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Pat::NumLit(lit));
            }
            TokenKind::StrLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Pat::StrLit(lit));
            }
            TokenKind::Ampersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let pat = self.parse_pat()?;
                return Ok(ast::Pat::Borrow(mut_, Box::new(pat)));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_grouped_or_tuple(
                    Self::parse_pat,
                    ast::Pat::Grouped,
                    ast::Pat::Tup,
                );
            }
            _ => {}
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Pat))
    }
}
