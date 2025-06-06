use super::{ParseError, Parser, Result, TokenKind, one_of};
use crate::{ast, parser::expr};

impl<'src> Parser<'src> {
    /// Parse a sequence of attributes of the given style.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attrs⟨style⟩ ::= ("#" Bang⟨style⟩ "[" Attr_Path … "]" )*
    /// Bang⟨Outer⟩ ::= ""
    /// Bang⟨Inner⟩ ::= "!"
    /// ```
    pub(super) fn parse_attrs(&mut self, style: ast::AttrStyle) -> Result<Vec<ast::Attr<'src>>> {
        // NOTE: To be kept in sync with `Self::begins_outer_attr`.
        // FIXME: Parse doc comments.

        let mut attrs = Vec::new();

        while self.token().kind == TokenKind::Hash {
            match style {
                ast::AttrStyle::Outer => self.advance(),
                // We don't expect(Bang) here because the caller may want to
                // parse outer attributes next.
                ast::AttrStyle::Inner => {
                    if self.look_ahead(1, |token| token.kind == TokenKind::Bang) {
                        self.advance();
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            attrs.push(self.fin_parse_attr(style)?);
        }

        Ok(attrs)
    }

    pub(super) fn begins_outer_attr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_attr`.

        self.token().kind == TokenKind::Hash
    }

    fn fin_parse_attr(&mut self, style: ast::AttrStyle) -> Result<ast::Attr<'src>> {
        self.parse(TokenKind::OpenSquareBracket)?;
        let path = self.parse_path::<ast::GenericArgsPolicy::Forbidden>()?;
        let token = self.token();
        let kind = match token.kind {
            TokenKind::CloseSquareBracket => ast::AttrKind::Unit,
            // FIXME: Admits `==`.
            TokenKind::Equals => {
                self.advance();
                let expr = self.parse_expr(expr::StructLitPolicy::Allowed)?;
                ast::AttrKind::Assign(expr)
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Round)?;
                ast::AttrKind::Call(bracket, stream)
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Square)?;
                ast::AttrKind::Call(bracket, stream)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Curly)?;
                ast::AttrKind::Call(bracket, stream)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    one_of![
                        TokenKind::CloseSquareBracket,
                        TokenKind::Equals,
                        TokenKind::OpenRoundBracket,
                        TokenKind::OpenSquareBracket,
                        TokenKind::OpenCurlyBracket,
                    ],
                ));
            }
        };

        self.parse(TokenKind::CloseSquareBracket)?;

        Ok(ast::Attr { style, path, kind })
    }
}
