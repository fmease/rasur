use super::{Parser, Result, TokenKind, path::PathMode};
use crate::ast;

impl<'src> Parser<'_, '_, 'src> {
    /// Parse a sequence of attributes of the given style.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attrs⟨style⟩ ::= ("#" Bang⟨style⟩ "[" Attr_Path … "]" )*
    /// Bang⟨Outer⟩ ::= ""
    /// Bang⟨Inner⟩ ::= "!"
    /// ```
    // FIXME: Get rid of this in favor of `parse_inner_attrs` & `parse_outer_attrs`
    //        which return type-safe Attrs (i.e., M!=Any)
    pub(super) fn parse_attrs(&mut self, style: ast::AttrStyle) -> Result<Vec<ast::Attr<'src>>> {
        // NOTE: To be kept in sync with `Self::begins_outer_attr`.

        let mut attrs = Vec::new();
        self.parse_attrs_into(style, &mut attrs)?;
        Ok(attrs)
    }

    pub(super) fn parse_attrs_into(
        &mut self,
        style: ast::AttrStyle,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<()> {
        // NOTE: To be kept in sync with `Self::begins_outer_attr`.

        loop {
            let kind = match self.token.kind {
                TokenKind::Hash => {
                    match style {
                        ast::AttrStyle::Outer => self.advance(),
                        // We don't *expect* a bang here because the caller may want to
                        // parse outer attributes next.
                        ast::AttrStyle::Inner => {
                            if self.peek(1).kind == TokenKind::SingleBang {
                                self.advance();
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    ast::AttrKind::Normal(self.fin_parse_normal_attr()?)
                }
                TokenKind::OuterDocComment if let ast::AttrStyle::Outer = style => {
                    let span = self.token.span;
                    self.advance();
                    ast::AttrKind::DocComment(span)
                }
                TokenKind::InnerDocComment if let ast::AttrStyle::Inner = style => {
                    let span = self.token.span;
                    self.advance();
                    ast::AttrKind::DocComment(span)
                }
                _ => break,
            };
            attrs.push(ast::Attr { style, kind });
        }

        Ok(())
    }

    pub(super) fn begins_outer_attr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_attr`.

        matches!(self.token.kind, TokenKind::Hash | TokenKind::OuterDocComment)
    }

    fn fin_parse_normal_attr(&mut self) -> Result<ast::NormalAttr<'src>> {
        self.parse(TokenKind::OpenSquareBracket)?;

        let safety = if self.consume(TokenKind::Unsafe) {
            self.parse(TokenKind::OpenRoundBracket)?;
            ast::Safety::Unsafe
        } else {
            ast::Safety::Inherited
        };

        let path = self.parse_path::<ast::NoGenericArgs>(PathMode::Normal)?;

        let args = match self.token.kind {
            TokenKind::SingleEquals => {
                self.advance();
                let expr = self.parse_expr()?;
                ast::AttrArgs::Assign(expr)
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Round)?;
                ast::AttrArgs::Call(bracket, stream)
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Square)?;
                ast::AttrArgs::Call(bracket, stream)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Curly)?;
                ast::AttrArgs::Call(bracket, stream)
            }
            // FIXME: Better expectation for `#[x@]` where `@` is a bad token.
            _ => ast::AttrArgs::Unit,
        };

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe => self.parse(TokenKind::CloseRoundBracket)?,
        }

        self.parse(TokenKind::CloseSquareBracket)?;

        Ok(ast::NormalAttr { safety, path, args })
    }
}
