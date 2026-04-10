use super::{Error, Result, TokenKind, frags};
use crate::ast;

impl<'src> super::Parser<'_, '_, 'src> {
    pub(super) fn parse_delimited_token_stream(
        &mut self,
    ) -> Result<(ast::Bracket, ast::TokenStream)> {
        match self.token.kind {
            TokenKind::OpenRoundBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Round)
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Square)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Curly)
            }
            _ => self.fatal(Error::UnexpectedToken(
                self.token,
                frags![
                    TokenKind::OpenRoundBracket,
                    TokenKind::OpenSquareBracket,
                    TokenKind::OpenCurlyBracket,
                ],
            )),
        }
    }

    pub(super) fn fin_parse_delimited_token_stream(
        &mut self,
        bracket: ast::Bracket,
    ) -> Result<(ast::Bracket, ast::TokenStream)> {
        let stream = self.parse_token_stream(bracket)?;
        self.parse(match bracket {
            ast::Bracket::Round => TokenKind::CloseRoundBracket,
            ast::Bracket::Square => TokenKind::CloseSquareBracket,
            ast::Bracket::Curly => TokenKind::CloseCurlyBracket,
        })?;
        Ok((bracket, stream))
    }

    fn parse_token_stream(&mut self, exp_close_delim: ast::Bracket) -> Result<ast::TokenStream> {
        let mut stream = Vec::new();
        let mut stack = vec![exp_close_delim];

        #[expect(clippy::enum_glob_use)]
        loop {
            use ast::Bracket::*;
            use ast::Orientation::*;

            let act_delim = {
                match self.token.kind {
                    TokenKind::OpenRoundBracket => Some((Round, Open)),
                    TokenKind::OpenSquareBracket => Some((Square, Open)),
                    TokenKind::OpenCurlyBracket => Some((Curly, Open)),
                    TokenKind::CloseRoundBracket => Some((Round, Close)),
                    TokenKind::CloseSquareBracket => Some((Square, Close)),
                    TokenKind::CloseCurlyBracket => Some((Curly, Close)),
                    TokenKind::EndOfInput => break,
                    _ => None,
                }
            };

            if let Some((act_delim, orient)) = act_delim {
                match orient {
                    Open => stack.push(act_delim),
                    Close => match stack.pop() {
                        Some(open_delim) if act_delim == open_delim => {
                            if stack.is_empty() {
                                break;
                            }
                        }
                        _ => return self.fatal(Error::UnexpectedClosingDelimiter(self.token)),
                    },
                }
            }

            stream.push(self.token);
            self.advance();
        }

        if !stack.is_empty() {
            return self.fatal(Error::MissingClosingDelimiters(self.token.span));
        }

        stream.push(ast::Token { kind: TokenKind::EndOfInput, span: self.token.span.start.into() });

        Ok(stream)
    }
}
