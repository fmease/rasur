use super::{Error, Parser, Result, TokenKind, one_of};
use crate::ast;

impl<'src> Parser<'_, 'src> {
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
                one_of![
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
        let mut tokens = Vec::new();
        let mut stack = Vec::new();
        let mut is_delimited = false;

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
                if stack.is_empty() && (act_delim, orient) == (exp_close_delim, Close) {
                    is_delimited = true;
                    break;
                }

                match orient {
                    Open => stack.push(act_delim),
                    Close => match stack.pop() {
                        Some(open_delim) if act_delim == open_delim => {}
                        _ => return self.fatal(Error::UnexpectedClosingDelimiter(self.token)),
                    },
                }
            }

            tokens.push(self.token);
            self.advance();
        }

        if is_delimited && stack.is_empty() {
            Ok(tokens)
        } else {
            self.fatal(Error::MissingClosingDelimiters(self.token.span))
        }
    }
}
