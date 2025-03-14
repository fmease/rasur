use std::fmt;

use crate::ast;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

pub(crate) fn parse(tokens: Vec<Token>, source: &str) -> Result<ast::File<'_>> {
    Parser::new(tokens, source).parse_file()
}

struct Parser<'src> {
    tokens: Vec<Token>,
    index: usize,
    source: &'src str,
}

impl<'src> Parser<'src> {
    fn new(tokens: Vec<Token>, source: &'src str) -> Self {
        Self { tokens, index: 0, source }
    }

    fn parse_file(&mut self) -> Result<ast::File<'src>> {
        Ok(ast::File { items: self.parse_items(TokenKind::EndOfInput)? })
    }

    fn parse_items(&mut self, terminator: TokenKind) -> Result<Vec<ast::Item<'src>>> {
        let mut items = Vec::new();

        loop {
            if self.consume(terminator) {
                break;
            }

            let mut attrs = Vec::new();

            while self.consume(TokenKind::Hash) {
                self.expect(TokenKind::OpenSquareBracket)?;

                let _name = attrs.push(ast::Attr { name: self.parse_common_ident()? });

                self.expect(TokenKind::CloseSquareBracket)?;
            }

            let token = self.peek();

            let kind = match token.kind {
                TokenKind::Ident => match self.source(token.span) {
                    "fn" => {
                        self.advance();
                        self.finish_parse_fn()?
                    }
                    "mod" => {
                        self.advance();
                        self.finish_parse_mod()?
                    }
                    "struct" => {
                        self.advance();
                        self.finish_parse_struct()?
                    }
                    _ => return Err(Error::UnexpectedToken(token.kind, ExpectedFragment::Item)),
                },
                kind => return Err(Error::UnexpectedToken(kind, ExpectedFragment::Item)),
            };

            items.push(ast::Item { attrs, kind });
        }

        Ok(items)
    }

    fn finish_parse_fn(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let gen_params = self.parse_generic_params()?;
        let params = self.parse_params()?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;
        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            let body = self.parse_expr()?;
            self.expect(TokenKind::CloseCurlyBracket)?;
            Some(body)
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.expect(TokenKind::Semicolon)?;
            None
        };
        Ok(ast::ItemKind::Fn(ast::Fn {
            name,
            generics: ast::Generics { params: gen_params },
            params,
            ret_ty,
            body,
        }))
    }

    fn finish_parse_mod(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let items = if self.consume(TokenKind::OpenCurlyBracket) {
            Some(self.parse_items(TokenKind::CloseCurlyBracket)?)
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.expect(TokenKind::Semicolon)?;
            None
        };
        Ok(ast::ItemKind::Mod(ast::Mod { name, items }))
    }

    fn finish_parse_struct(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let gen_params = self.parse_generic_params()?;
        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            let mut fields = Vec::new();

            loop {
                if self.consume(TokenKind::CloseCurlyBracket) {
                    break;
                }

                let ident = self.parse_common_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_ty()?;

                self.consume(TokenKind::Comma);

                fields.push((ident, ty))
            }
            ast::StructBody::Normal { fields }
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.expect(TokenKind::Semicolon)?;
            ast::StructBody::Unit
        };
        Ok(ast::ItemKind::Struct(ast::Struct {
            name,
            generics: ast::Generics { params: gen_params },
            body,
        }))
    }

    fn parse_common_ident(&mut self) -> Result<&'src str> {
        let token = self.peek();

        if let TokenKind::Ident = token.kind
            && let ident = self.source(token.span)
            && !is_reserved(ident)
        {
            self.advance();
            return Ok(ident);
        }

        Err(Error::UnexpectedToken(token.kind, ExpectedFragment::Token(TokenKind::Ident)))
    }

    fn parse_generic_params(&mut self) -> Result<Vec<ast::GenParam<'src>>> {
        let mut params = Vec::new();

        if self.consume(TokenKind::OpenAngleBracket) {
            loop {
                if self.consume(TokenKind::CloseAngleBracket) {
                    break;
                }

                let ident = self.parse_common_ident()?;

                self.consume(TokenKind::Comma);

                params.push(ast::GenParam { name: ident })
            }
        }

        Ok(params)
    }

    fn parse_params(&mut self) -> Result<Vec<ast::Param<'src>>> {
        let mut params = Vec::new();

        if self.consume(TokenKind::OpenRoundBracket) {
            loop {
                if self.consume(TokenKind::CloseRoundBracket) {
                    break;
                }

                let ident = self.parse_common_ident()?;
                let ty = self.parse_opt_ty_ann()?;
                self.consume(TokenKind::Comma);

                params.push(ast::Param { name: ident, ty })
            }
        }

        Ok(params)
    }

    fn parse_ty(&mut self) -> Result<ast::Ty<'src>> {
        let token = self.peek();

        match token.kind {
            TokenKind::Ident
                if let ident = self.source(token.span)
                    && !is_reserved(ident) =>
            {
                self.advance();
                Ok(ast::Ty::Ident(ident))
            }
            kind => return Err(Error::UnexpectedToken(kind, ExpectedFragment::Ty)),
        }
    }

    fn parse_opt_ty_ann(&mut self) -> Result<Option<ast::Ty<'src>>> {
        if self.consume(TokenKind::Colon) { self.parse_ty().map(Some) } else { Ok(None) }
    }

    fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        let token = self.peek();

        match token.kind {
            TokenKind::Ident
                if let ident = self.source(token.span)
                    && !is_reserved(ident) =>
            {
                self.advance();
                Ok(ast::Expr::Ident(ident))
            }
            kind => return Err(Error::UnexpectedToken(kind, ExpectedFragment::Expr)),
        }
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.peek().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        let token = self.peek();
        if token.kind == kind {
            self.advance();
            return Ok(());
        }
        Err(Error::UnexpectedToken(token.kind, ExpectedFragment::Token(kind)))
    }

    fn peek(&self) -> Token {
        self.tokens[self.index]
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }
}

fn is_reserved(ident: &str) -> bool {
    matches!(ident, "fn")
}

#[derive(Debug)]
pub(crate) enum Error {
    #[allow(dead_code)] // used via Debug and thus Display
    UnexpectedToken(TokenKind, ExpectedFragment),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub(crate) enum ExpectedFragment {
    #[allow(dead_code)] // used via Debug and thus Display
    Token(TokenKind),
    Item,
    Ty,
    Expr,
}
