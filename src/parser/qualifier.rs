use super::{
    Parser, Result, TokenKind,
    ident::{AUTO, SAFE},
};
use crate::ast;

impl<'src> Parser<'_, 'src> {
    pub(crate) fn parse_qualifiers(&mut self) -> Result<Vec<Qualifier<'src>>> {
        std::iter::from_fn(|| self.parse_qualifier()).collect()
    }

    fn parse_qualifier(&mut self) -> Option<Result<Qualifier<'src>>> {
        let qualifier = match self.token.kind {
            TokenKind::Async
                if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket)
                    // HACK: for `async gen {`
                    && self.look_ahead(2, |t| t.kind != TokenKind::OpenCurlyBracket) =>
            {
                Qualifier::Async
            }
            TokenKind::Const if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket) => {
                Qualifier::Const
            }
            TokenKind::Extern => {
                self.advance();
                let span = self.token.span;
                let abi = self.consume(TokenKind::StrLit).then(|| self.source(span));
                return Some(Ok(Qualifier::Extern(abi)));
            }
            TokenKind::Fn => Qualifier::Fn,
            TokenKind::For => {
                self.advance();
                return Some(self.parse_generic_params().map(Qualifier::HigherRankedBinder));
            }
            TokenKind::Gen if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket) => {
                Qualifier::Gen
            }
            TokenKind::Ident => match self.source(self.token.span) {
                AUTO if self.look_ahead(1, |t| t.kind == TokenKind::Trait) => Qualifier::Auto,
                SAFE if self
                    .look_ahead(1, |t| matches!(t.kind, TokenKind::Fn | TokenKind::Extern)) =>
                {
                    Qualifier::Safe
                }
                _ => return None,
            },
            TokenKind::Impl => Qualifier::Impl,
            TokenKind::Mod => Qualifier::Mod,
            TokenKind::Trait => Qualifier::Trait,
            TokenKind::Unsafe if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket) => {
                Qualifier::Unsafe
            }
            _ => return None,
        };
        self.advance();
        Some(Ok(qualifier))
    }
}

pub(crate) enum Qualifier<'src> {
    Async,
    Auto,
    Const,
    Extern(Option<&'src str>),
    Fn,
    Gen,
    HigherRankedBinder(Vec<ast::GenericParam<'src>>),
    Impl,
    Mod,
    Safe,
    Trait,
    Unsafe,
}

impl<'src> Qualifier<'src> {
    pub(crate) fn strip_const(qualifiers: &[Self]) -> (ast::Constness, &[Self]) {
        match qualifiers {
            [Self::Const, qualifiers @ ..] => (ast::Constness::Const, qualifiers),
            _ => (ast::Constness::Not, qualifiers),
        }
    }

    pub(crate) fn strip_unsafe(qualifiers: &[Self]) -> (ast::Safety, &[Self]) {
        match qualifiers {
            [Self::Unsafe, qualifiers @ ..] => (ast::Safety::Unsafe, qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    pub(crate) fn strip_extern(qualifiers: &[Self]) -> (ast::Externness<'src>, &[Self]) {
        match qualifiers {
            [Qualifier::Extern(abi), qualifiers @ ..] => {
                (ast::Externness::Extern(*abi), qualifiers)
            }
            _ => (ast::Externness::Not, qualifiers),
        }
    }
}
