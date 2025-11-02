use super::{
    Parser, Result, TokenKind,
    error::ParseError,
    ident::{AUTO, SAFE},
    pat::OrPolicy,
};
use crate::ast;

impl<'src> Parser<'_, 'src> {
    /// Parse function parameters.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    pub(crate) fn parse_fn_params(&mut self, mode: FnParamMode) -> Result<Vec<ast::FnParam<'src>>> {
        self.parse(TokenKind::OpenRoundBracket)?;

        let mut first = true;
        self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            let first = std::mem::take(&mut first);

            // FIXME: Attrs.

            if let Some(param) = this.parse_self_param()? {
                if !first {
                    return Err(ParseError::MisplacedReceiver);
                }
                return Ok(param);
            }

            let pat = if (matches!(mode, FnParamMode::Required)
                && this.token.kind != TokenKind::TripleDot)
                || this.is_restricted_param_pat()
            {
                let pat = this.parse_pat(OrPolicy::Forbidden)?;
                this.parse(TokenKind::SingleColon)?;
                pat
            } else {
                ast::Pat::Wildcard(ast::WildcardKind::Empty)
            };

            // FIXME: Better expectation.
            let ty = if this.consume(TokenKind::TripleDot) {
                ast::Ty::CVariadics
            } else {
                this.parse_ty()?
            };

            Ok(ast::FnParam { pat, ty })
        })
    }

    fn parse_self_param(&mut self) -> Result<Option<ast::FnParam<'src>>> {
        if let Some((ref_, mut_)) = self.probe(|this| {
            let ref_ = this.consume(TokenKind::SingleAmpersand).then(|| this.parse_lifetime());
            let mut_ = this.parse_mutability();
            this.parse(TokenKind::SelfLower).ok()?;
            Some((ref_, mut_))
        }) {
            let pat = ast::Pat::Ident(ast::IdentPat {
                mut_: match ref_ {
                    Some(_) => ast::Mutability::Not,
                    None => mut_,
                },
                by_ref: ast::ByRef::No,
                ident: "self",
            });

            let self_ty = || ast::Ty::Path(Box::new(ast::ExtPath::ident("Self")));

            let ty = match ref_ {
                Some(lt) => ast::Ty::Ref(lt?, mut_, Box::new(self_ty())),
                None => match self.consume(TokenKind::SingleColon) {
                    // Indeed, C-variadics are not permitted here.
                    true => self.parse_ty()?,
                    false => self_ty(),
                },
            };

            return Ok(Some(ast::FnParam { pat, ty }));
        } else {
            Ok(None)
        }
    }

    fn is_restricted_param_pat(&self) -> bool {
        let offset = match self.token.kind {
            TokenKind::Mut | TokenKind::SingleAmpersand | TokenKind::DoubleAmpersand => 1,
            _ => 0,
        };
        self.look_ahead(offset, |t| {
            matches!(
                t.kind,
                TokenKind::False | TokenKind::Ident | TokenKind::True | TokenKind::Underscore
            )
        }) && self.look_ahead(offset + 1, |t| t.kind == TokenKind::SingleColon)
    }

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

pub(crate) enum FnParamMode {
    Required,
    Optional,
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
