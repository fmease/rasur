use super::{Parser, Result, TokenKind, error::ParseError, pat::OrPolicy};
use crate::ast;

impl<'src> Parser<'_, 'src> {
    /// Parse a list of function parameters.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    pub(crate) fn parse_fn_param_list(
        &mut self,
        mode: FnParamMode,
    ) -> Result<Vec<ast::FnParam<'src>>> {
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

    // FIXME: Rewrite this using "probe2"?
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

    // FIXME: Rewrite this using "probe2"?
    pub(crate) fn pick_generic_param_list_over_ext_path(&self, offset: usize) -> bool {
        self.look_ahead(offset, |t| t.kind == TokenKind::SingleLessThan)
            && self.look_ahead(offset + 1, |t| {
                matches!(t.kind, TokenKind::SingleGreaterThan | TokenKind::Const | TokenKind::Hash)
                    // FIXME: In rustc, it's general idents, not just common idents.
                    //        Investigate if/where it truly matters.
                    || matches!(t.kind, TokenKind::Lifetime | TokenKind::Ident)
                && self.look_ahead(offset + 2, |t| matches!(t.kind, TokenKind::SingleGreaterThan | TokenKind::Comma| TokenKind::SingleColon | TokenKind::SingleEquals))
            })
    }
}

pub(crate) enum FnParamMode {
    Required,
    Optional,
}
