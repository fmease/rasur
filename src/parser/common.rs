use super::{
    ExpectedFragment, Parser, Result, TokenKind,
    pat::OrPolicy,
    weak::{self, Weak as _},
};
use crate::{ast, error::Error};
use std::mem;

impl<'src> Parser<'_, '_, 'src> {
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
            let first = mem::take(&mut first);

            let mut attrs = this.parse_attrs(ast::AttrStyle::Outer)?;

            if let Some(param) = this.parse_self_param(&mut attrs)? {
                if !first {
                    return this.fatal(Error::MisplacedReceiver);
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

            Ok(ast::FnParam { attrs, pat, ty })
        })
    }

    fn parse_self_param(
        &mut self,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<Option<ast::FnParam<'src>>> {
        enum ShorthandKind<'src> {
            Ref(Result<Option<ast::Ident<'src>>>, ast::BorrowKind<!>),
            Bare,
        }

        if let Some((kind, mut_, span)) = self.probe(|this| {
            let (kind, mut_) = if this.consume(TokenKind::SingleAmpersand) {
                let lt = this.parse_lifetime();
                let (kind, mut_) = this.parse_borrow_kind_and_mutability();
                (ShorthandKind::Ref(lt, kind), mut_)
            } else {
                (ShorthandKind::Bare, this.parse_mutability())
            };
            // FIXME: make `parse` ret the Span
            let span = this.token.span;
            this.parse(TokenKind::SelfLower).ok()?;
            Some((kind, mut_, span))
        }) {
            let pat = ast::Pat::Binding(Box::new(ast::BindingPat {
                mut_: match kind {
                    ShorthandKind::Ref(..) => ast::Mutability::Not,
                    ShorthandKind::Bare => mut_,
                },
                by_ref: ast::ByRef::No,
                binder: ast::Ident::new("self", span),
                pat: None,
            }));

            // FIXME: Reintroduce a Ty::SelfTy, so we can losslessly reconstruct shorthands
            let self_ty =
                || ast::Ty::Path(Box::new(ast::ExtPath::ident(ast::Ident::new("Self", span))));

            let ty = match kind {
                ShorthandKind::Ref(lt, kind) => {
                    ast::Ty::Ref(Box::new(ast::RefTy { lt: lt?, kind, mut_, pointee: self_ty() }))
                }
                ShorthandKind::Bare => match self.consume(TokenKind::SingleColon) {
                    // Indeed, C-variadics are not permitted here.
                    true => self.parse_ty()?,
                    false => self_ty(),
                },
            };

            Ok(Some(ast::FnParam { attrs: mem::take(attrs), pat, ty }))
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

        if let TokenKind::False | TokenKind::CommonIdent | TokenKind::True | TokenKind::Underscore =
            self.peek(offset).kind
            && let TokenKind::SingleColon = self.peek(offset + 1).kind
        {
            return true;
        }

        false
    }

    // FIXME: Rewrite this using "probe2"?
    pub(crate) fn pick_generic_param_list_over_ext_path(&self, offset: usize) -> bool {
        if self.peek(offset).kind != TokenKind::SingleLessThan {
            return false;
        }

        let token = self.peek(offset + 1);

        if let TokenKind::SingleGreaterThan | TokenKind::Const | TokenKind::Hash = token.kind {
            return true;
        }

        // FIXME: rustc checks for (general) Ident, not CommonIdent. Investigate if it really matters.
        if let TokenKind::TickedIdent | TokenKind::CommonIdent = token.kind
            && let TokenKind::SingleGreaterThan
            | TokenKind::Comma
            | TokenKind::SingleColon
            | TokenKind::SingleEquals = self.peek(offset + 2).kind
        {
            return true;
        }

        false
    }

    pub(crate) fn opt_parse_negatable_lit(
        &mut self,
    ) -> Result<Option<(ast::Sign, ast::Lit<'src>)>> {
        // NOTE: To be kept in sync with `Self::begins_negatable_lit`.

        let sign =
            if self.consume(TokenKind::SingleHyphen) { ast::Sign::Neg } else { ast::Sign::None };

        let lit = match self.token.kind {
            TokenKind::CharLit => {
                let lit = self.source(self.token.span);
                self.advance();
                Some(ast::Lit::Char(lit))
            }
            TokenKind::False => {
                self.advance();
                Some(ast::Lit::Bool(false))
            }
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                Some(ast::Lit::Num(lit))
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                Some(ast::Lit::Str(lit))
            }
            TokenKind::True => {
                self.advance();
                Some(ast::Lit::Bool(true))
            }
            _ => None,
        };

        if let Some(lit) = lit {
            Ok(Some((sign, lit)))
        } else if let ast::Sign::Neg = sign {
            self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::Literal))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn begins_negatable_lit(&self) -> bool {
        // NOTE: To be kept in sync with `Self::opt_parse_negatable_lit`.

        match self.token.kind {
            | TokenKind::CharLit
            | TokenKind::False
            | TokenKind::NumLit
            | TokenKind::SingleHyphen
            | TokenKind::StrLit
            | TokenKind::True => true,
            _ => false,
        }
    }

    pub(crate) fn parse_borrow_kind_and_mutability<X: ParseBorrowKind>(
        &mut self,
    ) -> (ast::BorrowKind<X>, ast::Mutability) {
        if let TokenKind::CommonIdent = self.token.kind
            && let Some(mut_) = match self.peek(1).kind {
                TokenKind::Mut => Some(ast::Mutability::Mut),
                TokenKind::Const => Some(ast::Mutability::Not),
                _ => None,
            }
            // FEATURE: `pin_ergonomics` <https://github.com/rust-lang/rust/issues/130494>
            && let Some(kind) = match self.source(self.token.span) {
                weak::Pin::STR => Some(ast::BorrowKind::Pin),
                source => X::parse(source),
            }
        {
            self.advance();
            self.advance();
            (kind, mut_)
        } else {
            (ast::BorrowKind::Ref, self.parse_mutability())
        }
    }

    pub(super) fn parse_abi_str(&mut self) -> Option<&'src str> {
        let TokenKind::StrLit = self.token.kind else { return None };
        let abi = self.source(self.token.span);

        // FIXME: This is a bit awkward. Ideally the lexer would somehow encode this
        //        in the token. NB: We want to keep `TokenKind` payload-less if possible.
        //        Maybe add another field to `Token`?
        if !abi.starts_with(['r', '"']) || !abi.ends_with(['"', '#']) {
            // FIXME: Make the diagnostic more specific.
            self.error(Error::InvalidAbiStr(self.token.span));
        }

        self.advance();
        Some(abi)
    }
}

pub(crate) enum FnParamMode {
    Required,
    Optional,
}

pub(crate) trait ParseBorrowKind: Sized {
    fn parse(source: &str) -> Option<ast::BorrowKind<Self>>;
}

impl ParseBorrowKind for ! {
    fn parse(_: &str) -> Option<ast::BorrowKind<Self>> {
        None
    }
}

impl ParseBorrowKind for () {
    fn parse(source: &str) -> Option<ast::BorrowKind<Self>> {
        match source {
            weak::Raw::STR => Some(ast::BorrowKind::Raw(())),
            _ => None,
        }
    }
}
