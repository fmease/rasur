use super::{
    ExpectedFragment, Parser, Result, TokenKind, one_of,
    pat::OrPolicy,
    weak::{self, Weak as _},
};
use crate::{
    ast,
    error::Error,
    span::{ByteIndex, Span},
};
use std::mem;

impl<'src> Parser<'_, '_, 'src> {
    pub(crate) fn parse_ty_annotation(&mut self) -> Result<ast::Ty<'src>> {
        if self.parse(TokenKind::SingleColon).is_err() {
            return Ok(ast::Ty::Error(self.token.span.start().into()));
        }

        self.parse_ty()
    }

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

            if let start = this.token.span
                && let Some(param) = this.parse_self_param(&mut attrs)?
            {
                if !first {
                    this.error(Error::MisplacedReceiver(start.until(this.token.span)));
                }
                return Ok(param);
            }

            let pat = if (matches!(mode, FnParamMode::Required)
                && this.token.kind != TokenKind::TripleDot)
                || this.is_restricted_param_pat()
            {
                let pat = this.parse_pat(OrPolicy::Yield)?;
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
            Ref(Option<ast::Lifetime<'src>>, ast::BorrowKind<!>),
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
            // FIXME: HACK
            if let TokenKind::SingleColon | TokenKind::Comma | TokenKind::CloseRoundBracket =
                this.token.kind
            {
                Some((kind, mut_, span))
            } else {
                None
            }
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

            let ty = match kind {
                ShorthandKind::Ref(lt, kind) => ast::Ty::Ref(Box::new(ast::RefTy {
                    lt,
                    kind,
                    mut_,
                    pointee: ast::Ty::ImplicitSelf,
                })),
                ShorthandKind::Bare => match self.consume(TokenKind::SingleColon) {
                    // Indeed, C-variadics are not permitted here.
                    true => self.parse_ty()?,
                    false => ast::Ty::ImplicitSelf,
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
    ) -> Result<Option<(ast::Sign, Box<ast::Lit<'src>>)>> {
        // NOTE: To be kept in sync with `Self::begins_negatable_lit`.

        let sign =
            if self.consume(TokenKind::SingleHyphen) { ast::Sign::Neg } else { ast::Sign::None };

        let lit = match self.token.kind {
            TokenKind::CharLit => Some(ast::LitKind::Char),
            TokenKind::False | TokenKind::True => Some(ast::LitKind::Bool),
            TokenKind::NumLit => Some(ast::LitKind::Num),
            TokenKind::StrLit => Some(ast::LitKind::Str),
            _ => None,
        };

        if let Some(kind) = lit {
            Ok(Some((sign, Box::new(self.fin_parse_lit(kind)))))
        } else if let ast::Sign::Neg = sign {
            self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::Lit))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn begins_negatable_lit(&self) -> bool {
        // NOTE: To be kept in sync with `Self::opt_parse_negatable_lit`.

        #[expect(clippy::match_like_matches_macro)] // a match looks better here
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

    pub(crate) fn fin_parse_lit(&mut self, kind: ast::LitKind) -> ast::Lit<'src> {
        let value = self.source(self.token.span);
        self.advance();

        let suffix = if let TokenKind::LitSuffix = self.token.kind {
            let source = self.source(self.token.span);
            self.advance();
            Some(source)
        } else {
            None
        };

        ast::Lit { kind, value, suffix }
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
        if !abi.starts_with(['r', '"']) {
            // FIXME: Make the diagnostic more specific.
            self.error(Error::InvalidAbiStr(self.token.span));
        }
        self.advance();

        // This check isn't really necessary but it drastically improves diagnostics
        // in types since the qualifier system which would otherwise encounter this
        // token generally emits quite bad placeholder diagnostics at the moment.
        if let TokenKind::LitSuffix = self.token.kind {
            self.error(Error::AbiStrSuffix(self.token.span));
            self.advance();
        }

        Some(abi)
    }

    pub(super) fn fin_parse_builtin_syntax<T>(
        &mut self,
        start: Span,
        error: impl FnOnce(Span) -> T,
        parse: impl FnOnce(&mut Self, &'src str) -> Result<Option<T>>,
    ) -> Result<T> {
        self.parse(TokenKind::Hash)?;

        let ident = self.parse_common_ident()?;
        self.parse(TokenKind::OpenRoundBracket)?;

        Ok(match parse(self, ident.name)? {
            Some(value) => value,
            None => {
                self.error(Error::UnknownBuiltinSyntax(ident.span));
                let _stream = self.fin_parse_delimited_token_stream(ast::Bracket::Round)?;
                error(start.until(self.token.span))
            }
        })
    }

    pub(super) fn split_float_lit(&mut self) -> (ast::Ident<'src>, Option<ast::Ident<'src>>) {
        // FIXME: Unfortunately, we have to split float literals. It would be better if the lexer
        //        would emit "richer" token kinds. Well, ideally, we wouldn't lex eagerly but
        //        "on the parser's demand" using a parametrized step function that'd allow us to
        //        communicate the "expectation" or something like that.

        const DOT: char = '.';

        let mut ident = self.ident(self.token.span);
        let mut extra = None;

        let numeric = matches!(self.token.kind, TokenKind::NumLit);

        if numeric && let Some((left, right)) = ident.name.split_once(DOT) {
            let dot = ident.span.start + ByteIndex::new(left.len());

            if right.is_empty() {
                self.token.kind = TokenKind::SingleDot;
                self.token.span.start = dot;
            } else {
                let mut span = ident.span;
                span.start = dot + const { ByteIndex::new(DOT.len_utf8()) };
                extra = Some(ast::Ident::new(right, span));
                self.advance();
            }

            ident.span.end = dot;
            ident.name = left;
        } else {
            self.advance();
        }

        if numeric {
            self.validate_numeric_ident(ident, ExpInNumIdentPolicy::ParseIfUnsigned);
        }
        if let Some(ident) = extra {
            self.validate_numeric_ident(ident, ExpInNumIdentPolicy::ParseIfUnsigned);
        }

        (ident, extra)
    }

    pub(crate) fn validate_numeric_ident(
        &self,
        ident: ast::Ident<'src>,
        exp_policy: ExpInNumIdentPolicy,
    ) {
        let pattern: &[_] = match exp_policy {
            ExpInNumIdentPolicy::ParseIfUnsigned => &['+', '-', '.'],
            ExpInNumIdentPolicy::Reject => &['e', '.'],
        };
        if ident.name.contains(pattern) {
            // We could also split at the offending token and
            // generate a fake "unexpected token" diagnostic.
            self.error(Error::InvalidNumericIdent(ident.span));
        }
    }

    pub(super) fn fin_parse_delimited_field_seq(&mut self) -> Result<Vec<ast::Ident<'src>>> {
        let mut fields = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        const SEPARATOR: TokenKind = TokenKind::SingleDot;
        loop {
            let (TokenKind::CommonIdent | TokenKind::NumLit) = self.token.kind else {
                return self.fatal(Error::UnexpectedToken(
                    self.token,
                    one_of![TokenKind::CommonIdent, TokenKind::NumLit],
                ));
            };

            let (ident, extra) = self.split_float_lit();

            fields.push(ident);
            if let Some(ident) = extra {
                fields.push(ident);
            }

            if self.consume(DELIMITER) {
                break;
            }

            self.parse(SEPARATOR)?;
        }

        Ok(fields)
    }
}

#[derive(Clone, Copy)]
pub(crate) enum FnParamMode {
    Required,
    Optional,
}

pub(crate) enum ExpInNumIdentPolicy {
    ParseIfUnsigned,
    Reject,
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
