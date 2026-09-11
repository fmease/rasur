use super::{
    Fragment, Result, TokenKind, frags,
    pat::OrPolicy,
    weak::{self, Weak as _},
};
use crate::{
    ast,
    error::ErrorKind,
    feature::Feature,
    lexer::lex_ident,
    parser::{MatchAgainstArbitraryToken, TokenCategory},
    span::{ByteIndex, Span},
    token::Token,
};
use std::mem;

impl<'src> super::Parser<'_, '_, 'src> {
    pub(super) fn parse_mut(&mut self) -> ast::Mut {
        if self.consume(TokenKind::Mut) { ast::Mut::Yes } else { ast::Mut::No }
    }

    // FIXME: Temporary API
    pub(super) fn parse_common_ident_or(
        &mut self,
        exception: TokenKind,
    ) -> Result<(ast::Ident<'src>, bool)> {
        let exception = if self.token.kind == TokenKind::CommonIdent {
            false
        } else if self.token.kind == exception {
            true
        } else {
            self.unexpected(self.token, frags![TokenKind::CommonIdent, exception]);
            return Err(());
        };

        let ident = self.ident(self.token.span);
        self.advance();
        Ok((ident, exception))
    }

    // FIXME: Temporary API, replace with parse(CommonIdent)
    pub fn parse_common_ident(&mut self) -> Result<ast::Ident<'src>> {
        self.consume_common_ident()
            .ok_or_else(|| self.unexpected(self.token, frags![TokenKind::CommonIdent]))
    }

    // FIXME: Temporary API, replace with consume(CommonIdent)
    pub(super) fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        let TokenKind::CommonIdent = self.token.kind else { return None };
        let ident = self.ident(self.token.span);
        self.advance();
        Some(ident)
    }

    pub(super) fn parse_ticked_ident(
        &mut self,
        validate: fn(TokenKind) -> bool,
        error: ErrorKind,
    ) -> Option<ast::Ident<'src>> {
        let Token { kind: TokenKind::TickedIdent, span } = self.token else { return None };
        self.advance();

        let source = &self.source(span)[const { "'".len() }..];
        let source = source.strip_prefix("k#").unwrap_or(source);
        let ident = lex_ident(source, self.edition);
        if !validate(ident) {
            self.error(error, span);
        }

        let name = source.strip_prefix("r#").unwrap_or(source);
        Some(ast::Ident::new(name, span))
    }

    pub fn fin_parse_delim_seq<T, C>(
        &mut self,
        delimiter: C,
        separator: TokenKind,
        mut parse: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>>
    where
        C: TokenCategory + MatchAgainstArbitraryToken,
    {
        let mut nodes = Vec::new();

        while !self.consume(delimiter) {
            nodes.push(parse(self)?);

            if !self.matches(delimiter, self.token) {
                self.parse(separator)?;
            }
        }

        Ok(nodes)
    }

    pub(super) fn parse_ty_annotation(&mut self) -> Result<ast::Ty<'src>> {
        if self.parse(TokenKind::SingleColon).is_err() {
            return Ok(ast::Ty::Error(self.token.span.start().into()));
        }

        self.parse_ty()
    }

    /// Parse a list of function parameters.
    pub(super) fn parse_fn_param_list(
        &mut self,
        mode: FnParamMode,
    ) -> Result<Vec<ast::FnParam<'src>>> {
        self.parse(TokenKind::OpenRoundBracket)?;
        self.fin_parse_fn_param_list(mode)
    }

    /// Finish parsing a list of function parameters assuming the leading `(` has been parsed already.
    pub(super) fn fin_parse_fn_param_list(
        &mut self,
        mode: FnParamMode,
    ) -> Result<Vec<ast::FnParam<'src>>> {
        let mut first = true;
        self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            let first = mem::take(&mut first);

            let mut attrs = this.parse_attrs(ast::AttrStyle::Outer)?;

            if let start = this.token.span
                && let Some(param) = this.parse_self_param(&mut attrs)?
            {
                if !first {
                    this.error(ErrorKind::MisplacedReceiver, start.until(this.token.span));
                }
                return Ok(param);
            }

            let pat = if mode == FnParamMode::Required && this.token.kind != TokenKind::TripleDot
                || this.is_restricted_param_pat()
            {
                let pat = this.parse_pat(OrPolicy::Yield)?;
                this.parse(TokenKind::SingleColon)?;
                pat
            } else {
                ast::Pat::Wildcard(ast::WildcardKind::Empty)
            };

            // FIXME: Better expectation.
            let ty = if let span = this.token.span
                && this.consume(TokenKind::TripleDot)
            {
                ast::Ty::CVariadics(span)
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
                (ShorthandKind::Bare, this.parse_mut())
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
                    ShorthandKind::Ref(..) => ast::Mut::No,
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

        match self.peek(offset).kind {
            TokenKind::False
            | TokenKind::CommonIdent
            | TokenKind::SelfLower
            | TokenKind::True
            | TokenKind::Underscore => self.peek(offset + 1).kind == TokenKind::SingleColon,
            _ => false,
        }
    }

    // FIXME: Rewrite this using "probe2"?
    pub(super) fn pick_generic_param_list_over_ext_path(&self, offset: usize) -> bool {
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

    pub(super) fn opt_parse_negatable_lit(
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
            self.unexpected(self.token, frags![Fragment::Lit]);
            Err(())
        } else {
            Ok(None)
        }
    }

    pub(super) fn begins_negatable_lit(&self) -> bool {
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

    pub(super) fn fin_parse_lit(&mut self, kind: ast::LitKind) -> ast::Lit<'src> {
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

    pub(super) fn parse_borrow_kind_and_mutability<X: ParseBorrowKind>(
        &mut self,
    ) -> (ast::BorrowKind<X>, ast::Mut) {
        if let TokenKind::CommonIdent = self.token.kind
            && let Some(mut_) = match self.peek(1).kind {
                TokenKind::Mut => Some(ast::Mut::Yes),
                TokenKind::Const => Some(ast::Mut::No),
                _ => None,
            }
            && let Some(kind) = match self.source(self.token.span) {
                weak::Pin::STR => {
                    self.feature(Feature::pin_ergonomics, self.token.span);
                    Some(ast::BorrowKind::Pin)
                }
                source => X::parse(source),
            }
        {
            self.advance();
            self.advance();
            (kind, mut_)
        } else {
            (ast::BorrowKind::Ref, self.parse_mut())
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
            self.error(ErrorKind::InvalidAbiStr, self.token.span);
        }
        self.advance();

        // This check isn't really necessary but it drastically improves diagnostics
        // in types since the qualifier system which would otherwise encounter this
        // token generally emits quite bad placeholder diagnostics at the moment.
        if let TokenKind::LitSuffix = self.token.kind {
            self.error(ErrorKind::AbiStrSuffix, self.token.span);
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
        self.feature(Feature::builtin_syntax, start);
        self.parse(TokenKind::Hash)?;

        let ident = self.parse_common_ident()?;
        self.parse(TokenKind::OpenRoundBracket)?;

        Ok(match parse(self, ident.name)? {
            Some(value) => value,
            None => {
                self.error(ErrorKind::UnknownBuiltinSyntax, ident.span);
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

    pub(super) fn validate_numeric_ident(
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
            self.error(ErrorKind::InvalidNumericIdent, ident.span);
        }
    }

    pub(super) fn fin_parse_delimited_field_seq(&mut self) -> Result<Vec<ast::Ident<'src>>> {
        let mut fields = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        const SEPARATOR: TokenKind = TokenKind::SingleDot;
        loop {
            let (TokenKind::CommonIdent | TokenKind::NumLit) = self.token.kind else {
                self.unexpected(self.token, frags![TokenKind::CommonIdent, TokenKind::NumLit]);
                return Err(());
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum FnParamMode {
    Required,
    Optional,
}

pub(super) enum ExpInNumIdentPolicy {
    ParseIfUnsigned,
    Reject,
}

pub(super) trait ParseBorrowKind: Sized {
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
