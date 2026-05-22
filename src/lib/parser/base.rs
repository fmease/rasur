use super::{Fragment, Result, frags, weak};
use crate::{
    ast,
    edition::Edition,
    error::Error,
    feature::Feature,
    span::{At as _, ByteIndex, Span},
    store::Store,
    token::{Token, TokenKind},
};

pub struct Parser<'tok, 'sto, 'src> {
    tokens: &'tok [Token],
    pub store: &'sto Store,
    // FIXME: pub mut(self)
    pub token: Token,
    index: usize,
    // FIXME: pub mut(self)
    pub source: &'src str,
    // FIXME: pub mut(self)
    pub edition: Edition,
}

impl<'tok, 'sto, 'src> Parser<'tok, 'sto, 'src> {
    pub fn new(
        tokens: &'tok [Token],
        source: &'src str,
        edition: Edition,
        store: &'sto Store,
    ) -> Self {
        let index = 0;
        let token = tokens[index];
        Self { tokens, store, token, index, source, edition }
    }

    pub fn source(&self, span: Span) -> &'src str {
        self.source.at(span)
    }

    pub(super) fn ident(&self, span: Span) -> ast::Ident<'src> {
        let source = self.source(span);
        let name = source.strip_prefix("r#").unwrap_or(source);
        ast::Ident::new(name, span)
    }

    pub fn advance(&mut self) {
        self.index += 1;
        if let Some(&token) = self.tokens.get(self.index) {
            self.token = token;
        }
    }

    // FIXME: Overload the ret ty to allow for `-> Option<Span>`
    #[must_use]
    pub fn consume(&mut self, category: impl TokenCategory) -> bool {
        category.consume(self)
    }

    pub fn parse(&mut self, category: impl TokenCategory) -> Result<()> {
        if self.consume(category) {
            return Ok(());
        }

        self.fatal(Error::UnexpectedToken(self.token, frags![category.fragment()]))
    }

    pub(super) fn parse_unchecked(&mut self, category: impl TokenCategory) {
        let consumed = self.consume(category);
        debug_assert!(consumed);
    }

    // FIXME: better name
    pub(super) fn consume_or_parse(&mut self, kind: TokenKind, condition: bool) -> Result<bool> {
        match condition {
            true => Ok(self.consume(kind)),
            false => self.parse(kind).map(|()| true),
        }
    }

    pub fn check(&self, category: impl TokenCategory) -> bool {
        category.check(self)
    }

    // FIXME: If we provided a matches API that took an offset into the parser's tokens,
    //        any token category should be able to be used here allowing us to retire
    //        trait `MatchAgainstArbitraryToken`.
    pub fn matches<C>(&self, category: C, token: Token) -> bool
    where
        C: TokenCategory + MatchAgainstArbitraryToken,
    {
        category.matches(token, self)
    }

    pub fn peek(&self, amount: usize) -> Token {
        if amount == 0 {
            return self.token;
        }
        if let Some(index) = self.index.checked_add(amount)
            && let Some(&token) = self.tokens.get(index)
        {
            return token;
        }
        *self.tokens.last().unwrap()
    }

    // FIXME: No longer correct due to existence of `modify_in_place`. Remove altogether
    pub(super) fn prev_token(&self) -> Option<Token> {
        Some(self.tokens[self.index.checked_sub(1)?])
    }

    pub fn snapshot<'tmpsto>(&self, store: &'tmpsto Store) -> Parser<'tok, 'tmpsto, 'src> {
        Parser { store, ..*self }
    }

    pub fn probe<T>(
        &mut self,
        parse: impl FnOnce(&mut Parser<'_, '_, 'src>) -> Option<T>,
    ) -> Option<T> {
        let store = Store::default();
        let mut this = self.snapshot(&store);
        parse(&mut this).inspect(|_| {
            let Self { tokens: _, store: _, token: _, index: _, source: _, edition: _ };
            let Store { errors: _, features: _ };

            self.tokens = this.tokens;
            self.token = this.token;
            self.index = this.index;
            self.source = this.source;
            self.edition = this.edition;
            self.store.errors.extend(store.errors);
            self.store.features.extend(store.features);
        })
    }

    pub fn error(&self, error: Error) {
        self.store.errors.add(error);
    }

    pub fn fatal<T>(&self, error: Error) -> Result<T> {
        self.error(error);
        Err(())
    }

    pub(super) fn feature(&self, feature: Feature, span: Span) {
        self.store.features.add((feature, Some(span)));
    }

    // FIXME: Gradually get rid of this.
    pub(super) fn feature_no_span_fixme(&self, feature: Feature) {
        self.store.features.add((feature, None));
    }
}

// FIXME: Awful API for "casual" implementers!
pub trait TokenCategory: Copy {
    fn check(self, p: &Parser<'_, '_, '_>) -> bool;

    fn matches(self, token: Token, p: &Parser<'_, '_, '_>) -> bool
    where
        Self: MatchAgainstArbitraryToken;

    fn consume(self, p: &mut Parser<'_, '_, '_>) -> bool {
        if self.check(p) {
            p.advance();
            return true;
        }
        false
    }

    fn fragment(self) -> Fragment;
}

#[diagnostic::on_unimplemented(
    message = "token category `{Self}` cannot be matched against arbitrary tokens",
    label = "cannot be matched against arbitrary tokens"
)]
pub trait MatchAgainstArbitraryToken: TokenCategory {}

impl TokenCategory for TokenKind {
    fn check(self, p: &Parser<'_, '_, '_>) -> bool {
        self == p.token.kind
    }

    fn matches(self, token: Token, _: &Parser<'_, '_, '_>) -> bool {
        self == token.kind
    }

    fn fragment(self) -> Fragment {
        self.into()
    }
}

impl MatchAgainstArbitraryToken for TokenKind {}

impl TokenCategory for TokenPrefix {
    fn check(self, p: &Parser<'_, '_, '_>) -> bool {
        self.matches(p.token.kind)
    }

    fn matches(self, token: Token, _: &Parser<'_, '_, '_>) -> bool {
        self.matches(token.kind)
    }

    fn consume(self, p: &mut Parser<'_, '_, '_>) -> bool {
        let Ok(replacement) = self.strip(p.token.kind) else { return false };
        match replacement {
            Some(replacement) => {
                p.token.kind = replacement;
                p.token.span.start += Self::LEN;
            }
            None => p.advance(),
        }
        true
    }

    fn fragment(self) -> Fragment {
        // FIXME: Should we list all possible tokens or keep it under wraps?
        self.single().into()
    }
}

impl MatchAgainstArbitraryToken for TokenPrefix {}

impl<W: weak::Weak> TokenCategory for W {
    fn check(self, parser: &Parser<'_, '_, '_>) -> bool {
        self.check(parser)
    }

    fn matches(self, token: Token, p: &Parser<'_, '_, '_>) -> bool
    where
        Self: MatchAgainstArbitraryToken,
    {
        weak::Weak::matches(self, token, p)
    }

    fn fragment(self) -> Fragment {
        // FIXME: Ideally, we'd just disable this method
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
pub(super) enum TokenPrefix {
    GreaterThan,
    LessThan,
    Pipe,
    Plus,
}

impl TokenPrefix {
    const LEN: ByteIndex = ByteIndex::new(1);

    fn single(self) -> TokenKind {
        match self {
            Self::GreaterThan => TokenKind::SingleGreaterThan,
            Self::LessThan => TokenKind::SingleLessThan,
            Self::Pipe => TokenKind::SinglePipe,
            Self::Plus => TokenKind::SinglePlus,
        }
    }

    fn strip(self, token: TokenKind) -> Result<Option<TokenKind>> {
        // See also <https://github.com/rust-lang/rust/issues/152398>.

        #[expect(clippy::match_same_arms)] // leads to more legible code
        Ok(Some(match (self, token) {
            (Self::GreaterThan, TokenKind::DoubleGreaterThan) => TokenKind::SingleGreaterThan,
            (Self::GreaterThan, TokenKind::DoubleGreaterThanEquals) => TokenKind::GreaterThanEquals,
            (Self::GreaterThan, TokenKind::GreaterThanEquals) => TokenKind::SingleEquals,
            (Self::GreaterThan, TokenKind::SingleGreaterThan) => return Ok(None),
            // NB: `LessThanEquals` and `DoubleLessThanEquals` are indeed *not* eligible!
            (Self::LessThan, TokenKind::DoubleLessThan) => TokenKind::SingleLessThan,
            (Self::LessThan, TokenKind::SingleLessThan) => return Ok(None),
            (Self::LessThan, TokenKind::ThinBackArrow) => TokenKind::SingleHyphen,
            (Self::Pipe, TokenKind::DoublePipe) => TokenKind::SinglePipe,
            (Self::Pipe, TokenKind::PipeEquals) => TokenKind::SingleEquals,
            (Self::Pipe, TokenKind::SinglePipe) => return Ok(None),
            (Self::Plus, TokenKind::PlusEquals) => TokenKind::SingleEquals,
            (Self::Plus, TokenKind::SinglePlus) => return Ok(None),
            _ => return Err(()),
        }))
    }

    fn matches(self, token: TokenKind) -> bool {
        self.strip(token).is_ok()
    }
}
