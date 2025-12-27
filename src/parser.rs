use crate::{
    Edition, ast,
    error::{Buffer as ErrorBuffer, Error},
    lexer::lex_ident,
    span::Span,
    token::{Token, TokenKind},
};

mod attr;
mod common;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod stream;
#[cfg(test)]
mod test;
mod ty;
mod weak;

type Result<T, E = BufferedError> = std::result::Result<T, E>;

struct BufferedError(());

pub fn parse<'src>(
    tokens: &[Token],
    source: &'src str,
    edition: Edition,
    errors: &mut ErrorBuffer,
) -> Result<ast::File<'src>, ()> {
    Parser::new(tokens, source, edition, errors).parse_file().map_err(drop)
}

struct Parser<'t, 'e, 'src> {
    tokens: &'t [Token],
    errors: &'e mut ErrorBuffer,
    token: Token,
    index: usize,
    source: &'src str,
    edition: Edition,
}

// FIXME: Move some parsing methods into mod common.

impl<'t, 'e, 'src> Parser<'t, 'e, 'src> {
    fn new(
        tokens: &'t [Token],
        source: &'src str,
        edition: Edition,
        errors: &'e mut ErrorBuffer,
    ) -> Self {
        let index = 0;
        let token = tokens[index];
        Self { tokens, errors, token, index, source, edition }
    }

    /// Parse a source file.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// File ::= Attrs⟨Inner⟩ Items⟨#End_Of_Input⟩
    /// ```
    fn parse_file(&mut self) -> Result<ast::File<'src>> {
        let start = self.token.span;

        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let items = self.parse_items(item::ItemCx::Boring, TokenKind::EndOfInput)?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::File { attrs, items, span })
    }

    fn parse_ticked_ident<T>(
        &mut self,
        validate: fn(TokenKind) -> bool,
        error: fn(Span) -> Error,
        make: fn(&'src str) -> T,
    ) -> Result<Option<T>> {
        let TokenKind::TickedIdent = self.token.kind else { return Ok(None) };
        let span = self.token.span;
        let source = self.source(span);
        self.advance();
        // For better diagnostics, we lex here in the parser instead of in the lexer.
        // Otherwise we'd produce messages like "found invalid lifetime, expected XYZ".
        // FIXME: Now that we have token validation on `self.advance()`, we can rethink this approach.
        let ident = lex_ident(&source[1..], self.edition);
        if !validate(ident) {
            self.error(error(span));
        }
        Ok(Some(make(source)))
    }

    fn fin_parse_grouped_or_tuple<T, U>(
        &mut self,
        parse: impl Fn(&mut Self) -> Result<T>,
        grouped: impl FnOnce(Box<T>) -> U,
        tuple: impl FnOnce(Vec<T>) -> U,
    ) -> Result<U> {
        let mut nodes = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        const SEPARATOR: TokenKind = TokenKind::Comma;
        while !self.consume(DELIMITER) {
            let node = parse(self)?;

            // FIXME: Is there a better way to express this?
            if self.token.kind == DELIMITER {
                if nodes.is_empty() {
                    // This is actually a grouped node, not a tuple.
                    self.advance();
                    return Ok(grouped(Box::new(node)));
                }
            } else {
                self.parse(SEPARATOR)?;
            }

            nodes.push(node);
        }

        Ok(tuple(nodes))
    }

    fn fin_parse_delim_seq<T>(
        &mut self,
        delimiter: TokenKind,
        separator: TokenKind,
        mut parse: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut nodes = Vec::new();

        while !self.consume(delimiter) {
            // FIXME: Add delimiter and separator to "the list of expected tokens".
            nodes.push(parse(self)?);

            if self.token.kind != delimiter {
                self.parse(separator)?;
            }
        }

        Ok(nodes)
    }

    fn fin_parse_delim_seq_with<T>(
        &mut self,
        consume_delimiter: impl Fn(&mut Self) -> bool,
        check_delimiter: impl Fn(&Self) -> bool,
        separator: TokenKind,
        mut parse: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut nodes = Vec::new();

        while !consume_delimiter(self) {
            // FIXME: Add delimiter and separator to "the list of expected tokens".
            nodes.push(parse(self)?);

            if !check_delimiter(self) {
                self.parse(separator)?;
            }
        }

        Ok(nodes)
    }

    fn parse_mutability(&mut self) -> ast::Mutability {
        match self.consume(TokenKind::Mut) {
            true => ast::Mutability::Mut,
            false => ast::Mutability::Not,
        }
    }

    fn error(&mut self, error: Error) {
        self.errors.add(error);
    }

    fn fatal<T>(&mut self, error: Error) -> Result<T> {
        self.error(error);
        Err(BufferedError(()))
    }

    fn consume(&mut self, category: impl TokenCategory) -> bool {
        category.consume(self)
    }

    fn parse(&mut self, category: impl TokenCategory) -> Result<()> {
        if self.consume(category) {
            return Ok(());
        }

        self.fatal(Error::UnexpectedToken(self.token, category.fragment()))
    }

    // FIXME: better name
    fn consume_or_parse(&mut self, kind: TokenKind, condition: bool) -> Result<bool> {
        match condition {
            true => Ok(self.consume(kind)),
            false => self.parse(kind).map(|_| true),
        }
    }

    // FIXME: likely no longer correct due to modify_in_place
    fn prev_token(&self) -> Option<Token> {
        Some(self.tokens[self.index.checked_sub(1)?])
    }

    // FIXME: Temporary API and bad name.
    fn modify_in_place(&mut self, token: TokenKind) {
        self.token.kind = token;
        self.token.span.start += 1;
    }

    fn look_ahead<T: Default>(&self, amount: usize, inspect: impl FnOnce(Token) -> T) -> T {
        if let Some(index) = self.index.checked_add(amount)
            && let Some(&token) = self.tokens.get(index)
        {
            inspect(token)
        } else {
            T::default()
        }
    }

    fn advance(&mut self) {
        self.index += 1;
        if let Some(&token) = self.tokens.get(self.index) {
            self.token = token;
        }
    }

    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }

    fn snapshot<'r>(&self, errors: &'r mut ErrorBuffer) -> Parser<'t, 'r, 'src> {
        Parser { errors, ..*self }
    }

    // FIXME: Improve impl
    fn probe<T>(
        &mut self,
        parse: impl FnOnce(&mut Parser<'_, '_, 'src>) -> Option<T>,
    ) -> Option<T> {
        let mut errors = ErrorBuffer::Hold(Vec::new());
        let mut this = self.snapshot(&mut errors);
        parse(&mut this).inspect(|_| {
            self.tokens = this.tokens;
            self.token = this.token;
            self.index = this.index;
            self.source = this.source;
            self.edition = this.edition;
            self.errors.extend(errors);
        })
    }

    // FIXME: Temporary API
    fn parse_common_ident_or(&mut self, exception: TokenKind) -> Result<(ast::Ident<'src>, bool)> {
        let exception = if self.token.kind == TokenKind::CommonIdent {
            false
        } else if self.token.kind == exception {
            true
        } else {
            return self.fatal(Error::UnexpectedToken(
                self.token,
                one_of![ExpectedFragment::CommonIdent, exception],
            ));
        };

        let ident = self.source(self.token.span);
        self.advance();
        Ok((ident, exception))
    }

    // FIXME: Temporary API, replace with parse(CommonIdent)
    fn parse_common_ident(&mut self) -> Result<ast::Ident<'src>> {
        match self.consume_common_ident() {
            Some(ident) => Ok(ident),
            None => self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::CommonIdent)),
        }
    }

    // FIXME: Temporary API, replace with consume(CommonIdent)
    fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        let TokenKind::CommonIdent = self.token.kind else { return None };
        let ident = self.source(self.token.span);
        self.advance();
        Some(ident)
    }

    fn check(&self, category: impl TokenCategory) -> bool {
        category.check(self)
    }

    fn matches<T>(&self, category: T, token: Token) -> bool
    where
        T: TokenCategory + MatchAgainstArbitraryToken,
    {
        category.matches(token, self)
    }
}

trait TokenCategory: Copy {
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

    fn fragment(self) -> ExpectedFragment;
}

#[diagnostic::on_unimplemented(
    message = "token category `{Self}` cannot be matched against arbitrary tokens",
    label = "cannot be matched against arbitrary tokens"
)]
trait MatchAgainstArbitraryToken: TokenCategory {}

impl TokenCategory for TokenKind {
    fn check(self, p: &Parser<'_, '_, '_>) -> bool {
        self == p.token.kind
    }

    fn matches(self, token: Token, _: &Parser<'_, '_, '_>) -> bool {
        self == token.kind
    }

    fn fragment(self) -> ExpectedFragment {
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
            Some(replacement) => p.modify_in_place(replacement),
            None => p.advance(),
        }
        true
    }

    fn fragment(self) -> ExpectedFragment {
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

    fn fragment(self) -> ExpectedFragment {
        // FIXME: Ideally, we'd just disable this method
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
enum TokenPrefix {
    LessThan,
    GreaterThan,
    Plus,
}

impl TokenPrefix {
    fn single(self) -> TokenKind {
        match self {
            Self::LessThan => TokenKind::SingleLessThan,
            Self::GreaterThan => TokenKind::SingleGreaterThan,
            Self::Plus => TokenKind::SinglePlus,
        }
    }

    fn strip(self, token: TokenKind) -> Result<Option<TokenKind>, ()> {
        Ok(Some(match (self, token) {
            (Self::LessThan, TokenKind::SingleLessThan) => return Ok(None),
            (Self::LessThan, TokenKind::DoubleLessThan) => TokenKind::SingleLessThan,
            (Self::LessThan, TokenKind::LessThanEquals) => TokenKind::SingleEquals,
            (Self::LessThan, TokenKind::DoubleLessThanEquals) => TokenKind::LessThanEquals,
            (Self::GreaterThan, TokenKind::SingleGreaterThan) => return Ok(None),
            (Self::GreaterThan, TokenKind::DoubleGreaterThan) => TokenKind::SingleGreaterThan,
            (Self::GreaterThan, TokenKind::GreaterThanEquals) => TokenKind::SingleEquals,
            (Self::GreaterThan, TokenKind::DoubleGreaterThanEquals) => TokenKind::GreaterThanEquals,
            (Self::Plus, TokenKind::SinglePlus) => return Ok(None),
            (Self::Plus, TokenKind::PlusEquals) => TokenKind::SingleEquals,
            _ => return Err(()),
        }))
    }

    fn matches(self, token: TokenKind) -> bool {
        self.strip(token).is_ok()
    }
}

macro one_of($( $frag:expr ),+ $(,)?) {
    ExpectedFragment::OneOf(Box::new([$( ExpectedFragment::from($frag) ),+]))
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub enum ExpectedFragment {
    Bound,
    CommonIdent,
    ConstArg,
    Expr,
    ExtPath,
    GenericArg,
    GenericParam,
    Item,
    Literal,
    OneOf(Box<[Self]>),
    Pat,
    PathSegIdent,
    Predicate,
    Stmt,
    Term,
    Token(TokenKind),
    Ty,
}

impl From<TokenKind> for ExpectedFragment {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}
