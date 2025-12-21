use crate::{
    Edition, ast,
    lexer::lex_ident,
    span::Span,
    token::{Token, TokenKind},
};
pub use error::{Error, RenderCx};
use std::{borrow::Cow, fmt};

mod attr;
mod common;
mod error;
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
) -> (Result<ast::File<'src>, ()>, Vec<Error>) {
    let mut p = Parser::new(tokens, source, edition);
    let r = match p.parse_file() {
        Ok(file) if p.errors.is_empty() => Ok(file),
        _ => Err(()),
    };
    (r, p.errors)
}

#[derive(Clone)]
struct Parser<'a, 'src> {
    tokens: &'a [Token],
    errors: Vec<Error>,
    token: Token,
    index: usize,
    source: &'src str,
    edition: Edition,
}

// FIXME: Move some parsing methods into mod common.

impl<'a, 'src> Parser<'a, 'src> {
    fn new(tokens: &'a [Token], source: &'src str, edition: Edition) -> Self {
        let index = 0;
        let token = tokens[index];
        Self { tokens, errors: Vec::new(), token, index, source, edition }
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
        parse: impl FnOnce(&mut Self, TokenKind, &'src str, Span) -> Result<T>,
    ) -> Result<Option<T>> {
        let TokenKind::TickedIdent = self.token.kind else { return Ok(None) };
        let span = self.token.span;
        let source = self.source(span);
        self.advance();
        // For better diagnostics, we lex here in the parser instead of in the lexer.
        // Otherwise we'd produce messages like "found invalid lifetime, expected XYZ".
        // FIXME: Now that we have token validation on `self.advance()`, we can rethink this approach.
        let ident = lex_ident(&source[1..], self.edition);
        parse(self, ident, source, span).map(Some)
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

    fn error<T>(&mut self, error: Error) -> Result<T> {
        self.errors.push(error);
        Err(BufferedError(()))
    }

    fn consume(&mut self, category: impl TokenCategory) -> bool {
        category.consume(self)
    }

    fn parse(&mut self, category: impl TokenCategory) -> Result<()> {
        if self.consume(category) {
            return Ok(());
        }

        self.error(Error::UnexpectedToken(self.token, category.fragment()))
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
        self.advance_unchecked();
        // FIXME: I'm not so sure if that's really how we want to deal with essentially lexer errors.
        self.validate_token();
    }

    fn advance_unchecked(&mut self) {
        self.index += 1;
        if let Some(&token) = self.tokens.get(self.index) {
            self.token = token;
        }
    }

    fn validate_token(&mut self) {
        _ = match self.token.kind {
            TokenKind::ReservedPrefix => {
                let span = self.token.span;
                self.advance_unchecked();
                if let TokenKind::Hash = self.token.kind {
                    self.advance_unchecked();
                }
                self.error::<!>(Error::ReservedPrefix(span))
            }
            _ => return,
        };
    }

    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }

    fn probe<T>(&mut self, parse: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let mut this = self.clone();
        parse(&mut this).inspect(|_| *self = this)
    }

    // FIXME: Temporary API
    fn parse_common_ident_or(&mut self, exception: TokenKind) -> Result<(ast::Ident<'src>, bool)> {
        let exception = if self.token.kind == TokenKind::CommonIdent {
            false
        } else if self.token.kind == exception {
            true
        } else {
            return self.error(Error::UnexpectedToken(
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
            None => self.error(Error::UnexpectedToken(self.token, ExpectedFragment::CommonIdent)),
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

impl !Copy for Parser<'_, '_> {}

impl Token {
    fn to_diag_str(self, source: Option<&str>) -> Cow<'static, str> {
        // FIXME: Say "`{source}` (U+NNNN)" on TokenKind::Error | invalid tokens.
        match (self.kind, source) {
            (TokenKind::CommonIdent, Some(source)) => {
                let ident = &source[self.span.range()];
                format!("identifier `{ident}`").into()
            }
            _ => self.kind.to_diag_str(),
        }
    }
}

impl TokenKind {
    fn to_diag_str(self) -> Cow<'static, str> {
        match self.repr() {
            crate::token::Repr::Src(src) => format!("`{src}`").into(),
            crate::token::Repr::Tag(tag) => tag.into(),
        }
    }
}

trait TokenCategory: Copy {
    fn check(self, p: &Parser<'_, '_>) -> bool;

    fn matches(self, token: Token, p: &Parser<'_, '_>) -> bool
    where
        Self: MatchAgainstArbitraryToken;

    fn consume(self, p: &mut Parser<'_, '_>) -> bool {
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
    fn check(self, p: &Parser<'_, '_>) -> bool {
        self == p.token.kind
    }

    fn matches(self, token: Token, _: &Parser<'_, '_>) -> bool {
        self == token.kind
    }

    fn fragment(self) -> ExpectedFragment {
        self.into()
    }
}

impl MatchAgainstArbitraryToken for TokenKind {}

impl TokenCategory for TokenPrefix {
    fn check(self, p: &Parser<'_, '_>) -> bool {
        self.matches(p.token.kind)
    }

    fn matches(self, token: Token, _: &Parser<'_, '_>) -> bool {
        self.matches(token.kind)
    }

    fn consume(self, p: &mut Parser<'_, '_>) -> bool {
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
    fn check(self, parser: &Parser<'_, '_>) -> bool {
        self.check(parser)
    }

    fn matches(self, token: Token, p: &Parser<'_, '_>) -> bool
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

macro PathSegIdent() {
    TokenKind::SelfLower
        | TokenKind::Super
        | TokenKind::Crate
        | TokenKind::SelfUpper
        | TokenKind::CommonIdent
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

impl fmt::Display for ExpectedFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bound => "bound",
            Self::CommonIdent => "common identifier",
            Self::ConstArg => "const argument",
            Self::Expr => "expression",
            Self::ExtPath => "extended path",
            Self::GenericArg => "generic argument",
            Self::GenericParam => "generic parameter",
            Self::Item => "item",
            Self::Literal => "literal",
            Self::OneOf(frags) => {
                let frags = frags
                    .iter()
                    .map(|frag| Cow::Owned(frag.to_string()))
                    .intersperse(Cow::Borrowed(" or "))
                    .collect::<String>();
                return write!(f, "{frags}");
            }
            Self::Pat => "pattern",
            Self::PathSegIdent => "path segment",
            Self::Predicate => "predicate",
            Self::Stmt => "statement",
            Self::Term => "type or const argument",
            Self::Token(token) => return write!(f, "{}", token.to_diag_str()),
            Self::Ty => "type",
        })
    }
}
