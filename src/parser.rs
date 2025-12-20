use crate::{
    Edition, ast,
    lexer::lex_ident_or_keyword,
    span::Span,
    token::{Token, TokenKind},
};
pub use error::{ParseError, RenderCx};
use std::{borrow::Cow, fmt};

mod attr;
mod common;
mod error;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
#[cfg(test)]
mod test;
mod ty;

pub(crate) type Result<T, E = ParseError> = std::result::Result<T, E>;

pub fn parse<'src>(
    tokens: &[Token],
    source: &'src str,
    edition: Edition,
) -> Result<ast::File<'src>> {
    Parser::new(tokens, source, edition).parse_file()
}

#[derive(Clone)]
struct Parser<'a, 'src> {
    tokens: &'a [Token],
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
        Self { tokens, token, index, source, edition }
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

    /// Optionally parse a lifetime.
    fn parse_lifetime(&mut self) -> Result<Option<ast::Lifetime<'src>>> {
        self.parse_ticked_ident(|kind, lifetime, span| match kind {
            TokenKind::CommonIdent | TokenKind::Underscore | TokenKind::Static => {
                Ok(ast::Lifetime(lifetime))
            }
            _ => Err(ParseError::ReservedLifetime(span)),
        })
    }

    /// Optionally parse a label.
    fn parse_label(&mut self) -> Result<Option<ast::Ident<'src>>> {
        self.parse_ticked_ident(|kind, label, span| match kind {
            TokenKind::CommonIdent => Ok(label),
            _ => Err(ParseError::ReservedLabel(span)),
        })
    }

    fn parse_ticked_ident<T>(
        &mut self,
        parse: impl FnOnce(TokenKind, &'src str, Span) -> Result<T>,
    ) -> Result<Option<T>> {
        let TokenKind::TickedIdent = self.token.kind else { return Ok(None) };
        let span = self.token.span;
        let source = self.source(span);
        self.advance();
        // For better diagnostics, we lex here in the parser instead of in the lexer.
        // Otherwise we'd produce messages like "found invalid lifetime, expected XYZ".
        parse(lex_ident_or_keyword(&source[1..], self.edition), source, span).map(Some)
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

    fn parse_delimited_token_stream(&mut self) -> Result<(ast::Bracket, ast::TokenStream)> {
        match self.token.kind {
            TokenKind::OpenRoundBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Round)
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Square)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Curly)
            }
            _ => Err(ParseError::UnexpectedToken(
                self.token,
                one_of![
                    TokenKind::OpenRoundBracket,
                    TokenKind::OpenSquareBracket,
                    TokenKind::OpenCurlyBracket,
                ],
            )),
        }
    }

    fn fin_parse_delimited_token_stream(
        &mut self,
        bracket: ast::Bracket,
    ) -> Result<(ast::Bracket, ast::TokenStream)> {
        let stream = self.parse_token_stream(bracket)?;
        self.parse(match bracket {
            ast::Bracket::Round => TokenKind::CloseRoundBracket,
            ast::Bracket::Square => TokenKind::CloseSquareBracket,
            ast::Bracket::Curly => TokenKind::CloseCurlyBracket,
        })?;
        Ok((bracket, stream))
    }

    fn parse_token_stream(&mut self, exp_close_delim: ast::Bracket) -> Result<ast::TokenStream> {
        let mut tokens = Vec::new();
        let mut stack = Vec::new();
        let mut is_delimited = false;

        #[expect(clippy::enum_glob_use)]
        loop {
            use ast::Bracket::*;
            use ast::Orientation::*;

            let act_delim = {
                match self.token.kind {
                    TokenKind::OpenRoundBracket => Some((Round, Open)),
                    TokenKind::OpenSquareBracket => Some((Square, Open)),
                    TokenKind::OpenCurlyBracket => Some((Curly, Open)),
                    TokenKind::CloseRoundBracket => Some((Round, Close)),
                    TokenKind::CloseSquareBracket => Some((Square, Close)),
                    TokenKind::CloseCurlyBracket => Some((Curly, Close)),
                    TokenKind::EndOfInput => break,
                    _ => None,
                }
            };

            if let Some((act_delim, orient)) = act_delim {
                if stack.is_empty() && (act_delim, orient) == (exp_close_delim, Close) {
                    is_delimited = true;
                    break;
                }

                match orient {
                    Open => stack.push(act_delim),
                    Close => match stack.pop() {
                        Some(open_delim) if act_delim == open_delim => {}
                        _ => return Err(ParseError::UnexpectedClosingDelimiter(self.token)),
                    },
                }
            }

            tokens.push(self.token);
            self.advance();
        }

        if is_delimited && stack.is_empty() {
            Ok(tokens)
        } else {
            Err(ParseError::MissingClosingDelimiters(self.token.span))
        }
    }

    fn parse_mutability(&mut self) -> ast::Mutability {
        match self.consume(TokenKind::Mut) {
            true => ast::Mutability::Mut,
            false => ast::Mutability::Not,
        }
    }

    fn consume(&mut self, category: impl TokenCategory) -> bool {
        category.consume(self)
    }

    fn parse(&mut self, category: impl TokenCategory) -> Result<()> {
        if self.consume(category) {
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(self.token, category.fragment()))
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
            return Err(ParseError::UnexpectedToken(
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
        self.consume_common_ident()
            .ok_or_else(|| ParseError::UnexpectedToken(self.token, ExpectedFragment::CommonIdent))
    }

    // FIXME: Temporary API, replace with consume(CommonIdent)
    fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        let TokenKind::CommonIdent = self.token.kind else { return None };
        let ident = self.source(self.token.span);
        self.advance();
        Some(ident)
    }

    // FIXME: Temporary API, replace with is(WeakKeyword::Xyz)
    fn is_common_ident(&self, source: &str) -> bool {
        matches!(self.token.kind, TokenKind::CommonIdent if self.source(self.token.span) == source)
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
    fn consume(self, parser: &mut Parser<'_, '_>) -> bool;

    fn fragment(self) -> ExpectedFragment;
}

impl TokenCategory for TokenKind {
    fn consume(self, parser: &mut Parser<'_, '_>) -> bool {
        if self == parser.token.kind {
            parser.advance();
            true
        } else {
            false
        }
    }

    fn fragment(self) -> ExpectedFragment {
        self.into()
    }
}

impl TokenCategory for TokenPrefix {
    fn consume(self, parser: &mut Parser<'_, '_>) -> bool {
        let Ok(replacement) = self.strip(parser.token.kind) else { return false };
        match replacement {
            Some(replacement) => parser.modify_in_place(replacement),
            None => parser.advance(),
        }
        true
    }

    fn fragment(self) -> ExpectedFragment {
        // FIXME: List all possibilities.
        self.single().into()
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

/// Weak keywords.
mod weak {
    use super::*;

    pub(super) const AUTO: &str = "auto";
    pub(super) const BIKESHED: &str = "bikeshed";
    pub(super) const BUILTIN: &str = "builtin";
    pub(super) const DEFAULT: &str = "default";
    pub(super) const DYN: &str = "dyn"; // in Rust 2015
    pub(super) const MACRO_RULES: &str = "macro_rules";
    pub(super) const PIN: &str = "pin";
    pub(super) const RAW: &str = "raw";
    pub(super) const SAFE: &str = "safe";
    pub(super) const TYPE_ASCRIBE: &str = "type_ascribe";
    pub(super) const YEET: &str = "yeet";

    pub(super) enum Reuse {}

    impl Reuse {
        pub(super) const SRC: &str = "reuse";

        pub(super) fn applies(parser: &Parser<'_, '_>) -> bool {
            // NOTE: This check isn't precise enough. See upstream issue:
            //       <https://github.com/rust-lang/rust/issues/148238>

            parser.look_ahead(1, |t| {
                matches!(t.kind, PathSegIdent!())
                    || TokenPrefix::LessThan.matches(t.kind)
                        && parser.look_ahead(2, |t| parser.begins_ty(t))
            })
        }
    }

    pub(super) enum Union {}

    impl Union {
        pub(super) const SRC: &str = "union";

        pub(super) fn applies(parser: &Parser<'_, '_>) -> bool {
            parser.look_ahead(1, |t| t.kind == TokenKind::CommonIdent)
        }
    }
}

macro one_of($( $frag:expr ),+ $(,)?) {
    ExpectedFragment::OneOf(Box::new([$( ExpectedFragment::from($frag) ),+]))
}

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
