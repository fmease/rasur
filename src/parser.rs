use crate::{
    ast,
    edition::Edition,
    span::Span,
    token::{Token, TokenKind},
};
use keyword::Keyword;
use std::{borrow::Cow, fmt};

mod attr;
mod error;
mod expr;
mod item;
mod keyword;
mod pat;
mod path;
mod stmt;
#[cfg(test)]
mod test;
mod ty;

pub(crate) type Result<T, E = error::ParseError> = std::result::Result<T, E>;

pub(crate) fn parse<'src>(
    tokens: &[Token],
    source: &'src str,
    edition: Edition,
) -> Result<ast::File<'src>> {
    Parser::new(tokens, source, edition).parse_file()
}

struct Parser<'a, 'src> {
    tokens: &'a [Token],
    token: Token,
    index: usize,
    source: &'src str,
    edition: Edition,
}

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
        let items = self.parse_items(TokenKind::EndOfInput)?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::File { attrs, items, span })
    }

    /// Optionally parse a common lifetime.
    fn parse_common_lifetime(&mut self) -> Result<Option<ast::Lifetime<'src>>> {
        let token = self.token;
        if let TokenKind::Lifetime = token.kind {
            self.advance();
            let lifetime = self.source(token.span);
            if lifetime == "'_" || lifetime == "'static" || self.ident_is_common(&lifetime[1..]) {
                Ok(Some(ast::Lifetime(lifetime)))
            } else {
                Err(error::ParseError::ReservedLifetime(token.span))
            }
        } else {
            Ok(None)
        }
    }

    fn fin_parse_grouped_or_tuple<T>(
        &mut self,
        parse: impl Fn(&mut Self) -> Result<T>,
        grouped: impl FnOnce(Box<T>) -> T,
        tuple: impl FnOnce(Vec<T>) -> T,
    ) -> Result<T> {
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
            _ => Err(error::ParseError::UnexpectedToken(
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
                        _ => return Err(error::ParseError::UnexpectedClosingDelimiter(self.token)),
                    },
                }
            }

            tokens.push(self.token);
            self.advance();
        }

        if is_delimited && stack.is_empty() {
            Ok(tokens)
        } else {
            Err(error::ParseError::MissingClosingDelimiters(self.token.span))
        }
    }

    fn parse_mutability(&mut self) -> ast::Mutability {
        match self.consume(Keyword::Mut) {
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

        Err(error::ParseError::UnexpectedToken(self.token, category.fragment()))
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
        let mut this = Self { ..*self };
        parse(&mut this).inspect(|_| *self = this)
    }
}

impl !Clone for Parser<'_, '_> {}
impl !Copy for Parser<'_, '_> {}

#[derive(Clone, Copy)]
enum MacroCallPolicy {
    #[expect(dead_code)] // FIXME
    Allowed,
    Forbidden,
}

fn is_path_seg_keyword(ident: &str) -> bool {
    matches!(ident, "_" | "self" | "Self" | "super" | "crate")
}

impl Token {
    fn to_diag_str(self, source: Option<&str>) -> Cow<'static, str> {
        // FIXME: Say "`{source}` (U+NNNN)" on TokenKind::Error | invalid tokens.
        match (self.kind, source) {
            (TokenKind::Ident, Some(source)) => {
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

impl TokenCategory for Keyword {
    fn consume(self, parser: &mut Parser<'_, '_>) -> bool {
        if parser.as_keyword(parser.token) == Some(self) {
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

macro one_of($( $frag:expr ),+ $(,)?) {
    ExpectedFragment::OneOf(Box::new([$( ExpectedFragment::from($frag) ),+]))
}

#[cfg_attr(test, derive(Debug))]
pub(crate) enum ExpectedFragment {
    Bound,
    CommonIdent,
    Expr,
    GenericArg,
    GenericParam,
    Keyword(Keyword),
    Item,
    OneOf(Box<[Self]>),
    Pat,
    PathSegIdent,
    Predicate,
    Raw(&'static str),
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

impl From<Keyword> for ExpectedFragment {
    fn from(keyword: Keyword) -> Self {
        Self::Keyword(keyword)
    }
}

impl fmt::Display for ExpectedFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bound => "bound",
            Self::CommonIdent => "identifier",
            Self::Expr => "expression",
            Self::GenericArg => "generic argument",
            Self::GenericParam => "generic parameter",
            Self::Keyword(keyword) => return write!(f, "keyword `{}`", keyword.to_str()),
            Self::Item => "item",
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
            Self::Raw(frag) => return write!(f, "`{frag}`"),
            Self::Stmt => "statement",
            Self::Term => "type or const argument",
            Self::Token(token) => return write!(f, "{}", token.to_diag_str()),
            Self::Ty => "type",
        })
    }
}
