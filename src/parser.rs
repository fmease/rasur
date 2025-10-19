use crate::{
    ast,
    edition::Edition,
    span::Span,
    token::{Token, TokenKind},
};
use std::{borrow::Cow, fmt};

mod attr;
mod error;
mod expr;
mod item;
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

    fn consume_common_lifetime(&mut self) -> Result<Option<ast::Lifetime<'src>>> {
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
        match self.consume_ident("mut") {
            true => ast::Mutability::Mut,
            false => ast::Mutability::Not,
        }
    }

    // FIXME: generalize
    fn consume_single_less_than(&mut self) -> bool {
        match self.token.kind {
            TokenKind::SingleLessThan => {
                self.advance();
                true
            }
            TokenKind::DoubleLessThan => {
                self.modify_in_place(TokenKind::SingleLessThan);
                true
            }
            TokenKind::LessThanEquals => {
                self.modify_in_place(TokenKind::SingleEquals);
                true
            }
            TokenKind::DoubleLessThanEquals => {
                self.modify_in_place(TokenKind::LessThanEquals);
                true
            }
            _ => false,
        }
    }

    // FIXME: generalize
    fn consume_single_greater_than(&mut self) -> bool {
        match self.token.kind {
            TokenKind::SingleGreaterThan => {
                self.advance();
                true
            }
            TokenKind::DoubleGreaterThan => {
                self.modify_in_place(TokenKind::SingleGreaterThan);
                true
            }
            TokenKind::GreaterThanEquals => {
                self.modify_in_place(TokenKind::SingleEquals);
                true
            }
            TokenKind::DoubleGreaterThanEquals => {
                self.modify_in_place(TokenKind::GreaterThanEquals);
                true
            }
            _ => false,
        }
    }

    fn begins_single_greater_than(&self) -> bool {
        matches!(
            self.token.kind,
            TokenKind::SingleGreaterThan
                | TokenKind::DoubleGreaterThan
                | TokenKind::GreaterThanEquals
                | TokenKind::DoubleGreaterThanEquals
        )
    }

    // FIXME: generalize
    fn consume_single_plus(&mut self) -> bool {
        match self.token.kind {
            TokenKind::SinglePlus => {
                self.advance();
                true
            }
            TokenKind::PlusEquals => {
                self.modify_in_place(TokenKind::SingleEquals);
                true
            }
            _ => false,
        }
    }

    fn consume(&mut self, expected: TokenKind) -> bool {
        if self.token.kind == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn parse(&mut self, expected: TokenKind) -> Result<()> {
        if self.token.kind == expected {
            self.advance();
            return Ok(());
        }

        Err(error::ParseError::UnexpectedToken(self.token, expected.into()))
    }

    // FIXME: likely no longer correct
    fn prev_token(&self) -> Option<Token> {
        Some(self.tokens[self.index.checked_sub(1)?])
    }

    // FIXME: Temporary and bad name
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

// FIXME: Check master if this is still up to date
fn is_reserved(ident: &str, edition: Edition) -> bool {
    #[rustfmt::skip]
    fn is_used_keyword(ident: &str) -> bool {
        matches!(
            ident,
            | "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern" | "false" | "fn"
            | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" | "mut"
            | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct" | "super" | "trait" | "true"
            | "type" | "unsafe" | "use" | "where" | "while"
        )
    }

    #[rustfmt::skip]
    fn is_unused_keyword(ident: &str) -> bool {
        matches!(
            ident,
            | "abstract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv" | "typeof" | "unsized"
            | "virtual" | "yield"
        )
    }

    fn is_used_keyword_if(ident: &str, edition: Edition) -> bool {
        edition >= Edition::Rust2018 && matches!(ident, "async" | "await" | "dyn")
    }

    fn is_unused_keyword_if(ident: &str, edition: Edition) -> bool {
        edition >= Edition::Rust2018 && matches!(ident, "try")
            || edition >= Edition::Rust2024 && matches!(ident, "gen")
    }

    ident == "_"
        || is_used_keyword(ident)
        || is_unused_keyword(ident)
        || is_used_keyword_if(ident, edition)
        || is_unused_keyword_if(ident, edition)
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

macro one_of($( $frag:expr ),+ $(,)?) {
    ExpectedFragment::OneOf(Box::new([$( ExpectedFragment::from($frag) ),+]))
}

#[cfg_attr(test, derive(Debug))]
pub(crate) enum ExpectedFragment {
    Raw(&'static str),
    Token(TokenKind),
    Bound,
    CommonIdent,
    Expr,
    GenericArg,
    GenericParam,
    Item,
    OneOf(Box<[Self]>),
    Pat,
    PathSegIdent,
    Predicate,
    Stmt,
    Ty,
    Term,
}

impl From<TokenKind> for ExpectedFragment {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}

impl fmt::Display for ExpectedFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Raw(frag) => return write!(f, "`{frag}`"),
            Self::Token(token) => return write!(f, "{}", token.to_diag_str()),
            Self::Bound => "bound",
            Self::Predicate => "predicate",
            Self::CommonIdent => "identifier",
            Self::Expr => "expression",
            Self::GenericParam => "generic parameter",
            Self::GenericArg => "generic argument",
            Self::Item => "item",
            Self::OneOf(frags) => {
                let frags = frags
                    .iter()
                    .map(|frag| Cow::Owned(frag.to_string()))
                    .intersperse(Cow::Borrowed(" or "))
                    .collect::<String>();
                return write!(f, "{frags}");
            }
            Self::PathSegIdent => "path segment",
            Self::Stmt => "statement",
            Self::Ty => "type",
            Self::Pat => "pattern",
            Self::Term => "type or const argument",
        })
    }
}
