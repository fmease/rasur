use crate::ast;
use crate::edition::Edition;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;
use std::borrow::Cow;
use std::fmt;
use std::path::Path;

mod attr;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

pub(crate) type Result<T, E = ParseError> = std::result::Result<T, E>;

pub(crate) fn parse(tokens: Vec<Token>, source: &str, edition: Edition) -> Result<ast::File<'_>> {
    Parser { tokens, index: 0, source, edition }.parse_file()
}

struct Parser<'src> {
    tokens: Vec<Token>,
    index: usize,
    source: &'src str,
    edition: Edition,
}

impl<'src> Parser<'src> {
    /// Parse a source file.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// File ::= Attrs⟨Inner⟩ Items⟨#End_Of_Input⟩
    /// ```
    fn parse_file(&mut self) -> Result<ast::File<'src>> {
        let start = self.token().span;

        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let items = self.parse_items(TokenKind::EndOfInput)?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::File { attrs, items, span })
    }

    fn parse_ident_if_common_or(&mut self, exception: &'static str) -> Result<ast::Ident<'src>> {
        let token = self.token();
        self.as_ident(token)
            .filter(|&ident| ident == exception || self.ident_is_common(ident))
            .inspect(|_| self.advance())
            .ok_or_else(|| {
                ParseError::UnexpectedToken(
                    token,
                    one_of![ExpectedFragment::CommonIdent, ExpectedFragment::Raw(exception)],
                )
            })
    }

    fn parse_common_ident(&mut self) -> Result<ast::Ident<'src>> {
        self.consume_common_ident()
            .ok_or_else(|| ParseError::UnexpectedToken(self.token(), ExpectedFragment::CommonIdent))
    }

    fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        self.as_common_ident(self.token()).inspect(|_| self.advance())
    }

    fn as_common_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        self.as_ident(token).filter(|ident| self.ident_is_common(ident))
    }

    fn ident_is_common(&self, ident: &str) -> bool {
        !is_reserved(ident, self.edition)
    }

    fn as_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        matches!(token.kind, TokenKind::Ident).then(|| self.source(token.span))
    }

    fn consume_lifetime(&mut self) -> Result<Option<ast::Lifetime<'src>>> {
        let token = self.token();
        if let TokenKind::Lifetime = token.kind {
            self.advance();
            let lifetime = self.source(token.span);
            if lifetime == "'_" || lifetime == "'static" || self.ident_is_common(&lifetime[1..]) {
                Ok(Some(ast::Lifetime(lifetime)))
            } else {
                Err(ParseError::ReservedLifetime(token.span))
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
            if self.token().kind == DELIMITER {
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

    fn parse_delimited_sequence<T>(
        &mut self,
        delimiter: TokenKind,
        separator: TokenKind,
        mut parse: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut nodes = Vec::new();

        while !self.consume(delimiter) {
            // FIXME: Add delimiter and separator to "the list of expected tokens".
            nodes.push(parse(self)?);

            if self.token().kind != delimiter {
                self.parse(separator)?;
            }
        }

        Ok(nodes)
    }

    fn parse_delimited_token_stream(&mut self) -> Result<(ast::Bracket, ast::TokenStream)> {
        let bracket = self.token();
        match bracket.kind {
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
                bracket,
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
        let stream = self.parse_token_strean(bracket)?;
        self.parse(match bracket {
            ast::Bracket::Round => TokenKind::CloseRoundBracket,
            ast::Bracket::Square => TokenKind::CloseSquareBracket,
            ast::Bracket::Curly => TokenKind::CloseCurlyBracket,
        })?;
        Ok((bracket, stream))
    }

    fn parse_token_strean(&mut self, exp_delim: ast::Bracket) -> Result<ast::TokenStream> {
        let mut tokens = Vec::new();
        let mut stack = Vec::new();
        let mut is_delimited = false;

        loop {
            let token = self.token();

            #[expect(clippy::enum_glob_use)]
            let act_delim = {
                use ast::Bracket::*;
                use ast::Orientation::*;
                match token.kind {
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
                if stack.is_empty()
                    && act_delim == exp_delim
                    && let ast::Orientation::Close = orient
                {
                    is_delimited = true;
                    break;
                }

                match orient {
                    ast::Orientation::Open => stack.push(act_delim),
                    ast::Orientation::Close => {
                        let close_delim = act_delim;
                        match stack.pop() {
                            Some(open_delim) if open_delim == close_delim => {}
                            // FIXME: Better error.
                            _ => return Err(ParseError::InvalidDelimiter),
                        }
                    }
                }
            }

            tokens.push(token);
            self.advance();
        }

        if is_delimited && stack.is_empty() {
            Ok(tokens)
        } else {
            // FIXME: Better error.
            Err(ParseError::InvalidDelimiter)
        }
    }

    fn parse_visibility(&mut self) -> Result<ast::Visibility<'src>> {
        // To kept in sync with `Self::begins_visibility`.

        if !self.consume(Ident("pub")) {
            return Ok(ast::Visibility::Inherited);
        }

        // FIXME: Only do this lookahead dance for tuple struct fields. This way, we can
        // can give better errors on invalid vis restrictions in the common cases.
        if self.token().kind == TokenKind::OpenRoundBracket
            && let Some(ident) = self.look_ahead(1, |token| self.as_ident(token))
        {
            let path = match ident {
                "in" => {
                    self.advance();
                    self.advance();
                    Some(self.parse_path()?)
                }
                "crate" | "super" | "self" => {
                    self.advance();
                    self.advance();
                    Some(ast::Path::ident(ident))
                }
                _ => None,
            };
            if let Some(path) = path {
                self.parse(TokenKind::CloseRoundBracket)?;
                return Ok(ast::Visibility::Restricted(path));
            }
        }

        Ok(ast::Visibility::Public)
    }

    fn begins_visibility(&self) -> bool {
        // To kept in sync with `Self::parse_visibility`.

        Ident("pub").check(self)
    }

    fn parse_mutability(&mut self) -> ast::Mutability {
        match self.consume(Ident("mut")) {
            true => ast::Mutability::Mut,
            false => ast::Mutability::Not,
        }
    }

    fn consume<S: Shape>(&mut self, shape: S) -> bool {
        if shape.check(self) {
            S::advance(self);
            true
        } else {
            false
        }
    }

    fn parse<S: Shape>(&mut self, shape: S) -> Result<()> {
        if shape.check(self) {
            S::advance(self);
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(self.token(), shape.fragment()))
    }

    fn prev_token(&self) -> Option<Token> {
        Some(self.tokens[self.index.checked_sub(1)?])
    }

    fn token(&self) -> Token {
        self.tokens[self.index]
    }

    fn look_ahead<T: Default>(&self, amount: usize, pred: impl FnOnce(Token) -> T) -> T {
        if let Some(index) = self.index.checked_add(amount)
            && let Some(&token) = self.tokens.get(index)
        {
            pred(token)
        } else {
            T::default()
        }
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }
}

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

pub(crate) enum ParseError {
    UnexpectedToken(Token, ExpectedFragment),
    // FIXME: Temporary
    InvalidDelimiter,
    InvalidAssocItemKind(Span),
    InvalidExternItemKind(Span),
    ExpectedTraitFoundTy,
    ModifierOnOutlivesBound,
    MisplacedReceiver,
    OpCannotBeChained(ast::BinOp),
    TyRelMacroCall,
    ReservedLifetime(Span),
}

impl ParseError {
    pub(crate) fn print(&self, source: &str, path: &Path) {
        use annotate_snippets as ann;
        let lvl = ann::Level::Error;
        let msg = match self {
            Self::UnexpectedToken(token, expected) => {
                let found = token.to_diag_str(Some(source));
                super let title = format!("found {found} but expected {expected}");
                super let path = path.to_string_lossy();

                lvl.title(&title).snippet(
                    ann::Snippet::source(source)
                        .origin(&path)
                        .annotation(lvl.span(token.span.range()).label("unexpected token"))
                        .fold(true),
                )
            }
            Self::InvalidAssocItemKind(span) => {
                super let path = path.to_string_lossy();

                lvl.title("invalid associated item kind").snippet(
                    ann::Snippet::source(source)
                        .origin(&path)
                        .annotation(lvl.span(span.range()))
                        .fold(true),
                )
            }
            Self::InvalidDelimiter => lvl.title("invalid delimiter"),
            Self::InvalidExternItemKind(span) => {
                super let path = path.to_string_lossy();

                lvl.title("invalid extern item kind").snippet(
                    ann::Snippet::source(source)
                        .origin(&path)
                        .annotation(lvl.span(span.range()))
                        .fold(true),
                )
            }
            Self::ExpectedTraitFoundTy => lvl.title("found type expected trait"),
            Self::ModifierOnOutlivesBound => lvl.title("only trait bounds may have modifiers"),
            Self::MisplacedReceiver => lvl.title("misplaced receiver"),
            Self::OpCannotBeChained(op) => {
                super let title = format!("operator `{}` cannot be chained", op.symbol());
                lvl.title(&title)
            }
            Self::TyRelMacroCall => lvl.title("type-relative macro call"),
            Self::ReservedLifetime(span) => {
                super let path = path.to_string_lossy();

                lvl.title("reserved lifetime").snippet(
                    ann::Snippet::source(source)
                        .origin(&path)
                        .annotation(lvl.span(span.range()))
                        .fold(true),
                )
            }
        };
        let renderer = ann::Renderer::styled();
        eprintln!("{}", renderer.render(msg));
    }
}

impl Token {
    fn to_diag_str(self, source: Option<&str>) -> Cow<'static, str> {
        // FIXME: Say "`{source}` (U+NNNN)" on TokenKind::Error | invalid tokens.
        match (self.kind, source) {
            (TokenKind::Ident, Some(source)) => {
                let ident = &source[self.span.range()];
                Cow::Owned(format!("identifier `{ident}`"))
            }
            _ => Cow::Borrowed(self.kind.to_diag_str()),
        }
    }
}

impl TokenKind {
    fn to_diag_str(self) -> &'static str {
        match self {
            Self::Ampersand => "`&`",
            Self::Asterisk => "`*`",
            Self::At => "`@`",
            Self::Bang => "`!`",
            Self::BangEquals => "`!=`",
            Self::Caret => "`^`",
            Self::CloseCurlyBracket => "`}`",
            Self::CloseRoundBracket => "`)`",
            Self::CloseSquareBracket => "`]`",
            Self::Colon => "`:`",
            Self::Comma => "`,`",
            Self::Dot => "`.`",
            Self::DoubleAmpersand => "`&&`",
            Self::DoubleColon => "`::`",
            Self::DoubleDot => "`..`",
            Self::DoubleEquals => "`==`",
            Self::DoublePipe => "`||`",
            Self::EndOfInput => "end of input",
            Self::Equals => "`=`",
            Self::Error => "error",
            Self::GreaterThan => "`>`",
            Self::GreaterThanEquals => "`>=`",
            Self::Hash => "`#`",
            Self::Hyphen => "-",
            Self::Ident => "identifier",
            Self::Lifetime => "lifetime",
            Self::LessThan => "`<`",
            Self::LessThanEquals => "`<=`",
            Self::NumLit => "number literal",
            Self::OpenCurlyBracket => "`{`",
            Self::OpenRoundBracket => "`(`",
            Self::OpenSquareBracket => "`[`",
            Self::Percent => "`%`",
            Self::Pipe => "`|`",
            Self::Plus => "`+`",
            Self::QuestionMark => "`?`",
            Self::Semicolon => "`;`",
            Self::Slash => "`/`",
            Self::StrLit => "string literal",
            Self::ThinArrow => "`->`",
            Self::TripleDot => "`...`",
            Self::WideArrow => "`=>`",
        }
    }
}

macro one_of($( $frag:expr ),+ $(,)?) {
    ExpectedFragment::OneOf(Box::new([$( ExpectedFragment::from($frag) ),+]))
}

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
            Self::Token(token) => token.to_diag_str(),
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

trait Shape: Copy {
    const LENGTH: usize;

    fn check(self, parser: &Parser<'_>) -> bool;

    fn fragment(self) -> ExpectedFragment;

    fn advance(parser: &mut Parser<'_>) {
        for _ in 0..Self::LENGTH {
            parser.advance();
        }
    }
}

impl Shape for TokenKind {
    const LENGTH: usize = 1;

    fn check(self, parser: &Parser<'_>) -> bool {
        // FIXME: This permits `==` if `=` is requested. This is not okay
        parser.token().kind == self
    }

    fn fragment(self) -> ExpectedFragment {
        self.into()
    }
}

#[derive(Clone, Copy)]
struct Ident(&'static str);

impl Shape for Ident {
    const LENGTH: usize = 1;

    fn check(self, parser: &Parser<'_>) -> bool {
        let actual = parser.token();
        actual.kind == TokenKind::Ident && parser.source(actual.span) == self.0
    }

    fn fragment(self) -> ExpectedFragment {
        ExpectedFragment::Raw(self.0)
    }
}
