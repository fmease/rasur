use crate::span::{ByteIndex, Span};
use std::{fmt, str::CharIndices};

pub(crate) fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).run()
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TokenKind {
    Comma,
    Semicolon,
    Dot,
    Colon,
    Bang,
    Hyphen,
    Slash,
    Hash,
    Equals,
    OpenRoundBracket,
    CloseRoundBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenAngleBracket,
    CloseAngleBracket,
    ThinArrow,
    WideArrow,
    Ident,
    NumLit,
    StrLit,
    Error,
    EndOfInput,
}

#[derive(Clone, Copy)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind, self.span)
    }
}

struct Lexer<'src> {
    chars: CharIndices<'src>,
    peeked: Option<Option<(usize, char)>>,
    tokens: Vec<Token>,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Self {
        Self { chars: source.char_indices(), peeked: None, tokens: Vec::new() }
    }

    fn run(mut self) -> Vec<Token> {
        while let Some(char) = self.peek() {
            let start = self.index();

            match char {
                _ if char.is_whitespace() => self.advance(),
                '/' => {
                    self.advance();

                    if let Some('/') = self.peek() {
                        self.advance();

                        // FIXME: Should we make `\n` part of the comment span? What does rustc do?
                        while self.peek().is_some_and(|char| char != '\n') {
                            self.advance();
                        }
                    } else {
                        self.add(TokenKind::Slash, start);
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    self.advance();

                    while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::Ident, start);
                }
                '0'..='9' => {
                    self.advance();

                    // FIXME: Float lits and suffixes
                    while let Some('0'..='9' | '_') = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::NumLit, start);
                }
                '"' => {
                    self.advance();
                    let start = self.index();

                    // FIXME: Escape sequences;
                    while self.peek().is_some_and(|char| char != '"') {
                        self.advance();
                    }

                    // FIXME: Smh. taint unterminated str lits (but don't bail out early!)
                    self.add(TokenKind::StrLit, start);

                    if let Some('"') = self.peek() {
                        self.advance();
                    }
                }
                ',' => {
                    self.advance();
                    self.add(TokenKind::Comma, start);
                }
                ';' => {
                    self.advance();
                    self.add(TokenKind::Semicolon, start);
                }
                '.' => {
                    self.advance();
                    self.add(TokenKind::Dot, start);
                }
                ':' => {
                    self.advance();
                    self.add(TokenKind::Colon, start);
                }
                '!' => {
                    self.advance();
                    self.add(TokenKind::Bang, start);
                }
                '-' => {
                    self.advance();
                    if let Some('>') = self.peek() {
                        self.advance();
                        self.add(TokenKind::ThinArrow, start);
                    } else {
                        self.add(TokenKind::Hyphen, start);
                    }
                }
                '=' => {
                    self.advance();
                    if let Some('>') = self.peek() {
                        self.advance();
                        self.add(TokenKind::WideArrow, start);
                    } else {
                        self.add(TokenKind::Equals, start);
                    }
                }
                '#' => {
                    self.advance();
                    self.add(TokenKind::Hash, start);
                }
                '(' => {
                    self.advance();
                    self.add(TokenKind::OpenRoundBracket, start);
                }
                ')' => {
                    self.advance();
                    self.add(TokenKind::CloseRoundBracket, start);
                }
                '[' => {
                    self.advance();
                    self.add(TokenKind::OpenSquareBracket, start);
                }
                ']' => {
                    self.advance();
                    self.add(TokenKind::CloseSquareBracket, start);
                }
                '{' => {
                    self.advance();
                    self.add(TokenKind::OpenCurlyBracket, start);
                }
                '}' => {
                    self.advance();
                    self.add(TokenKind::CloseCurlyBracket, start);
                }
                '<' => {
                    self.advance();
                    self.add(TokenKind::OpenAngleBracket, start);
                }
                '>' => {
                    self.advance();
                    self.add(TokenKind::CloseAngleBracket, start);
                }
                _ => {
                    self.advance();
                    self.add(TokenKind::Error, start)
                }
            };
        }

        self.add(TokenKind::EndOfInput, self.index());

        self.tokens
    }

    fn peek(&mut self) -> Option<char> {
        self.peeked.get_or_insert_with(|| self.chars.next()).map(|(_, char)| char)
    }

    fn advance(&mut self) {
        if self.peeked.take().is_none() {
            self.chars.next();
        }
    }

    fn index(&self) -> ByteIndex {
        let index = match self.peeked {
            Some(Some((index, _))) => index,
            _ => self.chars.offset(),
        };
        ByteIndex::new(index)
    }

    fn add(&mut self, kind: TokenKind, start: ByteIndex) {
        self.tokens.push(Token { kind, span: Span { start, end: self.index() } });
    }
}
