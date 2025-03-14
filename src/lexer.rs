use crate::span::{ByteIndex, Span};
use std::{fmt, str::CharIndices};

pub(crate) fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).run()
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TokenKind {
    Ident,
    Comma,
    Colon,
    Semicolon,
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
                _ if char.is_ascii_alphabetic() => {
                    let start = self.index();
                    self.advance();

                    while let Some(char) = self.peek()
                        && char.is_ascii_alphanumeric()
                    {
                        self.advance();
                    }

                    self.add(TokenKind::Ident, start);
                }
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
                ',' => {
                    self.advance();
                    self.add(TokenKind::Comma, start);
                }
                ';' => {
                    self.advance();
                    self.add(TokenKind::Semicolon, start);
                }
                ':' => {
                    self.advance();
                    self.add(TokenKind::Colon, start);
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
                '=' => {
                    self.advance();
                    if let Some('>') = self.peek() {
                        self.advance();
                        self.add(TokenKind::WideArrow, start);
                    } else {
                        self.add(TokenKind::Equals, start);
                    }
                }
                '<' => {
                    self.advance();
                    self.add(TokenKind::OpenAngleBracket, start);
                }
                '>' => {
                    self.advance();
                    self.add(TokenKind::CloseAngleBracket, start);
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
                _ => panic!("unexpected token {char}"),
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
