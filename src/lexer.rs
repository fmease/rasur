use iter::PeekableCharIndices;

use crate::span::{ByteIndex, Span};
use std::fmt;

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
    chars: iter::PeekableCharIndices<'src>,
    tokens: Vec<Token>,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Self {
        Self { chars: PeekableCharIndices::new(source), tokens: Vec::new() }
    }

    fn run(mut self) -> Vec<Token> {
        while let Some(char) = self.peek() {
            let start = self.index();

            match char {
                _ if char.is_whitespace() => self.advance(),
                '/' => {
                    self.advance();

                    match self.peek() {
                        Some('/') => {
                            self.advance();
                            while self.peek().is_some_and(|char| char != '\n') {
                                self.advance();
                            }
                        }
                        // FIXME: Support nested multi-line comments!
                        // FIXME: Smh. taint unterminated m-l comments (but don't fatal!)
                        Some('*') => {
                            self.advance();

                            // FIXME: Use next()? instead of "uncond_peek+advance"?
                            while let Some(prev) = self.peek() {
                                self.advance();

                                if let ('*', Some('/')) = (prev, self.peek()) {
                                    self.advance();
                                    break;
                                }
                            }
                        }
                        _ => {
                            self.add(TokenKind::Slash, start);
                        }
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

                    // FIXME: Smh. taint unterminated str lits (but don't fatal!)
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

    fn add(&mut self, kind: TokenKind, start: ByteIndex) {
        self.tokens.push(Token { kind, span: Span { start, end: self.index() } });
    }
}

impl<'src> std::ops::Deref for Lexer<'src> {
    type Target = PeekableCharIndices<'src>;

    fn deref(&self) -> &Self::Target {
        &self.chars
    }
}

impl<'src> std::ops::DerefMut for Lexer<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chars
    }
}

mod iter {
    use crate::span::ByteIndex;
    use std::str::CharIndices;

    // FIXME: Add explainer as to how this differs from Peekable<CharIndices<'src>>.
    pub(super) struct PeekableCharIndices<'src> {
        chars: CharIndices<'src>,
        peeked: Option<Option<(usize, char)>>,
    }

    impl<'src> PeekableCharIndices<'src> {
        pub(super) fn new(source: &'src str) -> Self {
            Self { chars: source.char_indices(), peeked: None }
        }

        pub(super) fn peek(&mut self) -> Option<char> {
            self.peeked.get_or_insert_with(|| self.chars.next()).map(|(_, char)| char)
        }

        pub(super) fn advance(&mut self) {
            if self.peeked.take().is_none() {
                self.chars.next();
            }
        }

        pub(super) fn index(&self) -> ByteIndex {
            let index = match self.peeked {
                Some(Some((index, _))) => index,
                _ => self.chars.offset(),
            };
            ByteIndex::new(index)
        }
    }
}
