use crate::span::{ByteIndex, Span};
use iter::PeekableCharIndices;
use std::fmt;

pub(crate) fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).run()
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TokenKind {
    Ampersand,
    Asterisk,
    At,
    Bang,
    BangEquals,
    Caret,
    CloseCurlyBracket,
    CloseRoundBracket,
    CloseSquareBracket,
    Colon,
    Comma,
    Dot,
    DoubleAmpersand,
    DoubleColon,
    DoubleDot,
    DoubleEquals,
    DoublePipe,
    EndOfInput,
    Equals,
    Error,
    GreaterThan,
    GreaterThanEquals,
    Hash,
    Hyphen,
    Ident,
    Lifetime,
    LessThan,
    LessThanEquals,
    NumLit,
    OpenCurlyBracket,
    OpenRoundBracket,
    OpenSquareBracket,
    Percent,
    Pipe,
    Plus,
    QuestionMark,
    Semicolon,
    Slash,
    StrLit,
    ThinArrow,
    TripleDot,
    WideArrow,
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

    #[expect(clippy::too_many_lines)]
    fn run(mut self) -> Vec<Token> {
        while let Some((start, char)) = self.next() {
            match char {
                _ if char.is_whitespace() => {}
                '/' => {
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
                    while let Some(IdentMiddle![]) = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::Ident, start);
                }
                '0'..='9' => {
                    // FIXME: Float lits and suffixes
                    while let Some('0'..='9' | '_') = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::NumLit, start);
                }
                '"' => {
                    // FIXME: Escape sequences;
                    while self.peek().is_some_and(|char| char != '"') {
                        self.advance();
                    }

                    if let Some('"') = self.peek() {
                        self.advance();
                    }

                    // FIXME: Smh. taint unterminated str lits (but don't fatal!)
                    self.add(TokenKind::StrLit, start);
                }
                '@' => self.add(TokenKind::At, start),
                ',' => self.add(TokenKind::Comma, start),
                ';' => self.add(TokenKind::Semicolon, start),
                '.' => {
                    if let Some('.') = self.peek() {
                        self.advance();
                        if let Some('.') = self.peek() {
                            self.advance();
                            self.add(TokenKind::TripleDot, start);
                        } else {
                            self.add(TokenKind::DoubleDot, start);
                        }
                    } else {
                        self.add(TokenKind::Dot, start);
                    }
                }
                ':' => {
                    if let Some(':') = self.peek() {
                        self.advance();
                        self.add(TokenKind::DoubleColon, start);
                    } else {
                        self.add(TokenKind::Colon, start);
                    }
                }
                '!' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::BangEquals, start);
                    } else {
                        self.add(TokenKind::Bang, start);
                    }
                }
                '?' => self.add(TokenKind::QuestionMark, start),
                '+' => self.add(TokenKind::Plus, start),
                '*' => self.add(TokenKind::Asterisk, start),
                '-' => {
                    if let Some('>') = self.peek() {
                        self.advance();
                        self.add(TokenKind::ThinArrow, start);
                    } else {
                        self.add(TokenKind::Hyphen, start);
                    }
                }
                '=' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        self.add(TokenKind::WideArrow, start);
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::DoubleEquals, start);
                    }
                    _ => self.add(TokenKind::Equals, start),
                },
                '#' => self.add(TokenKind::Hash, start),
                '&' => {
                    if let Some('&') = self.peek() {
                        self.advance();
                        self.add(TokenKind::DoubleAmpersand, start);
                    } else {
                        self.add(TokenKind::Ampersand, start);
                    }
                }
                '|' => {
                    if let Some('|') = self.peek() {
                        self.advance();
                        self.add(TokenKind::DoublePipe, start);
                    } else {
                        self.add(TokenKind::Pipe, start);
                    }
                }
                '%' => self.add(TokenKind::Percent, start),
                '^' => self.add(TokenKind::Caret, start),
                '(' => self.add(TokenKind::OpenRoundBracket, start),
                ')' => self.add(TokenKind::CloseRoundBracket, start),
                '[' => self.add(TokenKind::OpenSquareBracket, start),
                ']' => self.add(TokenKind::CloseSquareBracket, start),
                '{' => self.add(TokenKind::OpenCurlyBracket, start),
                '}' => self.add(TokenKind::CloseCurlyBracket, start),
                '<' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::LessThanEquals, start);
                    } else {
                        self.add(TokenKind::LessThan, start);
                    }
                }
                '>' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::GreaterThanEquals, start);
                    } else {
                        self.add(TokenKind::GreaterThan, start);
                    }
                }
                // FIXME: Character literals (without breaking lifetimes).
                '\'' => {
                    while let Some(IdentMiddle![]) = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::Lifetime, start);
                }
                _ => self.add(TokenKind::Error, start),
            }
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

impl std::ops::DerefMut for Lexer<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chars
    }
}

macro IdentMiddle() {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_'
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

        pub(super) fn next(&mut self) -> Option<(ByteIndex, char)> {
            self.peeked
                .take()
                .unwrap_or_else(|| self.chars.next())
                .map(|(index, char)| (ByteIndex::new(index), char))
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
