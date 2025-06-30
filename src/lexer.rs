use crate::{
    span::{ByteIndex, Span},
    token::{Token, TokenKind},
};
use iter::PeekableCharIndices;

pub(crate) fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).run()
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
                        Some('=') => {
                            self.advance();
                            self.add(TokenKind::SlashEquals, start);
                        }
                        _ => {
                            self.add(TokenKind::SingleSlash, start);
                        }
                    }
                }
                'b' if self.peek() == Some('\'') => {
                    self.advance();
                    self.fin_lex_char_lit(start);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    while let Some(IdentMiddle![]) = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::Ident, start);
                }
                '0'..='9' => {
                    // FIXME: Float literals
                    while let Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') = self.peek() {
                        self.advance();
                    }

                    self.add(TokenKind::NumLit, start);
                }
                '"' => {
                    // FIXME: Escape sequences
                    while self.next().is_some_and(|(_, char)| char != '"') {}

                    // FIXME: Suffixes

                    // FIXME: We currently don't mark unterminated str lits
                    //        and the parser doesn't report them.
                    self.add(TokenKind::StrLit, start);
                }
                '@' => self.add(TokenKind::At, start),
                ',' => self.add(TokenKind::Comma, start),
                ';' => self.add(TokenKind::Semicolon, start),
                '.' => {
                    if let Some('.') = self.peek() {
                        self.advance();
                        match self.peek() {
                            Some('.') => {
                                self.advance();
                                self.add(TokenKind::TripleDot, start);
                            }
                            Some('=') => {
                                self.advance();
                                self.add(TokenKind::DoubleDotEquals, start);
                            }
                            _ => {
                                self.add(TokenKind::DoubleDot, start);
                            }
                        }
                    } else {
                        self.add(TokenKind::SingleDot, start);
                    }
                }
                ':' => {
                    if let Some(':') = self.peek() {
                        self.advance();
                        self.add(TokenKind::DoubleColon, start);
                    } else {
                        self.add(TokenKind::SingleColon, start);
                    }
                }
                '!' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::BangEquals, start);
                    } else {
                        self.add(TokenKind::SingleBang, start);
                    }
                }
                '?' => self.add(TokenKind::QuestionMark, start),
                '+' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::PlusEquals, start);
                    } else {
                        self.add(TokenKind::Plus, start);
                    }
                }
                '*' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::AsteriskEquals, start);
                    } else {
                        self.add(TokenKind::SingleAsterisk, start)
                    }
                }
                '-' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        self.add(TokenKind::ThinArrow, start);
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::HypenEquals, start);
                    }
                    _ => {
                        self.add(TokenKind::SingleHyphen, start);
                    }
                },
                '=' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        self.add(TokenKind::WideArrow, start);
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::DoubleEquals, start);
                    }
                    _ => self.add(TokenKind::SingleEquals, start),
                },
                '#' => self.add(TokenKind::Hash, start),
                '&' => match self.peek() {
                    Some('&') => {
                        self.advance();
                        self.add(TokenKind::DoubleAmpersand, start);
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::AmpersandEquals, start);
                    }
                    _ => {
                        self.add(TokenKind::SingleAmpersand, start);
                    }
                },
                '|' => match self.peek() {
                    Some('|') => {
                        self.advance();
                        self.add(TokenKind::DoublePipe, start);
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::PipeEquals, start);
                    }
                    _ => {
                        self.add(TokenKind::SinglePipe, start);
                    }
                },
                '%' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::PercentEquals, start)
                    } else {
                        self.add(TokenKind::SinglePercent, start)
                    }
                }
                '^' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add(TokenKind::CaretEquals, start);
                    } else {
                        self.add(TokenKind::SingleCaret, start)
                    }
                }
                '(' => self.add(TokenKind::OpenRoundBracket, start),
                ')' => self.add(TokenKind::CloseRoundBracket, start),
                '[' => self.add(TokenKind::OpenSquareBracket, start),
                ']' => self.add(TokenKind::CloseSquareBracket, start),
                '{' => self.add(TokenKind::OpenCurlyBracket, start),
                '}' => self.add(TokenKind::CloseCurlyBracket, start),
                '<' => match self.peek() {
                    Some('<') => {
                        self.advance();
                        if let Some('=') = self.peek() {
                            self.advance();
                            self.add(TokenKind::DoubleLessThanEquals, start);
                        } else {
                            self.add(TokenKind::DoubleLessThan, start);
                        }
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::LessThanEquals, start);
                    }
                    _ => {
                        self.add(TokenKind::SingleLessThan, start);
                    }
                },
                '>' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        if let Some('=') = self.peek() {
                            self.advance();
                            self.add(TokenKind::DoubleGreaterThanEquals, start);
                        } else {
                            self.add(TokenKind::DoubleGreaterThan, start);
                        }
                    }
                    Some('=') => {
                        self.advance();
                        self.add(TokenKind::GreaterThanEquals, start);
                    }
                    _ => {
                        self.add(TokenKind::SingleGreaterThan, start);
                    }
                },
                '\'' => match self.peek() {
                    Some(IdentMiddle![]) => {
                        self.advance();
                        let kind = loop {
                            match self.peek() {
                                Some(IdentMiddle![]) => self.advance(),
                                // FIXME: Escaped apostrophe
                                Some('\'') => {
                                    self.advance();
                                    break TokenKind::CharLit;
                                }
                                _ => break TokenKind::Lifetime,
                            }
                        };
                        self.add(kind, start);
                    }
                    _ => self.fin_lex_char_lit(start),
                },
                _ => self.add(TokenKind::Error, start),
            }
        }

        self.add(TokenKind::EndOfInput, self.index());

        self.tokens
    }

    fn fin_lex_char_lit(&mut self, start: ByteIndex) {
        // FIXME: Escape sequences, most importantly escaped apostrophe.
        while self.next().is_some_and(|(_, char)| char != '\'') {}

        // FIXME: We currently don't mark unterminated str lits
        //        and the parser doesn't report them.
        self.add(TokenKind::CharLit, start);
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
