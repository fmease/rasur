use crate::{
    edition::Edition,
    span::Span,
    token::{Token, TokenKind},
};
use iter::PeekableCharIndices;

pub(crate) fn lex(source: &str, edition: Edition, strip_shebang: StripShebang) -> Vec<Token> {
    let offset = strip_shebang.apply(source, edition);
    let mut chars = PeekableCharIndices::new(source, offset);
    let mut tokens = Vec::new();

    loop {
        let token = chars.lex(edition);

        if let TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment = token.kind
        {
            continue;
        }

        tokens.push(token);

        if token.kind == TokenKind::EndOfInput {
            break;
        }
    }

    tokens
}

#[derive(Clone, Copy)]
pub(crate) enum StripShebang {
    Yes,
    #[allow(dead_code)] // used in tests
    No,
}

impl StripShebang {
    fn apply(self, source: &str, edition: Edition) -> usize {
        let Self::Yes = self else { return 0 };
        let Some(suffix) = source.strip_prefix("#!") else { return 0 };
        let mut chars = PeekableCharIndices::new(suffix, 0);

        loop {
            let token = chars.lex(edition);

            if let TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment =
                token.kind
            {
                continue;
            }

            if token.kind == TokenKind::OpenSquareBracket {
                return 0;
            }

            return source.lines().next().unwrap_or_default().len();
        }
    }
}

impl iter::PeekableCharIndices<'_> {
    fn lex(&mut self, edition: Edition) -> Token {
        let Some((start, char)) = self.next() else {
            let index = self.index();
            return Token::new(TokenKind::EndOfInput, Span::new(index, index));
        };

        let kind = match char {
            _ if char.is_whitespace() => {
                while self.peek().is_some_and(|char| char.is_whitespace()) {
                    self.advance();
                }

                TokenKind::Whitespace
            }
            '/' => {
                match self.peek() {
                    Some('/') => {
                        self.advance();
                        while self.peek().is_some_and(|char| char != '\n') {
                            self.advance();
                        }

                        TokenKind::LineComment
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

                        TokenKind::BlockComment
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::SlashEquals
                    }
                    _ => TokenKind::SingleSlash,
                }
            }
            'b' => match self.peek() {
                Some('\'') => {
                    self.advance();
                    self.fin_lex_char_lit()
                }
                Some('"') => {
                    self.advance();
                    self.fin_lex_str_lit(SkipBackslashes::Yes)
                }
                Some('r') => {
                    self.advance();
                    self.fin_lex_raw_str_lit_or_ident()
                }
                _ => self.fin_lex_ident(),
            },
            'c' if edition >= Edition::Rust2021 => match self.peek() {
                Some('"') => {
                    self.advance();
                    self.fin_lex_str_lit(SkipBackslashes::Yes)
                }
                Some('r') => {
                    self.advance();
                    self.fin_lex_raw_str_lit_or_ident()
                }
                _ => self.fin_lex_ident(),
            },
            'r' => self.fin_lex_raw_str_lit_or_ident(),
            'a'..='z' | 'A'..='Z' | '_' => self.fin_lex_ident(),
            '0'..='9' => {
                // FIXME: Float literals
                while let Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') = self.peek() {
                    self.advance();
                }

                TokenKind::NumLit
            }
            '"' => self.fin_lex_str_lit(SkipBackslashes::Yes),
            '@' => TokenKind::At,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            '.' => {
                if let Some('.') = self.peek() {
                    self.advance();
                    match self.peek() {
                        Some('.') => {
                            self.advance();
                            TokenKind::TripleDot
                        }
                        Some('=') => {
                            self.advance();
                            TokenKind::DoubleDotEquals
                        }
                        _ => TokenKind::DoubleDot,
                    }
                } else {
                    TokenKind::SingleDot
                }
            }
            ':' => {
                if let Some(':') = self.peek() {
                    self.advance();
                    TokenKind::DoubleColon
                } else {
                    TokenKind::SingleColon
                }
            }
            '!' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenKind::BangEquals
                } else {
                    TokenKind::SingleBang
                }
            }
            '?' => TokenKind::QuestionMark,
            '+' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenKind::PlusEquals
                } else {
                    TokenKind::Plus
                }
            }
            '*' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenKind::AsteriskEquals
                } else {
                    TokenKind::SingleAsterisk
                }
            }
            '-' => match self.peek() {
                Some('>') => {
                    self.advance();
                    TokenKind::ThinArrow
                }
                Some('=') => {
                    self.advance();
                    TokenKind::HypenEquals
                }
                _ => TokenKind::SingleHyphen,
            },
            '=' => match self.peek() {
                Some('>') => {
                    self.advance();
                    TokenKind::WideArrow
                }
                Some('=') => {
                    self.advance();
                    TokenKind::DoubleEquals
                }
                _ => TokenKind::SingleEquals,
            },
            '#' => TokenKind::Hash,
            '&' => match self.peek() {
                Some('&') => {
                    self.advance();
                    TokenKind::DoubleAmpersand
                }
                Some('=') => {
                    self.advance();
                    TokenKind::AmpersandEquals
                }
                _ => TokenKind::SingleAmpersand,
            },
            '|' => match self.peek() {
                Some('|') => {
                    self.advance();
                    TokenKind::DoublePipe
                }
                Some('=') => {
                    self.advance();
                    TokenKind::PipeEquals
                }
                _ => TokenKind::SinglePipe,
            },
            '%' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenKind::PercentEquals
                } else {
                    TokenKind::SinglePercent
                }
            }
            '^' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenKind::CaretEquals
                } else {
                    TokenKind::SingleCaret
                }
            }
            '(' => TokenKind::OpenRoundBracket,
            ')' => TokenKind::CloseRoundBracket,
            '[' => TokenKind::OpenSquareBracket,
            ']' => TokenKind::CloseSquareBracket,
            '{' => TokenKind::OpenCurlyBracket,
            '}' => TokenKind::CloseCurlyBracket,
            '<' => match self.peek() {
                Some('<') => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        TokenKind::DoubleLessThanEquals
                    } else {
                        TokenKind::DoubleLessThan
                    }
                }
                Some('=') => {
                    self.advance();
                    TokenKind::LessThanEquals
                }
                _ => TokenKind::SingleLessThan,
            },
            '>' => match self.peek() {
                Some('>') => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        TokenKind::DoubleGreaterThanEquals
                    } else {
                        TokenKind::DoubleGreaterThan
                    }
                }
                Some('=') => {
                    self.advance();
                    TokenKind::GreaterThanEquals
                }
                _ => TokenKind::SingleGreaterThan,
            },
            '\'' => match self.peek() {
                Some(IdentMiddle![]) => {
                    self.advance();
                    loop {
                        match self.peek() {
                            Some(IdentMiddle![]) => self.advance(),
                            // FIXME: Escaped apostrophe
                            Some('\'') => {
                                self.advance();
                                break TokenKind::CharLit;
                            }
                            _ => break TokenKind::Lifetime,
                        }
                    }
                }
                _ => self.fin_lex_char_lit(),
            },
            _ => TokenKind::Error,
        };

        Token::new(kind, Span::new(start, self.index()))
    }

    fn fin_lex_char_lit(&mut self) -> TokenKind {
        // FIXME: Escape sequences, most importantly escaped apostrophe.
        while self.next().is_some_and(|(_, char)| char != '\'') {}

        // FIXME: We currently don't mark unterminated str lits
        //        and the parser doesn't report them.
        TokenKind::CharLit
    }

    // FIXME: Support raw idents without accepting `br#ident` or `cr#ident`
    // FIXME: Do the 256 `#` max validation in the parser.
    fn fin_lex_raw_str_lit_or_ident(&mut self) -> TokenKind {
        match self.peek() {
            Some('"') => {
                self.advance();
                self.fin_lex_str_lit(SkipBackslashes::No)
            }
            Some('#') => {
                self.advance();

                let mut open = 1usize;
                while let Some('#') = self.peek() {
                    self.advance();
                    open += 1;
                }

                'outer: loop {
                    while self.next().is_some_and(|(_, char)| char != '"') {}

                    let mut close = 0usize;

                    loop {
                        match self.peek() {
                            Some('#') => {
                                self.advance();
                                close += 1;
                                if open == close {
                                    break 'outer;
                                }
                            }
                            Some(_) => break,
                            None => break 'outer,
                        }
                    }
                }

                TokenKind::StrLit
            }
            _ => self.fin_lex_ident(),
        }
    }

    fn fin_lex_str_lit(&mut self, skip: SkipBackslashes) -> TokenKind {
        while let Some((_, char)) = self.next() {
            match char {
                '\\' if let SkipBackslashes::Yes = skip => self.advance(),
                '"' => break,
                _ => {}
            }
        }

        // FIXME: Suffixes

        // FIXME: We currently don't mark unterminated str lits
        //        and the parser doesn't report them.
        TokenKind::StrLit
    }

    fn fin_lex_ident(&mut self) -> TokenKind {
        while let Some(IdentMiddle![]) = self.peek() {
            self.advance();
        }

        TokenKind::Ident
    }
}

macro IdentMiddle() {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_'
}

enum SkipBackslashes {
    Yes,
    No,
}

mod iter {
    use crate::span::ByteIndex;
    use std::str::CharIndices;

    // FIXME: Add explainer as to how this differs from Peekable<CharIndices<'src>>.
    pub(super) struct PeekableCharIndices<'src> {
        chars: CharIndices<'src>,
        peeked: Option<Option<(usize, char)>>,
        // FIXME: Awkward!
        offset: usize,
    }

    impl<'src> PeekableCharIndices<'src> {
        pub(super) fn new(source: &'src str, offset: usize) -> Self {
            Self { chars: source[offset..].char_indices(), peeked: None, offset }
        }

        pub(super) fn peek(&mut self) -> Option<char> {
            self.peeked.get_or_insert_with(|| self.chars.next()).map(|(_, char)| char)
        }

        pub(super) fn next(&mut self) -> Option<(ByteIndex, char)> {
            self.peeked
                .take()
                .unwrap_or_else(|| self.chars.next())
                .map(|(index, char)| (ByteIndex::new(index + self.offset), char))
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
            ByteIndex::new(index + self.offset)
        }
    }
}
