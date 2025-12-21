use crate::{
    Edition,
    span::{ByteIndex, Span},
    token::{Token, TokenKind},
};
use unicode_xid::UnicodeXID;

pub fn lex(source: &str, edition: Edition, strip_shebang: StripShebang) -> Vec<Token> {
    let offset = strip_shebang.apply(source, edition);
    let mut chars = Lexer::new(source, offset, edition);
    let mut tokens = Vec::new();

    loop {
        let token = chars.lex();

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
pub enum StripShebang {
    Yes,
    No,
}

impl StripShebang {
    fn apply(self, source: &str, edition: Edition) -> usize {
        let Self::Yes = self else { return 0 };
        let Some(suffix) = source.strip_prefix("#!") else { return 0 };
        let mut lexer = Lexer::new(suffix, 0, edition);

        loop {
            let token = lexer.lex();

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

struct Lexer<'src> {
    source: &'src str,
    edition: Edition,
    chars: iter::PeekableCharIndices<'src>,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str, offset: usize, edition: Edition) -> Self {
        Self { source, edition, chars: iter::PeekableCharIndices::new(source, offset) }
    }

    fn lex(&mut self) -> Token {
        let Some((start, char)) = self.next() else {
            let index = self.index();
            return Token::new(TokenKind::EndOfInput, Span::new(index, index));
        };

        // FIXME: Don't lex prefixes manually below (e.g., `c`, `br`), just parse the
        //        potential prefix as an ident and decide later if it's one.
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
                    // FIXME: Smh. taint unterminated m-l comments (but don't fatal!)
                    Some('*') => {
                        self.advance();

                        let mut depth = 0;

                        while let Some((_, prev)) = self.next() {
                            match (prev, self.peek()) {
                                ('/', Some('*')) => {
                                    self.advance();
                                    depth += 1;
                                }
                                ('*', Some('/')) => {
                                    self.advance();
                                    if depth == 0 {
                                        break;
                                    }
                                    depth -= 1;
                                }
                                _ => (),
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
                    self.fin_lex_raw_str_lit_or_ident(RawStrKind::Byte, start)
                }
                _ => self.fin_lex_ident(start),
            },
            'c' if self.edition >= Edition::Rust2021 => match self.peek() {
                Some('"') => {
                    self.advance();
                    self.fin_lex_str_lit(SkipBackslashes::Yes)
                }
                Some('r') => {
                    self.advance();
                    self.fin_lex_raw_str_lit_or_ident(RawStrKind::Cee, start)
                }
                _ => self.fin_lex_ident(start),
            },
            'r' => self.fin_lex_raw_str_lit_or_ident(RawStrKind::Normal, start),
            _ if is_ident_start(char) => self.fin_lex_ident(start),
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
                    TokenKind::SinglePlus
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
            '\'' => self.fin_lex_char_lit_or_ticked_ident(),
            _ => TokenKind::Invalid,
        };

        Token::new(kind, Span::new(start, self.index()))
    }

    fn fin_lex_char_lit_or_ticked_ident(&mut self) -> TokenKind {
        if !self.peek().is_some_and(is_ident_middle) {
            return self.fin_lex_char_lit();
        }
        self.advance();

        // FIXME: In >=2021 detect & smw reject arbitrary ticked ident prefixes unless it's `r`.
        loop {
            match self.peek() {
                Some(char) if is_ident_middle(char) => self.advance(),
                // FIXME: Escaped apostrophe
                Some('\'') => {
                    self.advance();
                    break TokenKind::CharLit;
                }
                _ => break TokenKind::TickedIdent,
            }
        }
    }

    fn fin_lex_char_lit(&mut self) -> TokenKind {
        while let Some((_, char)) = self.next() {
            match char {
                '\\' => self.advance(),
                '\'' => break,
                _ => {}
            }
        }

        // FIXME: We currently don't mark unterminated str lits
        //        and the parser doesn't report them.
        TokenKind::CharLit
    }

    // FIXME: Do the 256 `#` max validation in the parser.
    fn fin_lex_raw_str_lit_or_ident(&mut self, kind: RawStrKind, start: ByteIndex) -> TokenKind {
        match self.peek() {
            Some('"') => {
                self.advance();
                self.fin_lex_str_lit(SkipBackslashes::No)
            }
            Some('#') => {
                self.advance();

                if let RawStrKind::Normal = kind
                    && self.peek().is_some_and(is_ident_start)
                {
                    self.advance();
                    return self.fin_lex_ident(start);
                }

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
            _ => self.fin_lex_ident(start),
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

    fn fin_lex_ident(&mut self, start: ByteIndex) -> TokenKind {
        while self.peek().is_some_and(is_ident_middle) {
            self.advance();
        }

        if self.edition >= Edition::Rust2021
            && let Some('#' | '"' | '\'') = self.peek()
        {
            return TokenKind::ReservedPrefix;
        }

        lex_ident_or_keyword(self.source(start), self.edition)
    }

    fn source(&self, start: ByteIndex) -> &'src str {
        &self.source[Span::new(start, self.index()).range()]
    }
}

impl<'src> std::ops::Deref for Lexer<'src> {
    type Target = iter::PeekableCharIndices<'src>;

    fn deref(&self) -> &Self::Target {
        &self.chars
    }
}

impl<'src> std::ops::DerefMut for Lexer<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chars
    }
}

enum SkipBackslashes {
    Yes,
    No,
}

enum RawStrKind {
    Normal,
    Byte,
    Cee,
}

fn is_ident_start(char: char) -> bool {
    char == '_' || char.is_xid_start()
}

fn is_ident_middle(char: char) -> bool {
    char.is_xid_continue()
}

pub(crate) fn lex_ident_or_keyword(source: &str, edition: Edition) -> TokenKind {
    match source {
        "Self" => TokenKind::SelfUpper,
        "_" => TokenKind::Underscore,
        "abstract" => TokenKind::Abstract,
        "as" => TokenKind::As,
        "async" if edition >= Edition::Rust2018 => TokenKind::Async,
        "await" if edition >= Edition::Rust2018 => TokenKind::Await,
        "become" => TokenKind::Become,
        "box" => TokenKind::Box,
        "break" => TokenKind::Break,
        "const" => TokenKind::Const,
        "continue" => TokenKind::Continue,
        "crate" => TokenKind::Crate,
        "do" => TokenKind::Do,
        "dyn" if edition >= Edition::Rust2018 => TokenKind::Dyn,
        "else" => TokenKind::Else,
        "enum" => TokenKind::Enum,
        "extern" => TokenKind::Extern,
        "false" => TokenKind::False,
        "final" => TokenKind::Final,
        "fn" => TokenKind::Fn,
        "for" => TokenKind::For,
        "gen" if edition >= Edition::Rust2024 => TokenKind::Gen,
        "if" => TokenKind::If,
        "impl" => TokenKind::Impl,
        "in" => TokenKind::In,
        "let" => TokenKind::Let,
        "loop" => TokenKind::Loop,
        "macro" => TokenKind::Macro,
        "match" => TokenKind::Match,
        "mod" => TokenKind::Mod,
        "move" => TokenKind::Move,
        "mut" => TokenKind::Mut,
        "override" => TokenKind::Override,
        "priv" => TokenKind::Priv,
        "pub" => TokenKind::Pub,
        "ref" => TokenKind::Ref,
        "return" => TokenKind::Return,
        "self" => TokenKind::SelfLower,
        "static" => TokenKind::Static,
        "struct" => TokenKind::Struct,
        "super" => TokenKind::Super,
        "trait" => TokenKind::Trait,
        "true" => TokenKind::True,
        "try" if edition >= Edition::Rust2018 => TokenKind::Try,
        "type" => TokenKind::Type,
        "typeof" => TokenKind::Typeof,
        "unsafe" => TokenKind::Unsafe,
        "use" => TokenKind::Use,
        "virtual" => TokenKind::Virtual,
        "where" => TokenKind::Where,
        "while" => TokenKind::While,
        "yield" => TokenKind::Yield,
        _ => TokenKind::CommonIdent,
    }
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
