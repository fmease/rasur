use crate::{
    Edition,
    error::{Buffer as ErrorBuffer, Error},
    span::{ByteIndex, Span},
    token::{PathSegKeyword, Token, TokenKind},
};
use unicode_xid::UnicodeXID;

pub fn lex(
    source: &str,
    edition: Edition,
    strip_shebang: StripShebang,
    errors: &mut ErrorBuffer,
) -> Vec<Token> {
    let offset = strip_shebang.apply(source, edition);
    let mut chars = Lexer::new(source, offset, edition, errors);
    let mut tokens = Vec::new();

    loop {
        let token = chars.lex();

        if let TokenKind::Trivia | TokenKind::Error = token.kind {
            continue;
        }

        tokens.push(token);

        if let TokenKind::EndOfInput = token.kind {
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
        let mut errors = ErrorBuffer::Void;
        let mut lexer = Lexer::new(suffix, 0, edition, &mut errors);

        loop {
            let token = lexer.lex();

            if let TokenKind::Trivia = token.kind {
                continue;
            }

            if let TokenKind::OpenSquareBracket = token.kind {
                return 0;
            }

            return source.lines().next().unwrap_or_default().len();
        }
    }
}

struct Lexer<'a, 'src> {
    source: &'src str,
    edition: Edition,
    chars: iter::PeekableCharIndices<'src>,
    errors: &'a mut ErrorBuffer,
}

impl<'a, 'src> Lexer<'a, 'src> {
    fn new(
        source: &'src str,
        offset: usize,
        edition: Edition,
        errors: &'a mut ErrorBuffer,
    ) -> Self {
        Self { source, edition, chars: iter::PeekableCharIndices::new(source, offset), errors }
    }

    fn lex(&mut self) -> Token {
        let Some((start, char)) = self.next() else {
            let index = self.index();
            return Token::new(TokenKind::EndOfInput, Span::new(index, index));
        };

        let kind = match char {
            _ if char.is_whitespace() => {
                while self.peek().is_some_and(|char| char.is_whitespace()) {
                    self.advance();
                }

                TokenKind::Trivia
            }
            '/' => match self.peek() {
                Some('/') => {
                    self.advance();

                    // FIXME: Using peek2 would lead to nicer code
                    let kind = match self.peek() {
                        Some('!') => {
                            self.advance();
                            TokenKind::InnerDocComment
                        }
                        Some('/') => {
                            self.advance();
                            match self.peek() {
                                Some('/') => TokenKind::Trivia,
                                _ => TokenKind::OuterDocComment,
                            }
                        }
                        _ => TokenKind::Trivia,
                    };

                    while self.peek().is_some_and(|char| char != '\n') {
                        self.advance();
                    }

                    kind
                }
                Some('*') => {
                    self.advance();
                    self.fin_lex_block_comment(start)
                }
                Some('=') => {
                    self.advance();
                    TokenKind::SlashEquals
                }
                _ => TokenKind::SingleSlash,
            },
            _ if is_ident_start(char) => self.fin_lex_ident_or_str_or_char_lit(start),
            '0'..='9' => {
                // FIXME: Float literals
                // FIXME: Int literal validation if it has an explicit base
                while let Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') = self.peek() {
                    self.advance();
                }

                TokenKind::NumLit
            }
            '"' => self.fin_lex_str_lit(start, SkipBackslashes::Yes),
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
            '\'' => self.fin_lex_char_lit_or_ticked_ident(start),
            _ => TokenKind::Invalid,
        };

        Token::new(kind, Span::new(start, self.index()))
    }

    fn fin_lex_block_comment(&mut self, start: ByteIndex) -> TokenKind {
        let mut depth = 0;
        let mut terminated = false;

        let kind = match self.peek() {
            Some('!') => {
                self.advance();
                TokenKind::InnerDocComment
            }
            // FIXME: Using peek2 would lead to nicer code
            Some('*') => {
                self.advance();
                match self.peek() {
                    Some('*') => TokenKind::Trivia,
                    Some('/') => {
                        self.advance();
                        return TokenKind::Trivia;
                    }
                    _ => TokenKind::OuterDocComment,
                }
            }
            _ => TokenKind::Trivia,
        };

        while let Some((_, char)) = self.next() {
            match (char, self.peek()) {
                ('/', Some('*')) => {
                    self.advance();
                    depth += 1;
                }
                ('*', Some('/')) => {
                    self.advance();
                    if depth == 0 {
                        terminated = true;
                        break;
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }

        if !terminated {
            self.error(Error::UnterminatedBlockComment(self.span(start)));
        }

        kind
    }

    // FIXME: Consolidate with fin_lex_char_lit smh
    fn fin_lex_char_lit_or_ticked_ident(&mut self, start: ByteIndex) -> TokenKind {
        if !self.peek().is_some_and(is_ident_middle) {
            return self.fin_lex_char_lit(start);
        }

        let unticked = self.index();
        self.advance();

        let mut raw = None;
        let mut is_lit = false;

        loop {
            match self.peek() {
                Some(char) if is_ident_middle(char) => self.advance(),
                Some('#') if raw.is_none() && self.edition >= Edition::Rust2021 => {
                    match self.source(unticked) {
                        "r" => {
                            self.advance();
                            raw = Some(self.index());
                        }
                        _ => {
                            self.error(Error::ReservedPrefix(self.span(unticked)));
                            break TokenKind::Error;
                        }
                    }
                }
                Some('\\') => {
                    is_lit = true;
                    self.advance();
                    self.advance();
                }
                Some('\'') => {
                    self.advance();
                    break TokenKind::CharLit;
                }
                _ if is_lit => {
                    self.error(Error::UnterminatedCharLit(self.span(start)));
                    break TokenKind::CharLit;
                }
                _ => {
                    if let Some(start) = raw
                        && let TokenKind::Underscore = lex_ident(self.source(start), self.edition)
                    {
                        self.error(Error::InvalidRawTickedIdent(self.span(start)));
                    }

                    break TokenKind::TickedIdent;
                }
            }
        }
    }

    fn fin_lex_char_lit(&mut self, start: ByteIndex) -> TokenKind {
        let mut terminated = false;

        while let Some((_, char)) = self.next() {
            match char {
                '\\' => self.advance(),
                '\'' => {
                    terminated = true;
                    break;
                }
                _ => {}
            }
        }

        if !terminated {
            self.error(Error::UnterminatedCharLit(self.span(start)));
        }

        // FIXME: Lex suffixes.

        TokenKind::CharLit
    }

    fn fin_lex_str_lit(&mut self, start: ByteIndex, skip: SkipBackslashes) -> TokenKind {
        let mut terminated = false;

        while let Some((_, char)) = self.next() {
            match char {
                '\\' if let SkipBackslashes::Yes = skip => self.advance(),
                '"' => {
                    terminated = true;
                    break;
                }
                _ => {}
            }
        }

        if !terminated {
            self.error(Error::UnterminatedStrLit(self.span(start)));
        }

        // FIXME: Suffixes

        TokenKind::StrLit
    }

    fn fin_lex_ident_or_str_or_char_lit(&mut self, start: ByteIndex) -> TokenKind {
        while self.peek().is_some_and(is_ident_middle) {
            self.advance();
        }

        let ident = self.source(start);

        match (ident, self.peek()) {
            ("b", Some('"')) => {
                self.advance();
                self.fin_lex_str_lit(start, SkipBackslashes::Yes)
            }
            ("br", Some('"')) => {
                self.advance();
                self.fin_lex_str_lit(start, SkipBackslashes::No)
            }
            ("c", Some('"')) if self.edition >= Edition::Rust2021 => {
                self.advance();
                self.fin_lex_str_lit(start, SkipBackslashes::Yes)
            }
            ("cr", Some('"')) if self.edition >= Edition::Rust2021 => {
                self.advance();
                self.fin_lex_str_lit(start, SkipBackslashes::No)
            }
            ("r", Some('"')) => {
                self.advance();
                self.fin_lex_str_lit(start, SkipBackslashes::No)
            }
            ("b", Some('\'')) => {
                self.advance();
                self.fin_lex_char_lit(start)
            }
            ("r", Some('#')) => {
                self.advance();

                let unprefixed = self.index();
                if self.peek().is_some_and(is_ident_start) {
                    while self.peek().is_some_and(is_ident_middle) {
                        self.advance();
                    }

                    if let PathSegKeyword!() | TokenKind::Underscore =
                        lex_ident(self.source(unprefixed), self.edition)
                    {
                        self.error(Error::InvalidRawIdent(self.span(unprefixed)));
                    }

                    return TokenKind::CommonIdent;
                }

                self.fin_lex_raw_guarded_str_lit(start)
            }
            ("br", Some('#')) => {
                self.advance();
                self.fin_lex_raw_guarded_str_lit(start)
            }
            ("cr", Some('#')) if self.edition >= Edition::Rust2021 => {
                self.advance();
                self.fin_lex_raw_guarded_str_lit(start)
            }
            (_, Some(char @ ('"' | '\'' | '#'))) if self.edition >= Edition::Rust2021 => {
                self.error(Error::ReservedPrefix(self.span(start)));
                if let '#' = char {
                    self.advance();
                }
                TokenKind::Error
            }
            _ => lex_ident(ident, self.edition),
        }
    }

    // FIXME: Consolidate with `fin_lex_str_lit` smh
    fn fin_lex_raw_guarded_str_lit(&mut self, start: ByteIndex) -> TokenKind {
        let mut terminated = false;
        let mut open = 1usize;

        while let Some('#') = self.peek() {
            self.advance();
            open += 1;
        }

        // FIXME: Emit an error if there isn't any double quote.
        'outer: loop {
            while self.next().is_some_and(|(_, char)| char != '"') {}

            let mut close = 0usize;

            loop {
                match self.peek() {
                    Some('#') => {
                        self.advance();
                        close += 1;
                        if open == close {
                            terminated = true;
                            break 'outer;
                        }
                    }
                    Some(_) => break,
                    None => break 'outer,
                }
            }
        }

        if !terminated {
            self.error(Error::UnterminatedStrLit(self.span(start)));
        } else if open > 255 {
            self.error(Error::StrLitGuardTooLarge(self.span(start)));
        }

        // FIXME: Lex suffixes.

        TokenKind::StrLit
    }

    fn span(&self, start: ByteIndex) -> Span {
        Span::new(start, self.index())
    }

    fn source(&self, start: ByteIndex) -> &'src str {
        &self.source[self.span(start).range()]
    }

    fn error(&mut self, error: Error) {
        self.errors.add(error);
    }
}

impl<'src> std::ops::Deref for Lexer<'_, 'src> {
    type Target = iter::PeekableCharIndices<'src>;

    fn deref(&self) -> &Self::Target {
        &self.chars
    }
}

impl std::ops::DerefMut for Lexer<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chars
    }
}

enum SkipBackslashes {
    Yes,
    No,
}

fn is_ident_start(char: char) -> bool {
    char == '_' || char.is_xid_start()
}

fn is_ident_middle(char: char) -> bool {
    char.is_xid_continue()
}

pub(crate) fn lex_ident(source: &str, edition: Edition) -> TokenKind {
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
