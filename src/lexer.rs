use crate::{
    Edition,
    error::{Buffer as ErrorBuffer, Error},
    span::{ByteIndex, Span},
    token::{PathSegKeyword, Token, TokenKind},
};
use unicode_xid::UnicodeXID;

// FIXME: Unicode BOM removal
// FIXME: CRLF→LF normalization

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
        if let Some((start, char)) = self.next() {
            let kind = self.fin_lex(char, start);
            Token::new(kind, Span::new(start, self.index()))
        } else {
            let index = self.index();
            Token::new(TokenKind::EndOfInput, Span::new(index, index))
        }
    }

    fn fin_lex(&mut self, char: char, start: ByteIndex) -> TokenKind {
        match char {
            _ if is_whitespace(char) => {
                self.next_while(is_whitespace);
                TokenKind::Trivia
            }
            '/' => match self.peek() {
                Some('/') => {
                    self.next();

                    let mut this = self.snapshot();
                    let kind = match this.next() {
                        Some('!') => {
                            self.next();
                            TokenKind::InnerDocComment
                        }
                        Some('/') if this.next().is_none_or(|char| char != '/') => {
                            self.next();
                            TokenKind::OuterDocComment
                        }
                        _ => TokenKind::Trivia,
                    };

                    self.next_while(|char| char != '\n');

                    kind
                }
                Some('*') => {
                    self.next();

                    let mut depth = 0;
                    let mut terminated = false;

                    let mut this = self.snapshot();
                    let kind = match this.next() {
                        Some('!') => {
                            self.next();
                            TokenKind::InnerDocComment
                        }
                        Some('*') if this.next().is_none_or(|char| char != '*' && char != '/') => {
                            self.next();
                            TokenKind::OuterDocComment
                        }
                        _ => TokenKind::Trivia,
                    };

                    while let Some((_, char)) = self.next() {
                        match (char, self.peek()) {
                            ('/', Some('*')) => {
                                self.next();
                                depth += 1;
                            }
                            ('*', Some('/')) => {
                                self.next();
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
                Some('=') => {
                    self.next();
                    TokenKind::SlashEquals
                }
                _ => TokenKind::SingleSlash,
            },
            _ if is_ident_start(char) => self.fin_lex_ident_or_str_or_char_lit(start),
            '0'..='9' => {
                #[derive(Clone, Copy)]
                enum Base {
                    Bin,
                    Oct,
                    Dec,
                    Hex,
                }

                let base = match (char, self.peek()) {
                    ('0', Some('b')) => Base::Bin,
                    ('0', Some('o')) => Base::Oct,
                    ('0', Some('x')) => Base::Hex,
                    _ => Base::Dec,
                };
                let mut is_empty = match base {
                    Base::Bin | Base::Oct | Base::Hex => {
                        self.next();
                        true
                    }
                    Base::Dec => false,
                };

                while let Some(char) = self.peek() {
                    if char == '_' {
                        self.next();
                        continue;
                    }
                    match base {
                        Base::Dec if is_dec_digit(char) => {}
                        Base::Bin if is_bin_digit(char) => {}
                        Base::Oct if is_oct_digit(char) => {}
                        Base::Hex if is_hex_digit(char) => {}
                        Base::Bin | Base::Oct | Base::Hex if is_dec_digit(char) => {
                            self.error(Error::InvalidDigit(self.span(self.index())));
                        }
                        _ => break,
                    }
                    is_empty = false;
                    self.next();
                }

                if is_empty {
                    self.error(Error::EmptyNumLit(self.span(start)));
                }

                let mut this = self.snapshot();
                if let Some('.') = this.next()
                    && !this.next().is_some_and(|char| char == '.' || is_ident_start(char))
                {
                    self.next();
                    self.next_while(|char| char == '_' || is_dec_digit(char));

                    match base {
                        Base::Dec => {}
                        Base::Bin | Base::Oct | Base::Hex => {
                            self.error(Error::NonDecFloatLit(self.span(start)));
                        }
                    }
                }

                if let Some('e' | 'E') = self.peek() {
                    self.next();

                    if let Some('+' | '-') = self.peek() {
                        self.next();
                    }

                    let mut is_empty = true;

                    while let Some(char) = self.peek() {
                        if char == '_' {
                            self.next();
                            continue;
                        }
                        if !is_dec_digit(char) {
                            break;
                        }
                        is_empty = false;
                        self.next();
                    }

                    if is_empty {
                        self.error(Error::EmptyExponent(self.span(start)));
                    }
                }

                self.lex_lit_suffix();

                TokenKind::NumLit
            }
            '"' => self.fin_lex_str_lit(Raw::No, LitKind::Str, start),
            '@' => TokenKind::At,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            '.' => {
                if let Some('.') = self.peek() {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            TokenKind::TripleDot
                        }
                        Some('=') => {
                            self.next();
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
                    self.next();
                    TokenKind::DoubleColon
                } else {
                    TokenKind::SingleColon
                }
            }
            '!' => {
                if let Some('=') = self.peek() {
                    self.next();
                    TokenKind::BangEquals
                } else {
                    TokenKind::SingleBang
                }
            }
            '?' => TokenKind::QuestionMark,
            '+' => {
                if let Some('=') = self.peek() {
                    self.next();
                    TokenKind::PlusEquals
                } else {
                    TokenKind::SinglePlus
                }
            }
            '*' => {
                if let Some('=') = self.peek() {
                    self.next();
                    TokenKind::AsteriskEquals
                } else {
                    TokenKind::SingleAsterisk
                }
            }
            '-' => match self.peek() {
                Some('>') => {
                    self.next();
                    TokenKind::ThinArrow
                }
                Some('=') => {
                    self.next();
                    TokenKind::HypenEquals
                }
                _ => TokenKind::SingleHyphen,
            },
            '=' => match self.peek() {
                Some('>') => {
                    self.next();
                    TokenKind::WideArrow
                }
                Some('=') => {
                    self.next();
                    TokenKind::DoubleEquals
                }
                _ => TokenKind::SingleEquals,
            },
            '#' => {
                if self.edition >= Edition::Rust2024 {
                    let mut multi = false;

                    while self.peek().is_some_and(|char| char == '#') {
                        multi = true;
                        self.next();
                    }

                    if let Some('"') = self.peek() {
                        self.error(Error::ReservedPrefix(self.span(start)));
                        return TokenKind::Error;
                    }

                    if multi {
                        self.error(Error::ReservedMultiHash(self.span(start)));
                        return TokenKind::Error;
                    }
                }

                TokenKind::Hash
            }
            '&' => match self.peek() {
                Some('&') => {
                    self.next();
                    TokenKind::DoubleAmpersand
                }
                Some('=') => {
                    self.next();
                    TokenKind::AmpersandEquals
                }
                _ => TokenKind::SingleAmpersand,
            },
            '|' => match self.peek() {
                Some('|') => {
                    self.next();
                    TokenKind::DoublePipe
                }
                Some('=') => {
                    self.next();
                    TokenKind::PipeEquals
                }
                _ => TokenKind::SinglePipe,
            },
            '%' => {
                if let Some('=') = self.peek() {
                    self.next();
                    TokenKind::PercentEquals
                } else {
                    TokenKind::SinglePercent
                }
            }
            '^' => {
                if let Some('=') = self.peek() {
                    self.next();
                    TokenKind::CaretEquals
                } else {
                    TokenKind::SingleCaret
                }
            }
            '$' => TokenKind::Dollar,
            '~' => TokenKind::Tilde,
            '(' => TokenKind::OpenRoundBracket,
            ')' => TokenKind::CloseRoundBracket,
            '[' => TokenKind::OpenSquareBracket,
            ']' => TokenKind::CloseSquareBracket,
            '{' => TokenKind::OpenCurlyBracket,
            '}' => TokenKind::CloseCurlyBracket,
            '<' => match self.peek() {
                Some('<') => {
                    self.next();
                    if let Some('=') = self.peek() {
                        self.next();
                        TokenKind::DoubleLessThanEquals
                    } else {
                        TokenKind::DoubleLessThan
                    }
                }
                Some('=') => {
                    self.next();
                    TokenKind::LessThanEquals
                }
                Some('-') => {
                    self.next();
                    TokenKind::ThinBackArrow
                }
                _ => TokenKind::SingleLessThan,
            },
            '>' => match self.peek() {
                Some('>') => {
                    self.next();
                    if let Some('=') = self.peek() {
                        self.next();
                        TokenKind::DoubleGreaterThanEquals
                    } else {
                        TokenKind::DoubleGreaterThan
                    }
                }
                Some('=') => {
                    self.next();
                    TokenKind::GreaterThanEquals
                }
                _ => TokenKind::SingleGreaterThan,
            },
            '\'' => self.fin_lex_ticked_ident_or_char_lit(start),
            _ => {
                self.error(Error::InvalidToken(char, self.span(start)));
                TokenKind::Error
            }
        }
    }

    // FIXME: Consolidate with fin_lex_char_lit smh
    fn fin_lex_ticked_ident_or_char_lit(&mut self, start: ByteIndex) -> TokenKind {
        if !self.peek().is_some_and(is_ident_start) {
            return self.fin_lex_char_lit(LitKind::Char, start);
        }

        let unticked = self.index();
        self.next();

        let mut raw = None;
        let mut count = 1usize;

        loop {
            match self.peek() {
                Some(char) if is_ident_middle(char) => {
                    self.next();
                    count += 1;
                }
                Some('#') if raw.is_none() && self.edition >= Edition::Rust2021 => {
                    match self.source(unticked) {
                        "r" => {
                            self.next();
                            raw = Some(self.index());
                        }
                        _ => {
                            self.error(Error::ReservedPrefix(self.span(unticked)));
                            break TokenKind::Error;
                        }
                    }
                }
                Some('\'') => {
                    self.next();

                    match count {
                        0 => unreachable!(),
                        1 => {}
                        _ => self.error(Error::MultiScalarCharLit(self.span(start))),
                    }

                    self.lex_lit_suffix();

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

    fn fin_lex_char_lit(&mut self, kind: LitKind, start: ByteIndex) -> TokenKind {
        let mut count = 0usize;
        let mut terminated = false;
        let mut has_invalid_escape_seqs = false;
        let mut invalid_scalar = None;

        while let Some((index, char)) = self.next() {
            match char {
                '\\' => has_invalid_escape_seqs |= !self.fin_lex_escape_seq(kind),
                '\'' => {
                    terminated = true;
                    break;
                }
                '\n' | '\t' | '\r' => {
                    invalid_scalar.get_or_insert(self.span(index));
                }
                _ => {}
            }
            count += 1;
        }

        let span = self.span(start);

        if !terminated {
            self.error(Error::UnterminatedCharLit(span));
        } else {
            if !has_invalid_escape_seqs {
                match count {
                    0 => self.error(Error::EmptyCharLit(span)),
                    1 => {}
                    _ => self.error(Error::MultiScalarCharLit(span)),
                }
            }
            if let Some(span) = invalid_scalar
                && count == 1
            {
                self.error(Error::InvalidScalarInLit(span));
            }
        }

        self.lex_lit_suffix();

        TokenKind::CharLit
    }

    fn fin_lex_str_lit(&mut self, raw: Raw, kind: LitKind, start: ByteIndex) -> TokenKind {
        let mut terminated = false;
        let mut invalid_scalar = None;

        while let Some((index, char)) = self.next() {
            match char {
                '\\' if let Raw::No = raw => {
                    self.fin_lex_escape_seq(kind);
                }
                '"' => {
                    terminated = true;
                    break;
                }
                '\r' => {
                    invalid_scalar.get_or_insert(self.span(index));
                }
                _ => {}
            }
        }

        if !terminated {
            self.error(Error::UnterminatedStrLit(self.span(start)));
        } else if let Some(span) = invalid_scalar {
            self.error(Error::InvalidScalarInLit(span));
        }

        self.lex_lit_suffix();

        TokenKind::StrLit
    }

    fn fin_lex_ident_or_str_or_char_lit(&mut self, start: ByteIndex) -> TokenKind {
        self.next_while(is_ident_middle);

        let ident = self.source(start);

        let (raw, kind) = match (ident, self.peek()) {
            ("b", Some('"')) => (Raw::No, LitKind::ByteStr),
            ("br", Some('"')) => (Raw::Yes, LitKind::ByteStr),
            ("c", Some('"')) if self.edition >= Edition::Rust2021 => (Raw::No, LitKind::CStr),
            ("cr", Some('"')) if self.edition >= Edition::Rust2021 => (Raw::Yes, LitKind::CStr),
            ("r", Some('"')) => (Raw::Yes, LitKind::Str),
            ("b", Some('\'')) => {
                self.next();
                return self.fin_lex_char_lit(LitKind::Byte, start);
            }
            ("r", Some('#')) => {
                self.next();

                let unprefixed = self.index();
                if self.peek().is_some_and(is_ident_start) {
                    self.next_while(is_ident_middle);

                    if let PathSegKeyword!() | TokenKind::Underscore =
                        lex_ident(self.source(unprefixed), self.edition)
                    {
                        self.error(Error::InvalidRawIdent(self.span(unprefixed)));
                    }

                    return TokenKind::CommonIdent;
                }

                return self.fin_lex_raw_guarded_str_lit(start);
            }
            ("br", Some('#')) => {
                self.next();
                return self.fin_lex_raw_guarded_str_lit(start);
            }
            ("cr", Some('#')) if self.edition >= Edition::Rust2021 => {
                self.next();
                return self.fin_lex_raw_guarded_str_lit(start);
            }
            (_, Some(char @ ('"' | '\'' | '#'))) if self.edition >= Edition::Rust2021 => {
                self.error(Error::ReservedPrefix(self.span(start)));
                if let '#' = char {
                    self.next();
                }
                return TokenKind::Error;
            }
            _ => return lex_ident(ident, self.edition),
        };
        self.next();
        self.fin_lex_str_lit(raw, kind, start)
    }

    // FIXME: Consolidate with `fin_lex_str_lit` smh
    fn fin_lex_raw_guarded_str_lit(&mut self, start: ByteIndex) -> TokenKind {
        let mut terminated = false;
        let mut open = 1usize;

        while let Some('#') = self.peek() {
            self.next();
            open += 1;
        }

        if let Some((index, char)) = self.next()
            && char != '"'
        {
            self.error(Error::InvalidStrLitDelim(self.span(index)));
            return TokenKind::StrLit;
        }

        'outer: loop {
            while self.next().is_some_and(|(_, char)| char != '"') {}

            let mut close = 0usize;

            loop {
                match self.peek() {
                    Some('#') => {
                        self.next();
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

        self.lex_lit_suffix();

        TokenKind::StrLit
    }

    fn fin_lex_escape_seq(&mut self, kind: LitKind) -> bool {
        let index = self.index();
        if !self.fin_lex_escape_seq_inner(kind) {
            self.error(Error::InvalidEscapeSequence(self.span(index)));
            return false;
        }
        true
    }

    // FIXME: Emit slightly more precise diagnostics & better diagnostic spans.
    fn fin_lex_escape_seq_inner(&mut self, kind: LitKind) -> bool {
        let Some((_, char)) = self.next() else { return false };

        match (char, kind) {
            | ('\\' | '"' | '\'' | 'n' | 'r' | 't', _)
            | ('0', LitKind::Char | LitKind::Byte | LitKind::Str | LitKind::ByteStr)
            | ('\n', LitKind::Str | LitKind::ByteStr | LitKind::CStr) => true,
            ('x', _) => {
                let Some(char) = self.peek() else { return false };
                match kind {
                    LitKind::Char | LitKind::Str if is_oct_digit(char) => {}
                    LitKind::Byte | LitKind::ByteStr | LitKind::CStr if is_hex_digit(char) => {}
                    _ => return false,
                }
                self.next();
                if !self.peek().is_some_and(is_hex_digit) {
                    return false;
                }
                self.next();
                true
            }
            ('u', _) => {
                let Some((_, '{')) = self.next() else { return false };

                let mut is_empty = true;
                let mut value = 0;

                while let Some(char) = self.peek() {
                    let sub = |x: char, y: char| x as u32 - y as u32;

                    let plus = match char {
                        '0'..='9' => sub(char, '0'),
                        'a'..='f' => sub(char, 'a') + 10,
                        'A'..='F' => sub(char, 'A') + 10,
                        '_' if !is_empty => {
                            self.next();
                            continue;
                        }
                        '}' => {
                            self.next();
                            break;
                        }
                        _ => return false,
                    };
                    value *= 16;
                    value += plus;

                    is_empty = false;
                    self.next();
                }

                !is_empty && value <= 0x10FFFF
            }
            _ => false,
        }
    }

    fn lex_lit_suffix(&mut self) {
        if let Some(char) = self.peek()
            && is_ident_start(char)
        {
            let start = self.index();
            self.next();
            self.next_while(is_ident_middle);

            let span = self.span(start);
            if char == '_' && span.len() == 1 {
                self.error(Error::InvalidLitSuffix(span));
            }
        }
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

#[derive(Clone, Copy)]
enum LitKind {
    Byte,
    ByteStr,
    CStr,
    Char,
    Str,
}

enum Raw {
    Yes,
    No,
}

#[rustfmt::skip]
fn is_whitespace(char: char) -> bool {
    // Whitespace according to Unicode Pattern_White_Space
    // contrary to White_Space as used in the stdlib fn.

    matches!(
        char,
        | '\t' | '\n' | '\x0B' | '\x0C' | '\r' | ' ' | '\u{85}'
        | '\u{200E}' | '\u{200F}' | '\u{2028}' | '\u{2029}'
    )
}

fn is_ident_start(char: char) -> bool {
    char == '_' || char.is_xid_start()
}

fn is_ident_middle(char: char) -> bool {
    char.is_xid_continue()
}

fn is_bin_digit(char: char) -> bool {
    matches!(char, '0' | '1')
}

fn is_oct_digit(char: char) -> bool {
    matches!(char, '0'..='7')
}

fn is_dec_digit(char: char) -> bool {
    matches!(char, '0'..='9')
}

fn is_hex_digit(char: char) -> bool {
    matches!(char, '0'..='9' | 'a'..='f' | 'A'..='F')
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
    use std::str::Chars;

    pub(super) struct PeekableCharIndices<'src> {
        chars: Chars<'src>,
        index: ByteIndex,
    }

    impl<'src> PeekableCharIndices<'src> {
        pub(super) fn new(source: &'src str, offset: usize) -> Self {
            Self { chars: source[offset..].chars(), index: ByteIndex::new(offset) }
        }

        pub(super) fn next(&mut self) -> Option<(ByteIndex, char)> {
            self.chars.next().map(|char| {
                let index = self.index();
                self.index += char.len_utf8() as _;
                (index, char)
            })
        }

        pub(super) fn snapshot(&self) -> impl Iterator<Item = char> + use<'src> {
            self.chars.clone()
        }

        pub(super) fn peek(&mut self) -> Option<char> {
            let mut chars = self.chars.clone();
            chars.next()
        }

        pub(super) fn index(&self) -> ByteIndex {
            self.index
        }

        pub(super) fn next_while(&mut self, predicate: impl Fn(char) -> bool) {
            while let Some(char) = self.peek()
                && predicate(char)
            {
                self.next();
            }
        }
    }
}
