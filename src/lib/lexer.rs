mod cutter;
mod transformer;

use crate::{
    edition::Edition,
    error::{Buffer as ErrorBuffer, Error, InvalidScalarPlace},
    span::{ByteIndex, Span},
    token::{PathSegKeyword, Token, TokenKind},
};
use cutter::Cutter;
pub use transformer::{Frontmatter, normalize, strip_frontmatter, strip_shebang};

pub type Tokens<'err, 'src> = impl Iterator<Item = Token>;

#[define_opaque(Tokens)]
pub fn lex<'err, 'src>(
    source: &'src str,
    offset: ByteIndex,
    edition: Edition,
    errors: &'err ErrorBuffer,
) -> Tokens<'err, 'src> {
    Lexer { source, edition, cutter: Cutter::new(source, offset), previous: None, errors }
}

struct Lexer<'err, 'src> {
    source: &'src str,
    edition: Edition,
    cutter: Cutter<'src>,
    previous: Option<TokenKind>,
    errors: &'err ErrorBuffer,
}

impl<'err, 'src> Lexer<'err, 'src> {
    fn fin_lex_token(&mut self, char: char, start: ByteIndex) -> TokenKind {
        match char {
            _ if is_whitespace(char) => {
                self.advance_while(is_whitespace);
                TokenKind::Whitespace
            }
            '/' => match self.peek() {
                Some('/') => {
                    self.advance();
                    self.fin_lex_line_comment()
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
            '0'..='9' => self.fin_lex_num_lit(char, start),
            _ if is_ident_start(char) => self.fin_lex_ident_prefixed_token(start),
            '"' => self.fin_lex_str_lit(Raw::No, TextLitFlavor::Utf8, start),
            '\'' => self.fin_lex_ticked_ident_or_char_lit(start),
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
            '#' => {
                if self.edition >= Edition::Rust2024 {
                    let mut multi = false;

                    while self.peek().is_some_and(|char| char == '#') {
                        multi = true;
                        self.advance();
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
                Some('-') => {
                    self.advance();
                    TokenKind::ThinBackArrow
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
            _ => {
                self.error(Error::InvalidScalar(char, InvalidScalarPlace::File, self.span(start)));
                TokenKind::Error
            }
        }
    }

    fn fin_lex_line_comment(&mut self) -> TokenKind {
        let mut this = self.snapshot();
        let kind = match this.next() {
            Some('!') => {
                self.advance();
                TokenKind::InnerDocComment
            }
            Some('/') if this.next().is_none_or(|char| char != '/') => {
                self.advance();
                TokenKind::OuterDocComment
            }
            _ => TokenKind::Comment,
        };

        while let Some(char) = self.peek() {
            if let '\n' = char {
                break;
            }
            let start = self.index();
            self.advance();
            if let TokenKind::InnerDocComment | TokenKind::OuterDocComment = kind
                && let '\r' = char
            {
                self.error(Error::InvalidScalar(
                    char,
                    InvalidScalarPlace::DocComment,
                    self.span(start),
                ));
            }
        }

        kind
    }

    fn fin_lex_block_comment(&mut self, start: ByteIndex) -> TokenKind {
        let mut depth = 0;
        let mut terminated = false;

        let mut this = self.snapshot();
        let kind = match this.next() {
            Some('!') => {
                self.advance();
                TokenKind::InnerDocComment
            }
            Some('*') if this.next().is_none_or(|char| char != '*' && char != '/') => {
                self.advance();
                TokenKind::OuterDocComment
            }
            _ => TokenKind::Comment,
        };

        while let Some((index, char)) = self.advance() {
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
                ('\r', _) if let TokenKind::InnerDocComment | TokenKind::OuterDocComment = kind => {
                    self.error(Error::InvalidScalar(
                        char,
                        InvalidScalarPlace::DocComment,
                        self.span(index),
                    ))
                }
                _ => {}
            }
        }

        if !terminated {
            self.error(Error::UnterminatedBlockComment(self.span(start)));
        }

        kind
    }

    fn fin_lex_num_lit(&mut self, char: char, start: ByteIndex) -> TokenKind {
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
                self.advance();
                true
            }
            Base::Dec => false,
        };

        while let Some(char) = self.peek() {
            if char == '_' {
                self.advance();
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
            self.advance();
        }

        if is_empty {
            self.error(Error::EmptyNumLit(self.span(start)));
        }

        let mut this = self.snapshot();
        if let Some('.') = this.next()
            && !this.next().is_some_and(|char| char == '.' || is_ident_start(char))
        {
            self.advance();
            self.advance_while(|char| char == '_' || is_dec_digit(char));

            match base {
                Base::Dec => {}
                Base::Bin | Base::Oct | Base::Hex => {
                    self.error(Error::NonDecFloatLit(self.span(start)));
                }
            }
        }

        if let Some('e' | 'E') = self.peek() {
            self.advance();

            if let Some('+' | '-') = self.peek() {
                self.advance();
            }

            let mut is_empty = true;

            while let Some(char) = self.peek() {
                if char == '_' {
                    self.advance();
                    continue;
                }
                if !is_dec_digit(char) {
                    break;
                }
                is_empty = false;
                self.advance();
            }

            if is_empty {
                self.error(Error::EmptyExponent(self.span(start)));
            }
        }

        TokenKind::NumLit
    }

    // FIXME: Consolidate with fin_lex_char_lit smh
    fn fin_lex_ticked_ident_or_char_lit(&mut self, start: ByteIndex) -> TokenKind {
        if !self.peek().is_some_and(is_ident_start) {
            return self.fin_lex_char_lit(TextLitFlavor::Utf8, start);
        }

        let unticked = self.index();
        self.advance();

        let mut count = 1usize;

        loop {
            match self.peek() {
                Some(char) if is_ident_middle(char) => {
                    self.advance();
                    count += 1;
                }
                Some('\'') => {
                    self.advance();

                    match count {
                        0 => unreachable!(),
                        1 => {}
                        _ => self.error(Error::MultiScalarCharLit(self.span(start))),
                    }

                    return TokenKind::CharLit;
                }
                Some('#') if self.edition >= Edition::Rust2021 => {
                    if self.source(unticked) != "r" {
                        self.error(Error::ReservedPrefix(self.span(unticked)));
                        self.advance(); // `#`
                        return TokenKind::Error;
                    }

                    self.advance();

                    if !self.peek().is_some_and(is_ident_start) {
                        self.error(Error::InvalidRawTickedIdent(self.span(start)));
                        return TokenKind::Error;
                    }

                    let unprefixed = self.index();
                    self.advance_while(is_ident_middle);

                    if let TokenKind::Underscore = lex_ident(self.source(unprefixed), self.edition)
                    {
                        self.error(Error::InvalidRawTickedIdent(self.span(start)));
                    }

                    break;
                }
                _ => break,
            }
        }

        TokenKind::TickedIdent
    }

    fn fin_lex_char_lit(&mut self, flavor: TextLitFlavor, start: ByteIndex) -> TokenKind {
        let mut count = 0usize;
        let mut terminated = false;
        let mut has_invalid_escape_seqs = false;
        let mut invalid_scalar = None;

        while let Some((index, char)) = self.advance() {
            match char {
                '\\' => {
                    has_invalid_escape_seqs |= !self.fin_lex_escape_seq(TextLitKind::Char, flavor);
                }
                '\'' => {
                    terminated = true;
                    break;
                }
                '\n' | '\t' | '\r' => {
                    invalid_scalar.get_or_insert((char, self.span(index)));
                }
                _ if let TextLitFlavor::Ascii = flavor
                    && !char.is_ascii() =>
                {
                    invalid_scalar.get_or_insert((char, self.span(index)));
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
            if let Some((char, span)) = invalid_scalar
                && count == 1
            {
                self.error(Error::InvalidScalar(char, InvalidScalarPlace::Lit, span));
            }
        }

        TokenKind::CharLit
    }

    fn fin_lex_str_lit(&mut self, raw: Raw, flavor: TextLitFlavor, start: ByteIndex) -> TokenKind {
        let mut terminated = false;
        let mut invalid_scalar = None;

        while let Some((index, char)) = self.advance() {
            match char {
                '\\' if let Raw::No = raw => {
                    self.fin_lex_escape_seq(TextLitKind::Str, flavor);
                }
                '"' => {
                    terminated = true;
                    break;
                }
                '\r' => {
                    invalid_scalar.get_or_insert((char, self.span(index)));
                }
                _ if let TextLitFlavor::Ascii = flavor
                    && !char.is_ascii() =>
                {
                    invalid_scalar.get_or_insert((char, self.span(index)));
                }
                _ => {}
            }
        }

        if !terminated {
            self.error(Error::UnterminatedStrLit(self.span(start)));
        } else if let Some((char, span)) = invalid_scalar {
            self.error(Error::InvalidScalar(char, InvalidScalarPlace::Lit, span));
        }

        TokenKind::StrLit
    }

    fn fin_lex_ident_prefixed_token(&mut self, start: ByteIndex) -> TokenKind {
        self.advance_while(is_ident_middle);

        let ident = self.source(start);

        if let Some(TokenKind::CharLit | TokenKind::NumLit | TokenKind::StrLit) = self.previous {
            if let "_" = ident {
                self.error(Error::InvalidLitSuffix(self.span(start)));
            }
            return TokenKind::LitSuffix;
        }

        let (raw, flavor) = match (ident, self.peek()) {
            ("b", Some('"')) => (Raw::No, TextLitFlavor::Ascii),
            ("br", Some('"')) => (Raw::Yes, TextLitFlavor::Ascii),
            ("c", Some('"')) if self.edition >= Edition::Rust2021 => (Raw::No, TextLitFlavor::C),
            ("cr", Some('"')) if self.edition >= Edition::Rust2021 => (Raw::Yes, TextLitFlavor::C),
            ("r", Some('"')) => (Raw::Yes, TextLitFlavor::Utf8),
            ("b", Some('\'')) => {
                self.advance();
                return self.fin_lex_char_lit(TextLitFlavor::Ascii, start);
            }
            ("r", Some('#')) => {
                self.advance();

                let unprefixed = self.index();
                if self.peek().is_some_and(is_ident_start) {
                    self.advance_while(is_ident_middle);

                    if let PathSegKeyword!() | TokenKind::Underscore =
                        lex_ident(self.source(unprefixed), self.edition)
                    {
                        self.error(Error::InvalidRawIdent(self.span(start)));
                    }

                    return TokenKind::CommonIdent;
                }

                return self.fin_lex_raw_guarded_str_lit(start);
            }
            ("br", Some('#')) => {
                self.advance();
                return self.fin_lex_raw_guarded_str_lit(start);
            }
            ("cr", Some('#')) if self.edition >= Edition::Rust2021 => {
                self.advance();
                return self.fin_lex_raw_guarded_str_lit(start);
            }
            (_, Some(char @ ('"' | '\'' | '#'))) if self.edition >= Edition::Rust2021 => {
                self.error(Error::ReservedPrefix(self.span(start)));
                if char == '#' {
                    self.advance();
                }
                return TokenKind::Error;
            }
            _ => return lex_ident(ident, self.edition),
        };
        self.advance();
        self.fin_lex_str_lit(raw, flavor, start)
    }

    // FIXME: Consolidate with `fin_lex_str_lit` smh
    fn fin_lex_raw_guarded_str_lit(&mut self, start: ByteIndex) -> TokenKind {
        let mut terminated = false;
        let mut open = 1usize;

        while let Some('#') = self.peek() {
            self.advance();
            open += 1;
        }

        if let Some((index, char)) = self.advance()
            && char != '"'
        {
            self.error(Error::InvalidStrLitDelimiter(self.span(index)));
            return TokenKind::StrLit;
        }

        'outer: loop {
            while self.advance().is_some_and(|(_, char)| char != '"') {}

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

        TokenKind::StrLit
    }

    fn fin_lex_escape_seq(&mut self, kind: TextLitKind, flavor: TextLitFlavor) -> bool {
        let index = self.index();
        if !self.fin_lex_escape_seq_inner(kind, flavor) {
            self.error(Error::InvalidEscapeSequence(self.span(index)));
            return false;
        }
        true
    }

    // FIXME: Emit slightly more precise diagnostics & better diagnostic spans.
    fn fin_lex_escape_seq_inner(&mut self, kind: TextLitKind, flavor: TextLitFlavor) -> bool {
        let Some((_, char)) = self.advance() else { return false };

        match (char, kind, flavor) {
            | ('\\' | '"' | '\'' | 'n' | 'r' | 't', _, _)
            | ('0', _, TextLitFlavor::Utf8 | TextLitFlavor::Ascii)
            | ('\n', TextLitKind::Str, _) => true,
            ('x', _, _) => {
                let Some(char) = self.peek() else { return false };
                match flavor {
                    TextLitFlavor::Utf8 if is_oct_digit(char) => {}
                    TextLitFlavor::Ascii | TextLitFlavor::C if is_hex_digit(char) => {}
                    _ => return false,
                }
                self.advance();
                if !self.peek().is_some_and(is_hex_digit) {
                    return false;
                }
                self.advance();
                true
            }
            ('u', _, _) => {
                let Some((_, '{')) = self.advance() else { return false };

                let mut is_empty = true;
                let mut value = 0;

                while let Some(char) = self.peek() {
                    let sub = |x: char, y: char| x as u32 - y as u32;

                    let plus = match char {
                        '0'..='9' => sub(char, '0'),
                        'a'..='f' => sub(char, 'a') + 10,
                        'A'..='F' => sub(char, 'A') + 10,
                        '_' if !is_empty => {
                            self.advance();
                            continue;
                        }
                        '}' => {
                            self.advance();
                            break;
                        }
                        _ => return false,
                    };
                    value *= 16;
                    value += plus;

                    is_empty = false;
                    self.advance();
                }

                !is_empty && value <= 0x10_FFFF
            }
            _ => false,
        }
    }

    fn source(&self, start: ByteIndex) -> &'src str {
        &self.source[self.span(start).range()]
    }

    fn error(&self, error: Error) {
        self.errors.add(error);
    }
}

impl<'src> std::ops::Deref for Lexer<'_, 'src> {
    type Target = Cutter<'src>;

    fn deref(&self) -> &Self::Target {
        &self.cutter
    }
}

impl std::ops::DerefMut for Lexer<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cutter
    }
}

impl Iterator for Lexer<'_, '_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let (kind, start) = match self.advance() {
            Some((start, char)) => (self.fin_lex_token(char, start), start),
            None if let Some(TokenKind::EndOfInput) = self.previous => return None,
            None => (TokenKind::EndOfInput, self.index()),
        };
        self.previous = Some(kind);
        Some(Token::new(kind, Span::new(start, self.index())))
    }
}

#[derive(Clone, Copy)]
enum TextLitKind {
    Str,
    Char,
}

#[derive(Clone, Copy)]
enum TextLitFlavor {
    Utf8,
    Ascii,
    C,
}

#[derive(Clone, Copy)]
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

fn is_horizontal_whitespace(char: char) -> bool {
    matches!(char, '\t' | ' ')
}

fn is_ident_start(char: char) -> bool {
    char == '_' || unicode_ident::is_xid_start(char)
}

fn is_ident_middle(char: char) -> bool {
    unicode_ident::is_xid_continue(char)
}

fn is_bin_digit(char: char) -> bool {
    matches!(char, '0' | '1')
}

fn is_oct_digit(char: char) -> bool {
    matches!(char, '0'..='7')
}

#[expect(clippy::manual_is_ascii_check)] // this one is by value
fn is_dec_digit(char: char) -> bool {
    matches!(char, '0'..='9')
}

#[expect(clippy::manual_is_ascii_check)] // this one is by value
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
