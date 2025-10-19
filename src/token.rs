use crate::span::Span;
use std::fmt;

#[derive(Clone, Copy)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
}

impl Token {
    pub(crate) const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind, self.span)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TokenKind {
    AmpersandEquals,
    AsteriskEquals,
    At,
    BangEquals,
    BlockComment,
    CaretEquals,
    CharLit,
    CloseCurlyBracket,
    CloseRoundBracket,
    CloseSquareBracket,
    Comma,
    DoubleAmpersand,
    DoubleColon,
    DoubleDot,
    DoubleDotEquals,
    DoubleEquals,
    DoubleGreaterThan,
    DoubleGreaterThanEquals,
    DoubleLessThan,
    DoubleLessThanEquals,
    DoublePipe,
    EndOfInput,
    Error,
    GreaterThanEquals,
    Hash,
    HypenEquals,
    Ident,
    LessThanEquals,
    Lifetime,
    LineComment,
    NumLit,
    OpenCurlyBracket,
    OpenRoundBracket,
    OpenSquareBracket,
    PercentEquals,
    PipeEquals,
    PlusEquals,
    QuestionMark,
    Semicolon,
    SingleAmpersand,
    SingleAsterisk,
    SingleBang,
    SingleCaret,
    SingleColon,
    SingleDot,
    SingleEquals,
    SingleGreaterThan,
    SingleHyphen,
    SingleLessThan,
    SinglePercent,
    SinglePipe,
    SinglePlus,
    SingleSlash,
    SlashEquals,
    StrLit,
    ThinArrow,
    TripleDot,
    Whitespace,
    WideArrow,
}

impl TokenKind {
    pub(crate) fn repr(self) -> Repr {
        match self {
            Self::AmpersandEquals => Repr::Src("&="),
            Self::AsteriskEquals => Repr::Src("*="),
            Self::At => Repr::Src("@"),
            Self::BangEquals => Repr::Src("!="),
            Self::BlockComment => Repr::Tag("block comment"),
            Self::CaretEquals => Repr::Src("^="),
            Self::CharLit => Repr::Tag("char literal"),
            Self::CloseCurlyBracket => Repr::Src("}"),
            Self::CloseRoundBracket => Repr::Src(")"),
            Self::CloseSquareBracket => Repr::Src("]"),
            Self::Comma => Repr::Src(","),
            Self::DoubleAmpersand => Repr::Src("&&"),
            Self::DoubleColon => Repr::Src("::"),
            Self::DoubleDot => Repr::Src(".."),
            Self::DoubleDotEquals => Repr::Src("..="),
            Self::DoubleEquals => Repr::Src("=="),
            Self::DoubleGreaterThan => Repr::Src(">>"),
            Self::DoubleGreaterThanEquals => Repr::Src(">>="),
            Self::DoubleLessThan => Repr::Src("<<"),
            Self::DoubleLessThanEquals => Repr::Src("<<="),
            Self::DoublePipe => Repr::Src("||"),
            Self::EndOfInput => Repr::Tag("end of input"),
            Self::Error => Repr::Tag("error"),
            Self::GreaterThanEquals => Repr::Src(">="),
            Self::Hash => Repr::Src("#"),
            Self::HypenEquals => Repr::Src("-="),
            Self::Ident => Repr::Tag("identifier"),
            Self::LessThanEquals => Repr::Src("`<=`"),
            Self::Lifetime => Repr::Tag("lifetime"),
            Self::LineComment => Repr::Tag("line comment"),
            Self::NumLit => Repr::Tag("number literal"),
            Self::OpenCurlyBracket => Repr::Src("{"),
            Self::OpenRoundBracket => Repr::Src("("),
            Self::OpenSquareBracket => Repr::Src("["),
            Self::PercentEquals => Repr::Src("%="),
            Self::PipeEquals => Repr::Src("|="),
            Self::SinglePlus => Repr::Src("+"),
            Self::PlusEquals => Repr::Src("+="),
            Self::QuestionMark => Repr::Src("?"),
            Self::Semicolon => Repr::Src(";"),
            Self::SingleAmpersand => Repr::Src("&"),
            Self::SingleAsterisk => Repr::Src("*"),
            Self::SingleBang => Repr::Src("!"),
            Self::SingleCaret => Repr::Src("^"),
            Self::SingleColon => Repr::Src(":"),
            Self::SingleDot => Repr::Src("."),
            Self::SingleEquals => Repr::Src("="),
            Self::SingleGreaterThan => Repr::Src(">"),
            Self::SingleHyphen => Repr::Src("-"),
            Self::SingleLessThan => Repr::Src("<"),
            Self::SinglePercent => Repr::Src("%"),
            Self::SinglePipe => Repr::Src("|"),
            Self::SingleSlash => Repr::Src("/"),
            Self::SlashEquals => Repr::Src("/="),
            Self::StrLit => Repr::Tag("string literal"),
            Self::ThinArrow => Repr::Src("->"),
            Self::TripleDot => Repr::Src("..."),
            Self::Whitespace => Repr::Tag("whitespace"),
            Self::WideArrow => Repr::Src("=>"),
        }
    }
}

pub(crate) enum Repr {
    Src(&'static str),
    Tag(&'static str),
}
