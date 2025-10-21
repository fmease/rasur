use crate::edition::Edition;

#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(test, derive(Debug))]
pub(crate) enum Keyword {
    Abstract,
    As,
    Async,
    Auto,
    Await,
    Become,
    Box,
    Break,
    Const,
    Continue,
    Crate,
    Do,
    Dyn,
    Else,
    Enum,
    Extern,
    False,
    Final,
    Fn,
    For,
    Gen,
    If,
    Impl,
    In,
    Let,
    Loop,
    Macro,
    Match,
    Mod,
    Move,
    Mut,
    Override,
    Pin,
    Priv,
    Pub,
    Ref,
    Return,
    Safe,
    SelfLower,
    SelfUpper,
    Static,
    Struct,
    Super,
    Trait,
    True,
    Try,
    Type,
    Typeof,
    Underscore,
    Union,
    Unsafe,
    Use,
    Virtual,
    Where,
    While,
    Yield,
}

macro_rules! map {
    ($( $str:literal <=> $keyword:ident ),+ $(,)?) => {
        impl Keyword {
            fn from_str(source: &str) -> Option<Self> {
                Some(match source {
                    $( $str => Self::$keyword, )+
                    _ => return None,
                })
            }

            pub(crate) fn to_str(self) -> &'static str {
                match self {
                    $( Self::$keyword => $str ),+
                }
            }
        }
    };
}

map! {
    "Self" <=> SelfUpper,
    "_" <=> Underscore,
    "abstract" <=> Abstract,
    "as" <=> As,
    "async" <=> Async,
    "auto" <=> Auto,
    "await" <=> Await,
    "become" <=> Become,
    "box" <=> Box,
    "break" <=> Break,
    "const" <=> Const,
    "continue" <=> Continue,
    "crate" <=> Crate,
    "do" <=> Do,
    "dyn" <=> Dyn,
    "else" <=> Else,
    "enum" <=> Enum,
    "extern" <=> Extern,
    "false" <=> False,
    "final" <=> Final,
    "fn" <=> Fn,
    "for" <=> For,
    "gen" <=> Gen,
    "if" <=> If,
    "impl" <=> Impl,
    "in" <=> In,
    "let" <=> Let,
    "loop" <=> Loop,
    "macro" <=> Macro,
    "match" <=> Match,
    "mod" <=> Mod,
    "move" <=> Move,
    "mut" <=> Mut,
    "override" <=> Override,
    "pin" <=> Pin,
    "priv" <=> Priv,
    "pub" <=> Pub,
    "ref" <=> Ref,
    "return" <=> Return,
    "safe" <=> Safe,
    "self" <=> SelfLower,
    "static" <=> Static,
    "struct" <=> Struct,
    "super" <=> Super,
    "trait" <=> Trait,
    "true" <=> True,
    "try" <=> Try,
    "type" <=> Type,
    "typeof" <=> Typeof,
    "union" <=> Union,
    "unsafe" <=> Unsafe,
    "use" <=> Use,
    "virtual" <=> Virtual,
    "where" <=> Where,
    "while" <=> While,
    "yield" <=> Yield,
}

impl Keyword {
    pub(crate) fn parse(source: &str, edition: Edition, quality: Quality) -> Option<Self> {
        Self::from_str(source).filter(|keyword| edition >= keyword.edition()).filter(|keyword| {
            match quality {
                Quality::Hard => !keyword.is_soft(),
                Quality::Any => true,
            }
        })
    }

    fn edition(self) -> Edition {
        match self {
            Self::Async | Self::Await | Self::Dyn | Self::Try => Edition::Rust2018,
            Self::Gen => Edition::Rust2024,
            _ => Edition::Rust2015,
        }
    }

    fn is_soft(self) -> bool {
        matches!(self, Self::Auto | Self::Pin | Self::Safe | Self::Union)
    }

    pub(crate) fn is_path_seg(self) -> bool {
        matches!(self, Self::Crate | Self::Super | Self::SelfLower | Self::SelfUpper)
    }
}

pub(crate) enum Quality {
    Hard,
    Any,
}
