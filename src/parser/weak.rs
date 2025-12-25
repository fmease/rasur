//! Weak keywords.

use super::{Edition, MatchAgainstArbitraryToken, Parser, Token, TokenKind, TokenPrefix};

pub(super) trait Weak: Copy {
    const STR: &str;

    fn check(self, p: &Parser<'_, '_>) -> bool {
        p.token.kind == TokenKind::CommonIdent
            && p.source(p.token.span) == Self::STR
            && self.qualifies(p)
    }

    fn matches(self, token: Token, p: &Parser<'_, '_>) -> bool
    where
        Self: MatchAgainstArbitraryToken,
    {
        token.kind == TokenKind::CommonIdent && p.source(token.span) == Self::STR
    }

    fn qualifies(self, _: &Parser<'_, '_>) -> bool {
        true
    }
}

macro_rules! weak {
    ($( $ty:ident $str:literal $($qualifies:expr)?, )+) => {
        $(
            #[derive(Clone, Copy)]
            pub(super) struct $ty;
            impl Weak for $ty {
                const STR: &str = $str;
                $(
                    fn qualifies(self, p: &Parser<'_, '_>) -> bool {
                        ($qualifies as fn(&Parser<'_, '_>) -> _)(p)
                    }
                )?
            }
            impl $( ${ignore($qualifies)} ! )? MatchAgainstArbitraryToken for $ty {}
        )+
    };
}

weak! {
    // FIXME: Do we want to generalize this to `is_ident`?
    Auto "auto" |p| p.look_ahead(1, |t| t.kind == TokenKind::Trait),
    Bikeshed "bikeshed",
    Builtin "builtin" |p| p.look_ahead(1, |t| t.kind == TokenKind::Hash),
    ContractEnsures "contract_ensures",
    ContractRequires "contract_requires",
    Default "default" |p| p.look_ahead(1, |t| t.kind.is_ident()),
    Dyn "dyn" |p| p.edition == Edition::Rust2015 && p.look_ahead(1, |t| p.begins_2015_dyn_bound(t)),
    MacroRules "macro_rules" |p|
        p.look_ahead(1, |t| t.kind == TokenKind::SingleBang)
            && p.look_ahead(2, |t| t.kind == TokenKind::CommonIdent),
    Pin "pin",
    Raw "raw",
    // NOTE: This check isn't precise enough. See upstream issue:
    //       <https://github.com/rust-lang/rust/issues/148238>
    Reuse "reuse" |p| p.look_ahead(1, |t| {
        t.kind.is_ident()
            || TokenPrefix::LessThan.matches(t.kind)
            && p.look_ahead(2, |t| p.begins_ty(t))
    }),
    // FIXME: Do we want to generalize this to `is_ident`?
    Safe "safe" |p| p.look_ahead(1, |t| matches!(t.kind, TokenKind::Extern | TokenKind::Fn | TokenKind::Static)),
    TypeAscribe "type_ascribe",
    Union "union" |p| p.look_ahead(1, |t| t.kind == TokenKind::CommonIdent),
    Yeet "yeet",
}
