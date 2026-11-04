//! Weak keywords.

// FIXME: This is so janky: Some weak keywords come with "inherent predicates" & some don't.
//        For the latter kind, it's expected that the caller checks for the necessary
//        conditions.

use super::{Edition, MatchAgainstArbitraryToken, Parser, Token, TokenKind, TokenPrefix};

pub(super) trait Weak: Copy {
    const STR: &str;

    fn check(self, p: &Parser<'_, '_, '_>) -> bool {
        matches!(p.token.kind, TokenKind::CommonIdent | TokenKind::StroppedIdent)
            && p.keyword(p.token.span) == Self::STR
            && self.qualifies(p)
    }

    fn matches(self, token: Token, p: &Parser<'_, '_, '_>) -> bool
    where
        Self: MatchAgainstArbitraryToken,
    {
        matches!(token.kind, TokenKind::CommonIdent | TokenKind::StroppedIdent)
            && p.keyword(token.span) == Self::STR
    }

    fn qualifies(self, _: &Parser<'_, '_, '_>) -> bool {
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
                    fn qualifies(self, p: &Parser<'_, '_, '_>) -> bool {
                        ($qualifies as fn(&Parser<'_, '_, '_>) -> _)(p)
                    }
                )?
            }

            impl $( ${ignore($qualifies)} ! )? MatchAgainstArbitraryToken for $ty {}
        )+
    };
}

weak! {
    // Alternatively, we could check `$kind.is_ident() && $kind != TokenKind::As`
    Auto "auto" |p| matches!(p.peek(1).kind, TokenKind::Trait | TokenKind::Impl),
    Bikeshed "bikeshed",
    ContractEnsures "contract_ensures",
    ContractRequires "contract_requires",
    Default "default" |p| p.peek(1).kind.is_ident(),
    Dyn "dyn" |p| p.edition == Edition::Rust2015 && p.begins_2015_dyn_bound(p.peek(1)),
    MacroRules "macro_rules" |p|
        p.peek(1).kind == TokenKind::SingleBang
            && p.peek(2).kind == TokenKind::CommonIdent,
    Pin "pin",
    Raw "raw",
    // NOTE: This check isn't precise enough. See upstream issue:
    //       <https://github.com/rust-lang/rust/issues/148238>
    Reuse "reuse" |p| {
        let token = p.peek(1);
        token.kind.is_ident()
            || p.matches(TokenPrefix::LessThan, token)
            && p.begins_ty(2)
    },
    // Alternatively, we could check `$kind.is_ident() && $kind != TokenKind::As`
    Safe "safe" |p| matches!(p.peek(1).kind, TokenKind::Extern | TokenKind::Fn | TokenKind::Static),
    Union "union" |p| p.peek(1).kind == TokenKind::CommonIdent,
    Yeet "yeet",
}
