//! Weak keywords.

use super::{Edition, MatchAgainstArbitraryToken, Parser, Token, TokenKind, TokenPrefix};

pub(super) trait Weak: Copy {
    const STR: &str;

    fn check(self, p: &Parser<'_, '_, '_>) -> bool {
        p.token.kind == TokenKind::CommonIdent
            && p.source(p.token.span) == Self::STR
            && self.qualifies(p)
    }

    fn matches(self, token: Token, p: &Parser<'_, '_, '_>) -> bool
    where
        Self: MatchAgainstArbitraryToken,
    {
        token.kind == TokenKind::CommonIdent && p.source(token.span) == Self::STR
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
    // FIXME: Do we want to generalize this to `is_ident`?
    Auto "auto" |p| p.peek(1).kind == TokenKind::Trait,
    Bikeshed "bikeshed",
    Builtin "builtin" |p| p.peek(1).kind == TokenKind::Hash,
    ContractEnsures "contract_ensures",
    ContractRequires "contract_requires",
    Default "default" |p| p.peek(1).kind.is_ident(),
    Deref "deref",
    Dyn "dyn" |p| p.edition == Edition::Rust2015 && p.begins_2015_dyn_bound(p.peek(1)),
    MacroRules "macro_rules" |p|
        p.peek(1).kind == TokenKind::SingleBang
            && p.peek(2).kind == TokenKind::CommonIdent,
    OffsetOf "offset_of",
    Pin "pin",
    Raw "raw",
    // NOTE: This check isn't precise enough. See upstream issue:
    //       <https://github.com/rust-lang/rust/issues/148238>
    Reuse "reuse" |p| {
        let token = p.peek(1);
        token.kind.is_ident()
            || TokenPrefix::LessThan.matches(token.kind)
            && p.begins_ty(2)
    },
    // FIXME: Do we want to generalize this to `is_ident`?
    Safe "safe" |p| matches!(p.peek(1).kind, TokenKind::Extern | TokenKind::Fn | TokenKind::Static),
    TypeAscribe "type_ascribe",
    Union "union" |p| p.peek(1).kind == TokenKind::CommonIdent,
    UnwrapBinder "unwrap_binder",
    WrapBinder "wrap_binder",
    Yeet "yeet",
}
