use super::{ExtPath, GenericArgsPolicy, Ident, MacroCall, Mutable};

#[derive(Debug)]
pub(crate) enum Pat<'src> {
    Path(Box<ExtPath<'src, GenericArgsPolicy::DisambiguatedOnly>>),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Wildcard,
    Tup(Vec<Pat<'src>>),
    Borrow(Mutable, Box<Pat<'src>>),
    Grouped(Box<Pat<'src>>),
    MacroCall(MacroCall<'src, GenericArgsPolicy::DisambiguatedOnly>),
}
