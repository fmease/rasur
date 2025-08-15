use super::{ExtPath, GenericArgsPolicy, Ident, MacroCall, Mutability};

#[derive(Debug)]
pub(crate) enum Pat<'src> {
    Ident(IdentPat<'src>),
    Wildcard,
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Tup(Vec<Pat<'src>>),
    Borrow(Mutability, Box<Pat<'src>>),
    Grouped(Box<Pat<'src>>),
    Path(Box<ExtPath<'src, GenericArgsPolicy::DisambiguatedOnly>>),
    MacroCall(MacroCall<'src, GenericArgsPolicy::DisambiguatedOnly>),
    TupleStruct(Box<TupleStructPat<'src>>),
    Or(Box<Pat<'src>>, Box<Pat<'src>>),
    Range(Option<Box<Pat<'src>>>, Option<Box<Pat<'src>>>, RangePatKind),
}

// FIXME: I hate this name
#[derive(Debug)]
pub(crate) struct IdentPat<'src> {
    pub(crate) mut_: Mutability,
    pub(crate) by_ref: ByRef,
    pub(crate) ident: Ident<'src>,
}

#[derive(Debug)]
pub(crate) enum ByRef {
    Yes(Mutability),
    No,
}

#[derive(Debug)]
pub(crate) struct TupleStructPat<'src> {
    pub(crate) path: ExtPath<'src, GenericArgsPolicy::DisambiguatedOnly>,
    pub(crate) fields: Vec<Pat<'src>>,
}

#[derive(Debug)]
pub(crate) enum RangePatKind {
    Inclusive(RangeInclusivePatKind),
    Exclusive,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum RangeInclusivePatKind {
    Normal,
    Legacy,
}
