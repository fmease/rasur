use super::{ExtPath, Ident, Lit, MacroCall, Mutability, ObligatorilyDisambiguatedGenericArgs};

#[derive(Debug)]
pub(crate) enum Pat<'src> {
    Borrow(Mutability, Box<Pat<'src>>),
    Grouped(Box<Pat<'src>>),
    Ident(IdentPat<'src>),
    Lit(Lit<'src>),
    MacroCall(MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>),
    Or(Box<Pat<'src>>, Box<Pat<'src>>),
    Path(Box<ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Range(Option<Box<Pat<'src>>>, Option<Box<Pat<'src>>>, RangePatKind),
    Slice(Vec<Pat<'src>>),
    Struct(Box<StructPat<'src>>),
    Tup(Vec<Pat<'src>>),
    TupleStruct(Box<TupleStructPat<'src>>),
    Wildcard,
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
    pub(crate) path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub(crate) fields: Vec<Pat<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructPat<'src> {
    pub(crate) path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub(crate) fields: Vec<StructPatField<'src>>,
    pub(crate) rest: bool,
}

#[derive(Debug)]
pub(crate) struct StructPatField<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) body: Option<Pat<'src>>,
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
