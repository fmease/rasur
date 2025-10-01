use super::{ExtPath, Ident, Lit, MacroCall, Mutability, ObligatorilyDisambiguatedGenericArgs};

#[derive(Debug)]
pub(crate) enum Pat<'src> {
    Ident(IdentPat<'src>),
    Wildcard,
    Lit(Lit<'src>),
    Tup(Vec<Pat<'src>>),
    Borrow(Mutability, Box<Pat<'src>>),
    Grouped(Box<Pat<'src>>),
    Path(Box<ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>>),
    MacroCall(MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>),
    TupleStruct(Box<TupleStructPat<'src>>),
    Struct(Box<StructPat<'src>>),
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
