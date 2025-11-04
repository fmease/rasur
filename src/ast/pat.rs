use super::{
    Attr, ExtPath, Ident, Lit, MacroCall, Mutability, ObligatorilyDisambiguatedGenericArgs,
};

#[derive(Debug)]
pub(crate) enum Pat<'src> {
    Binding(Box<BindingPat<'src>>),
    Borrow(Mutability, Box<Pat<'src>>),
    Box(Box<Pat<'src>>),
    Grouped(Box<Pat<'src>>),
    Lit(Sign, Lit<'src>),
    MacroCall(Box<MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Never,
    Or(Box<Pat<'src>>, Box<Pat<'src>>),
    Path(Box<ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Range(Option<Box<Pat<'src>>>, Option<Box<Pat<'src>>>, RangePatKind),
    Slice(Vec<Pat<'src>>),
    Struct(Box<StructPat<'src>>),
    Tuple(Vec<Pat<'src>>),
    TupleStruct(Box<TupleStructPat<'src>>),
    Wildcard(WildcardKind),
}

#[derive(Debug)]
pub(crate) enum Sign {
    None,
    Neg,
}

#[derive(Debug)]
pub(crate) enum WildcardKind {
    Normal,
    Empty,
}

#[derive(Debug)]
pub(crate) struct BindingPat<'src> {
    pub(crate) mut_: Mutability,
    pub(crate) by_ref: ByRef,
    pub(crate) binder: Ident<'src>,
    pub(crate) pat: Option<Box<Pat<'src>>>,
}

#[derive(Clone, Copy, Debug)]
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
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) mut_: Mutability,
    pub(crate) by_ref: ByRef,
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
