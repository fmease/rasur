use super::{
    Attr, BorrowKind, Expr, ExtPath, Ident, Lit, MacroCall, Mut,
    ObligatorilyDisambiguatedGenericArgs,
};
use crate::span::Span;

#[derive(Debug)]
pub struct Pat<'src> {
    pub kind: PatKind<'src>,
    pub span: Span,
}

#[derive(Debug)]
pub enum PatKind<'src> {
    Binding(Box<BindingPat<'src>>),
    Borrow(BorrowKind<!>, Mut, Box<Pat<'src>>),
    Deref(Box<Pat<'src>>),
    Error(Span),
    Grouped(Span, Box<Pat<'src>>),
    Guarded(Box<Pat<'src>>, Box<Expr<'src>>),
    Lit(Sign, Box<Lit<'src>>),
    MacroCall(Box<MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Never,
    Or(Box<Pat<'src>>, Box<Pat<'src>>),
    Path(Box<ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Rest,
    Range(Option<Box<RangePatBound<'src>>>, Option<Box<RangePatBound<'src>>>, RangePatKind),
    Slice(Vec<Pat<'src>>),
    Struct(Box<StructPat<'src>>),
    Tuple(Vec<Pat<'src>>),
    TupleStruct(Box<TupleStructPat<'src>>),
    Wildcard,
}

#[derive(Debug)]
pub enum Sign {
    None,
    Neg,
}

#[derive(Debug)]
pub struct BindingPat<'src> {
    pub mut_: Mut = Mut::No,
    pub by_ref: ByRef = ByRef::No,
    pub binder: Ident<'src>,
    pub pat: Option<Box<Pat<'src>>> = None,
}

#[derive(Clone, Copy, Debug)]
pub enum ByRef {
    Yes(BorrowKind<!>, Mut),
    No,
}

#[derive(Debug)]
pub struct TupleStructPat<'src> {
    pub path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub fields: Vec<Pat<'src>>,
}

#[derive(Debug)]
pub struct StructPat<'src> {
    pub path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub fields: Vec<StructPatField<'src>>,
    pub rest: bool,
}

#[derive(Debug)]
pub struct StructPatField<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub binder: Option<Ident<'src>>,
    pub body: Pat<'src>,
}

#[derive(Debug)]
pub enum RangePatBound<'src> {
    Lit(Sign, Box<Lit<'src>>),
    Path(ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>),
}

#[derive(Debug)]
pub enum RangePatKind {
    Exclusive,
    Inclusive { legacy: bool },
}
