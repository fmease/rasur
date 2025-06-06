use super::{Expr, ExtPath, GenericArgsPolicy, Ident, MacroCall, Mutable, Path};

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Never,
    Inferred,
    DynTrait(Vec<Bound<'src>>),
    FnPtr((), Option<Box<Ty<'src>>>),
    ImplTrait(Vec<Bound<'src>>),
    Path(Box<ExtPath<'src, GenericArgsPolicy::Allowed>>),
    Ref(Option<Lifetime<'src>>, Mutable, Box<Ty<'src>>),
    Ptr(Mutable, Box<Ty<'src>>),
    Array(Box<Ty<'src>>, Expr<'src>),
    Slice(Box<Ty<'src>>),
    Tup(Vec<Ty<'src>>),
    Grouped(Box<Ty<'src>>),
    MacroCall(MacroCall<'src, GenericArgsPolicy::Allowed>),
    Error,
}

#[derive(Debug)]
pub(crate) struct Lifetime<'src>(pub(crate) Ident<'src>);

#[derive(Debug)]
pub(crate) struct Generics<'src> {
    pub(crate) params: Vec<GenericParam<'src>>,
    pub(crate) preds: Vec<Predicate<'src>>,
}

#[derive(Debug)]
pub(crate) struct GenericParam<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) kind: GenericParamKind<'src>,
}

#[derive(Debug)]
pub(crate) enum GenericParamKind<'src> {
    Ty(Vec<Bound<'src>>),
    Const(Ty<'src>),
    Lifetime(Vec<Lifetime<'src>>),
}

#[derive(Debug)]
pub(crate) enum Predicate<'src> {
    Trait(TraitPredicate<'src>),
    Outlives(OutlivesPredicate<'src>),
}

#[derive(Debug)]
pub(crate) struct TraitPredicate<'src> {
    pub(crate) ty: Ty<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
}

#[derive(Debug)]
pub(crate) struct OutlivesPredicate<'src> {
    pub(crate) lt: Lifetime<'src>,
    pub(crate) bounds: Vec<Lifetime<'src>>,
}

#[derive(Debug)]
pub(crate) enum Bound<'src> {
    Trait(TraitBoundModifiers, Path<'src, GenericArgsPolicy::Allowed>),
    Outlives(Lifetime<'src>),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TraitBoundModifiers {
    pub(crate) polarity: BoundPolarity,
    // constness
    // asyncness
}

impl TraitBoundModifiers {
    pub(crate) const NONE: Self = Self { polarity: BoundPolarity::Positive };
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum BoundPolarity {
    Positive,
    Negative,
    Maybe,
}
