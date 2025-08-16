use super::{Expr, ExtPath, Ident, MacroCall, Mutability, Path, UnambiguousGenericArgs};

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Never,
    Inferred,
    DynTrait(Vec<Bound<'src>>),
    FnPtr((), Option<Box<Ty<'src>>>),
    ImplTrait(Vec<Bound<'src>>),
    Path(Box<ExtPath<'src, UnambiguousGenericArgs>>),
    Ref(Option<Lifetime<'src>>, Mutability, Box<Ty<'src>>),
    Ptr(Mutability, Box<Ty<'src>>),
    Array(Box<Ty<'src>>, Expr<'src>),
    Slice(Box<Ty<'src>>),
    Tup(Vec<Ty<'src>>),
    Grouped(Box<Ty<'src>>),
    MacroCall(MacroCall<'src, UnambiguousGenericArgs>),
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
    Ty { bounds: Vec<Bound<'src>>, default: Option<Ty<'src>> },
    Const { ty: Ty<'src>, default: Option<Expr<'src>> },
    Lifetime(Vec<Lifetime<'src>>),
}

#[derive(Debug)]
pub(crate) enum Predicate<'src> {
    Trait(TraitPredicate<'src>),
    Outlives(OutlivesPredicate<'src>),
}

#[derive(Debug)]
pub(crate) struct TraitPredicate<'src> {
    pub(crate) bound_vars: Vec<GenericParam<'src>>,
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
    Trait {
        bound_vars: Vec<GenericParam<'src>>,
        modifiers: TraitBoundModifiers,
        trait_ref: Path<'src, UnambiguousGenericArgs>,
    },
    Outlives(Lifetime<'src>),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TraitBoundModifiers {
    pub(crate) constness: BoundConstness,
    // FIXME: pub(crate) asyncness: BoundAsyncness,
    pub(crate) polarity: BoundPolarity,
}

impl TraitBoundModifiers {
    pub(crate) const NONE: Self =
        Self { constness: BoundConstness::Never, polarity: BoundPolarity::Positive };
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum BoundConstness {
    Never,
    Maybe,
    Always,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum BoundPolarity {
    Positive,
    Negative,
    Maybe,
}
