use super::{
    Attr, BorrowKind, Expr, ExtPath, Externness, FnParam, Ident, MacroCall, Mutability, Path,
    Safety, UnambiguousGenericArgs,
};
use crate::span::Span;

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Array(Box<Ty<'src>>, Expr<'src>),
    CVariadics,
    DynTrait(DynKind, Vec<Bound<'src>>),
    // FIXME: Get rid of this payload once `Ty` carries a `Span`
    Error(Span),
    FnPtr(Box<FnPtrTy<'src>>),
    Grouped(Box<Ty<'src>>),
    ImplTrait(Vec<Bound<'src>>),
    Inferred,
    MacroCall(MacroCall<'src, UnambiguousGenericArgs>),
    Never,
    Path(Box<ExtPath<'src, UnambiguousGenericArgs>>),
    Ptr(Mutability, Box<Ty<'src>>),
    Ref(Box<RefTy<'src>>),
    Slice(Box<Ty<'src>>),
    Tuple(Vec<Ty<'src>>),
    UnsafeBinder(Vec<GenericParam<'src>>, Box<Ty<'src>>),
}

#[derive(Debug)]
pub(crate) enum DynKind {
    Dyn,
    Bare,
}

#[derive(Debug)]
pub(crate) struct FnPtrTy<'src> {
    pub(crate) bound_vars: Vec<GenericParam<'src>>,
    pub(crate) modifiers: FnPtrTyModifiers<'src>,
    pub(crate) inputs: Vec<FnParam<'src>>,
    pub(crate) output: Option<Ty<'src>>,
}

#[derive(Default, Debug)]
pub(crate) struct FnPtrTyModifiers<'src> {
    pub(crate) safety: Safety<()>,
    pub(crate) externness: Externness<'src>,
}

#[derive(Debug)]
pub(crate) struct RefTy<'src> {
    pub(crate) lt: Option<Ident<'src>>,
    pub(crate) kind: BorrowKind<!>,
    pub(crate) mut_: Mutability,
    pub(crate) pointee: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct Generics<'src> {
    pub(crate) params: Vec<GenericParam<'src>>,
    pub(crate) preds: Vec<Predicate<'src>>,
}

#[derive(Debug)]
pub(crate) struct GenericParam<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) binder: Ident<'src>,
    pub(crate) kind: GenericParamKind<'src>,
}

#[derive(Debug)]
pub(crate) enum GenericParamKind<'src> {
    Ty { bounds: Vec<Bound<'src>>, default: Option<Ty<'src>> },
    Const { ty: Ty<'src>, default: Option<Expr<'src>> },
    Lifetime(Vec<Ident<'src>>),
}

#[derive(Debug)]
pub(crate) struct Predicate<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) kind: PredicateKind<'src>,
}

#[derive(Debug)]
pub(crate) enum PredicateKind<'src> {
    Trait(TraitPredicate<'src>),
    Outlives(OutlivesPredicate<'src>),
    Equality(Ty<'src>, Ty<'src>),
}

#[derive(Debug)]
pub(crate) struct TraitPredicate<'src> {
    pub(crate) bound_vars: Vec<GenericParam<'src>>,
    pub(crate) ty: Ty<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
}

#[derive(Debug)]
pub(crate) struct OutlivesPredicate<'src> {
    pub(crate) lt: Ident<'src>,
    pub(crate) bounds: Vec<Ident<'src>>,
}

#[derive(Debug)]
pub(crate) enum Bound<'src> {
    Outlives(Ident<'src>),
    Use(Vec<Ident<'src>>),
    Trait {
        // FIXME: Make this more type-safe: binders are
        //        incompatible with non-normal polarity
        bound_vars: Vec<GenericParam<'src>>,
        modifiers: TraitBoundModifiers,
        path: Path<'src, UnambiguousGenericArgs>,
    },
}

impl<'src> From<Path<'src, UnambiguousGenericArgs>> for Bound<'src> {
    fn from(path: Path<'src, UnambiguousGenericArgs>) -> Self {
        Self::Trait { bound_vars: Vec::new(), modifiers: TraitBoundModifiers::NONE, path }
    }
}

// FIXME: Make this more type-safe: non-normal polarity is
//        incompatible with constness & asyncness
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct TraitBoundModifiers {
    pub(crate) constness: BoundConstness,
    pub(crate) asyncness: BoundAsyncness,
    pub(crate) polarity: BoundPolarity,
}

impl TraitBoundModifiers {
    pub(crate) const NONE: Self = Self {
        constness: BoundConstness::Never,
        asyncness: BoundAsyncness::Never,
        polarity: BoundPolarity::Positive,
    };
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) enum BoundConstness {
    Never,
    Maybe,
    Always,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) enum BoundAsyncness {
    Never,
    Always,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) enum BoundPolarity {
    Positive,
    Negative,
    Maybe,
}
