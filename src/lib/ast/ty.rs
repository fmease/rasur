use super::{
    Attr, BorrowKind, Expr, ExtPath, Extern, FnParam, Ident, MacroCall, Mut, Path, Safety,
    UnambiguousGenericArgs,
};
use crate::span::Span;

#[derive(Debug)]
pub enum Ty<'src> {
    /// The `..` in `impl Trait for .. {}`, formerly called *default impls*[^1], an ancient predecessor to auto traits.
    ///
    /// This used to be part of the now removed unstable feature `optin_builtin_traits`.
    /// Upstream issue that tracks its planned removal: <https://github.com/rust-lang/rust/issues/154045>.
    ///
    /// [^1]: Not to be confused with specialization's default impls `default impl Trait for Type {}`.
    All,
    Array(Box<Ty<'src>>, Expr<'src>),
    CVariadics,
    DynTrait(DynKind, Vec<Bound<'src>>),
    Error(Span),
    FieldOf(Box<Ty<'src>>, Option<Ident<'src>>, Ident<'src>),
    FnPtr(Box<FnPtrTy<'src>>),
    Grouped(Box<Ty<'src>>),
    ImplTrait(Vec<Bound<'src>>),
    ImplicitSelf,
    Inferred,
    MacroCall(MacroCall<'src, UnambiguousGenericArgs>),
    Never,
    Path(Box<ExtPath<'src, UnambiguousGenericArgs>>),
    Ptr(Mut, Box<Ty<'src>>),
    Ref(Box<RefTy<'src>>),
    Slice(Box<Ty<'src>>),
    Tuple(Vec<(Vec<Attr<'src>>, Ty<'src>)>),
    UnsafeBinder(Vec<GenericParam<'src>>, Box<Ty<'src>>),
}

#[derive(Debug)]
pub enum DynKind {
    Dyn,
    Bare,
}

#[derive(Debug)]
pub struct FnPtrTy<'src> {
    pub bound_vars: Vec<GenericParam<'src>>,
    pub modifiers: FnPtrTyModifiers<'src>,
    pub inputs: Vec<FnParam<'src>>,
    pub output: Option<Ty<'src>>,
}

#[derive(Default, Debug)]
pub struct FnPtrTyModifiers<'src> {
    pub safety: Safety<()>,
    pub extern_: Extern<'src>,
}

#[derive(Debug)]
pub struct RefTy<'src> {
    pub lt: Option<Lifetime<'src>>,
    pub kind: BorrowKind<!>,
    pub mut_: Mut,
    pub pointee: Ty<'src>,
    pub view: Option<Vec<Ident<'src>>>,
}

#[derive(Debug)]
pub struct Generics<'src> {
    pub params: Vec<GenericParam<'src>>,
    pub preds: Vec<Predicate<'src>>,
}

#[derive(Debug)]
pub struct GenericParam<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub binder: Ident<'src>,
    pub kind: GenericParamKind<'src>,
}

#[derive(Debug)]
pub enum GenericParamKind<'src> {
    Ty { bounds: Vec<Bound<'src>>, default: Option<Ty<'src>> },
    Const { ty: Ty<'src>, default: Option<Expr<'src>> },
    Lifetime(Vec<Lifetime<'src>>),
}

#[derive(Debug)]
pub struct Predicate<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub kind: PredicateKind<'src>,
}

#[derive(Debug)]
pub enum PredicateKind<'src> {
    Trait(TraitPredicate<'src>),
    Outlives(OutlivesPredicate<'src>),
    Equality(Ty<'src>, Ty<'src>),
}

#[derive(Debug)]
pub struct TraitPredicate<'src> {
    pub bound_vars: Vec<GenericParam<'src>>,
    pub ty: Ty<'src>,
    pub bounds: Vec<Bound<'src>>,
}

#[derive(Debug)]
pub struct OutlivesPredicate<'src> {
    pub lt: Lifetime<'src>,
    pub bounds: Vec<Lifetime<'src>>,
}

#[derive(Debug)]
pub enum Bound<'src> {
    Outlives(Lifetime<'src>),
    Use(Vec<Capture<'src>>),
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

#[derive(Debug)]
pub enum Capture<'src> {
    Lifetime(Lifetime<'src>),
    TyOrConst(Ident<'src>),
}

// FIXME: Make this more type-safe: non-normal polarity is
//        incompatible with constness & asyncness
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct TraitBoundModifiers {
    pub constness: BoundConstness,
    pub asyncness: BoundAsyncness,
    pub polarity: BoundPolarity,
}

impl TraitBoundModifiers {
    pub const NONE: Self = Self {
        constness: BoundConstness::Never,
        asyncness: BoundAsyncness::Never,
        polarity: BoundPolarity::Positive,
    };
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BoundConstness {
    Never,
    Maybe,
    Always,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BoundAsyncness {
    Never,
    Always,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BoundPolarity {
    Positive,
    Negative,
    Maybe,
}

#[derive(Debug)]
pub struct Lifetime<'src>(pub Ident<'src>);
