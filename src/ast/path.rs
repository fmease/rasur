use super::{Bound, Expr, Lifetime, Ty};

#[derive(Debug)]
pub(crate) struct Path<'src, M: GenericArgsMode> {
    // Invariant: Has to be non-empty!
    pub(crate) segs: Vec<PathSeg<'src, M>>,
}

impl<'src, M: GenericArgsMode> Path<'src, M> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { segs: vec![PathSeg::ident(ident)] }
    }
}

#[derive(Debug)]
pub(crate) struct PathSeg<'src, M: GenericArgsMode> {
    pub(crate) ident: Ident<'src>,
    pub(crate) args: M::Args<'src>,
}

impl<'src, M: GenericArgsMode> PathSeg<'src, M> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { ident, args: Default::default() }
    }
}

// FIXME: Create newtype for idents!
pub(crate) type Ident<'src> = &'src str;

#[derive(Debug)]
pub(crate) struct ExtPath<'src, S: GenericArgsStyle> {
    pub(crate) ext: Option<PathExt<'src>>,
    pub(crate) path: Path<'src, S>,
}

impl<'src, S: GenericArgsStyle> ExtPath<'src, S> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { ext: None, path: Path::ident(ident) }
    }
}

#[derive(Debug)]
pub(crate) struct PathExt<'src> {
    pub(crate) self_ty: Ty<'src>,
    pub(crate) trait_ref: Option<Path<'src, UnambiguousGenericArgs>>,
}

#[derive(Debug)]
pub(crate) struct PathTree<'src> {
    pub(crate) path: Path<'src, NoGenericArgs>,
    pub(crate) kind: PathTreeKind<'src>,
}

#[derive(Debug)]
pub(crate) enum PathTreeKind<'src> {
    Global,
    Stump(Option<Ident<'src>>),
    Branch(Vec<PathTree<'src>>),
}

#[derive(Debug)]
pub(crate) enum GenericArgs<'src> {
    Angle(Vec<AngleGenericArg<'src>>),
    Paren { inputs: Vec<Ty<'src>>, output: Option<Ty<'src>> },
    ParenElided,
}

// FIXME: Merge AngleGenericArg & GenericArg?
//        So we end up with { Lt, Ty, Ct, Eq, Bd }?.
#[derive(Debug)]
pub(crate) enum AngleGenericArg<'src> {
    Argument(GenericArg<'src>),
    Constraint(AssocItemConstraint<'src>),
}

#[derive(Debug)]
pub(crate) enum GenericArg<'src> {
    Ty(Ty<'src>),
    Const(Expr<'src>),
    Lifetime(Lifetime<'src>),
}

#[derive(Debug)]
pub(crate) struct AssocItemConstraint<'src> {
    pub(crate) ident: Ident<'src>,
    pub(crate) args: Option<GenericArgs<'src>>,
    pub(crate) kind: AssocItemConstraintKind<'src>,
}

#[derive(Debug)]
pub(crate) enum AssocItemConstraintKind<'src> {
    Equality(Term<'src>),
    Bound(Vec<Bound<'src>>),
}

#[derive(Debug)]
pub(crate) enum Term<'src> {
    Ty(Ty<'src>),
    Const(Expr<'src>),
}

#[derive(Debug)]
pub(crate) enum NoGenericArgs {}
#[derive(Debug)]
pub(crate) enum UnambiguousGenericArgs {}
#[derive(Debug)]
pub(crate) enum ObligatorilyDisambiguatedGenericArgs {}

pub(crate) trait GenericArgsMode {
    type Args<'src>: Default + std::fmt::Debug;
}

impl GenericArgsMode for NoGenericArgs {
    type Args<'src> = ();
}

impl GenericArgsMode for UnambiguousGenericArgs {
    type Args<'src> = Option<super::GenericArgs<'src>>;
}

impl GenericArgsMode for ObligatorilyDisambiguatedGenericArgs {
    type Args<'src> = <UnambiguousGenericArgs as GenericArgsMode>::Args<'src>;
}

pub(crate) trait GenericArgsStyle: GenericArgsMode {}

impl GenericArgsStyle for UnambiguousGenericArgs {}
impl GenericArgsStyle for ObligatorilyDisambiguatedGenericArgs {}
