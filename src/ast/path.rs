use super::{Bound, Expr, Lifetime, Ty};

#[derive(Debug)]
pub(crate) struct Path<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) segs: Vec<PathSeg<'src, A>>,
}

impl<'src, A: GenericArgsPolicy::Kind> Path<'src, A> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { segs: vec![PathSeg::ident(ident)] }
    }
}

#[derive(Debug)]
pub(crate) struct PathSeg<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) ident: Ident<'src>,
    pub(crate) args: A::Args<'src>,
}

impl<'src, A: GenericArgsPolicy::Kind> PathSeg<'src, A> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { ident, args: Default::default() }
    }
}

// FIXME: Create newtype for idents!
pub(crate) type Ident<'src> = &'src str;

#[derive(Debug)]
pub(crate) struct ExtPath<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) self_ty: Option<SelfTy<'src>>,
    pub(crate) path: Path<'src, A>,
}

impl<'src, A: GenericArgsPolicy::Kind> ExtPath<'src, A> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { self_ty: None, path: Path::ident(ident) }
    }
}

#[derive(Debug)]
pub(crate) struct SelfTy<'src> {
    pub(crate) ty: Ty<'src>,
    // FIXME: Better name, this doesn't make any sense
    pub(crate) offset: usize,
}

#[derive(Debug)]
pub(crate) struct PathTree<'src> {
    pub(crate) path: Path<'src, GenericArgsPolicy::Forbidden>,
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

#[expect(non_snake_case)]
pub(crate) mod GenericArgsPolicy {
    #[derive(Debug)]
    pub(crate) enum Forbidden {}
    #[derive(Debug)]
    pub(crate) enum Allowed {}
    #[derive(Debug)]
    pub(crate) enum DisambiguatedOnly {}

    pub(crate) trait Kind {
        type Args<'src>: Default + std::fmt::Debug;
    }

    impl Kind for Allowed {
        type Args<'src> = Option<super::GenericArgs<'src>>;
    }

    impl Kind for DisambiguatedOnly {
        type Args<'src> = <Allowed as Kind>::Args<'src>;
    }

    impl Kind for Forbidden {
        type Args<'src> = ();
    }
}
