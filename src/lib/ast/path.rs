use super::{Bound, Expr, Ty};
use crate::span::Span;
use std::fmt;

pub struct Path<'src, M: GenericArgsMode> {
    pub segs: Vec<PathSeg<'src, M>>,
}

impl<'src, M: GenericArgsMode> Path<'src, M> {
    pub fn ident(ident: Ident<'src>) -> Self {
        Self { segs: vec![PathSeg::ident(ident)] }
    }
}

impl<M: GenericArgsMode> fmt::Debug for Path<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { segs } = self;

        f.debug_struct("Path").field("segs", segs).finish()
    }
}

pub struct PathSeg<'src, M: GenericArgsMode> {
    pub ident: Ident<'src>,
    pub args: M::Args<'src>,
}

impl<'src, M: GenericArgsMode> PathSeg<'src, M> {
    pub fn ident(ident: Ident<'src>) -> Self {
        Self { ident, args: Default::default() }
    }
}

impl<M: GenericArgsMode> fmt::Debug for PathSeg<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ident, args } = self;

        f.debug_struct("PathSeg").field("ident", ident).field("args", args).finish()
    }
}

#[derive(Clone, Copy)]
pub struct Ident<'src> {
    pub name: &'src str,
    pub span: Span,
}

impl<'src> Ident<'src> {
    pub fn new(name: &'src str, span: Span) -> Self {
        Self { name, span }
    }
}

impl fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.name, self.span)
    }
}

pub macro Ident($pat:pat) {
    Ident { name: $pat, .. }
}

pub struct ExtPath<'src, S: GenericArgsStyle> {
    pub ext: Option<PathExt<'src>>,
    pub path: Path<'src, S>,
}

impl<'src, S: GenericArgsStyle> ExtPath<'src, S> {
    pub fn ident(ident: Ident<'src>) -> Self {
        Self { ext: None, path: Path::ident(ident) }
    }
}

impl<S: GenericArgsStyle> fmt::Debug for ExtPath<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ext, path } = self;

        f.debug_struct("ExtPath").field("ext", ext).field("path", path).finish()
    }
}

#[derive(Debug)]
pub struct PathExt<'src> {
    pub self_ty: Ty<'src>,
    pub trait_ref: Option<Path<'src, UnambiguousGenericArgs>>,
}

#[derive(Debug)]
pub enum GenericArgs<'src> {
    Angle(Vec<AngleGenericArg<'src>>),
    Paren(Vec<Ty<'src>>, Option<Ty<'src>>),
    ParenElided,
}

#[derive(Debug)]
pub enum AngleGenericArg<'src> {
    Const(Expr<'src>),
    Constraint(AssocItemConstraint<'src>),
    Lifetime(Ident<'src>),
    Ty(Ty<'src>),
}

#[derive(Debug)]
pub struct AssocItemConstraint<'src> {
    pub ident: Ident<'src>,
    pub args: Option<GenericArgs<'src>>,
    pub kind: AssocItemConstraintKind<'src>,
}

#[derive(Debug)]
pub enum AssocItemConstraintKind<'src> {
    Equality(Term<'src>),
    Bound(Vec<Bound<'src>>),
}

#[derive(Debug)]
pub enum Term<'src> {
    Ty(Ty<'src>),
    Const(Expr<'src>),
}

pub enum NoGenericArgs {}
pub enum UnambiguousGenericArgs {}
pub enum ObligatorilyDisambiguatedGenericArgs {}

pub trait GenericArgsMode {
    type Args<'src>: Default + fmt::Debug;
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

pub trait GenericArgsStyle: GenericArgsMode {}

impl GenericArgsStyle for UnambiguousGenericArgs {}
impl GenericArgsStyle for ObligatorilyDisambiguatedGenericArgs {}
