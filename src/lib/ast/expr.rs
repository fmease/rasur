use super::{
    Async, Attr, BorrowKind, Bracket, Const, ExtPath, Gen, GenericParam, Ident, Lit, MacroCall,
    Mut, ObligatorilyDisambiguatedGenericArgs, Pat, PathSeg, Stmt, Ty,
};
use crate::span::Span;

#[derive(Debug)]
pub struct Expr<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub kind: ExprKind<'src>,
}

impl<'src> From<ExprKind<'src>> for Expr<'src> {
    fn from(kind: ExprKind<'src>) -> Self {
        Self { attrs: Vec::new(), kind }
    }
}

#[derive(Debug)]
pub enum ExprKind<'src> {
    Array(Vec<Expr<'src>>),
    Ascription(Box<Expr<'src>>, Box<Ty<'src>>),
    Await(Box<Expr<'src>>),
    Become(Box<Expr<'src>>),
    BinOp(BinOp, Box<Expr<'src>>, Box<Expr<'src>>),
    Block(Option<Ident<'src>>, Box<BlockExpr<'src>>),
    Borrow(BorrowKind, Mut, Box<Expr<'src>>),
    Break(Option<Ident<'src>>, Option<Box<Expr<'src>>>),
    Call(Box<Expr<'src>>, Vec<Expr<'src>>),
    Cast(Box<Expr<'src>>, Box<Ty<'src>>),
    Closure(Box<ClosureExpr<'src>>),
    Continue(Option<Ident<'src>>),
    Field(Box<Expr<'src>>, Ident<'src>),
    ForLoop(Box<ForLoopExpr<'src>>),
    GenBlock(GenBlockKind, CaptureMode, Box<BlockExpr<'src>>),
    Grouped(Box<Expr<'src>>),
    If(Box<IfExpr<'src>>),
    Index(Box<Expr<'src>>, Box<Expr<'src>>),
    Let(Box<LetExpr<'src>>),
    Lit(Box<Lit<'src>>),
    Loop(Option<Ident<'src>>, Box<BlockExpr<'src>>),
    MacroCall(Box<MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Match(Box<MatchExpr<'src>>),
    MethodCall(Box<MethodCallExpr<'src>>),
    Move(Box<Expr<'src>>),
    OffsetOf(Box<Ty<'src>>, Vec<Ident<'src>>),
    Path(Box<ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Range(Option<Box<Expr<'src>>>, Option<Box<Expr<'src>>>, RangeExprKind),
    Repeat(Box<Expr<'src>>, Box<Expr<'src>>),
    Return(Option<Box<Expr<'src>>>),
    SpecialBlock(SpecialBlockKind<'src>, Box<BlockExpr<'src>>),
    Struct(Box<StructExpr<'src>>),
    Try(Box<Expr<'src>>),
    Tuple(Vec<Expr<'src>>),
    UnOp(UnOp, Box<Expr<'src>>),
    UnsafeBinderCast(UnsafeBinderCastKind, Box<Expr<'src>>),
    Use(Box<Expr<'src>>),
    WhileLoop(Box<WhileLoopExpr<'src>>),
    Wildcard,
    Yeet(Option<Box<Expr<'src>>>),
    Yield(YieldExpr<'src>),
    // FIXME: Obviously, `Expr` should carry a span. Once it does, remove this payload.
    Error(Span),
}

impl ExprKind<'_> {
    pub(crate) fn is_boundary(&self, extra: CurlyBracketedMacroCallIsBoundary) -> bool {
        match self {
            | Self::Block(..)
            | Self::SpecialBlock(SpecialBlockKind::Const | SpecialBlockKind::Try(_) | SpecialBlockKind::Unsafe, _)
            | Self::If(_)
            | Self::Loop(..)
            | Self::Match(_)
            | Self::WhileLoop(_)
            | Self::ForLoop(_)
            // NOTE: Not so sure about this one. What is better for recovery?
            | Self::Error(_) => true,
            Self::MacroCall(deref!(MacroCall { bracket: Bracket::Curly, .. })) => match extra {
                CurlyBracketedMacroCallIsBoundary::Yes => true,
                CurlyBracketedMacroCallIsBoundary::No => false,
            },
            | Self::Array(_)
            | Self::Ascription(..)
            | Self::Await(_)
            | Self::Become(_)
            | Self::BinOp(..)
            | Self::Borrow(..)
            | Self::Break(..)
            | Self::Call(..)
            | Self::Cast(..)
            | Self::Closure(_)
            | Self::Continue(_)
            | Self::Field(..)
            | Self::GenBlock(..) // indeed
            | Self::Grouped(_)
            | Self::Index(..)
            | Self::Let(_)
            | Self::Lit(_)
            | Self::MacroCall(_)
            | Self::MethodCall(_)
            | Self::Move(_)
            | Self::OffsetOf(..)
            | Self::Path(_)
            | Self::Range(..)
            | Self::Repeat(..)
            | Self::Return(_)
            | Self::Struct(_)
            | Self::Try(_)
            | Self::Tuple(_)
            | Self::UnOp(..)
            | Self::UnsafeBinderCast(..)
            | Self::Use(_)
            | Self::Wildcard
            | Self::Yeet(_)
            | Self::Yield(..) => false,
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum CurlyBracketedMacroCallIsBoundary {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Deref,
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    AddAssign,
    And,
    Assign,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitShiftLeft,
    BitShiftLeftAssign,
    BitShiftRight,
    BitShiftRightAssign,
    BitXor,
    BitXorAssign,
    Div,
    DivAssign,
    Eq,
    Ge,
    Gt,
    Le,
    Lt,
    Mul,
    MulAssign,
    Ne,
    Or,
    Rem,
    RemAssign,
    Sub,
    SubAssign,
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::AddAssign => "+=",
            Self::And => "&&",
            Self::Assign => "=",
            Self::BitAnd => "&",
            Self::BitAndAssign => "&=",
            Self::BitOr => "|",
            Self::BitOrAssign => "|=",
            Self::BitShiftLeft => "<<",
            Self::BitShiftLeftAssign => ">>=",
            Self::BitShiftRight => ">>",
            Self::BitShiftRightAssign => "<<=",
            Self::BitXor => "^",
            Self::BitXorAssign => "^=",
            Self::Div => "/",
            Self::DivAssign => "/=",
            Self::Eq => "==",
            Self::Ge => ">=",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Lt => "<",
            Self::Mul => "*",
            Self::MulAssign => "*=",
            Self::Ne => "!=",
            Self::Or => "||",
            Self::Rem => "%",
            Self::RemAssign => "%=",
            Self::Sub => "-",
            Self::SubAssign => "-=",
        }
    }
}

#[rustfmt::skip]
pub(crate) macro AssignOp() {
    | BinOp::Assign
    | BinOp::AddAssign | BinOp::BitAndAssign | BinOp::BitOrAssign | BinOp::BitShiftLeftAssign | BinOp::BitShiftRightAssign
    | BinOp::BitXorAssign | BinOp::DivAssign | BinOp::MulAssign | BinOp::RemAssign | BinOp::SubAssign
}

pub(crate) macro CompareOp() {
    BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Le | BinOp::Lt | BinOp::Ne
}

#[derive(Debug)]
pub struct IfExpr<'src> {
    pub condition: Expr<'src>,
    pub consequent: BlockExpr<'src>,
    pub alternate: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct MatchExpr<'src> {
    pub kind: MatchKind,
    pub scrutinee: Expr<'src>,
    pub arms: Vec<MatchArm<'src>>,
}

#[derive(Debug)]
pub enum MatchKind {
    Prefix,
    Postfix,
}

#[derive(Debug)]
pub struct MatchArm<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub pat: Pat<'src>,
    pub guard: Option<Expr<'src>>,
    pub body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct WhileLoopExpr<'src> {
    pub label: Option<Ident<'src>>,
    pub condition: Expr<'src>,
    pub body: BlockExpr<'src>,
}

// FIXME: Bad name
#[derive(Debug)]
pub enum SpecialBlockKind<'src> {
    Const,
    Try(Option<Box<Ty<'src>>>),
    Unsafe,
}

#[derive(Debug)]
pub enum GenBlockKind {
    Async,
    AsyncGen,
    Gen,
}

#[derive(Debug)]
pub struct BlockExpr<'src> {
    pub stmts: Vec<Stmt<'src>>,
}

#[derive(Debug)]
pub struct StructExpr<'src> {
    pub path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub fields: Vec<StructExprField<'src>>,
    pub base: Option<Option<Expr<'src>>>,
}

#[derive(Debug)]
pub struct StructExprField<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub binder: Ident<'src>,
    pub body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct MethodCallExpr<'src> {
    pub receiver: Expr<'src>,
    pub seg: PathSeg<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub args: Vec<Expr<'src>>,
}

#[derive(Debug)]
pub struct ClosureExpr<'src> {
    pub bound_vars: Vec<GenericParam<'src>>,
    pub modifiers: ClosureExprModifiers,
    pub params: Vec<ClosureParam<'src>>,
    pub ret_ty: Option<Ty<'src>>,
    pub body: Expr<'src>,
}

#[derive(Default, Debug)]
pub struct ClosureExprModifiers {
    pub const_: Const,
    pub static_: Static,
    pub async_: Async,
    pub gen_: Gen,
    pub mode: CaptureMode,
}

#[derive(Default, Debug)]
pub enum Static {
    Yes,
    #[default]
    No,
}

#[derive(Default, Debug)]
pub enum CaptureMode {
    #[default]
    Ref,
    Move,
    Use,
}

#[derive(Debug)]
pub struct ClosureParam<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub pat: Pat<'src>,
    pub ty: Option<Ty<'src>>,
}

#[derive(Debug)]
pub struct LetExpr<'src> {
    pub pat: Pat<'src>,
    pub body: Expr<'src>,
}

#[derive(Debug)]
pub struct ForLoopExpr<'src> {
    pub label: Option<Ident<'src>>,
    pub await_: Await,
    pub pat: Pat<'src>,
    pub head: Expr<'src>,
    pub body: BlockExpr<'src>,
}

#[derive(Debug)]
pub enum Await {
    Yes,
    No,
}

#[derive(Debug)]
pub enum YieldExpr<'src> {
    Prefix(Option<Box<Expr<'src>>>),
    Postfix(Box<Expr<'src>>),
}

#[derive(Clone, Copy, Debug)]
pub enum RangeExprKind {
    Exclusive,
    Inclusive,
}

#[derive(Debug)]
pub enum UnsafeBinderCastKind {
    Wrap,
    Unwrap,
}
