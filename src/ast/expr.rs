use super::{
    Asyncness, Attr, BorrowKind, Bracket, Constness, ExtPath, GenericParam, Genness, Ident, Lit,
    MacroCall, Mutability, ObligatorilyDisambiguatedGenericArgs, Pat, PathSeg, Stmt, Ty,
};
use crate::span::Span;

#[derive(Debug)]
pub(crate) struct Expr<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) kind: ExprKind<'src>,
}

impl<'src> From<ExprKind<'src>> for Expr<'src> {
    fn from(kind: ExprKind<'src>) -> Self {
        Self { attrs: Vec::new(), kind }
    }
}

#[derive(Debug)]
pub(crate) enum ExprKind<'src> {
    Array(Vec<Expr<'src>>),
    Ascription(Box<Expr<'src>>, Box<Ty<'src>>),
    Await(Box<Expr<'src>>),
    Become(Box<Expr<'src>>),
    BinOp(BinOp, Box<Expr<'src>>, Box<Expr<'src>>),
    Block(Option<Ident<'src>>, Box<BlockExpr<'src>>),
    Borrow(BorrowKind, Mutability, Box<Expr<'src>>),
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
    Lit(Lit<'src>),
    Loop(Option<Ident<'src>>, Box<BlockExpr<'src>>),
    MacroCall(Box<MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Match(Box<MatchExpr<'src>>),
    MethodCall(Box<MethodCallExpr<'src>>),
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
            Self::MacroCall(MacroCall { bracket: Bracket::Curly, .. }) => match extra {
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
            | Self::OffsetOf(..)
            | Self::Path(_)
            | Self::Range(..)
            | Self::Repeat(..)
            | Self::Return(_)
            | Self::Struct(_)
            | Self::Try(_)
            | Self::Tuple(_)
            | Self::UnOp(..)
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum UnOp {
    Deref,
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BinOp {
    Add,
    And,
    Assign(AssignOp),
    BitAnd,
    BitOr,
    BitShiftLeft,
    BitShiftRight,
    BitXor,
    Div,
    Eq,
    Ge,
    Gt,
    Le,
    Lt,
    Mul,
    Ne,
    Or,
    Rem,
    Sub,
}

impl BinOp {
    pub(crate) fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::And => "&&",
            Self::Assign(op) => op.symbol(),
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitShiftLeft => "<<",
            Self::BitShiftRight => ">>",
            Self::BitXor => "^",
            Self::Div => "/",
            Self::Eq => "==",
            Self::Ge => ">=",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Lt => "<",
            Self::Mul => "*",
            Self::Ne => "!=",
            Self::Or => "||",
            Self::Rem => "%",
            Self::Sub => "-",
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum AssignOp {
    Normal,
    Add,
    BitAnd,
    BitOr,
    BitShiftLeft,
    BitShiftRight,
    BitXor,
    Div,
    Mul,
    Rem,
    Sub,
}

impl AssignOp {
    fn symbol(self) -> &'static str {
        match self {
            Self::Normal => "=",
            Self::Add => "+=",
            Self::BitAnd => "&=",
            Self::BitOr => "|=",
            Self::BitShiftLeft => ">>=",
            Self::BitShiftRight => "<<=",
            Self::BitXor => "^=",
            Self::Div => "/=",
            Self::Mul => "*=",
            Self::Rem => "%=",
            Self::Sub => "-=",
        }
    }
}

#[derive(Debug)]
pub(crate) struct IfExpr<'src> {
    pub(crate) condition: Expr<'src>,
    pub(crate) consequent: BlockExpr<'src>,
    pub(crate) alternate: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct MatchExpr<'src> {
    pub(crate) kind: MatchKind,
    pub(crate) scrutinee: Expr<'src>,
    pub(crate) arms: Vec<MatchArm<'src>>,
}

#[derive(Debug)]
pub(crate) enum MatchKind {
    Prefix,
    Postfix,
}

#[derive(Debug)]
pub(crate) struct MatchArm<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) pat: Pat<'src>,
    pub(crate) guard: Option<Expr<'src>>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct WhileLoopExpr<'src> {
    pub(crate) label: Option<Ident<'src>>,
    pub(crate) condition: Expr<'src>,
    pub(crate) body: BlockExpr<'src>,
}

// FIXME: Bad name
#[derive(Debug)]
pub(crate) enum SpecialBlockKind<'src> {
    Const,
    Try(Option<Box<Ty<'src>>>),
    Unsafe,
}

#[derive(Debug)]
pub(crate) enum GenBlockKind {
    Async,
    AsyncGen,
    Gen,
}

#[derive(Debug)]
pub(crate) struct BlockExpr<'src> {
    pub(crate) attrs: Vec<Attr<'src, super::attr::Inner>>,
    pub(crate) stmts: Vec<Stmt<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructExpr<'src> {
    pub(crate) path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub(crate) fields: Vec<StructExprField<'src>>,
    pub(crate) base: Option<Option<Expr<'src>>>,
}

#[derive(Debug)]
pub(crate) struct StructExprField<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) binder: Ident<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct MethodCallExpr<'src> {
    pub(crate) receiver: Expr<'src>,
    pub(crate) seg: PathSeg<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub(crate) args: Vec<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct ClosureExpr<'src> {
    pub(crate) bound_vars: Vec<GenericParam<'src>>,
    pub(crate) modifiers: ClosureExprModifiers,
    pub(crate) params: Vec<ClosureParam<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) body: Expr<'src>,
}

#[derive(Default, Debug)]
pub(crate) struct ClosureExprModifiers {
    pub(crate) constness: Constness,
    pub(crate) asyncness: Asyncness,
    // FIXME: Horrible naming!
    pub(crate) genness: Genness,
    pub(crate) staticness: Staticness,
    pub(crate) mode: CaptureMode,
}

#[derive(Default, Debug)]
pub(crate) enum Staticness {
    Static,
    #[default]
    Not,
}

#[derive(Default, Debug)]
pub(crate) enum CaptureMode {
    #[default]
    Ref,
    Move,
    Use,
}

#[derive(Debug)]
pub(crate) struct ClosureParam<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) struct LetExpr<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) body: Expr<'src>,
}

#[derive(Debug)]
pub(crate) struct ForLoopExpr<'src> {
    pub(crate) label: Option<Ident<'src>>,
    // FIXME: Horrendous naming scheme, replace.
    pub(crate) awaitness: Awaitness,
    pub(crate) pat: Pat<'src>,
    pub(crate) head: Expr<'src>,
    pub(crate) body: BlockExpr<'src>,
}

#[derive(Debug)]
pub(crate) enum Awaitness {
    Await,
    Not,
}

#[derive(Debug)]
pub(crate) enum YieldExpr<'src> {
    Prefix(Option<Box<Expr<'src>>>),
    Postfix(Box<Expr<'src>>),
}

#[derive(Debug)]
pub(crate) enum RangeExprKind {
    Inclusive,
    Exclusive,
}
