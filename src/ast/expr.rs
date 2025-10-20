use super::{
    Attr, Bracket, ExtPath, Ident, Lit, MacroCall, Mutability,
    ObligatorilyDisambiguatedGenericArgs, Pat, PathSeg, Stmt, Ty,
};

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    Array(Vec<Expr<'src>>),
    BinOp(BinOp, Box<Expr<'src>>, Box<Expr<'src>>),
    Block(BlockKind, Box<BlockExpr<'src>>),
    Borrow(Mutability, Box<Expr<'src>>),
    Break(Option<&'src str>, Option<Box<Expr<'src>>>),
    Call(Box<Expr<'src>>, Vec<Expr<'src>>),
    Cast(Box<Expr<'src>>, Box<Ty<'src>>),
    Closure(Box<ClosureExpr<'src>>),
    Continue,
    Field(Box<Expr<'src>>, Ident<'src>),
    ForLoop(Box<ForLoopExpr<'src>>),
    Grouped(Box<Expr<'src>>),
    If(Box<IfExpr<'src>>),
    Index(Box<Expr<'src>>, Box<Expr<'src>>),
    Let(Box<LetExpr<'src>>),
    Lit(Lit<'src>),
    Loop(Box<BlockExpr<'src>>),
    MacroCall(Box<MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Match(Box<MatchExpr<'src>>),
    MethodCall(Box<MethodCallExpr<'src>>),
    Path(Box<ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>>),
    Range(Option<Box<Expr<'src>>>, Option<Box<Expr<'src>>>, RangeExprKind),
    Return(Option<Box<Expr<'src>>>),
    Struct(Box<StructExpr<'src>>),
    Try(Box<Expr<'src>>),
    Tup(Vec<Expr<'src>>),
    UnOp(UnOp, Box<Expr<'src>>),
    While(Box<WhileExpr<'src>>),
    Wildcard,
}

impl Expr<'_> {
    pub(crate) fn needs_semicolon_as_stmt(&self) -> bool {
        self.needs_delimiter(false)
    }

    pub(crate) fn needs_comma_as_match_arm_body(&self) -> bool {
        self.needs_delimiter(true)
    }

    fn needs_delimiter(&self, curly_macro_call_counts: bool) -> bool {
        match self {
            | Self::Block(BlockKind::Bare | BlockKind::Const | BlockKind::Try | BlockKind::Unsafe, _)
            | Self::If(_)
            | Self::Loop(_)
            | Self::Match(_)
            | Self::While(_)
            | Self::ForLoop(_) => false,
            Self::MacroCall(MacroCall { bracket: Bracket::Curly, .. }) => curly_macro_call_counts,
            | Self::Array(_)
            | Self::Block(BlockKind::Async | BlockKind::AsyncGen | BlockKind::Gen, _) // indeed
            | Self::BinOp(..)
            | Self::Borrow(..)
            | Self::Break(..)
            | Self::Call(..)
            | Self::Cast(..)
            | Self::Closure(_)
            | Self::Continue
            | Self::Field(..)
            | Self::Grouped(_)
            | Self::Index(..)
            | Self::Let(_)
            | Self::Lit(_)
            | Self::MacroCall(_)
            | Self::MethodCall(_)
            | Self::Path(_)
            | Self::Range(..)
            | Self::Return(_)
            | Self::Struct(_)
            | Self::Try(_)
            | Self::Tup(_)
            | Self::UnOp(..)
            | Self::Wildcard => true,
        }
    }
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
    pub(crate) fn symbol(self) -> &'static str {
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

#[derive(Debug)]
pub(crate) struct IfExpr<'src> {
    pub(crate) condition: Expr<'src>,
    pub(crate) consequent: BlockExpr<'src>,
    pub(crate) alternate: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct MatchExpr<'src> {
    pub(crate) scrutinee: Expr<'src>,
    pub(crate) arms: Vec<MatchArm<'src>>,
}

#[derive(Debug)]
pub(crate) struct MatchArm<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) pat: Pat<'src>,
    pub(crate) body: Expr<'src>,
}

#[derive(Debug)]
pub(crate) struct WhileExpr<'src> {
    pub(crate) condition: Expr<'src>,
    pub(crate) body: BlockExpr<'src>,
}

#[derive(Debug)]
pub(crate) enum BlockKind {
    Async,
    AsyncGen,
    Bare,
    Const,
    Gen,
    Try,
    Unsafe,
}

#[derive(Debug)]
pub(crate) struct BlockExpr<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) stmts: Vec<Stmt<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructExpr<'src> {
    pub(crate) path: ExtPath<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub(crate) fields: Vec<StructExprField<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructExprField<'src> {
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
    pub(crate) kind: ClosureKind,
    pub(crate) params: Vec<ClosureParam<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) body: Expr<'src>,
}

#[derive(Debug)]
pub(crate) enum ClosureKind {
    Normal,
    Move,
}

#[derive(Debug)]
pub(crate) struct ClosureParam<'src> {
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
    pub(crate) pat: Pat<'src>,
    pub(crate) head: Expr<'src>,
    pub(crate) body: BlockExpr<'src>,
}

#[derive(Debug)]
pub(crate) enum RangeExprKind {
    Inclusive,
    Exclusive,
}
