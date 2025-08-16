use super::{
    Attr, Bracket, ExtPath, Ident, Lit, MacroCall, Mutability,
    ObligatorilyDisambiguatedGenericArgs, Pat, PathSeg, Stmt, Ty,
};

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    Array(Vec<Expr<'src>>),
    BinOp(BinOp, Box<Expr<'src>>, Box<Expr<'src>>),
    Block(Box<BlockExpr<'src>>),
    Borrow(Mutability, Box<Expr<'src>>),
    Break(Option<&'src str>, Option<Box<Expr<'src>>>),
    Call(Box<Expr<'src>>, Vec<Expr<'src>>),
    Cast(Box<Expr<'src>>, Box<Ty<'src>>),
    Closure(Box<ClosureExpr<'src>>),
    ConstBlock(Box<BlockExpr<'src>>),
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
    UnsafeBlock(Box<BlockExpr<'src>>),
    While(Box<WhileExpr<'src>>),
    Wildcard,
}

impl Expr<'_> {
    // FIXME: Bad name (e.g. `break {}` is `false` despite "ha[ving] [æ] trailing block")
    pub(crate) fn has_trailing_block(&self, mode: TrailingBlockMode) -> bool {
        match self {
            | Self::Block(_)
            | Self::ConstBlock(_)
            | Self::If(_)
            | Self::Loop(_)
            | Self::Match(_)
            | Self::UnsafeBlock(_)
            | Self::While(_)
            | Self::ForLoop(_) => true,
            Self::MacroCall(MacroCall { bracket: Bracket::Curly, .. }) => match mode {
                TrailingBlockMode::Normal => true,
                TrailingBlockMode::Match => false,
            },
            | Self::Array(_)
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
            | Self::Wildcard => false,
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

#[derive(Clone, Copy, Debug)]
pub(crate) enum TrailingBlockMode {
    Normal,
    Match,
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
    pub(crate) ident: Ident<'src>,
    pub(crate) expr: Expr<'src>,
}

#[derive(Debug)]
pub(crate) struct MethodCallExpr<'src> {
    pub(crate) receiver: Expr<'src>,
    pub(crate) seg: PathSeg<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub(crate) args: Vec<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct ClosureExpr<'src> {
    pub(crate) params: Vec<ClosureParam<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) body: Expr<'src>,
}

#[derive(Debug)]
pub(crate) struct ClosureParam<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) struct LetExpr<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) expr: Expr<'src>,
}

#[derive(Debug)]
pub(crate) struct ForLoopExpr<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) expr: Expr<'src>,
    pub(crate) body: BlockExpr<'src>,
}

#[derive(Debug)]
pub(crate) enum RangeExprKind {
    Inclusive,
    Exclusive,
}
