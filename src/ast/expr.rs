use super::{Attr, Bracket, ExtPath, GenericArgsPolicy, Ident, MacroCall, Mutability, Pat, Stmt, Ty};

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    UnOp(UnOp, Box<Expr<'src>>),
    BinOp(BinOp, Box<Expr<'src>>, Box<Expr<'src>>),
    Cast(Box<Expr<'src>>, Box<Ty<'src>>),
    Path(Box<ExtPath<'src, GenericArgsPolicy::DisambiguatedOnly>>),
    Wildcard,
    Continue,
    Break(Option<&'src str>, Option<Box<Expr<'src>>>),
    Return(Option<Box<Expr<'src>>>),
    If(Box<IfExpr<'src>>),
    Loop(Box<BlockExpr<'src>>),
    Match(Box<MatchExpr<'src>>),
    While(Box<WhileExpr<'src>>),
    BoolLit(bool),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    StructLit(Box<StructLit<'src>>),
    Borrow(Mutability, Box<Expr<'src>>),
    Try(Box<Expr<'src>>),
    Field(Box<Expr<'src>>, Ident<'src>),
    Call(Box<Expr<'src>>, Vec<Expr<'src>>),
    Index(Box<Expr<'src>>, Box<Expr<'src>>),
    Block(Box<BlockExpr<'src>>),
    ConstBlock(Box<BlockExpr<'src>>),
    UnsafeBlock(Box<BlockExpr<'src>>),
    Closure(Box<ClosureExpr<'src>>),
    Tup(Vec<Expr<'src>>),
    Grouped(Box<Expr<'src>>),
    MacroCall(Box<MacroCall<'src, GenericArgsPolicy::DisambiguatedOnly>>),
}

impl Expr<'_> {
    // FIXME: Bad name (e.g. `break {}` is `false` despite "ha[ving] [æ] trailing block")
    pub(crate) fn has_trailing_block(&self, mode: TrailingBlockMode) -> bool {
        match self {
            Self::If(_)
            | Self::Loop(_)
            | Self::Match(_)
            | Self::While(_)
            | Self::Block(_)
            | Self::ConstBlock(_)
            | Self::UnsafeBlock(_) => true,
            Self::MacroCall(MacroCall { bracket: Bracket::Curly, .. }) => match mode {
                TrailingBlockMode::Normal => true,
                TrailingBlockMode::Match => false,
            },
            Self::UnOp(..)
            | Self::BinOp(..)
            | Self::Cast(..)
            | Self::Path(_)
            | Self::Wildcard
            | Self::Continue
            | Self::Break(..)
            | Self::Return(_)
            | Self::BoolLit(_)
            | Self::NumLit(_)
            | Self::StrLit(_)
            | Self::StructLit(_)
            | Self::Borrow(..)
            | Self::Try(_)
            | Self::Field(..)
            | Self::Call(..)
            | Self::Index(..)
            | Self::Closure(_)
            | Self::Tup(_)
            | Self::Grouped(_)
            | Self::MacroCall(_) => false,
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
    And,
    BitAnd,
    BitOr,
    BitXor,
    Div,
    Mul,
    Or,
    Rem,
    Sub,
    Assign,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl BinOp {
    pub(crate) fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::And => "&&",
            Self::Assign => "=",
            Self::BitAnd => "&",
            Self::BitOr => "|",
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
pub(crate) struct StructLit<'src> {
    pub(crate) path: ExtPath<'src, GenericArgsPolicy::DisambiguatedOnly>,
    pub(crate) fields: Vec<StructLitField<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructLitField<'src> {
    pub(crate) ident: Ident<'src>,
    pub(crate) expr: Expr<'src>,
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
