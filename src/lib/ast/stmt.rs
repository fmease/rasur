use super::{
    Attr, BlockExpr, Expr, Item, MacroCall, ObligatorilyDisambiguatedGenericArgs, Pat, Ty,
};

#[derive(Debug)]
pub enum Stmt<'src> {
    Empty,
    Expr(Expr<'src>, Semicolon),
    Item(Item<'src>),
    Let(Box<LetStmt<'src>>),
    MacroCall(Box<MacroCall<'src, ObligatorilyDisambiguatedGenericArgs>>),
}

#[derive(Debug)]
pub struct LetStmt<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub super_: Super,
    pub pat: Pat<'src>,
    pub ty: Option<Ty<'src>>,
    pub body: Option<LetStmtBody<'src>>,
}

#[derive(Debug)]
pub enum Super {
    Yes,
    No,
}

#[derive(Debug)]
pub struct LetStmtBody<'src> {
    pub consequent: Expr<'src>,
    pub alternate: Option<BlockExpr<'src>>,
}

#[derive(Debug)]
pub enum Semicolon {
    Yes,
    No,
}
