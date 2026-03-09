use super::{Attr, BlockExpr, Expr, Item, Pat, Ty};

#[derive(Debug)]
pub enum Stmt<'src> {
    Item(Item<'src>),
    Let(Box<LetStmt<'src>>),
    Expr(Expr<'src>, Semicolon),
    Empty,
}

#[derive(Debug)]
pub struct LetStmt<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub superness: Superness,
    pub pat: Pat<'src>,
    pub ty: Option<Ty<'src>>,
    pub body: Option<LetStmtBody<'src>>,
}

#[derive(Debug)]
pub enum Superness {
    Super,
    Not,
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
