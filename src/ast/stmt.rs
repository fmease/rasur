use super::{Attr, BlockExpr, Expr, Item, Pat, Ty};

#[derive(Debug)]
pub(crate) enum Stmt<'src> {
    Item(Item<'src>),
    Let(Box<LetStmt<'src>>),
    Expr(Expr<'src>, Semicolon),
    Empty,
}

#[derive(Debug)]
pub(crate) struct LetStmt<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Option<Ty<'src>>,
    pub(crate) body: Option<LetStmtBody<'src>>,
}

#[derive(Debug)]
pub(crate) struct LetStmtBody<'src> {
    pub(crate) consequent: Expr<'src>,
    pub(crate) alternate: Option<BlockExpr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Semicolon {
    Yes,
    No,
}
