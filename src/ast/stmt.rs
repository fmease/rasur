use super::{Expr, Item, Pat, Ty};

#[derive(Debug)]
pub(crate) enum Stmt<'src> {
    Item(Item<'src>),
    Let(LetStmt<'src>),
    Expr(Expr<'src>, Semicolon),
    Empty,
}

#[derive(Debug)]
pub(crate) struct LetStmt<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Option<Ty<'src>>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Semicolon {
    Yes,
    No,
}
