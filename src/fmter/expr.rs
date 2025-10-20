use super::{Cx, Fmt, Punctuated, TrailingSpace, TrailingSpaceExt as _, Tup, fmt};
use crate::ast;

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::UnOp(op, expr) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "{}(", op.symbol());
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Cast(expr, ty) => {
                expr.fmt(cx);
                fmt!(cx, " as ");
                ty.fmt(cx);
            }
            Self::BinOp(op, left, right) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                left.fmt(cx);
                fmt!(cx, ") {} (", op.symbol());
                right.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Range(left, right, kind) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                if let Some(left) = left {
                    fmt!(cx, "(");
                    left.fmt(cx);
                    fmt!(cx, ")");
                }
                let symbol = match kind {
                    ast::RangeExprKind::Exclusive => "..",
                    ast::RangeExprKind::Inclusive => "..=",
                };
                fmt!(cx, "{symbol}");
                if let Some(right) = right {
                    fmt!(cx, "(");
                    right.fmt(cx);
                    fmt!(cx, ")");
                }
            }
            Self::Wildcard => fmt!(cx, "_"),
            Self::Continue => fmt!(cx, "continue"),
            Self::Break(label, expr) => {
                fmt!(cx, "break");
                if let Some(label) = label {
                    fmt!(cx, " {label}");
                }
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            Self::Return(expr) => {
                fmt!(cx, "return");
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            Self::If(expr) => expr.fmt(cx),
            Self::Loop(body) => {
                fmt!(cx, "loop ");
                body.fmt(cx);
            }
            Self::Match(expr) => expr.fmt(cx),
            Self::While(expr) => expr.fmt(cx),
            Self::Let(expr) => expr.fmt(cx),
            Self::Lit(lit) => lit.fmt(cx),
            Self::Borrow(mut_, expr) => {
                fmt!(cx, "&");
                mut_.trailing_space().fmt(cx);
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Try(expr) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")?");
            }
            Self::Field(expr, field) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").{field}");
            }
            Self::Call(expr, args) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")(");
                Punctuated::new(args, ", ").fmt(cx);
                fmt!(cx, ")");
            }
            Self::MethodCall(call) => call.fmt(cx),
            Self::Index(expr, index) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")[");
                index.fmt(cx);
                fmt!(cx, "]");
            }
            Self::Block(kind, block) => {
                kind.trailing_space().fmt(cx);
                block.fmt(cx);
            }
            Self::Closure(expr) => expr.fmt(cx),
            Self::ForLoop(expr) => expr.fmt(cx),
            Self::Tup(exprs) => Tup(exprs).fmt(cx),
            Self::Array(elems) => {
                fmt!(cx, "[");
                Punctuated::new(elems, ", ").fmt(cx);
                fmt!(cx, "]");
            }
            Self::Grouped(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Path(path) => path.fmt(cx),
            Self::MacroCall(call) => call.fmt(cx),
            Self::Struct(expr) => expr.fmt(cx),
        }
    }
}

impl ast::UnOp {
    fn symbol(self) -> &'static str {
        match self {
            Self::Deref => "*",
            Self::Neg => "-",
            Self::Not => "!",
        }
    }
}

impl Fmt for ast::IfExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { condition, consequent, alternate } = self;

        fmt!(cx, "if ");
        condition.fmt(cx);
        fmt!(cx, " ");
        consequent.fmt(cx);
        if let Some(alternate) = alternate {
            fmt!(cx, " else ");
            alternate.fmt(cx);
        }
    }
}

impl Fmt for ast::MatchExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { scrutinee, arms } = self;

        fmt!(cx, "match ");
        scrutinee.fmt(cx);
        fmt!(cx, " {{");
        if !arms.is_empty() {
            cx.indent();
            cx.line_break();
            let mut arms = arms.into_iter().peekable();
            while let Some(arm) = arms.next() {
                let needs_comma = arm.body.needs_comma_as_match_arm_body();
                arm.fmt(cx);
                if needs_comma {
                    fmt!(cx, ",");
                }
                if arms.peek().is_some() {
                    cx.line_break();
                }
            }
            cx.dedent();
            cx.line_break();
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::MatchArm<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, pat, body } = self;

        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }
        pat.fmt(cx);
        fmt!(cx, " => ");
        body.fmt(cx);
    }
}

impl Fmt for ast::WhileExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { condition, body } = self;

        fmt!(cx, "while ");
        condition.fmt(cx);
        fmt!(cx, " ");
        body.fmt(cx);
    }
}

impl Fmt for ast::StructExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, fields } = self;

        path.fmt(cx);
        fmt!(cx, " {{ ");
        Punctuated::new(fields, ", ").fmt(cx);
        fmt!(cx, " }}");
    }
}

impl Fmt for ast::StructExprField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, body } = self;

        fmt!(cx, "{binder}");
        if let Some(body) = body {
            fmt!(cx, ": ");
            body.fmt(cx);
        }
    }
}

impl Fmt for ast::MethodCallExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { receiver, seg, args } = self;

        // FIXME: Less parens
        fmt!(cx, "(");
        receiver.fmt(cx);
        fmt!(cx, ").");
        seg.fmt(cx);
        fmt!(cx, "(");
        Punctuated::new(args, ", ").fmt(cx);
        fmt!(cx, ")");
    }
}

impl Fmt for ast::ClosureExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { params, ret_ty, body } = self;

        fmt!(cx, "|");
        Punctuated::new(params, ", ").fmt(cx);
        fmt!(cx, "|");

        if let Some(ty) = ret_ty {
            fmt!(cx, " -> ");
            ty.fmt(cx);
        }

        fmt!(cx, " ");

        body.fmt(cx);
    }
}

impl Fmt for ast::ClosureParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, ty } = self;
        pat.fmt(cx);
        if let Some(ty) = ty {
            fmt!(cx, ": ");
            ty.fmt(cx);
        }
    }
}

impl Fmt for ast::ForLoopExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, head: expr, body } = self;

        fmt!(cx, "for ");
        pat.fmt(cx);
        fmt!(cx, " in ");
        expr.fmt(cx);
        fmt!(cx, " ");
        body.fmt(cx);
    }
}

impl Fmt for TrailingSpace<ast::BlockKind> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(kind) = self;
        let kind = match kind {
            ast::BlockKind::Async => "async ",
            ast::BlockKind::AsyncGen => "async gen ",
            ast::BlockKind::Bare => return,
            ast::BlockKind::Const => "const ",
            ast::BlockKind::Gen => "gen ",
            ast::BlockKind::Try => "try ",
            ast::BlockKind::Unsafe => "unsafe ",
        };

        fmt!(cx, "{kind}");
    }
}

impl Fmt for ast::BlockExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, stmts } = self;

        fmt!(cx, "{{");
        if !attrs.is_empty() || !stmts.is_empty() {
            cx.indent();
            cx.line_break();
            for attr in attrs {
                attr.fmt(cx);
                cx.line_break();
            }
            let mut stmts = stmts.into_iter().peekable();
            while let Some(stmt) = stmts.next() {
                if let ast::Stmt::Empty = stmt {
                    continue;
                }
                stmt.fmt(cx);
                if stmts.peek().is_some() {
                    cx.line_break();
                }
            }
            cx.dedent();
            cx.line_break();
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::LetExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, body: expr } = self;

        fmt!(cx, "let ");
        pat.fmt(cx);
        fmt!(cx, " = ");
        expr.fmt(cx);
    }
}
