use super::{Cx, Fmt, Punctuated, TrailingSpace, TrailingSpaceExt as _, Tup, fmt};
use crate::ast;

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, kind } = self;

        // FIXME: Scan for & respect skip attr.

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        kind.fmt(cx);
    }
}

// FIXME: Don't render unnecessary parentheses!
impl Fmt for ast::ExprKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Await(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").await");
            }
            Self::Become(expr) => {
                fmt!(cx, "become ");
                expr.fmt(cx);
            }
            Self::UnOp(op, expr) => {
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
                fmt!(cx, "(");
                left.fmt(cx);
                fmt!(cx, ") {} (", op.symbol());
                right.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Range(left, right, kind) => {
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
            Self::Continue(label) => {
                fmt!(cx, "continue");
                if let Some(label) = label {
                    fmt!(cx, " {label}");
                }
            }
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
            Self::Loop(label, body) => {
                if let Some(label) = label {
                    fmt!(cx, "{label}: ");
                }
                fmt!(cx, "loop ");
                body.fmt(cx);
            }
            Self::Match(expr) => expr.fmt(cx),
            Self::WhileLoop(expr) => expr.fmt(cx),
            Self::Let(expr) => expr.fmt(cx),
            Self::Lit(lit) => lit.fmt(cx),
            Self::Borrow(kind, mut_, expr) => {
                fmt!(cx, "&");
                match kind {
                    ast::BorrowKind::Ref => {}
                    ast::BorrowKind::Raw => fmt!(cx, "raw "),
                }
                mut_.trailing_space().fmt(cx);
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Try(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")?");
            }
            Self::Field(expr, field) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").{field}");
            }
            Self::Call(expr, args) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")(");
                Punctuated::new(args, ", ").fmt(cx);
                fmt!(cx, ")");
            }
            Self::MethodCall(call) => call.fmt(cx),
            Self::Index(expr, index) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")[");
                index.fmt(cx);
                fmt!(cx, "]");
            }
            Self::Block(label, block) => {
                if let Some(label) = label {
                    fmt!(cx, "{label}: ");
                }
                block.fmt(cx);
            }
            Self::GenBlock(kind, mode, block) => {
                kind.trailing_space().fmt(cx);
                mode.trailing_space().fmt(cx);
                block.fmt(cx);
            }
            Self::SpecialBlock(kind, block) => {
                kind.trailing_space().fmt(cx);
                block.fmt(cx);
            }
            Self::Closure(expr) => expr.fmt(cx),
            Self::ForLoop(expr) => expr.fmt(cx),
            Self::Tuple(exprs) => Tup(exprs).fmt(cx),
            Self::Array(elems) => {
                fmt!(cx, "[");
                Punctuated::new(elems, ", ").fmt(cx);
                fmt!(cx, "]");
            }
            Self::Repeat(elem, count) => {
                fmt!(cx, "[");
                elem.fmt(cx);
                fmt!(cx, "; ");
                count.fmt(cx);
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
            Self::Yeet(expr) => {
                fmt!(cx, "do yeet");
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            Self::Yield(expr) => {
                fmt!(cx, "yield");
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
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
        let Self { kind, scrutinee, arms } = self;

        match kind {
            ast::MatchKind::Prefix => fmt!(cx, "match "),
            ast::MatchKind::Postfix => {}
        }
        scrutinee.fmt(cx);
        match kind {
            ast::MatchKind::Prefix => {}
            ast::MatchKind::Postfix => fmt!(cx, ".match"),
        }

        fmt!(cx, " {{");
        if !arms.is_empty() {
            cx.indent();
            cx.line_break();
            let mut arms = arms.into_iter().peekable();
            while let Some(arm) = arms.next() {
                let needs_comma = !arm.body.as_ref().is_some_and(|body| {
                    body.kind.is_boundary(ast::CurlyBracketedMacroCallIsBoundary::No)
                });
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
        let Self { attrs, pat, guard, body } = self;

        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }
        pat.fmt(cx);
        if let Some(guard) = guard {
            fmt!(cx, " if ");
            guard.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " => ");
            body.fmt(cx);
        }
    }
}

impl Fmt for ast::WhileLoopExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { label, condition, body } = self;

        if let Some(label) = label {
            fmt!(cx, "{label}: ");
        }

        fmt!(cx, "while ");
        condition.fmt(cx);
        fmt!(cx, " ");
        body.fmt(cx);
    }
}

impl Fmt for ast::StructExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, fields, base } = self;
        let non_empty = !fields.is_empty();

        path.fmt(cx);
        fmt!(cx, " {{ ");
        Punctuated::new(fields, ", ").fmt(cx);
        if let Some(base) = base {
            if non_empty {
                fmt!(cx, ", ");
            }
            fmt!(cx, "..");
            base.fmt(cx);
        }
        fmt!(cx, " }}");
    }
}

impl Fmt for ast::StructExprField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, binder, body } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

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

        // FIXME: Fewer parens
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
        let Self { bound_vars, modifiers, params, ret_ty, body } = self;

        if !bound_vars.is_empty() {
            fmt!(cx, "for");
            bound_vars.fmt(cx);
            fmt!(cx, " ");
        }

        modifiers.trailing_space().fmt(cx);

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

impl Fmt for TrailingSpace<ast::ClosureExprModifiers> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(ast::ClosureExprModifiers { constness, asyncness, genness, mode }) = self;

        constness.trailing_space().fmt(cx);
        asyncness.trailing_space().fmt(cx);
        genness.trailing_space().fmt(cx);
        mode.trailing_space().fmt(cx);
    }
}

impl Fmt for TrailingSpace<ast::CaptureMode> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(mode) = self;
        match mode {
            ast::CaptureMode::Ref => {}
            ast::CaptureMode::Move => fmt!(cx, "move "),
        }
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
        let Self { label, awaitness, pat, head, body } = self;

        if let Some(label) = label {
            fmt!(cx, "{label}: ");
        }

        fmt!(cx, "for ");
        match awaitness {
            ast::Awaitness::Await => fmt!(cx, "await "),
            ast::Awaitness::Not => {}
        }

        pat.fmt(cx);
        fmt!(cx, " in ");
        head.fmt(cx);
        fmt!(cx, " ");
        body.fmt(cx);
    }
}

impl Fmt for TrailingSpace<ast::SpecialBlockKind> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(kind) = self;
        let kind = match kind {
            ast::SpecialBlockKind::Const => "const",
            ast::SpecialBlockKind::Try => "try",
            ast::SpecialBlockKind::Unsafe => "unsafe",
        };
        fmt!(cx, "{kind} ");
    }
}

impl Fmt for TrailingSpace<ast::GenBlockKind> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(mode) = self;
        let mode = match mode {
            ast::GenBlockKind::Async => "async",
            ast::GenBlockKind::AsyncGen => "async gen",
            ast::GenBlockKind::Gen => "gen",
        };
        fmt!(cx, "{mode} ");
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
