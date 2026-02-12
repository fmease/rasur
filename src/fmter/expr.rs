use super::{
    Cluster, Cx, Fmt, InterleaveExt as _, LineBreak, TrailingSpace, TrailingSpaceExt as _, Tup, fmt,
};
use crate::ast::{self, AttrsExt as _};

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, kind } = self;

        let (outer_attrs, inner_attrs) = attrs.partition();

        // FIXME: Honor skip attrs (requires expr span)!
        for attr in outer_attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        (kind, inner_attrs).fmt(cx);
    }
}

// FIXME: Don't render unnecessary parentheses!
impl<'src> Fmt for (ast::ExprKind<'src>, Vec<ast::Attr<'src, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: Assert inner attrs for most expr kinds.
        let (expr, attrs) = self;

        match expr {
            ast::ExprKind::Ascription(expr, ty) => {
                fmt!(cx, "builtin # type_ascribe(");
                expr.fmt(cx);
                fmt!(cx, ", ");
                ty.fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::Await(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").await");
            }
            ast::ExprKind::Become(expr) => {
                fmt!(cx, "become ");
                expr.fmt(cx);
            }
            ast::ExprKind::UnOp(op, expr) => {
                fmt!(cx, "{}(", op.symbol());
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::Cast(expr, ty) => {
                expr.fmt(cx);
                fmt!(cx, " as ");
                ty.fmt(cx);
            }
            ast::ExprKind::BinOp(op, left, right) => {
                fmt!(cx, "(");
                left.fmt(cx);
                fmt!(cx, ") {} (", op.symbol());
                right.fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::Range(left, right, kind) => {
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
            ast::ExprKind::Wildcard => fmt!(cx, "_"),
            ast::ExprKind::Continue(label) => {
                fmt!(cx, "continue");
                if let Some(label) = label {
                    fmt!(cx, " {label}");
                }
            }
            ast::ExprKind::Break(label, expr) => {
                fmt!(cx, "break");
                if let Some(label) = label {
                    fmt!(cx, " {label}");
                }
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            ast::ExprKind::Return(expr) => {
                fmt!(cx, "return");
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            ast::ExprKind::If(expr) => expr.fmt(cx),
            ast::ExprKind::Loop(label, body) => {
                if let Some(label) = label {
                    fmt!(cx, "{label}: ");
                }
                fmt!(cx, "loop ");
                body.fmt(cx);
            }
            ast::ExprKind::Match(expr) => (*expr, attrs).fmt(cx),
            ast::ExprKind::OffsetOf(ty, fields) => {
                fmt!(cx, "builtin # offset(");
                ty.fmt(cx);
                fmt!(cx, ", ");
                fields.interleave(".").fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::WhileLoop(expr) => expr.fmt(cx),
            ast::ExprKind::Let(expr) => expr.fmt(cx),
            ast::ExprKind::Lit(lit) => lit.fmt(cx),
            ast::ExprKind::Borrow(kind, mut_, expr) => {
                fmt!(cx, "&");
                (kind, mut_).trailing_space().fmt(cx);
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::Try(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")?");
            }
            ast::ExprKind::Field(expr, field) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").{field}");
            }
            ast::ExprKind::Call(expr, args) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")(");
                args.interleave(", ").fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::MethodCall(call) => call.fmt(cx),
            ast::ExprKind::Index(expr, index) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")[");
                index.fmt(cx);
                fmt!(cx, "]");
            }
            ast::ExprKind::Block(label, block) => {
                if let Some(label) = label {
                    fmt!(cx, "{label}: ");
                }
                block.fmt(cx);
            }
            ast::ExprKind::GenBlock(kind, mode, block) => {
                kind.trailing_space().fmt(cx);
                mode.trailing_space().fmt(cx);
                block.fmt(cx);
            }
            ast::ExprKind::SpecialBlock(kind, block) => {
                kind.trailing_space().fmt(cx);
                block.fmt(cx);
            }
            ast::ExprKind::Closure(expr) => expr.fmt(cx),
            ast::ExprKind::ForLoop(expr) => expr.fmt(cx),
            ast::ExprKind::Tuple(exprs) => Tup(exprs).fmt(cx),
            ast::ExprKind::Array(elems) => {
                fmt!(cx, "[");
                elems.interleave(", ").fmt(cx);
                fmt!(cx, "]");
            }
            ast::ExprKind::Repeat(elem, count) => {
                fmt!(cx, "[");
                elem.fmt(cx);
                fmt!(cx, "; ");
                count.fmt(cx);
                fmt!(cx, "]");
            }
            ast::ExprKind::Grouped(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            ast::ExprKind::Path(path) => path.fmt(cx),
            ast::ExprKind::MacroCall(call) => call.fmt(cx),
            ast::ExprKind::Struct(expr) => expr.fmt(cx),
            ast::ExprKind::Use(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").use");
            }
            ast::ExprKind::Yeet(expr) => {
                fmt!(cx, "do yeet");
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            ast::ExprKind::Yield(ast::YieldExpr::Prefix(expr)) => {
                fmt!(cx, "yield");
                if let Some(expr) = expr {
                    fmt!(cx, " ");
                    expr.fmt(cx);
                }
            }
            ast::ExprKind::Yield(ast::YieldExpr::Postfix(expr)) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ").yield");
            }
            ast::ExprKind::Error(span) => fmt!(cx, "{}", cx.source(span)),
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

impl Fmt for (ast::MatchExpr<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (expr, attrs) = self;
        let ast::MatchExpr { kind, scrutinee, arms } = expr;

        match kind {
            ast::MatchKind::Prefix => fmt!(cx, "match "),
            ast::MatchKind::Postfix => {}
        }
        scrutinee.fmt(cx);
        match kind {
            ast::MatchKind::Prefix => {}
            ast::MatchKind::Postfix => fmt!(cx, ".match"),
        }

        fmt!(cx, " ");
        Cluster { attrs, nodes: arms }.fmt(cx);
    }
}

impl Fmt for ast::MatchArm<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, pat, guard, body } = self;

        let needs_comma = body
            .as_ref()
            .is_none_or(|body| !body.kind.is_boundary(ast::CurlyBracketedMacroCallIsBoundary::No));

        for attr in attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
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
        if needs_comma {
            fmt!(cx, ",");
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
        fields.interleave(", ").fmt(cx);
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
        args.interleave(", ").fmt(cx);
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
        params.interleave(", ").fmt(cx);
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
        let Self(ast::ClosureExprModifiers { constness, asyncness, genness, staticness, mode }) =
            self;

        constness.trailing_space().fmt(cx);
        asyncness.trailing_space().fmt(cx);
        genness.trailing_space().fmt(cx);
        staticness.trailing_space().fmt(cx);
        mode.trailing_space().fmt(cx);
    }
}

impl Fmt for TrailingSpace<ast::Staticness> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(staticness) = self;
        match staticness {
            ast::Staticness::Static => fmt!(cx, "static "),
            ast::Staticness::Not => {}
        }
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
        let Self { attrs, pat, ty } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

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

impl Fmt for TrailingSpace<ast::SpecialBlockKind<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(kind) = self;
        match kind {
            ast::SpecialBlockKind::Const => fmt!(cx, "const"),
            ast::SpecialBlockKind::Try(ty) => {
                fmt!(cx, "try");
                if let Some(ty) = ty {
                    fmt!(cx, " bikeshed ");
                    ty.fmt(cx);
                }
            }
            ast::SpecialBlockKind::Unsafe => fmt!(cx, "unsafe"),
        }
        fmt!(cx, " ");
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

        // FIXME: Skip Stmt::Empty here
        Cluster { attrs, nodes: stmts }.fmt(cx);
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
