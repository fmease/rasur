use crate::{ast, span::Span};
use std::fmt::Write as _;

mod item;

// FIXME: Reproduce comments.
// FIXME: The indentation setup is busted.

pub(crate) struct Cfg {
    pub(crate) indent: usize,
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4 }
    }
}

macro fmt($cx:ident, $($arg:tt)*) {
    _ = $cx.output.write_fmt(format_args!($($arg)*))
}

pub(crate) fn fmt(file: ast::File<'_>, source: &str, cfg: Cfg) -> String {
    let mut cx = Cx { cfg, source, indent: 0, output: String::with_capacity(source.len()) };
    file.fmt(&mut cx);
    cx.output
}

struct Cx<'src> {
    cfg: Cfg,
    source: &'src str,
    indent: usize,
    output: String,
}

impl<'src> Cx<'src> {
    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }

    fn indent(&mut self) {
        self.indent += self.cfg.indent;
    }

    fn dedent(&mut self) {
        self.indent -= self.cfg.indent;
    }

    fn line_break(&mut self) {
        self.output.push('\n');
        _ = self.output.write_fmt(format_args!("{0:1$}", "", self.indent));
    }

    fn skip(&self, attrs: &[ast::Attr<'_>]) -> bool {
        // FIXME: Look into cfg_attrs, too
        // FIXME: Make tool mod config'able: "rasur"|"rustfmt"|both
        // FIXME: Support rustfmt_skip or whatever that legacy attr is called
        attrs.iter().any(|attr| {
            matches!(
                &*attr.path.segs,
                [
                    ast::PathSeg { ident: "rustfmt", args: () },
                    ast::PathSeg { ident: "skip", args: () }
                ]
            ) && matches!(attr.kind, ast::AttrKind::Unit)
        })
    }
}

impl Fmt for ast::File<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, items, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }

        if !attrs.is_empty() {
            for attr in attrs {
                attr.fmt(cx);
                cx.line_break();
            }
            cx.line_break();
        }

        for item in items {
            item.fmt(cx);
            cx.line_break();
        }
    }
}

impl Fmt for ast::Attr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { style, path, kind } = self;

        fmt!(cx, "#");
        if let ast::AttrStyle::Inner = style {
            fmt!(cx, "!");
        }
        fmt!(cx, "[");
        path.fmt(cx);
        match kind {
            ast::AttrKind::Unit => {}
            ast::AttrKind::Call(bracket, stream) => {
                (bracket, ast::Orientation::Open).fmt(cx);
                stream.fmt(cx);
                (bracket, ast::Orientation::Close).fmt(cx);
            }
            ast::AttrKind::Assign(expr) => {
                fmt!(cx, " = ");
                expr.fmt(cx);
            }
        }
        fmt!(cx, "]");
    }
}

impl<'src, A: FmtGenericArgs> Fmt for ast::Path<'src, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { segs } = self;
        Punctuated::new(segs, "::").fmt(cx);
    }
}

impl<'src, A: FmtGenericArgs> Fmt for ast::PathSeg<'src, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args } = self;

        fmt!(cx, "{ident}");
        A::fmt(args, cx);
    }
}

impl Fmt for ast::TokenStream {
    // FIXME: Actually just print as is for now
    fn fmt(self, cx: &mut Cx<'_>) {
        Punctuated::new(self, " ").fmt(cx);
    }
}

impl Fmt for ast::Token {
    fn fmt(self, cx: &mut Cx<'_>) {
        let str = match self.kind {
            ast::TokenKind::Ampersand => "&",
            ast::TokenKind::Apostrophe => "'",
            ast::TokenKind::Asterisk => "*",
            ast::TokenKind::At => "@",
            ast::TokenKind::Bang => "!",
            ast::TokenKind::BangEquals => "!=",
            ast::TokenKind::Caret => "^",
            ast::TokenKind::CloseCurlyBracket => "}",
            ast::TokenKind::CloseRoundBracket => ")",
            ast::TokenKind::CloseSquareBracket => "]",
            ast::TokenKind::Colon => ":",
            ast::TokenKind::Comma => ",",
            ast::TokenKind::Dot => ".",
            ast::TokenKind::DoubleAmpersand => "&&",
            ast::TokenKind::DoubleColon => "::",
            ast::TokenKind::DoubleDot => "..",
            ast::TokenKind::DoubleEquals => "==",
            ast::TokenKind::DoublePipe => "||",
            ast::TokenKind::EndOfInput => "",
            ast::TokenKind::Equals => "=",
            ast::TokenKind::GreaterThan => ">",
            ast::TokenKind::GreaterThanEquals => ">=",
            ast::TokenKind::Hash => "#",
            ast::TokenKind::Hyphen => "-",
            ast::TokenKind::LessThan => "<",
            ast::TokenKind::LessThanEquals => "<=",
            ast::TokenKind::OpenCurlyBracket => "{",
            ast::TokenKind::OpenRoundBracket => "(",
            ast::TokenKind::OpenSquareBracket => "[",
            ast::TokenKind::Percent => "%",
            ast::TokenKind::Pipe => "|",
            ast::TokenKind::Plus => "+",
            ast::TokenKind::QuestionMark => "?",
            ast::TokenKind::Semicolon => ";",
            ast::TokenKind::Slash => "/",
            ast::TokenKind::ThinArrow => "->",
            ast::TokenKind::TripleDot => "...",
            ast::TokenKind::WideArrow => "=>",
            ast::TokenKind::Ident
            | ast::TokenKind::NumLit
            | ast::TokenKind::StrLit
            | ast::TokenKind::Error => cx.source(self.span),
        };
        fmt!(cx, "{str}")
    }
}

impl Fmt for ast::Generics<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { params, preds } = self;

        params.fmt(cx);
        preds.fmt(cx);
    }
}

impl Fmt for Vec<ast::GenericParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if !self.is_empty() {
            fmt!(cx, "<");
            Punctuated::new(self, ", ").fmt(cx);
            fmt!(cx, ">");
        }
    }
}

impl Fmt for ast::GenericParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self.kind {
            ast::GenericParamKind::Ty(bounds) => {
                fmt!(cx, "{}", self.binder);
                if !bounds.is_empty() {
                    fmt!(cx, ": ");
                    bounds.fmt(cx);
                }
            }
            ast::GenericParamKind::Const(ty) => {
                fmt!(cx, "const {}: ", self.binder);
                ty.fmt(cx);
            }
            ast::GenericParamKind::Lifetime(bounds) => {
                ast::Lifetime(self.binder).fmt(cx);
                if !bounds.is_empty() {
                    fmt!(cx, ": ");
                    Punctuated::new(bounds, " + ").fmt(cx);
                }
            }
        }
    }
}

trait FmtGenericArgs: ast::GenericArgsPolicy::Kind {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>);
}

impl FmtGenericArgs for ast::GenericArgsPolicy::Forbidden {
    fn fmt((): Self::Args<'_>, _: &mut Cx<'_>) {}
}

impl FmtGenericArgs for ast::GenericArgsPolicy::Allowed {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        args.fmt(cx);
    }
}

impl FmtGenericArgs for ast::GenericArgsPolicy::DisambiguatedOnly {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        if let Some(args) = args {
            let is_empty = match &args {
                ast::GenericArgs::Angle(args) => args.is_empty(),
                ast::GenericArgs::Paren { .. } | ast::GenericArgs::ParenElided => true,
            };
            if !is_empty {
                fmt!(cx, "::");
            }
            args.fmt(cx);
        }
    }
}

impl Fmt for ast::GenericArgs<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Angle(args) => args.fmt(cx),
            Self::Paren { inputs, output } => {
                fmt!(cx, "(");
                Punctuated::new(inputs, ", ").fmt(cx);
                fmt!(cx, ")");
                if let Some(output) = output {
                    fmt!(cx, " -> ");
                    output.fmt(cx);
                }
            }
            Self::ParenElided => fmt!(cx, "(..)"),
        }
    }
}

impl Fmt for Vec<ast::AngleGenericArg<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if !self.is_empty() {
            fmt!(cx, "<");
            Punctuated::new(self, ", ").fmt(cx);
            fmt!(cx, ">");
        }
    }
}

impl Fmt for ast::AngleGenericArg<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Argument(arg) => arg.fmt(cx),
            Self::Constraint(constraint) => constraint.fmt(cx),
        }
    }
}

impl Fmt for ast::GenericArg<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Ty(ty) => ty.fmt(cx),
            Self::Const(expr) => expr.fmt(cx),
            Self::Lifetime(lt) => lt.fmt(cx),
        }
    }
}

impl Fmt for ast::AssocItemConstraint<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args, kind } = self;
        fmt!(cx, "{ident}");
        args.fmt(cx);
        kind.fmt(cx);
    }
}

impl Fmt for ast::AssocItemConstraintKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Equality(term) => {
                fmt!(cx, " = ");
                term.fmt(cx);
            }
            Self::Bound(bounds) => {
                fmt!(cx, ": ");
                bounds.fmt(cx);
            }
        }
    }
}

impl Fmt for ast::Term<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Ty(ty) => ty.fmt(cx),
            Self::Const(expr) => expr.fmt(cx),
        }
    }
}

impl Fmt for Vec<ast::Predicate<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if self.is_empty() {
            return;
        }
        fmt!(cx, " where ");
        Punctuated::new(self, ", ").fmt(cx);
    }
}

impl Fmt for ast::Predicate<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(pred) => {
                pred.ty.fmt(cx);
                fmt!(cx, ":");
                if !pred.bounds.is_empty() {
                    fmt!(cx, " ");
                }
                pred.bounds.fmt(cx);
            }
            Self::Outlives(pred) => {
                pred.lt.fmt(cx);
                fmt!(cx, ":");
                if !pred.bounds.is_empty() {
                    fmt!(cx, " ");
                }
                Punctuated::new(pred.bounds, " + ").fmt(cx);
            }
        }
    }
}

impl Fmt for Vec<ast::Bound<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        Punctuated::new(self, " + ").fmt(cx);
    }
}

impl Fmt for ast::Bound<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(mods, path) => {
                mods.fmt(cx);
                path.fmt(cx)
            }
            Self::Outlives(lt) => lt.fmt(cx),
        }
    }
}

impl Fmt for ast::TraitBoundModifiers {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { polarity } = self;

        match polarity {
            ast::BoundPolarity::Positive => {}
            ast::BoundPolarity::Negative => fmt!(cx, "!"),
            ast::BoundPolarity::Maybe => fmt!(cx, "?"),
        }
    }
}

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Inferred => fmt!(cx, "_"),
            Self::FnPtr((), ret_ty) => {
                fmt!(cx, "fn()");
                if let Some(ret_ty) = ret_ty {
                    fmt!(cx, " -> ");
                    ret_ty.fmt(cx);
                }
            }
            Self::Ref(lt, mut_, ty) => {
                fmt!(cx, "&");
                if let Some(lt) = lt {
                    lt.fmt(cx);
                    fmt!(cx, " ");
                }
                match mut_ {
                    ast::Mutable::Yes => fmt!(cx, "mut "),
                    ast::Mutable::No => {}
                }
                ty.fmt(cx);
            }
            Self::Ptr(mut_, ty) => {
                fmt!(cx, "*");
                match mut_ {
                    ast::Mutable::Yes => fmt!(cx, "mut "),
                    ast::Mutable::No => fmt!(cx, "const "),
                }
                ty.fmt(cx);
            }
            Self::Never => fmt!(cx, "!"),
            // FIXME: In Rust 2015 if `bounds.is_empty()`, you need to render it as `r#dyn`.
            Self::DynTrait(bounds) => {
                fmt!(cx, "dyn");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::ImplTrait(bounds) => {
                fmt!(cx, "impl");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::Array(ty, expr) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "; ");
                expr.fmt(cx);
                fmt!(cx, "]")
            }
            Self::Slice(ty) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "]")
            }
            Self::Tup(tys) => Tup(tys).fmt(cx),
            Self::Grouped(ty) => {
                fmt!(cx, "(");
                ty.fmt(cx);
                fmt!(cx, ")");
            }
            Self::Error => fmt!(cx, "/*error*/"),
        }
    }
}

impl Fmt for ast::Lifetime<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(lt) = self;
        fmt!(cx, "'{lt}");
    }
}

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::UnOp(op @ ast::UnOp::Try, expr) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, "){}", op.symbol());
            }
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
            Self::Path(path) => path.fmt(cx),
            Self::Wildcard => fmt!(cx, "_"),
            Self::Continue => fmt!(cx, "continue"),
            Self::Break(label, expr) => {
                fmt!(cx, "break");
                if let Some(label) = label {
                    fmt!(cx, " '{label}");
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
            Self::If(expr) => {
                fmt!(cx, "if ");
                expr.condition.fmt(cx);
                fmt!(cx, " ");
                expr.consequent.fmt(cx);
                if let Some(alternate) = expr.alternate {
                    fmt!(cx, " else ");
                    alternate.fmt(cx);
                }
            }
            Self::Loop(body) => {
                fmt!(cx, "loop ");
                body.fmt(cx);
            }
            Self::Match { scrutinee, arms } => {
                fmt!(cx, "match ");
                scrutinee.fmt(cx);
                fmt!(cx, " {{");
                if !arms.is_empty() {
                    cx.indent();
                    cx.line_break();
                    let mut arms = arms.into_iter().peekable();
                    while let Some(arm) = arms.next() {
                        let needs_comma =
                            !arm.body.has_trailing_block(ast::TrailingBlockMode::Match);
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
            Self::While { condition, body } => {
                fmt!(cx, "while ");
                condition.fmt(cx);
                fmt!(cx, " ");
                body.fmt(cx);
            }
            Self::BoolLit(lit) => fmt!(cx, "{lit}"),
            Self::NumLit(lit) => fmt!(cx, "{lit}"),
            Self::StrLit(lit) => fmt!(cx, "{lit}"),
            Self::Borrow(mut_, expr) => {
                fmt!(cx, "&");
                match mut_ {
                    ast::Mutable::Yes => fmt!(cx, "mut "),
                    ast::Mutable::No => {}
                }
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
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
            Self::Index(expr, index) => {
                // FIXME: Temporary: Don't render unnecessary parentheses!
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")[");
                index.fmt(cx);
                fmt!(cx, "]");
            }
            Self::Block(expr) => expr.fmt(cx),
            Self::Tup(exprs) => Tup(exprs).fmt(cx),
            Self::Grouped(expr) => {
                fmt!(cx, "(");
                expr.fmt(cx);
                fmt!(cx, ")");
            }
            Self::MacroCall(call) => call.fmt(cx),
        }
    }
}

impl ast::UnOp {
    fn symbol(self) -> &'static str {
        match self {
            Self::Deref => "*",
            Self::Neg => "-",
            Self::Not => "!",
            Self::Try => "?",
        }
    }
}

impl ast::BinOp {
    fn symbol(self) -> &'static str {
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

impl Fmt for ast::MatchArm<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, body } = self;

        pat.fmt(cx);
        fmt!(cx, " => ");
        body.fmt(cx);
    }
}

impl Fmt for ast::Pat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Wildcard => fmt!(cx, "_"),
            Self::NumLit(lit) => fmt!(cx, "{lit}"),
            Self::StrLit(lit) => fmt!(cx, "{lit:?}"),
            Self::Borrow(mut_, pat) => {
                fmt!(cx, "&");
                match mut_ {
                    ast::Mutable::Yes => fmt!(cx, "mut "),
                    ast::Mutable::No => {}
                }
                pat.fmt(cx);
            }
            Self::Tup(pats) => Tup(pats).fmt(cx),
            Self::Grouped(pat) => {
                fmt!(cx, "(");
                pat.fmt(cx);
                fmt!(cx, ")");
            }
            Self::MacroCall(call) => call.fmt(cx),
        }
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

impl Fmt for ast::Stmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Item(item) => item.fmt(cx),
            Self::Let(stmt) => stmt.fmt(cx),
            Self::Expr(expr, semi) => {
                let needs_semi = matches!(semi, ast::Semicolon::Yes if !expr.has_trailing_block(ast::TrailingBlockMode::Normal));
                expr.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
            Self::Empty => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::LetStmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, ty, body } = self;

        fmt!(cx, "let ");
        pat.fmt(cx);
        if let Some(ty) = ty {
            fmt!(cx, ": ");
            ty.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";");
    }
}

impl<'src, A: FmtGenericArgs> Fmt for ast::MacroCall<'src, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, bracket, stream } = self;

        path.fmt(cx);
        fmt!(cx, "!");
        if let ast::Bracket::Curly = bracket {
            fmt!(cx, " ");
        }
        (bracket, ast::Orientation::Open).fmt(cx);
        stream.fmt(cx);
        (bracket, ast::Orientation::Close).fmt(cx);
    }
}

impl Fmt for (ast::Bracket, ast::Orientation) {
    fn fmt(self, cx: &mut Cx<'_>) {
        use ast::Bracket::*;
        use ast::Orientation::*;
        let fmt = match self {
            (Round, Open) => "(",
            (Round, Close) => ")",
            (Square, Open) => "[",
            (Square, Close) => "]",
            (Curly, Open) => "{",
            (Curly, Close) => "}",
        };
        fmt!(cx, "{fmt}")
    }
}

impl Fmt for ast::Visibility<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            ast::Visibility::Inherited => {}
            ast::Visibility::Restricted(path) => {
                fmt!(cx, "pub(");
                match &*path.segs {
                    [ast::PathSeg { ident: "crate" | "super" | "self", args: () }] => {}
                    _ => fmt!(cx, "in "),
                }
                path.fmt(cx);
                fmt!(cx, ") ");
            }
            ast::Visibility::Public => fmt!(cx, "pub "),
        }
    }
}

struct Punctuated<T> {
    nodes: Vec<T>,
    sep: &'static str,
}

impl<T> Punctuated<T> {
    fn new(nodes: Vec<T>, sep: &'static str) -> Self {
        Self { nodes, sep }
    }
}

impl<T: Fmt> Fmt for Punctuated<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { nodes, sep } = self;
        let mut nodes = nodes.into_iter();
        if let Some(node) = nodes.next() {
            node.fmt(cx);
        }
        for node in nodes {
            fmt!(cx, "{sep}");
            node.fmt(cx);
        }
    }
}

struct Tup<T>(Vec<T>);

impl<T: Fmt> Fmt for Tup<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(nodes) = self;
        fmt!(cx, "(");
        // FIXME: Simplify!
        if !nodes.is_empty() {
            let mut nodes = nodes.into_iter();
            if let Some(node) = nodes.next() {
                node.fmt(cx);
            }
            match nodes.next() {
                Some(node) => {
                    fmt!(cx, ", ");
                    node.fmt(cx);
                }
                None => fmt!(cx, ","),
            }
            for node in nodes {
                fmt!(cx, ", ");
                node.fmt(cx);
            }
        }
        fmt!(cx, ")");
    }
}

impl<T: Fmt> Fmt for Box<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        (*self).fmt(cx);
    }
}

impl<T: Fmt> Fmt for Option<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if let Some(this) = self {
            this.fmt(cx);
        }
    }
}

trait Fmt {
    fn fmt(self, cx: &mut Cx<'_>);
}
