use crate::{ast, span::Span};
use std::fmt::Write as _;

// FIXME: Reproduce comments.
// FIXME: The indentation setup is busted.
//        The context should automatically push self.indent after `\n` on fmt (smh)

pub(crate) struct Cfg {
    pub(crate) indent: usize,
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4 }
    }
}

macro_rules! fmt {
    ($cx:ident, indent) => {
        _ = $cx.buf.write_str(&" ".repeat($cx.indent)) // FIXME: Don't allocate extra string!
    };
    ($cx:ident, $($arg:tt)*) => {
        _ = $cx.buf.write_fmt(format_args!($($arg)*))
    };
}

pub(crate) fn fmt(file: ast::File<'_>, source: &str, cfg: Cfg) -> String {
    let mut cx = Cx { cfg, source, indent: 0, buf: String::with_capacity(source.len()) };
    file.fmt(&mut cx);
    cx.buf
}

struct Cx<'src> {
    cfg: Cfg,
    source: &'src str,
    indent: usize,
    buf: String,
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

    fn skip(&self, attrs: &[ast::Attr<'_>]) -> bool {
        // FIXME: Look into cfg_attrs, too
        // FIXME: Make tool mod config'able: "rasur"|"rustfmt"|both
        attrs.iter().any(|attr| {
            matches!(attr.path.hook, ast::PathHook::Local)
                && matches!(
                    &*attr.path.segs,
                    [
                        ast::PathSeg { ident: "rustfmt", args: () },
                        ast::PathSeg { ident: "skip", args: () }
                    ]
                )
                && matches!(attr.kind, ast::AttrKind::Unit)
        })
    }
}

impl Fmt for ast::File<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if cx.skip(&self.attrs) {
            fmt!(cx, "{}", cx.source(self.span));
            return;
        }

        if !self.attrs.is_empty() {
            for attr in self.attrs {
                attr.fmt(cx);
                fmt!(cx, "\n");
            }
            fmt!(cx, "\n");
        }

        let mut items = self.items.into_iter();
        if let Some(item) = items.next() {
            item.fmt(cx);
        }
        for item in items {
            fmt!(cx, "\n\n");
            item.fmt(cx);
        }
    }
}

impl Fmt for ast::Item<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if cx.skip(&self.attrs) {
            fmt!(cx, "{}", cx.source(self.span));
            return;
        }
        for attr in self.attrs {
            attr.fmt(cx);
            fmt!(cx, "\n");
        }

        // FIXME: Not all items support visibility.
        self.vis.fmt(cx);

        match self.kind {
            ast::ItemKind::Const(item) => item.fmt(cx),
            ast::ItemKind::Enum(item) => item.fmt(cx),
            ast::ItemKind::Fn(item) => item.fmt(cx),
            ast::ItemKind::Impl(item) => item.fmt(cx),
            ast::ItemKind::Mod(item) => item.fmt(cx),
            ast::ItemKind::Static(item) => item.fmt(cx),
            ast::ItemKind::Struct(item) => item.fmt(cx),
            ast::ItemKind::Trait(item) => item.fmt(cx),
            ast::ItemKind::Ty(item) => item.fmt(cx),
            ast::ItemKind::Union(item) => item.fmt(cx),
            ast::ItemKind::MacroDef(item) => item.fmt(cx),
            ast::ItemKind::MacroCall(call) => {
                let needs_semi = call.bracket != ast::Bracket::Curly;
                call.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
        }
    }
}

impl Fmt for ast::Attr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "#");
        if let ast::AttrStyle::Inner = self.style {
            fmt!(cx, "!");
        }
        fmt!(cx, "[");
        self.path.fmt(cx);
        match self.kind {
            ast::AttrKind::Unit => {}
            ast::AttrKind::Call(bracket, stream) => {
                bracket.fmt(ast::Orientation::Open, cx);
                stream.fmt(cx);
                bracket.fmt(ast::Orientation::Close, cx);
            }
            ast::AttrKind::Assign(expr) => {
                fmt!(cx, " = ");
                expr.fmt(cx);
            }
        }
        fmt!(cx, "]");
    }
}

impl<A> Fmt for ast::Path<'_, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if let ast::PathHook::Global = self.hook {
            fmt!(cx, "::");
        }
        let mut segs = self.segs.into_iter();
        if let Some(seg) = segs.next() {
            seg.fmt(cx);
        }
        for seg in segs {
            fmt!(cx, "::");
            seg.fmt(cx);
        }
    }
}

impl<A> Fmt for ast::PathSeg<'_, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: Print generic args.
        fmt!(cx, "{}", self.ident);
    }
}

impl Fmt for ast::TokenStream {
    // FIXME: Actually just print as is for now
    fn fmt(self, cx: &mut Cx<'_>) {
        let mut tokens = self.into_iter();
        if let Some(token) = tokens.next() {
            token.fmt(cx);
        }
        for token in tokens {
            fmt!(cx, " ");
            token.fmt(cx);
        }
    }
}

impl Fmt for ast::TokenKind {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(
            cx,
            "{}",
            match self {
                Self::Apostrophe => "'",
                Self::Bang => "!",
                Self::CloseAngleBracket => ">",
                Self::CloseCurlyBracket => "}",
                Self::CloseRoundBracket => ")",
                Self::CloseSquareBracket => "]",
                Self::Colon => ":",
                Self::Comma => ",",
                Self::Dot => ".",
                Self::EndOfInput => "",
                Self::Equals => "=",
                Self::Error => "/*error*/",
                Self::Hash => "#",
                Self::Hyphen => "-",
                // FIXME: We need the span for the source!
                Self::Ident => "/*ident*/",
                // FIXME: We need the span for the source!
                Self::NumLit => "/*num*/",
                Self::OpenAngleBracket => "<",
                Self::OpenCurlyBracket => "{",
                Self::OpenRoundBracket => "(",
                Self::OpenSquareBracket => "[",
                Self::Plus => "+",
                Self::Semicolon => ";",
                Self::Slash => "/",
                Self::Star => "*",
                // FIXME: We need the span for the source!
                Self::StrLit => "/*str*/",
                Self::ThinArrow => "->",
                Self::WideArrow => "=>",
            }
        )
    }
}

impl Fmt for ast::Generics<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.params.fmt(cx);
        self.preds.fmt(cx);
    }
}

impl Fmt for Vec<ast::GenericParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let mut params = self.into_iter();
        if let Some(param) = params.next() {
            fmt!(cx, "<");
            param.fmt(cx);
            for param in params {
                fmt!(cx, ", ");
                param.fmt(cx);
            }
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
            ast::GenericParamKind::Lifetime => fmt!(cx, "'{}", self.binder),
        }
    }
}

impl Fmt for Vec<ast::Predicate<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if self.is_empty() {
            return;
        }
        fmt!(cx, " where ");
        let mut preds = self.into_iter();
        if let Some(pred) = preds.next() {
            pred.fmt(cx);
        }
        for pred in preds {
            fmt!(cx, ", ");
            pred.fmt(cx);
        }
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
        }
    }
}

impl Fmt for Vec<ast::Bound<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let mut bounds = self.into_iter();

        if let Some(bound) = bounds.next() {
            bound.fmt(cx);
        }
        for bound in bounds {
            fmt!(cx, " + ");
            bound.fmt(cx);
        }
    }
}

impl Fmt for ast::Bound<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(path) => path.fmt(cx),
        }
    }
}

impl Fmt for ast::ConstItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "const {}", self.binder);
        self.generics.params.fmt(cx);
        fmt!(cx, ": ");
        self.ty.fmt(cx);
        if let Some(body) = self.body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        self.generics.preds.fmt(cx);
        fmt!(cx, ";")
    }
}

impl Fmt for ast::EnumItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "enum {}", self.binder);
        self.generics.fmt(cx);
        fmt!(cx, " {{}}");
    }
}

impl Fmt for ast::FnItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self.constness {
            ast::Constness::Const => fmt!(cx, "const "),
            ast::Constness::Not => {}
        }
        fmt!(cx, "fn {}", self.binder);
        self.generics.params.fmt(cx);
        fmt!(cx, "(");
        for param in self.params {
            fmt!(cx, "{}: ", param.binder);
            param.ty.fmt(cx);
            fmt!(cx, ",");
        }
        fmt!(cx, ")");
        if let Some(ty) = self.ret_ty {
            fmt!(cx, " -> ");
            ty.fmt(cx);
        }
        self.generics.preds.fmt(cx);
        if let Some(body) = self.body {
            fmt!(cx, " ");
            body.fmt(cx);
        } else {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for ast::ImplItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "impl");
        self.generics.params.fmt(cx);
        fmt!(cx, " ");
        if let ast::Constness::Const = self.constness {
            fmt!(cx, "const ");
        }
        if let ast::ImplPolarity::Negative = self.polarity {
            fmt!(cx, "!");
        }
        if let Some(trait_ref) = self.trait_ref {
            trait_ref.fmt(cx);
            fmt!(cx, " for ");
        }
        self.self_ty.fmt(cx);
        self.generics.preds.fmt(cx);
        self.body.fmt(cx);
    }
}

impl Fmt for ast::ModItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "mod {}", self.binder);
        match self.body {
            Some(items) => {
                fmt!(cx, " {{\n");
                cx.indent();
                for item in items {
                    fmt!(cx, indent);
                    item.fmt(cx);
                }
                cx.dedent();
                fmt!(cx, indent);
                fmt!(cx, "}}");
            }
            None => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::StaticItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "static {}: ", self.binder);
        self.ty.fmt(cx);
        if let Some(body) = self.body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";")
    }
}

impl Fmt for ast::StructItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "struct {}", self.binder);
        self.generics.fmt(cx);
        match self.body {
            ast::StructBody::Normal { fields } => {
                fmt!(cx, " {{\n");
                cx.indent();
                for field in fields {
                    fmt!(cx, indent);
                    field.fmt(cx);
                    fmt!(cx, ",\n");
                }
                cx.dedent();
                fmt!(cx, indent);
                fmt!(cx, "}}");
            }
            ast::StructBody::Unit => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::StructField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.vis.fmt(cx);
        fmt!(cx, "{}: ", self.binder);
        self.ty.fmt(cx);
    }
}

impl Fmt for ast::TraitItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "trait {}", self.binder);
        self.generics.params.fmt(cx);
        if !self.bounds.is_empty() {
            fmt!(cx, ": ");
            self.bounds.fmt(cx);
        }
        self.generics.preds.fmt(cx);
        self.body.fmt(cx);
    }
}

impl Fmt for ast::TyItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "type {}", self.binder);
        self.generics.params.fmt(cx);
        if !self.bounds.is_empty() {
            fmt!(cx, ": ");
            self.bounds.fmt(cx);
        }
        if let Some(body) = self.body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        self.generics.preds.fmt(cx);
        fmt!(cx, ";")
    }
}

impl Fmt for ast::UnionItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "union {}", self.binder);
        self.generics.fmt(cx);
        fmt!(cx, " {{}}")
    }
}

impl Fmt for ast::MacroDef<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self.style {
            ast::MacroDefStyle::Old => {}
            ast::MacroDefStyle::New => todo!(), // FIXME
        }

        fmt!(cx, "macro_rules! {} {{ ", self.binder);
        self.stream.fmt(cx);
        fmt!(cx, " }}");
    }
}

impl Fmt for Vec<ast::AssocItem<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, " {{");
        if self.is_empty() {
            fmt!(cx, "}}")
        } else {
            fmt!(cx, "\n");
            cx.indent();
            let mut items = self.into_iter();
            if let Some(item) = items.next() {
                fmt!(cx, indent);
                item.fmt(cx);
            }
            for item in items {
                fmt!(cx, "\n\n");
                fmt!(cx, indent);
                item.fmt(cx);
            }
            cx.dedent();
            fmt!(cx, "\n}}");
        }
    }
}

impl Fmt for ast::AssocItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if cx.skip(&self.attrs) {
            fmt!(cx, "{}", cx.source(self.span));
            return;
        }
        for attr in self.attrs {
            attr.fmt(cx);
            fmt!(cx, "\n");
        }

        // FIXME: Not all assoc items support visibility.
        self.vis.fmt(cx);

        match self.kind {
            ast::AssocItemKind::Const(item) => item.fmt(cx),
            ast::AssocItemKind::Fn(item) => item.fmt(cx),
            ast::AssocItemKind::Ty(item) => item.fmt(cx),
            ast::AssocItemKind::MacroCall(call) => {
                let needs_semi = call.bracket != ast::Bracket::Curly;
                call.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
        }
    }
}

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Array(ty, expr) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "; ");
                expr.fmt(cx);
                fmt!(cx, "]")
            }
            Self::FnPtr((), ret_ty) => {
                fmt!(cx, "fn()");
                if let Some(ret_ty) = ret_ty {
                    fmt!(cx, " -> ");
                    ret_ty.fmt(cx);
                }
            }
            Self::Inferred => fmt!(cx, "_"),
            Self::Path(path) => path.fmt(cx),
            Self::Never => todo!(),
            Self::Slice(ty) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "]")
            }
            Self::Tup(tys) => {
                fmt!(cx, "(");
                // FIXME: Simplify!
                if !tys.is_empty() {
                    let mut tys = tys.into_iter();
                    if let Some(ty) = tys.next() {
                        ty.fmt(cx);
                    }
                    match tys.next() {
                        Some(ty) => {
                            fmt!(cx, ", ");
                            ty.fmt(cx);
                        }
                        None => fmt!(cx, ","),
                    }
                    for ty in tys {
                        fmt!(cx, ", ");
                        ty.fmt(cx);
                    }
                }
                fmt!(cx, ")");
            }
            Self::Error => fmt!(cx, "/*error*/"),
        }
    }
}

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Block(expr) => expr.fmt(cx),
            Self::Path(path) => path.fmt(cx),
            Self::NumLit(lit) => fmt!(cx, "{lit}"),
            Self::StrLit(lit) => fmt!(cx, "{lit:?}"),
            Self::MacroCall(call) => call.fmt(cx),
        }
    }
}

impl Fmt for ast::BlockExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "{{\n");
        cx.indent();
        for attr in self.attrs {
            fmt!(cx, indent);
            attr.fmt(cx);
            fmt!(cx, "\n");
        }
        for stmt in self.stmts {
            if let ast::Stmt::Empty = stmt {
                continue;
            }
            fmt!(cx, indent);
            stmt.fmt(cx);
            fmt!(cx, "\n");
        }
        cx.dedent();
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::Stmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Item(item) => item.fmt(cx),
            Self::Let(stmt) => stmt.fmt(cx),
            Self::Expr(expr, semi) => {
                let needs_semi = matches!(semi, ast::Semicolon::Yes if !expr.has_trailing_block());
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
        fmt!(cx, "let {}", self.binder);
        if let Some(ty) = self.ty {
            fmt!(cx, ": ");
            ty.fmt(cx);
        }
        if let Some(body) = self.body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";");
    }
}

impl<A> Fmt for ast::MacroCall<'_, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.path.fmt(cx);
        fmt!(cx, "!");
        if let ast::Bracket::Curly = self.bracket {
            fmt!(cx, " ");
        }
        self.bracket.fmt(ast::Orientation::Open, cx);
        self.stream.fmt(cx);
        self.bracket.fmt(ast::Orientation::Close, cx);
    }
}

impl ast::Bracket {
    fn fmt(self, orient: ast::Orientation, cx: &mut Cx<'_>) {
        use ast::Orientation::*;
        fmt!(
            cx,
            "{}",
            match (self, orient) {
                (Self::Round, Open) => "(",
                (Self::Round, Close) => ")",
                (Self::Square, Open) => "[",
                (Self::Square, Close) => "]",
                (Self::Curly, Open) => "{",
                (Self::Curly, Close) => "}",
            }
        )
    }
}

impl Fmt for ast::Visibility {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            ast::Visibility::Inherited => {}
            ast::Visibility::Public => fmt!(cx, "pub "),
        }
    }
}

impl<T: Fmt> Fmt for Box<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        (*self).fmt(cx);
    }
}

trait Fmt {
    fn fmt(self, cx: &mut Cx<'_>);
}
