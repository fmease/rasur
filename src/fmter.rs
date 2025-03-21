use crate::{ast, span::Span};
use std::fmt::Write as _;

// FIXME: Reproduce comments.

pub(crate) struct Cfg {
    pub(crate) indent: usize,
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4 }
    }
}

macro_rules! cx {
    ($cx:ident) => {
        macro_rules! fmt {
            (indent) => {
                _ = $cx.buf.write_str(&" ".repeat($cx.indent)) // FIXME: Don't allocate extra string!
            };
            ($$($$arg:tt)*) => {
                _ = $cx.buf.write_fmt(format_args!($$($$arg)*))
            };
        }
    }
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
            matches!(attr.path.locality, ast::PathLocality::Local)
                && attr.path.segs == ["rustfmt", "skip"]
                && matches!(attr.kind, ast::AttrKind::Unit)
        })
    }
}

impl Fmt for ast::File<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        if cx.skip(&self.attrs) {
            fmt!("{}", cx.source(self.span));
            return;
        }

        if !self.attrs.is_empty() {
            for attr in self.attrs {
                attr.fmt(cx);
                fmt!("\n");
            }
            fmt!("\n");
        }

        let mut items = self.items.into_iter();
        if let Some(item) = items.next() {
            item.fmt(cx);
        }
        for item in items {
            fmt!("\n\n");
            item.fmt(cx);
        }
    }
}

impl Fmt for ast::Item<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        if cx.skip(&self.attrs) {
            fmt!("{}", cx.source(self.span));
            return;
        }
        for attr in self.attrs {
            attr.fmt(cx);
            fmt!("\n");
        }

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
        }
    }
}

impl Fmt for ast::Attr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("#");
        if let ast::AttrStyle::Inner = self.style {
            fmt!("!");
        }
        fmt!("[");
        self.path.fmt(cx);
        match self.kind {
            ast::AttrKind::Unit => {}
            ast::AttrKind::Call(bracket, stream) => {
                let (open, close) = match bracket {
                    ast::Bracket::Round => ("(", ")"),
                    ast::Bracket::Square => ("[", "]"),
                    ast::Bracket::Curly => ("{", "}"),
                };
                fmt!("{open}");
                stream.fmt(cx);
                fmt!("{close}");
            }
            ast::AttrKind::Assign(expr) => {
                fmt!(" = ");
                expr.fmt(cx);
            }
        }
        fmt!("]");
    }
}

impl Fmt for ast::Path<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        if let ast::PathLocality::Global = self.locality {
            fmt!("::");
        }
        let mut segs = self.segs.into_iter();
        if let Some(seg) = segs.next() {
            fmt!("{seg}");
        }
        for seg in segs {
            fmt!("::{seg}");
        }
    }
}

impl Fmt for Vec<ast::TokenKind> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        // FIXME
        for token in self {
            fmt!("{token:?}");
        }
    }
}

impl Fmt for Vec<ast::GenParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        let mut params = self.into_iter();
        if let Some(param) = params.next() {
            fmt!("<");
            fmt!("{},", param.name);
            for param in params {
                fmt!("{},", param.name);
            }
            fmt!(">");
        }
    }
}

impl Fmt for ast::ConstItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("const ");
        // FIXME
    }
}

impl Fmt for ast::EnumItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("enum ");
        // FIXME
    }
}

impl Fmt for ast::FnItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("fn ");
        fmt!("{}", self.name);
        self.generics.params.fmt(cx);
        if !self.params.is_empty() {
            fmt!("(");
            for param in self.params {
                fmt!("{}", param.name);
                // FIXME: Doesn't generate valid code in Rust 2015 if ty==None.
                if let Some(ty) = param.ty {
                    fmt!(": ");
                    ty.fmt(cx);
                }
                fmt!(",");
            }
            fmt!(")");
        }
        if let Some(ty) = self.ret_ty {
            fmt!(" -> ");
            ty.fmt(cx);
        }
        if let Some(body) = self.body {
            fmt!(" ");
            body.fmt(cx);
        } else {
            fmt!(";");
        }
    }
}

impl Fmt for ast::ImplItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("impl");
        self.generics.params.fmt(cx);
        fmt!(" ");
        self.ty.fmt(cx);
        fmt!(" {{}}")
    }
}

impl Fmt for ast::ModItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("mod {}", self.name);
        match self.items {
            Some(items) => {
                fmt!(" {{\n");
                cx.indent();
                for item in items {
                    fmt!(indent);
                    item.fmt(cx);
                }
                cx.dedent();
                fmt!(indent);
                fmt!("}}");
            }
            None => fmt!(";"),
        }
    }
}

impl Fmt for ast::StaticItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("static {}: ", self.name);
        self.ty.fmt(cx);
        if let Some(body) = self.body {
            body.fmt(cx);
        }
        fmt!(";")
    }
}

impl Fmt for ast::StructItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("struct {}", self.name);
        self.generics.params.fmt(cx);
        match self.body {
            ast::StructBody::Normal { fields } => {
                fmt!(" {{\n");
                cx.indent();
                for (name, ty) in fields {
                    fmt!(indent);
                    fmt!("{name}: ");
                    ty.fmt(cx);
                    fmt!(",\n");
                }
                cx.dedent();
                fmt!(indent);
                fmt!("}}");
            }
            ast::StructBody::Unit => fmt!(";"),
        }
    }
}

impl Fmt for ast::TraitItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("trait {}", self.name);
        self.generics.params.fmt(cx);
        fmt!(" {{}}")
    }
}

impl Fmt for ast::TyItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("type {}", self.name);
        self.generics.params.fmt(cx);
        if let Some(body) = self.body {
            fmt!(" = ");
            body.fmt(cx);
        }
        fmt!(";")
    }
}

impl Fmt for ast::UnionItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("union {}", self.name);
        self.generics.params.fmt(cx);
        fmt!(" {{}}")
    }
}

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        match self {
            Self::Array(ty, expr) => {
                fmt!("[");
                ty.fmt(cx);
                fmt!("; ");
                expr.fmt(cx);
                fmt!("]")
            }
            Self::Ident(ident) => fmt!("{ident}"),
            Self::Inferred => fmt!("_"),
            Self::Never => todo!(),
            Self::Slice(ty) => {
                fmt!("[");
                ty.fmt(cx);
                fmt!("]")
            }
            Self::Tup(tys) => {
                fmt!("(");
                let mut tys = tys.into_iter();
                if let Some(ty) = tys.next() {
                    ty.fmt(cx);
                }
                match tys.next() {
                    Some(ty) => {
                        fmt!(", ");
                        ty.fmt(cx);
                    }
                    None => fmt!(","),
                }
                for ty in tys {
                    fmt!(", ");
                    ty.fmt(cx);
                }
                fmt!(")");
            }
        }
    }
}

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        match self {
            Self::Block(expr) => (*expr).fmt(cx),
            Self::Ident(ident) => fmt!("{ident}"),
            Self::NumLit(lit) => fmt!("{lit}"),
            Self::StrLit(lit) => fmt!("{lit}"),
        }
    }
}

impl Fmt for ast::BlockExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx!(cx);
        fmt!("{{\n");
        cx.indent();
        for attr in self.attrs {
            fmt!(indent);
            attr.fmt(cx);
            fmt!("\n");
        }
        if let Some(expr) = self.expr {
            fmt!(indent);
            expr.fmt(cx);
            fmt!("\n");
        }
        cx.dedent();
        fmt!("}}");
    }
}

trait Fmt {
    fn fmt(self, cx: &mut Cx<'_>);
}
