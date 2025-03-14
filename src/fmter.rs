use crate::ast;
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

pub(crate) fn fmt(file: ast::File<'_>, cfg: Cfg) -> String {
    let mut cx = Cx { cfg, indent: 0, buf: String::new() };
    cx!(cx);
    for item in file.items.into_iter() {
        fmt_item(item, &mut cx);
        fmt!("\n");
    }
    cx.buf
}

struct Cx {
    cfg: Cfg,
    indent: usize,
    buf: String,
}

impl Cx {
    fn indent(&mut self) {
        self.indent += self.cfg.indent;
    }

    fn dedent(&mut self) {
        self.indent -= self.cfg.indent;
    }
}

fn fmt_item(item: ast::Item<'_>, cx: &mut Cx) {
    cx!(cx);
    fmt_attrs(item.attrs, cx);
    match item.kind {
        ast::ItemKind::Fn(fn_) => fmt_fn(fn_, cx),
        ast::ItemKind::Mod(mot) => fmt_mod(mot, cx),
        ast::ItemKind::Struct(strukt) => fmt_struct(strukt, cx),
    }
    fmt!("\n");
}

fn fmt_attrs(attrs: Vec<ast::Attr<'_>>, cx: &mut Cx) {
    cx!(cx);
    for attr in attrs {
        fmt!("#[{}]\n", attr.name);
    }
}

fn fmt_gen_params(params: Vec<ast::GenParam<'_>>, cx: &mut Cx) {
    cx!(cx);
    let mut params = params.into_iter();
    if let Some(param) = params.next() {
        fmt!("<");
        fmt!("{},", param.name);
        for param in params {
            fmt!("{},", param.name);
        }
        fmt!(">");
    }
}

fn fmt_fn(item: ast::Fn<'_>, cx: &mut Cx) {
    cx!(cx);
    fmt!("fn ");
    fmt!("{}", item.name);
    fmt_gen_params(item.generics.params, cx);
    if !item.params.is_empty() {
        fmt!("(");
        for param in item.params {
            fmt!("{}", param.name);
            // FIXME: Doesn't generate valid code in Rust 2015 if ty==None.
            if let Some(ty) = param.ty {
                fmt!(": ");
                fmt_ty(ty, cx);
            }
            fmt!(",");
        }
        fmt!(")");
    }
    if let Some(ty) = item.ret_ty {
        fmt!(" -> ");
        fmt_ty(ty, cx);
    }
    if let Some(body) = item.body {
        fmt!(" {{ ");
        fmt_expr(body, cx);
        fmt!(" }}");
    } else {
        fmt!(";");
    }
}

fn fmt_mod(item: ast::Mod<'_>, cx: &mut Cx) {
    cx!(cx);
    fmt!("mod {}", item.name);
    match item.items {
        Some(items) => {
            fmt!(" {{\n");
            cx.indent();
            for item in items {
                fmt!(indent);
                fmt_item(item, cx);
            }
            cx.dedent();
            fmt!(indent);
            fmt!("}}");
        }
        None => fmt!(";"),
    }
}

fn fmt_struct(item: ast::Struct<'_>, cx: &mut Cx) {
    cx!(cx);
    fmt!("struct {}", item.name);
    fmt_gen_params(item.generics.params, cx);
    match item.body {
        ast::StructBody::Normal { fields } => {
            fmt!(" {{\n");
            cx.indent();
            for (name, ty) in fields {
                fmt!(indent);
                fmt!("{name}: ");
                fmt_ty(ty, cx);
                fmt!(",\n");
            }
            cx.dedent();
            fmt!(indent);
            fmt!("}}");
        }
        ast::StructBody::Unit => fmt!(";"),
    }
}

fn fmt_ty(ty: ast::Ty<'_>, cx: &mut Cx) {
    cx!(cx);
    match ty {
        ast::Ty::Ident(ident) => fmt!("{ident}"),
    }
}

fn fmt_expr(expr: ast::Expr<'_>, cx: &mut Cx) {
    cx!(cx);
    match expr {
        ast::Expr::Ident(ident) => fmt!("{ident}"),
    }
}
