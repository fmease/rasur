use super::{Cx, Fmt, fmt};
use crate::ast;

impl Fmt for ast::Attr<'_, ast::OuterAttrStyle> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.upcast().fmt(cx);
    }
}

impl Fmt for ast::Attr<'_, ast::InnerAttrStyle> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.upcast().fmt(cx);
    }
}

impl Fmt for ast::Attr<'_, ast::AnyAttrStyle> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { style, kind, span } = self;

        match kind {
            ast::AttrKind::Regular(attr) => (attr, style).fmt(cx),
            ast::AttrKind::DocComment => fmt!(cx, "{}", cx.source(span)),
        }
    }
}

impl Fmt for (ast::Meta<'_>, ast::AttrStyle) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (attr, style) = self;
        let ast::Meta { safety, path, args: kind } = attr;

        fmt!(cx, "#");
        match style {
            ast::AttrStyle::Inner => fmt!(cx, "!"),
            ast::AttrStyle::Outer => {}
        }
        fmt!(cx, "[");

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe(_) => fmt!(cx, "unsafe("),
        }

        path.fmt(cx);

        match kind {
            ast::MetaArgs::Unit => {}
            ast::MetaArgs::Call(bracket, stream) => {
                (bracket, ast::Orientation::Open).fmt(cx);
                stream.fmt(cx);
                (bracket, ast::Orientation::Close).fmt(cx);
            }
            ast::MetaArgs::Assign(expr) => {
                fmt!(cx, " = ");
                expr.fmt(cx);
            }
        }

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe(_) => fmt!(cx, ")"),
        }

        fmt!(cx, "]");
    }
}
