use super::{Cx, Fmt, Punctuated, fmt};
use crate::ast;

impl Fmt for ast::Item<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }

        // FIXME: Not all items support visibility.
        vis.fmt(cx);

        match kind {
            ast::ItemKind::Const(item) => item.fmt(cx),
            ast::ItemKind::Enum(item) => item.fmt(cx),
            ast::ItemKind::ExternBlock(item) => item.fmt(cx),
            ast::ItemKind::ExternCrate(item) => item.fmt(cx),
            ast::ItemKind::Fn(item) => item.fmt(cx),
            ast::ItemKind::Impl(item) => item.fmt(cx),
            ast::ItemKind::Mod(item) => item.fmt(cx),
            ast::ItemKind::Static(item) => item.fmt(cx),
            ast::ItemKind::Struct(item) => item.fmt(cx),
            ast::ItemKind::Trait(item) => item.fmt(cx),
            ast::ItemKind::Ty(item) => item.fmt(cx),
            ast::ItemKind::Union(item) => item.fmt(cx),
            ast::ItemKind::Use(item) => item.fmt(cx),
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

impl Fmt for ast::ConstItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, ty, body } = self;

        fmt!(cx, "const {binder}");
        generics.params.fmt(cx);
        fmt!(cx, ": ");
        ty.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";")
    }
}

impl Fmt for ast::EnumItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, variants } = self;

        fmt!(cx, "enum {binder}");
        generics.fmt(cx);

        fmt!(cx, " {{");
        if !variants.is_empty() {
            cx.indent();
            cx.line_break();
            let mut variants = variants.into_iter().peekable();
            while let Some(variant) = variants.next() {
                variant.fmt(cx);
                fmt!(cx, ",");
                if variants.peek().is_some() {
                    cx.line_break();
                }
            }
            cx.dedent();
            cx.line_break();
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::EnumVariant<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, binder } = self;

        // FIXME: Skip variant if it contains `#[rustfmt::skip]` (we need a span for that tho)
        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }
        fmt!(cx, "{binder}");
    }
}

impl Fmt for ast::ExternBlockItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { abi, body } = self;

        fmt!(cx, "extern {}", abi.unwrap_or(r#""C""#));
        body.fmt(cx);
    }
}

impl Fmt for ast::ExternCrateItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { target, binder } = self;

        fmt!(cx, "extern crate {target}");
        if let Some(binder) = binder {
            fmt!(cx, " as {binder}");
        }
        fmt!(cx, ";");
    }
}

impl Fmt for Vec<ast::ExternItem<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, " {{");
        if !self.is_empty() {
            cx.indent();
            cx.line_break();
            let mut items = self.into_iter().peekable();
            while let Some(item) = items.next() {
                item.fmt(cx);
                if items.peek().is_some() {
                    cx.line_break();
                }
            }
            cx.dedent();
            cx.line_break();
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::ExternItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }

        // FIXME: Not all assoc items support visibility.
        vis.fmt(cx);

        match kind {
            ast::ExternItemKind::Fn(item) => item.fmt(cx),
            ast::ExternItemKind::Static(item) => item.fmt(cx),
            ast::ExternItemKind::Ty(item) => item.fmt(cx),
            ast::ExternItemKind::MacroCall(call) => {
                let needs_semi = call.bracket != ast::Bracket::Curly;
                call.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
        }
    }
}

impl Fmt for ast::FnItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { constness, externness, binder, generics, params, ret_ty, body } = self;

        match constness {
            ast::Constness::Const => fmt!(cx, "const "),
            ast::Constness::Not => {}
        }

        match externness {
            ast::Externness::Extern(abi) => {
                fmt!(cx, "extern {} ", abi.unwrap_or(r#""C""#));
            }
            ast::Externness::Not => {}
        }

        fmt!(cx, "fn {binder}");
        generics.params.fmt(cx);
        params.fmt(cx);
        if let Some(ty) = ret_ty {
            fmt!(cx, " -> ");
            ty.fmt(cx);
        }
        generics.preds.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " ");
            body.fmt(cx);
        } else {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for Vec<ast::FnParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "(");
        Punctuated::new(self, ", ").fmt(cx);
        fmt!(cx, ")");
    }
}

impl Fmt for ast::FnParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, ty } = self;

        pat.fmt(cx);
        fmt!(cx, ": ");
        ty.fmt(cx);
    }
}

impl Fmt for ast::ImplItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { generics, constness, polarity, trait_ref, self_ty, body } = self;

        fmt!(cx, "impl");
        generics.params.fmt(cx);
        fmt!(cx, " ");
        if let ast::Constness::Const = constness {
            fmt!(cx, "const ");
        }
        if let ast::ImplPolarity::Negative = polarity {
            fmt!(cx, "!");
        }
        if let Some(trait_ref) = trait_ref {
            trait_ref.fmt(cx);
            fmt!(cx, " for ");
        }
        self_ty.fmt(cx);
        generics.preds.fmt(cx);
        body.fmt(cx);
    }
}

impl Fmt for ast::ModItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, body } = self;

        fmt!(cx, "mod {binder}");
        match body {
            Some(items) => {
                fmt!(cx, " {{");
                if !items.is_empty() {
                    cx.indent();
                    cx.line_break();
                    let mut items = items.into_iter().peekable();
                    while let Some(item) = items.next() {
                        item.fmt(cx);
                        if items.peek().is_some() {
                            cx.line_break();
                        }
                    }
                    cx.dedent();
                    cx.line_break();
                }
                fmt!(cx, "}}");
            }
            None => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::StaticItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { mut_, binder, ty, body } = self;

        fmt!(cx, "static ");
        match mut_ {
            ast::Mutable::Yes => fmt!(cx, "mut "),
            ast::Mutable::No => {}
        }
        fmt!(cx, "{binder}: ");
        ty.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";")
    }
}

impl Fmt for ast::StructItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, body } = self;

        fmt!(cx, "struct {binder}");
        generics.fmt(cx);
        match body {
            ast::StructBody::Normal { fields } => {
                fmt!(cx, " {{");
                if !fields.is_empty() {
                    cx.indent();
                    cx.line_break();
                    let mut fields = fields.into_iter().peekable();
                    while let Some(field) = fields.next() {
                        field.fmt(cx);
                        fmt!(cx, ",");
                        if fields.peek().is_some() {
                            cx.line_break();
                        }
                    }
                    cx.dedent();
                    cx.line_break();
                }
                fmt!(cx, "}}");
            }
            ast::StructBody::Unit => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::StructField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { vis, binder, ty } = self;

        vis.fmt(cx);
        fmt!(cx, "{binder}: ");
        ty.fmt(cx);
    }
}

impl Fmt for ast::TraitItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, bounds, body } = self;

        fmt!(cx, "trait {binder}");
        generics.params.fmt(cx);
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        generics.preds.fmt(cx);
        body.fmt(cx);
    }
}

impl Fmt for ast::TyItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, bounds, body } = self;

        fmt!(cx, "type {binder}");
        generics.params.fmt(cx);
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";")
    }
}

impl Fmt for ast::UnionItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics } = self;

        fmt!(cx, "union {binder}");
        generics.fmt(cx);
        fmt!(cx, " {{}}")
    }
}

impl Fmt for ast::UseItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { tree } = self;
        fmt!(cx, "use ");
        tree.fmt(cx);
        fmt!(cx, ";");
    }
}

impl Fmt for ast::PathTree<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, kind } = self;
        let is_non_empty = !path.segs.is_empty();
        path.fmt(cx);
        if is_non_empty && !matches!(kind, ast::PathTreeKind::Stump(_)) {
            fmt!(cx, "::");
        }
        kind.fmt(cx);
    }
}

impl Fmt for ast::PathTreeKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Global => fmt!(cx, "*"),
            Self::Stump(Some(binder)) => fmt!(cx, " as {binder}"),
            Self::Stump(None) => {}
            Self::Branch(trees) => {
                fmt!(cx, "{{");
                Punctuated::new(trees, ", ").fmt(cx);
                fmt!(cx, "}}");
            }
        }
    }
}

impl Fmt for ast::MacroDef<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, params, body, style } = self;

        let prefix = match style {
            ast::MacroDefStyle::Old => "macro_rules!",
            ast::MacroDefStyle::New => "macro",
        };

        fmt!(cx, "{prefix} {binder}");
        if let Some(params) = params {
            fmt!(cx, "(");
            params.fmt(cx);
            fmt!(cx, ")");
        }
        fmt!(cx, " {{ ");
        body.fmt(cx);
        fmt!(cx, " }}");
    }
}

impl Fmt for Vec<ast::AssocItem<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, " {{");
        if !self.is_empty() {
            cx.indent();
            cx.line_break();
            let mut items = self.into_iter().peekable();
            while let Some(item) = items.next() {
                item.fmt(cx);
                if items.peek().is_some() {
                    cx.line_break();
                }
            }
            cx.dedent();
            cx.line_break();
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::AssocItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            cx.line_break();
        }

        // FIXME: Not all assoc items support visibility.
        vis.fmt(cx);

        match kind {
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
