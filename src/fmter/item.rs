use super::{
    Cluster, Cx, Fmt, InterleaveExt as _, LineBreak, TrailingSpace, TrailingSpaceExt as _, fmt,
};
use crate::ast::{self, AttrsExt as _};
use std::mem;

impl Fmt for ast::Item<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        let (outer_attrs, inner_attrs) = attrs.partition();

        if cx.skip(&outer_attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in outer_attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);
        (kind, inner_attrs).fmt(cx);
    }
}

impl Fmt for (ast::ItemKind<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: Assert inner attrs is empty for most item kinds.
        let (item, attrs) = self;

        match item {
            ast::ItemKind::Const(item) => item.fmt(cx),
            ast::ItemKind::Delegation(item) => item.fmt(cx),
            ast::ItemKind::Enum(item) => item.fmt(cx),
            ast::ItemKind::ExternBlock(item) => (*item, attrs).fmt(cx),
            ast::ItemKind::ExternCrate(item) => item.fmt(cx),
            ast::ItemKind::Fn(item) => item.fmt(cx),
            ast::ItemKind::Impl(item) => (*item, attrs).fmt(cx),
            ast::ItemKind::Mod(item) => (*item, attrs).fmt(cx),
            ast::ItemKind::Static(item) => item.fmt(cx),
            ast::ItemKind::Struct(item) => item.fmt(cx),
            ast::ItemKind::Trait(item) => (*item, attrs).fmt(cx),
            ast::ItemKind::TraitAlias(item) => item.fmt(cx),
            ast::ItemKind::TyAlias(item) => item.fmt(cx),
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
        let Self { defaultness, binder, generics, ty, body } = self;

        defaultness.trailing_space().fmt(cx);
        fmt!(cx, "const {binder}");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        fmt!(cx, ": ");
        ty.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";");
    }
}

impl Fmt for ast::DelegationItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ext, path, body } = self;

        fmt!(cx, "reuse ");
        ext.fmt(cx);
        path.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " ");
            body.fmt(cx);
        } else {
            fmt!(cx, ";");
        }
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
            LineBreak.fmt(cx);
            let mut variants = variants.into_iter().peekable();
            while let Some(variant) = variants.next() {
                variant.fmt(cx);
                fmt!(cx, ",");
                if variants.peek().is_some() {
                    LineBreak.fmt(cx);
                }
            }
            cx.dedent();
            LineBreak.fmt(cx);
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::Variant<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, binder, kind, discr } = self;

        // FIXME: Skip variant if it contains `#[rustfmt::skip]` (we need a span for that tho)
        for attr in attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);

        fmt!(cx, "{binder}");

        kind.fmt(cx);

        if let Some(discr) = discr {
            fmt!(cx, " = ");
            discr.fmt(cx);
        }
    }
}

impl Fmt for ast::VariantKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Unit => {}
            Self::Tuple(fields) => {
                fmt!(cx, "(");
                fields.interleave(", ").fmt(cx);
                fmt!(cx, ")");
            }
            Self::Struct(fields) => fields.fmt(cx),
        }
    }
}

impl Fmt for Vec<ast::StructFieldDef<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, " {{");
        if !self.is_empty() {
            cx.indent();
            LineBreak.fmt(cx);
            let mut fields = self.into_iter().peekable();
            while let Some(field) = fields.next() {
                field.fmt(cx);
                fmt!(cx, ",");
                if fields.peek().is_some() {
                    LineBreak.fmt(cx);
                }
            }
            cx.dedent();
            LineBreak.fmt(cx);
        }
        fmt!(cx, "}}");
    }
}

impl Fmt for ast::TupleFieldDef<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, ty, default } = self;
        // FIXME: Inspect attrs to look for fmt skips.
        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }
        vis.trailing_space().fmt(cx);
        ty.fmt(cx);
        if let Some(default) = default {
            fmt!(cx, " = ");
            default.fmt(cx);
        }
    }
}

impl Fmt for ast::StructFieldDef<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, safety, binder, ty, default } = self;
        // FIXME: Inspect attrs to look for fmt skips.
        for attr in attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);
        safety.trailing_space().fmt(cx);

        fmt!(cx, "{binder}: ");
        ty.fmt(cx);
        if let Some(default) = default {
            fmt!(cx, " = ");
            default.fmt(cx);
        }
    }
}

impl Fmt for (ast::ExternBlockItem<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::ExternBlockItem { safety, abi, body } = item;

        safety.trailing_space().fmt(cx);
        fmt!(cx, "extern ");
        if let Some(abi) = abi {
            fmt!(cx, "{abi} ");
        }
        Cluster { attrs, nodes: body }.fmt(cx);
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

impl Fmt for ast::ExternItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);

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
        let Self { modifiers, binder, generics, params, ret_ty, contract, body } = self;

        modifiers.trailing_space().fmt(cx);
        fmt!(cx, "fn {binder}");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        fmt!(cx, "(");
        params.interleave(", ").fmt(cx);
        fmt!(cx, ")");
        if let Some(ty) = ret_ty {
            fmt!(cx, " -> ");
            ty.fmt(cx);
        }
        contract.fmt(cx);
        generics.preds.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " ");
            body.fmt(cx);
        } else {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for TrailingSpace<ast::FnItemModifiers<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(modifiers) = self;

        let ast::FnItemModifiers { defaultness, constness, asyncness, genness, safety, externness } =
            modifiers;

        defaultness.trailing_space().fmt(cx);
        constness.trailing_space().fmt(cx);
        asyncness.trailing_space().fmt(cx);
        genness.trailing_space().fmt(cx);
        safety.trailing_space().fmt(cx);
        externness.trailing_space().fmt(cx);
    }
}

impl Fmt for ast::FnParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, pat, ty } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        match pat {
            ast::Pat::Wildcard(ast::WildcardKind::Empty) => {}
            _ => {
                pat.fmt(cx);
                fmt!(cx, ": ");
            }
        }

        ty.fmt(cx);
    }
}

// FIXME: LeadingSpace<_>
impl Fmt for ast::Contract<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { requires, ensures } = self;

        if let Some(requires) = requires {
            fmt!(cx, " contract_requires ");
            requires.fmt(cx);
            if ensures.is_some() {
                fmt!(cx, " ");
            }
        }
        if let Some(ensures) = ensures {
            fmt!(cx, " contract_ensures ");
            ensures.fmt(cx);
        }
    }
}

impl Fmt for (ast::ImplItem<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::ImplItem { generics, constness, trait_ref, self_ty, body } = item;

        if let Some(ast::ImplTraitRef { defaultness, safety: _, polarity: _, path: _ }) = trait_ref
        {
            defaultness.trailing_space().fmt(cx);
        }

        match body {
            ast::ImplBody::Normal(_) => {}
            ast::ImplBody::Delegated(_) => fmt!(cx, "reuse "),
        }

        if let Some(ast::ImplTraitRef { defaultness: _, safety, polarity: _, path: _ }) = trait_ref
        {
            safety.trailing_space().fmt(cx);
        }

        fmt!(cx, "impl");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        fmt!(cx, " ");
        constness.trailing_space().fmt(cx);
        if let Some(ast::ImplTraitRef { defaultness: _, safety: _, polarity, path }) = trait_ref {
            match polarity {
                ast::ImplPolarity::Positive => {}
                ast::ImplPolarity::Negative => fmt!(cx, "!"),
            }
            path.fmt(cx);
            fmt!(cx, " for ");
        }
        self_ty.fmt(cx);
        generics.preds.fmt(cx);
        (body, attrs).fmt(cx);
    }
}

impl Fmt for (ast::ImplBody<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (body, attrs) = self;

        match body {
            ast::ImplBody::Normal(items) => {
                fmt!(cx, " ");
                Cluster { attrs, nodes: items }.fmt(cx);
            }
            ast::ImplBody::Delegated(block) => {
                debug_assert!(attrs.is_empty());

                if let Some(block) = block {
                    fmt!(cx, " ");
                    block.fmt(cx);
                } else {
                    fmt!(cx, ";");
                }
            }
        }
    }
}

impl Fmt for (ast::ModItem<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::ModItem { safety, binder, body } = item;

        safety.trailing_space().fmt(cx);
        fmt!(cx, "mod {binder}");
        if let Some(items) = body {
            fmt!(cx, " ");
            Cluster { attrs, nodes: items }.fmt(cx);
        } else {
            fmt!(cx, ";")
        }
    }
}

impl Fmt for ast::StaticItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { safety, mut_, binder, ty, body } = self;

        safety.trailing_space().fmt(cx);
        fmt!(cx, "static ");
        mut_.trailing_space().fmt(cx);
        fmt!(cx, "{binder}: ");
        ty.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";");
    }
}

impl Fmt for ast::StructItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, mut generics, kind } = self;

        let where_clause_is_trailing = matches!(kind, ast::VariantKind::Tuple(_));
        let needs_semicolon = kind.needs_semicolon();

        fmt!(cx, "struct {binder}");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        if !where_clause_is_trailing {
            mem::take(&mut generics.preds).fmt(cx);
        }
        kind.fmt(cx);
        if where_clause_is_trailing {
            generics.preds.fmt(cx);
        }
        if needs_semicolon {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for (ast::TraitItem<'_>, Vec<ast::Attr<'_, ast::attr::Inner>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::TraitItem { modifiers, binder, generics, bounds, body } = item;

        modifiers.trailing_space().fmt(cx);
        fmt!(cx, "trait {binder}");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        generics.preds.fmt(cx);
        Cluster { attrs, nodes: body }.fmt(cx);
    }
}

impl Fmt for TrailingSpace<ast::TraitItemModifiers> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(ast::TraitItemModifiers { constness, safety, autoness }) = self;

        constness.trailing_space().fmt(cx);
        safety.trailing_space().fmt(cx);
        autoness.trailing_space().fmt(cx);
    }
}

impl Fmt for ast::TraitAliasItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { constness, binder, generics, bounds } = self;

        constness.trailing_space().fmt(cx);
        fmt!(cx, "trait {binder}");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        fmt!(cx, " =");
        if !bounds.is_empty() {
            fmt!(cx, " ");
            bounds.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";");
    }
}

impl Fmt for ast::TyAliasItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { defaultness, binder, generics, bounds, body } = self;

        defaultness.trailing_space().fmt(cx);
        fmt!(cx, "type {binder}");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";");
    }
}

impl Fmt for ast::UnionItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, fields } = self;

        fmt!(cx, "union {binder}");
        generics.fmt(cx);
        fields.fmt(cx);
    }
}

impl Fmt for ast::UseItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path } = self;
        fmt!(cx, "use ");
        path.fmt(cx);
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
                trees.interleave(", ").fmt(cx);
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

impl Fmt for ast::AssocItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);

        match kind {
            ast::AssocItemKind::Const(item) => item.fmt(cx),
            ast::AssocItemKind::Delegation(item) => item.fmt(cx),
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

impl Fmt for TrailingSpace<ast::Visibility<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(vis) = self;

        match vis {
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

impl Fmt for TrailingSpace<ast::Constness> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(constness) = self;
        match constness {
            ast::Constness::Const => fmt!(cx, "const "),
            ast::Constness::Not => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Asyncness> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(asyncness) = self;
        match asyncness {
            ast::Asyncness::Async => fmt!(cx, "async "),
            ast::Asyncness::Not => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Genness> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(genness) = self;
        match genness {
            ast::Genness::Gen => fmt!(cx, "gen "),
            ast::Genness::Not => {}
        }
    }
}

impl<X> Fmt for TrailingSpace<ast::Safety<X>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(safety) = self;
        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Safe(_) => fmt!(cx, "safe "),
            ast::Safety::Unsafe => fmt!(cx, "unsafe "),
        }
    }
}

impl Fmt for TrailingSpace<ast::Externness<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(externness) = self;
        match externness {
            ast::Externness::Extern(abi) => {
                fmt!(cx, "extern ");
                if let Some(abi) = abi {
                    fmt!(cx, "{abi} ");
                }
            }
            ast::Externness::Not => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Autoness> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(autoness) = self;
        match autoness {
            ast::Autoness::Auto => fmt!(cx, "auto "),
            ast::Autoness::Not => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Defaultness> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(defaultness) = self;
        match defaultness {
            ast::Defaultness::Default => fmt!(cx, "default "),
            ast::Defaultness::Final => {}
        }
    }
}
