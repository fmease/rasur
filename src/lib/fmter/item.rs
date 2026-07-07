use super::{
    Cluster, Cx, Fmt, InterleaveExt as _, LineBreak, TrailingSpace, TrailingSpaceExt as _, fmt,
};
use crate::ast::{self, AttrsExt as _};
use std::mem;

impl Fmt for ast::Item<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        let (outer_attrs, inner_attrs) = attrs.partition();
        if cx.should_preserve(&outer_attrs) {
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

impl Fmt for (ast::ItemKind<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: Assert inner attrs is empty for most item kinds.
        let (item, attrs) = self;

        match item {
            ast::ItemKind::Const(item) => item.fmt(cx),
            ast::ItemKind::ConstBlock(item) => item.fmt(cx),
            ast::ItemKind::Delegation(item) => item.fmt(cx),
            ast::ItemKind::Enum(item) => item.fmt(cx),
            ast::ItemKind::ExternBlock(item) => (*item, attrs).fmt(cx),
            ast::ItemKind::ExternCrate(item) => item.fmt(cx),
            ast::ItemKind::Fn(item) => (*item, attrs).fmt(cx),
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
        let Self { override_policy, type_level: tyness, binder, generics, ty, body } = self;

        override_policy.trailing_space().fmt(cx);
        match tyness {
            ast::TypeLevel::Yes => fmt!(cx, "type "),
            ast::TypeLevel::No => {}
        }
        fmt!(cx, "const ");
        binder.fmt(cx);
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

impl Fmt for ast::ConstBlockItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { body } = self;

        fmt!(cx, "const ");
        body.fmt(cx);
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

impl Fmt for ast::DelegationPathTree<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, kind } = self;
        let is_non_empty = !path.segs.is_empty();
        path.fmt(cx);
        if is_non_empty && !matches!(kind, ast::DelegationPathTreeKind::Stump(_)) {
            fmt!(cx, "::");
        }
        kind.fmt(cx);
    }
}

impl Fmt for ast::DelegationPathTreeKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Global => fmt!(cx, "*"),
            Self::Stump(binder) => binder.map(Renaming).fmt(cx),
            Self::Branch(binders) => {
                fmt!(cx, "{{");
                binders
                    .into_iter()
                    .map(|(ident, binder)| {
                        super::Inline(move |cx| {
                            ident.fmt(cx);
                            binder.map(Renaming).fmt(cx);
                        })
                    })
                    .interleave(", ")
                    .fmt(cx);
                fmt!(cx, "}}");
            }
        }
    }
}

impl Fmt for ast::EnumItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, variants } = self;

        fmt!(cx, "enum ");
        binder.fmt(cx);
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

        binder.fmt(cx);
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
        let Self { attrs, vis, mut_restriction, ty, default } = self;
        // FIXME: Inspect attrs to look for fmt skips.
        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }
        vis.trailing_space().fmt(cx);
        if let Some(path) = mut_restriction {
            fmt!(cx, "mut");
            Restriction(path).fmt(cx);
            fmt!(cx, " ");
        }
        ty.fmt(cx);
        if let Some(default) = default {
            fmt!(cx, " = ");
            default.fmt(cx);
        }
    }
}

impl Fmt for ast::StructFieldDef<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, mut_restriction, safety, binder, ty, default } = self;
        // FIXME: Inspect attrs to look for fmt skips.
        for attr in attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);
        if let Some(path) = mut_restriction {
            fmt!(cx, "mut");
            Restriction(path).fmt(cx);
            fmt!(cx, " ");
        }
        safety.trailing_space().fmt(cx);

        binder.fmt(cx);
        fmt!(cx, ": ");
        ty.fmt(cx);
        if let Some(default) = default {
            fmt!(cx, " = ");
            default.fmt(cx);
        }
    }
}

impl Fmt for (ast::ExternBlockItem<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::ExternBlockItem { safety, abi, body } = item;

        safety.trailing_space().fmt(cx);
        fmt!(cx, "extern ");
        abi.trailing_space().fmt(cx);
        Cluster { attrs, nodes: body }.fmt(cx);
    }
}

impl Fmt for ast::ExternCrateItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { target, binder } = self;

        fmt!(cx, "extern crate ");
        target.fmt(cx);
        if let Some(binder) = binder {
            fmt!(cx, " as ");
            binder.fmt(cx);
        }
        fmt!(cx, ";");
    }
}

impl Fmt for ast::ExternItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        let (outer_attrs, inner_attrs) = attrs.partition();
        if cx.should_preserve(&outer_attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in outer_attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);

        match kind {
            ast::ExternItemKind::Fn(item) => (*item, inner_attrs).fmt(cx),
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

impl Fmt for (ast::FnItem<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::FnItem { modifiers, binder, generics, params, ret_ty, contract, body } = item;

        modifiers.trailing_space().fmt(cx);
        fmt!(cx, "fn ");
        binder.fmt(cx);
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
            (body, attrs).fmt(cx);
        } else {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for TrailingSpace<ast::FnItemModifiers<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(modifiers) = self;

        let ast::FnItemModifiers { override_policy, const_, async_, gen_, safety, extern_ } =
            modifiers;

        override_policy.trailing_space().fmt(cx);
        const_.trailing_space().fmt(cx);
        async_.trailing_space().fmt(cx);
        gen_.trailing_space().fmt(cx);
        safety.trailing_space().fmt(cx);
        extern_.trailing_space().fmt(cx);
    }
}

impl Fmt for ast::FnParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, pat, ty } = self;

        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, " ");
        }

        match (ty, pat) {
            (ast::Ty::ImplicitSelf, ast::Pat::Binding(ast::BindingPat { mut_, binder, .. })) => {
                mut_.trailing_space().fmt(cx);
                binder.fmt(cx);
            }
            (
                ast::Ty::Ref(ast::RefTy { lt, mut_, kind, pointee: ast::Ty::ImplicitSelf }),
                ast::Pat::Binding(ast::BindingPat { binder, .. }),
            ) => {
                fmt!(cx, "&");
                lt.trailing_space().fmt(cx);
                (kind, mut_).trailing_space().fmt(cx);
                binder.fmt(cx);
            }
            (ty, pat) => {
                if let ast::Pat::Wildcard(ast::WildcardKind::Empty) = pat {
                    // do nothing
                } else {
                    pat.fmt(cx);
                    fmt!(cx, ": ");
                }

                ty.fmt(cx);
            }
        }
    }
}

// FIXME: LeadingSpace<_>
impl Fmt for ast::Contract<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { requires, ensures } = self;

        if let Some(requires) = requires {
            fmt!(cx, " contract_requires ");
            requires.fmt(cx);
        }
        if let Some(ensures) = ensures {
            fmt!(cx, " contract_ensures ");
            ensures.fmt(cx);
        }
    }
}

impl Fmt for (ast::ImplItem<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::ImplItem { generics, const_, trait_ref, self_ty, body } = item;

        if let Some(ast::ImplTraitRef { override_policy, safety: _, polarity: _, path: _ }) =
            trait_ref
        {
            override_policy.trailing_space().fmt(cx);
        }

        match body {
            ast::ImplBody::Normal(_) => {}
            ast::ImplBody::Delegated(_) => fmt!(cx, "reuse "),
        }

        const_.trailing_space().fmt(cx);

        if let Some(ast::ImplTraitRef { override_policy: _, safety, polarity: _, path: _ }) =
            trait_ref
        {
            safety.trailing_space().fmt(cx);
        }

        fmt!(cx, "impl");
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        fmt!(cx, " ");

        if let Some(ast::ImplTraitRef { override_policy: _, safety: _, polarity, path }) = trait_ref
        {
            match polarity {
                ast::ImplPolarity::Positive => {}
                ast::ImplPolarity::Negative(_) => fmt!(cx, "!"),
            }
            path.fmt(cx);
            fmt!(cx, " for ");
        }
        self_ty.fmt(cx);
        generics.preds.fmt(cx);
        (body, attrs).fmt(cx);
    }
}

impl Fmt for (ast::ImplBody<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
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

impl Fmt for (ast::ModItem<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::ModItem { safety, binder, body } = item;

        safety.trailing_space().fmt(cx);
        fmt!(cx, "mod ");
        binder.fmt(cx);
        if let Some(items) = body {
            fmt!(cx, " ");
            Cluster { attrs, nodes: items }.fmt(cx);
        } else {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for ast::StaticItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { safety, mut_, binder, ty, body } = self;

        safety.trailing_space().fmt(cx);
        fmt!(cx, "static ");
        mut_.trailing_space().fmt(cx);
        binder.fmt(cx);
        fmt!(cx, ": ");
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

        fmt!(cx, "struct ");
        binder.fmt(cx);
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

impl Fmt for (ast::TraitItem<'_>, Vec<ast::Attr<'_, ast::InnerAttrStyle>>) {
    fn fmt(self, cx: &mut Cx<'_>) {
        let (item, attrs) = self;
        let ast::TraitItem { modifiers, binder, generics, bounds, body } = item;

        modifiers.trailing_space().fmt(cx);
        fmt!(cx, "trait ");
        binder.fmt(cx);
        if !generics.params.is_empty() {
            generics.params.fmt(cx);
        }
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, " ");
        Cluster { attrs, nodes: body }.fmt(cx);
    }
}

impl Fmt for TrailingSpace<ast::TraitItemModifiers<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(ast::TraitItemModifiers { impl_restriction, const_, safety, auto }) = self;

        if let Some((_, path)) = impl_restriction {
            fmt!(cx, "impl");
            Restriction(path).fmt(cx);
            fmt!(cx, " ");
        }
        const_.trailing_space().fmt(cx);
        safety.trailing_space().fmt(cx);
        auto.trailing_space().fmt(cx);
    }
}

impl Fmt for ast::TraitAliasItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { const_, binder, generics, bounds } = self;

        const_.trailing_space().fmt(cx);
        fmt!(cx, "trait ");
        binder.fmt(cx);
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
        let Self { override_policy, binder, generics, bounds, body } = self;

        override_policy.trailing_space().fmt(cx);
        fmt!(cx, "type ");
        binder.fmt(cx);
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

        fmt!(cx, "union ");
        binder.fmt(cx);
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

impl Fmt for ast::UsePathTree<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, kind } = self;
        let is_non_empty = !path.segs.is_empty();
        path.fmt(cx);
        if is_non_empty && !matches!(kind, ast::UsePathTreeKind::Stump(_)) {
            fmt!(cx, "::");
        }
        kind.fmt(cx);
    }
}

impl Fmt for ast::UsePathTreeKind<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Global => fmt!(cx, "*"),
            Self::Stump(binder) => binder.map(Renaming).fmt(cx),
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

        fmt!(cx, "{prefix} ");
        binder.fmt(cx);
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

        let (outer_attrs, inner_attrs) = attrs.partition();
        if cx.should_preserve(&outer_attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in outer_attrs {
            attr.fmt(cx);
            LineBreak.fmt(cx);
        }

        vis.trailing_space().fmt(cx);

        match kind {
            ast::AssocItemKind::Const(item) => item.fmt(cx),
            ast::AssocItemKind::Delegation(item) => item.fmt(cx),
            ast::AssocItemKind::Fn(item) => (*item, inner_attrs).fmt(cx),
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
                fmt!(cx, "pub");
                Restriction(path).fmt(cx);
                fmt!(cx, " ");
            }
            ast::Visibility::Public => fmt!(cx, "pub "),
        }
    }
}

struct Restriction<'src>(ast::Path<'src, ast::NoGenericArgs>);

impl Fmt for Restriction<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(path) = self;

        fmt!(cx, "(");
        if let [seg] = path.segs.as_slice()
            && let name @ ("crate" | "super" | "self") = seg.ident.name
        {
            name.fmt(cx);
        } else {
            fmt!(cx, "in ");
            path.fmt(cx);
        }
        fmt!(cx, ")");
    }
}

impl Fmt for TrailingSpace<ast::Const> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(const_) = self;
        match const_ {
            ast::Const::Yes => fmt!(cx, "const "),
            ast::Const::No => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Async> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(async_) = self;
        match async_ {
            ast::Async::Yes => fmt!(cx, "async "),
            ast::Async::No => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Gen> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(gen_) = self;
        match gen_ {
            ast::Gen::Yes => fmt!(cx, "gen "),
            ast::Gen::No => {}
        }
    }
}

impl<X> Fmt for TrailingSpace<ast::Safety<X>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(safety) = self;
        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Safe(_) => fmt!(cx, "safe "),
            ast::Safety::Unsafe(_) => fmt!(cx, "unsafe "),
        }
    }
}

impl Fmt for TrailingSpace<ast::Extern<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(extern_) = self;
        match extern_ {
            ast::Extern::Yes(abi) => {
                fmt!(cx, "extern ");
                abi.trailing_space().fmt(cx);
            }
            ast::Extern::No => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::Auto> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(auto) = self;
        match auto {
            ast::Auto::Yes(_) => fmt!(cx, "auto "),
            ast::Auto::No => {}
        }
    }
}

impl Fmt for TrailingSpace<ast::OverridePolicy> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(policy) = self;
        match policy {
            ast::OverridePolicy::Allowed => fmt!(cx, "default "),
            ast::OverridePolicy::Forbidden => fmt!(cx, "final "),
            ast::OverridePolicy::Implicit => {}
        }
    }
}

struct Renaming<'src>(ast::Ident<'src>);

impl Fmt for Renaming<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(binder) = self;

        fmt!(cx, " as ");
        binder.fmt(cx);
    }
}
