use super::{
    Fragment, Parser, Result, TokenKind, TokenPrefix,
    common::FnParamMode,
    expr::AttrPolicy,
    frags,
    path::{GenericArgsMode, PathMode},
    weak::{self, Weak as _},
};
use crate::{
    ast, edition::Edition, error::Error, feature::Feature, span::Span, store::Store,
    token::PathSegIdent,
};
use std::mem;

impl<'src> Parser<'_, '_, 'src> {
    /// Parse a source file.
    pub(super) fn parse_file(&mut self) -> Result<ast::File<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let items = self.parse_items(ItemCx::Boring, TokenKind::EndOfInput)?;

        Ok(ast::File { attrs, items })
    }

    /// Parse a sequence of items.
    pub(super) fn parse_items(
        &mut self,
        cx: ItemCx,
        delim: TokenKind,
    ) -> Result<Vec<ast::Item<'src>>> {
        let mut items = Vec::new();

        // We look for a delimiter instead of checking "`begins_item`" for better diagnostics.
        while !self.consume(delim) {
            items.push(self.parse_item(cx)?);
        }

        Ok(items)
    }

    /// Parse an item.
    pub(super) fn parse_item(&mut self, cx: ItemCx) -> Result<ast::Item<'src>> {
        // NOTE: To be kept in sync with `Self::begins_final_non_macro_call_item`.

        let start = self.token.span;

        let mut attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        let vis = self.parse_visibility()?;
        let override_policy = self.parse_override_policy();

        let kind = self.parse_item_kind(override_policy, cx, &mut attrs)?;

        // FIXME: Find a better way to obtain the span
        let span = self.prev_token().map_or(start, |token| start.to(token.span));

        if !matches!(vis, ast::Visibility::Inherited) && !kind.supports_visibility() {
            self.error(Error::VisibilityOnInvalidItem(span));
        }

        if !kind.supports_explicit_override_policy() {
            match override_policy {
                ast::OverridePolicy::Allowed => self.error(Error::DefaultOnInvalidItem(span)),
                ast::OverridePolicy::Forbidden => self.error(Error::FinalOnInvalidItem(span)),
                ast::OverridePolicy::Implicit => {}
            }
        }

        Ok(ast::Item { attrs, vis, kind, span })
    }

    /// Indicates whether the current token begins a restricted item.
    ///
    /// Restricted items exclude
    ///
    /// 1. items marked with modifier `default`,
    /// 2. macro call items and
    /// 3. const block items.
    //
    // FIXME: Experiment with doing the stmt(item<->expr) disambiguation in begins_expr instead.
    // FIXME: Experiment with replacing this with an parse_item_prefix that rets Option<ItemPrefix>
    //        to be then used for fin_parse_item(prefix)
    pub(super) fn begins_restricted_item(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_item`.

        match self.token.kind {
            | TokenKind::Enum
            | TokenKind::Final
            | TokenKind::Macro
            | TokenKind::Struct
            | TokenKind::Trait => return true,
            _ => {}
        }

        if let TokenKind::Use = self.token.kind {
            // NOTE: We need to disqualify `use |…` which denotes a closure
            //       under feature `ergonomic_clones`.
            //       We didn't turn `Use` into an `ItemQualifier` that could be discarded below
            //       since the rules are slightly more complex, so it's not worth it.
            //       E.g., while `async use {}` is an expr, `use {}` is an item.
            return !self.matches(TokenPrefix::Pipe, self.peek(1));
        }

        if self.begins_outer_attr() || self.begins_visibility() {
            return true;
        }

        if self.check(weak::MacroRules) || self.check(weak::Union) {
            return true;
        }

        let mut qualified = false;
        let store = Store::sealed();
        for (qualifier, token) in self.snapshot(&store).parse_item_qualifiers() {
            match qualifier {
                Qualifier::Async | Qualifier::Const(_) | Qualifier::Gen(_) | Qualifier::Static => {}
                _ => return true,
            }

            // The token sequence actually begins a block or closure expression, not an item; bail out.
            // FIXME: Also check for `|=` and `||=` for diagnostic purposes.
            if let TokenKind::OpenCurlyBracket
            | TokenKind::SinglePipe
            | TokenKind::DoublePipe
            | TokenKind::Move
            | TokenKind::Use = token
            {
                return false;
            }

            qualified = true;
        }

        qualified
    }

    fn parse_override_policy(&mut self) -> ast::OverridePolicy {
        let span = self.token.span;
        if self.consume(weak::Default) {
            // FIXME: replace with `MinSpecialization` if the item ends up being a fn.
            self.feature(Feature::specialization, span);
            ast::OverridePolicy::Allowed
        } else if self.consume(TokenKind::Final) {
            self.feature(Feature::final_associated_functions, span);
            ast::OverridePolicy::Forbidden
        } else {
            ast::OverridePolicy::Implicit
        }
    }

    fn parse_item_kind(
        &mut self,
        override_policy: ast::OverridePolicy,
        cx: ItemCx,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        // NOTE: To be kept in sync with `Self::begins_final_non_macro_call_item`.

        let start = self.token.span;

        let mut qualifiers: Vec<_> =
            self.parse_item_qualifiers().map(|(qualifier, _)| qualifier).collect();

        // FIXME: Provide more targeted diagnostics if the qualifiers don't make sense.
        match qualifiers.as_mut_slice() {
            [] => {}
            [Qualifier::Type(_)] => return self.fin_parse_ty_alias_item(override_policy),
            [Qualifier::Const(span)] if self.consume(TokenKind::OpenCurlyBracket) => {
                self.feature(Feature::const_block_items, *span);
                return self.fin_parse_const_block_item();
            }
            [qualifiers @ .., Qualifier::Const(_)] => {
                let (type_level, qualifiers) = match qualifiers {
                    [Qualifier::Type(span), qualifiers @ ..] => {
                        // FIXME: There's also feature gate `mgca_type_const_syntax`.
                        self.feature(Feature::min_generic_const_args, *span);
                        (ast::TypeLevel::Yes, qualifiers)
                    }
                    _ => (ast::TypeLevel::No, qualifiers),
                };
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_const_item(override_policy, type_level);
            }
            // `crate` can't be a qualifier itself because it may also begin paths & it's not worth the look-ahead.
            [Qualifier::Extern(None)] if self.consume(TokenKind::Crate) => {
                return self.fin_parse_extern_crate_item();
            }
            [Qualifier::Reuse(span)] => {
                self.feature(Feature::fn_delegation, *span);
                return self.fin_parse_delegation_item();
            }
            [qualifiers @ .., Qualifier::Mod] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_mod_item(safety, attrs);
            }
            [qualifiers @ .., Qualifier::Static] => {
                let (safety, qualifiers) = Qualifier::strip_safety(qualifiers);
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_static_item(safety);
            }
            &mut [mut ref mut qualifiers @ .., Qualifier::Fn] => {
                let mut modifiers = ast::FnItemModifiers { override_policy, .. };

                (modifiers.const_, qualifiers) = Qualifier::strip_const(qualifiers, drop);
                (modifiers.async_, qualifiers) = match qualifiers {
                    [Qualifier::Async, qualifiers @ ..] => (ast::Async::Yes, qualifiers),
                    _ => (ast::Async::No, qualifiers),
                };
                (modifiers.gen_, qualifiers) = match qualifiers {
                    [Qualifier::Gen(span), qualifiers @ ..] => {
                        self.feature(Feature::gen_blocks, *span);
                        (ast::Gen::Yes, qualifiers)
                    }
                    _ => (ast::Gen::No, qualifiers),
                };
                (modifiers.safety, qualifiers) = Qualifier::strip_safety(qualifiers);
                (modifiers.extern_, qualifiers) = Qualifier::strip_extern(qualifiers);
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_fn_item(modifiers, cx, attrs);
            }
            &mut [mut ref mut qualifiers @ .., Qualifier::Trait] => {
                let mut modifiers = ast::TraitItemModifiers::default();

                (modifiers.impl_restriction, qualifiers) = match qualifiers {
                    [Qualifier::ImplRestriction(span, path), qualifiers @ ..] => {
                        self.feature(Feature::impl_restriction, *span);
                        let Ok(path) = path else { return Err(()) };
                        let path = mem::replace(path, ast::Path { segs: Vec::new() });
                        (Some((*span, path)), qualifiers)
                    }
                    _ => (None, qualifiers),
                };
                (modifiers.const_, qualifiers) = Qualifier::strip_const(qualifiers, |span| {
                    self.feature(Feature::const_trait_impl, span)
                });
                (modifiers.safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                (modifiers.auto, qualifiers) = match qualifiers {
                    [Qualifier::Auto(span), qualifiers @ ..] => {
                        self.feature(Feature::auto_traits, *span);
                        (ast::Auto::Yes(*span), qualifiers)
                    }
                    _ => (ast::Auto::No, qualifiers),
                };
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_trait_item(modifiers, attrs);
            }
            [qualifiers @ .., Qualifier::Impl] => {
                let (kind, qualifiers) = match qualifiers {
                    [Qualifier::Reuse(span), qualifiers @ ..] => {
                        self.feature(Feature::fn_delegation, *span);
                        (ImplKind::Delegation(*span), qualifiers)
                    }
                    _ => (ImplKind::Normal, qualifiers),
                };
                let (const_, qualifiers) = Qualifier::strip_const(qualifiers, |span| {
                    self.feature(Feature::const_trait_impl, span)
                });
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_impl_item(override_policy, kind, const_, safety, attrs);
            }
            [qualifiers @ .., Qualifier::Extern(abi)] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_extern_block_item(safety, *abi, attrs);
            }
            _ => {
                return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
            }
        }

        match self.token.kind {
            TokenKind::Enum => {
                self.advance();
                return self.fin_parse_enum_item();
            }
            TokenKind::CommonIdent => match self.source(self.token.span) {
                weak::Union::STR if weak::Union.qualifies(self) => {
                    self.advance();
                    let binder = self.ident(self.token.span);
                    self.advance();
                    return self.fin_parse_union_item(binder);
                }
                _ => {}
            },
            TokenKind::Macro => {
                self.feature(Feature::decl_macro, self.token.span);
                self.advance();
                return self.fin_parse_macro_def();
            }
            TokenKind::Struct => {
                self.advance();
                return self.fin_parse_struct_item();
            }
            TokenKind::Use => {
                self.advance();
                return self.fin_parse_use_item();
            }
            _ => {}
        }

        if self.begins_path(0) {
            return self.parse_macro_call_item();
        }

        self.fatal(Error::UnexpectedToken(self.token, frags![Fragment::Item]))
    }

    gen fn parse_item_qualifiers(&mut self) -> (Qualifier<'src>, TokenKind) {
        loop {
            let qualifier = match self.token.kind {
                TokenKind::Async => Qualifier::Async,
                TokenKind::Const => Qualifier::Const(self.token.span),
                TokenKind::Extern => {
                    self.advance();
                    yield (Qualifier::Extern(self.parse_abi_str()), self.token.kind);
                    continue;
                }
                TokenKind::Fn => Qualifier::Fn,
                TokenKind::Gen => Qualifier::Gen(self.token.span),
                TokenKind::CommonIdent => match self.source(self.token.span) {
                    weak::Auto::STR if weak::Auto.qualifies(self) => {
                        Qualifier::Auto(self.token.span)
                    }
                    weak::Reuse::STR if weak::Reuse.qualifies(self) => {
                        Qualifier::Reuse(self.token.span)
                    }
                    weak::Safe::STR if weak::Safe.qualifies(self) => Qualifier::Safe,
                    _ => return,
                },
                TokenKind::Impl => {
                    let span = self.token.span;
                    self.advance();

                    // We disqualify sequences like `impl(crate) {` which should be
                    // interpreted as impl blocks instead.
                    if self.begins_restriction(|kind| kind != TokenKind::OpenCurlyBracket) {
                        let path = self.parse_restriction();
                        // FIXME: The `span` should contain the entire restriction.
                        yield (Qualifier::ImplRestriction(span, Box::new(path)), self.token.kind);
                        continue;
                    }

                    yield (Qualifier::Impl, self.token.kind);
                    // Once we encounter `impl`, don't attempt to look for more item qualifiers.
                    // That's because the grammar following an impl item's `impl` is very complex &
                    // partially clashes with item qualifiers.
                    //
                    // We need to be wary of cases like `impl impl Trait {}` (accept),
                    // `impl const Ty {}` (accept) `impl const <T> Ty {}` (reject),
                    // `impl const <() as Trait>::Ty {}` (accept).
                    //
                    // For the `const` case, we *could* utilize `pick_generic_param_list_over_ext_path`
                    // but it's not worth the complexity. Alternatively, we could disqualify
                    // [.., Impl, Const] in `parse_item_kind` using that very same "pick" method but
                    // that feels gnarly, esp. since qualifiers are meant to be "unambiguous" for
                    // downstream users but
                    // that feels gnarly, esp. since qualifiers are meant to be "unambiguous" for
                    // downstream users
                    // Another attempt at a solution could be to intro a generic param list qualifier
                    // similar to `expr::Qualifier::For`. That isn't really nice to work with though
                    // because we can't nicely match on "suffix" qualifiers as the premise of qualifier
                    // matching is avoiding combinatorial explosions of subparsers by fixing the "herald"
                    // (here: `impl`) in the final position and collecting all modifiers linearly.
                    // You can't do (that with) [.., Impl, ..].
                    return;
                }
                TokenKind::Mod => Qualifier::Mod,
                TokenKind::Static => Qualifier::Static,
                TokenKind::Trait => Qualifier::Trait,
                TokenKind::Type => Qualifier::Type(self.token.span),
                TokenKind::Unsafe if self.peek(1).kind != TokenKind::OpenCurlyBracket => {
                    Qualifier::Unsafe(self.token.span)
                }
                _ => return,
            };
            self.advance();
            yield (qualifier, self.token.kind);
        }
    }

    /// Finish parsing a constant item assuming the leading `const` has been parsed already.
    fn fin_parse_const_item(
        &mut self,
        override_policy: ast::OverridePolicy,
        type_level: ast::TypeLevel,
    ) -> Result<ast::ItemKind<'src>> {
        let (binder, _) = self.parse_common_ident_or(TokenKind::Underscore)?;
        let params = self
            .parse_generic_param_list()?
            .inspect(|_| self.feature_no_span_fixme(Feature::generic_const_items))
            .unwrap_or_default();
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        let preds = self
            .parse_where_clause()?
            .inspect(|_| self.feature_no_span_fixme(Feature::generic_const_items))
            .unwrap_or_default();
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Const(Box::new(ast::ConstItem {
            override_policy,
            type_level,
            binder,
            generics: ast::Generics { params, preds },
            ty,
            body,
        })))
    }

    /// Finish parsing a const block item assuming the leading `const {` has been parsed already.
    fn fin_parse_const_block_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let body = self.fin_parse_block_expr(AttrPolicy::Reject)?;

        Ok(ast::ItemKind::ConstBlock(Box::new(ast::ConstBlockItem { body })))
    }

    /// Finish parsing an enumeration item assuming the leading `enum` has been parsed already.
    fn fin_parse_enum_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let generics = self.parse_generics()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        let variants = self.fin_parse_delim_seq(
            TokenKind::CloseCurlyBracket,
            TokenKind::Comma,
            Self::parse_variant,
        )?;

        Ok(ast::ItemKind::Enum(Box::new(ast::EnumItem { binder, generics, variants })))
    }

    fn parse_variant(&mut self) -> Result<ast::Variant<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        let vis = self.parse_visibility()?;
        let (binder, unnamed) = self.parse_common_ident_or(TokenKind::Underscore)?;
        if unnamed {
            self.feature(Feature::unnamed_enum_variants, binder.span);
        }
        let kind = self.parse_variant_kind()?;
        let discr = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        Ok(ast::Variant { attrs, vis, binder, kind, discr })
    }

    fn parse_variant_kind(&mut self) -> Result<ast::VariantKind<'src>> {
        Ok(match self.token.kind {
            TokenKind::OpenRoundBracket => {
                self.advance();
                let fields = self.fin_parse_tuple_struct_fields()?;
                ast::VariantKind::Tuple(fields)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                let fields = self.fin_parse_struct_fields()?;
                ast::VariantKind::Struct(fields)
            }
            _ => ast::VariantKind::Unit,
        })
    }

    fn fin_parse_tuple_struct_fields(&mut self) -> Result<Vec<ast::TupleFieldDef<'src>>> {
        self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
            let vis = this.parse_visibility()?;
            let mut_restriction = this.parse_mut_restriction()?;
            let ty = this.parse_ty()?;
            let default = this.parse_field_default()?;
            Ok(ast::TupleFieldDef { attrs, vis, mut_restriction, ty, default })
        })
    }

    fn fin_parse_struct_fields(&mut self) -> Result<Vec<ast::StructFieldDef<'src>>> {
        self.fin_parse_delim_seq(TokenKind::CloseCurlyBracket, TokenKind::Comma, |this| {
            let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
            let vis = this.parse_visibility()?;
            let mut_restriction = this.parse_mut_restriction()?;
            let safety = if let span = this.token.span
                && this.consume(TokenKind::Unsafe)
            {
                this.feature(Feature::unsafe_fields, span);
                ast::Safety::Unsafe(span)
            } else {
                ast::Safety::Inherited
            };
            let binder = this.parse_common_ident()?;
            let ty = this.parse_ty_annotation()?;
            let default = this.parse_field_default()?;
            Ok(ast::StructFieldDef { attrs, vis, mut_restriction, safety, binder, ty, default })
        })
    }

    fn parse_mut_restriction(&mut self) -> Result<Option<ast::Path<'src, ast::NoGenericArgs>>> {
        // No need to consult `begins_restriction`, it's unambiguous.
        if let span = self.token.span
            && self.consume(TokenKind::Mut)
        {
            self.feature(Feature::mut_restriction, span);
            self.parse_restriction().map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_field_default(&mut self) -> Result<Option<ast::Expr<'src>>> {
        if let span = self.token.span
            && self.consume(TokenKind::SingleEquals)
        {
            self.feature(Feature::default_field_values, span);
            self.parse_expr().map(Some)
        } else {
            Ok(None)
        }
    }

    /// Finish parsing an extern block item assuming the leading `"extern" #Str_Lit?` has been parsed already.
    fn fin_parse_extern_block_item(
        &mut self,
        safety: ast::Safety,
        abi: Option<&'src str>,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        self.parse_attrs_into(ast::AttrStyle::Inner, attrs)?;

        let mut items = Vec::new();

        for item in self.parse_items(ItemCx::Boring, TokenKind::CloseCurlyBracket)? {
            items.push(ast::ExternItem {
                attrs: item.attrs,
                vis: item.vis,
                kind: match item.kind {
                    ast::ItemKind::Static(item) => ast::ExternItemKind::Static(item),
                    ast::ItemKind::Fn(item) => ast::ExternItemKind::Fn(item),
                    ast::ItemKind::MacroCall(item) => ast::ExternItemKind::MacroCall(item),
                    ast::ItemKind::TyAlias(item) => ast::ExternItemKind::Ty(item),
                    _ => {
                        self.error(Error::InvalidExternItemKind(item.span));
                        continue;
                    }
                },
                span: item.span,
            });
        }

        Ok(ast::ItemKind::ExternBlock(Box::new(ast::ExternBlockItem { safety, abi, body: items })))
    }

    /// Finish parsing an extern crate item assuming the leading `extern crate` has been parsed already.
    fn fin_parse_extern_crate_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let (target, _) = self.parse_common_ident_or(TokenKind::SelfLower)?;
        let binder = self
            .consume(TokenKind::As)
            .then(|| self.parse_common_ident_or(TokenKind::Underscore).map(|(binder, _)| binder))
            .transpose()?;

        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::ExternCrate(Box::new(ast::ExternCrateItem { target, binder })))
    }

    /// Finish parsing a function item assuming the leading `Fn_Modifiers "fn"` has already been parsed.
    fn fin_parse_fn_item(
        &mut self,
        modifiers: ast::FnItemModifiers<'src>,
        cx: ItemCx,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let gen_params = self.parse_generic_param_list()?.unwrap_or_default();
        let params = self.parse_fn_param_list(match (cx, self.edition) {
            (ItemCx::Trait, Edition::Rust2015) => FnParamMode::Optional,
            _ => FnParamMode::Required,
        })?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;
        let contract = self.parse_contract()?;
        let preds = self.parse_where_clause()?.unwrap_or_default();

        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            Some(self.fin_parse_block_expr(AttrPolicy::Parse(attrs))?)
        } else {
            self.parse(TokenKind::Semicolon)?;
            None
        };

        Ok(ast::ItemKind::Fn(Box::new(ast::FnItem {
            modifiers,
            binder,
            generics: ast::Generics { params: gen_params, preds },
            params,
            ret_ty,
            contract,
            body,
        })))
    }

    fn parse_contract(&mut self) -> Result<ast::Contract<'src>> {
        let mut contract = ast::Contract { requires: None, ensures: None };

        if let span = self.token.span
            && self.consume(weak::ContractRequires)
        {
            self.feature(Feature::contract_internals, span);
            let block = self.parse_block_expr(AttrPolicy::Reject)?;
            contract.requires = Some(Box::new(block));
        }

        if let span = self.token.span
            && self.consume(weak::ContractEnsures)
        {
            self.feature(Feature::contract_internals, span);
            contract.ensures = Some(Box::new(self.parse_expr()?));
        }

        Ok(contract)
    }

    /// Finish parsing an implementation item assuming the leading `impl` has been parsed already.
    fn fin_parse_impl_item(
        &mut self,
        override_policy: ast::OverridePolicy,
        kind: ImplKind,
        const_: ast::Const,
        safety: ast::Safety,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let params = if self.pick_generic_param_list_over_ext_path(0) {
            self.parse_generic_param_list()?.unwrap_or_default()
        } else {
            Vec::new()
        };

        let polarity = if self.token.kind == TokenKind::SingleBang
            && self.peek(1).kind != TokenKind::OpenCurlyBracket
        {
            let span = self.token.span;
            self.feature(Feature::negative_impls, span);
            self.advance();
            ast::ImplPolarity::Negative(span)
        } else {
            ast::ImplPolarity::Positive
        };

        // FIXME: HACK: `Ty` should just carry a span.
        let ty_start = self.token.span;
        let ty = self.parse_ty()?;
        let ty_span = ty_start.until(self.token.span);

        let (trait_ref, self_ty) = if self.consume(TokenKind::For) {
            let self_ty =
                if self.consume(TokenKind::DoubleDot) { ast::Ty::All } else { self.parse_ty()? };
            let trait_ref = if let ast::Ty::Path(ast::ExtPath { ext: None, path: trait_ref }) = ty {
                Some(trait_ref)
            } else {
                self.error(Error::ExpectedTraitFoundTy(ty_span));
                None
            };
            (trait_ref, self_ty)
        } else {
            (None, ty)
        };

        let preds = self.parse_where_clause()?.unwrap_or_default();

        let trait_ref = if let Some(path) = trait_ref {
            Some(ast::ImplTraitRef { override_policy, safety, polarity, path })
        } else {
            match polarity {
                ast::ImplPolarity::Positive => {}
                ast::ImplPolarity::Negative(span) => {
                    self.error(Error::TraitImplModifierInInherentImpl(span, "!"));
                }
            }

            match safety {
                ast::Safety::Inherited => {}
                ast::Safety::Unsafe(span) => {
                    self.error(Error::TraitImplModifierInInherentImpl(span, "unsafe"));
                }
            }

            None
        };

        let body = match kind {
            ImplKind::Normal => {
                let items = self.parse_delimited_assoc_items(ItemCx::Boring, attrs)?;
                ast::ImplBody::Normal(items)
            }
            ImplKind::Delegation(span) => {
                if trait_ref.is_none() {
                    self.error(Error::ReuseInherentImpl(span));
                }

                let body = self.parse_delegation_body()?;
                ast::ImplBody::Delegated(body)
            }
        };

        Ok(ast::ItemKind::Impl(Box::new(ast::ImplItem {
            generics: ast::Generics { params, preds },
            const_,
            trait_ref,
            self_ty,
            body,
        })))
    }

    /// Finish parsing a macro (2.0) definition assuming the leading `macro` has been parsed already.
    fn fin_parse_macro_def(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = if self.consume(TokenKind::OpenRoundBracket) {
            let (_, params) = self.fin_parse_delimited_token_stream(ast::Bracket::Round)?;
            Some(params)
        } else {
            None
        };
        self.parse(TokenKind::OpenCurlyBracket)?;
        let (_, body) = self.fin_parse_delimited_token_stream(ast::Bracket::Curly)?;
        Ok(ast::ItemKind::MacroDef(Box::new(ast::MacroDef {
            binder,
            params,
            body,
            style: ast::MacroDefStyle::New,
        })))
    }

    /// Finish parsing a module item assuming the leading `mod` has been parsed already.
    fn fin_parse_mod_item(
        &mut self,
        safety: ast::Safety,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let items = if self.consume(TokenKind::OpenCurlyBracket) {
            self.parse_attrs_into(ast::AttrStyle::Inner, attrs)?;
            Some(self.parse_items(ItemCx::Boring, TokenKind::CloseCurlyBracket)?)
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.parse(TokenKind::Semicolon)?;
            None
        };

        Ok(ast::ItemKind::Mod(Box::new(ast::ModItem { safety, binder, body: items })))
    }

    /// Finish parsing a static item assuming the leading `static` has been parsed already.
    fn fin_parse_static_item(&mut self, safety: ast::Safety<()>) -> Result<ast::ItemKind<'src>> {
        let mut_ = self.parse_mut();
        let binder = self.parse_common_ident()?;
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Static(Box::new(ast::StaticItem { safety, mut_, binder, ty, body })))
    }

    /// Finish parsing a struct item assuming the leading `struct` has been parsed already.
    fn fin_parse_struct_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let mut generics = self.parse_generics()?;
        let kind = self.parse_variant_kind()?;
        if let ast::VariantKind::Tuple(_) = kind {
            debug_assert!(generics.preds.is_empty());
            generics.preds = self.parse_where_clause()?.unwrap_or_default();
        }
        if kind.needs_semicolon() {
            self.parse(TokenKind::Semicolon)?;
        }
        Ok(ast::ItemKind::Struct(Box::new(ast::StructItem { binder, generics, kind })))
    }

    /// Finish parsing a trait item assuming the leading `trait` has been parsed already.
    fn fin_parse_trait_item(
        &mut self,
        modifiers: ast::TraitItemModifiers<'src>,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_param_list()?.unwrap_or_default();

        if let span = self.token.span
            && self.consume(TokenKind::SingleEquals)
        {
            self.feature(Feature::trait_alias, span);
            return self.fin_parse_trait_alias_item(modifiers, binder, params);
        }

        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let preds = self.parse_where_clause()?.unwrap_or_default();

        let body = self.parse_delimited_assoc_items(ItemCx::Trait, attrs)?;

        Ok(ast::ItemKind::Trait(Box::new(ast::TraitItem {
            modifiers,
            binder,
            generics: ast::Generics { params, preds },
            bounds,
            body,
        })))
    }

    /// Finish parsing a trait alias item.
    fn fin_parse_trait_alias_item(
        &mut self,
        modifiers: ast::TraitItemModifiers<'src>,
        binder: ast::Ident<'src>,
        params: Vec<ast::GenericParam<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let bounds = self.parse_bounds()?;
        let preds = self.parse_where_clause()?.unwrap_or_default();

        self.parse(TokenKind::Semicolon)?;

        let ast::TraitItemModifiers { impl_restriction, const_, safety, auto } = modifiers;

        if let Some((span, _)) = impl_restriction {
            self.error(Error::ImplRestrictedTraitAlias(span));
        }

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe(span) => self.error(Error::UnsafeTraitAlias(span)),
        }

        match auto {
            ast::Auto::Yes(span) => self.error(Error::AutoTraitAlias(span)),
            ast::Auto::No => {}
        }

        Ok(ast::ItemKind::TraitAlias(Box::new(ast::TraitAliasItem {
            const_,
            binder,
            generics: ast::Generics { params, preds },
            bounds,
        })))
    }

    /// Finish parsing a type item assuming the leading `type` has been parsed already.
    fn fin_parse_ty_alias_item(
        &mut self,
        override_policy: ast::OverridePolicy,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_param_list()?.unwrap_or_default();
        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let mut preds = self.parse_where_clause()?.unwrap_or_default();
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_ty()).transpose()?;
        if body.is_some() {
            preds.append(&mut self.parse_where_clause()?.unwrap_or_default());
        }
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::TyAlias(Box::new(ast::TyAliasItem {
            override_policy,
            binder,
            generics: ast::Generics { params, preds },
            bounds,
            body,
        })))
    }

    /// Finish parsing a union item assuming the leading `"union" Common_Ident` has been parsed already.
    fn fin_parse_union_item(&mut self, binder: ast::Ident<'src>) -> Result<ast::ItemKind<'src>> {
        let generics = self.parse_generics()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        let fields = self.fin_parse_struct_fields()?;

        Ok(ast::ItemKind::Union(Box::new(ast::UnionItem { binder, generics, fields })))
    }

    /// Finish parsing a use-item assuming the leading `use` has been parsed already.
    fn fin_parse_use_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let path = self.parse_use_path_tree(PathMode::Normal)?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Use(Box::new(ast::UseItem { path })))
    }

    fn parse_use_path_tree(&mut self, mode: PathMode) -> Result<ast::UsePathTree<'src>> {
        let mut path = self.parse_path_prefix(mode)?;

        match self.parse_use_path_tree_kind(&mut path)? {
            ast::UsePathTreeKind::Stump(None) => {}
            kind => return Ok(ast::UsePathTree { path, kind }),
        }

        while self.consume(TokenKind::DoubleColon) {
            match self.parse_use_path_tree_kind(&mut path)? {
                ast::UsePathTreeKind::Stump(None) => {}
                kind => return Ok(ast::UsePathTree { path, kind }),
            }
        }

        Ok(ast::UsePathTree { path, kind: ast::UsePathTreeKind::Stump(None) })
    }

    fn parse_use_path_tree_kind(
        &mut self,
        path: &mut ast::Path<'src, ast::NoGenericArgs>,
    ) -> Result<ast::UsePathTreeKind<'src>> {
        Ok(match self.token.kind {
            TokenKind::OpenCurlyBracket => {
                self.advance();
                ast::UsePathTreeKind::Branch(self.fin_parse_delim_seq(
                    TokenKind::CloseCurlyBracket,
                    TokenKind::Comma,
                    |this| this.parse_use_path_tree(PathMode::Normal),
                )?)
            }
            TokenKind::SingleAsterisk => {
                self.advance();
                ast::UsePathTreeKind::Global
            }
            PathSegIdent!() => {
                path.segs.push(ast::PathSeg::ident(self.ident(self.token.span)));
                self.advance();
                let binder = if self.consume(TokenKind::As) {
                    let (binder, _) = self.parse_common_ident_or(TokenKind::Underscore)?;
                    Some(binder)
                } else {
                    None
                };
                ast::UsePathTreeKind::Stump(binder)
            }
            _ => {
                return self.fatal(Error::UnexpectedToken(
                    self.token,
                    // FIXME: Technically also DoubleColon under certain circumstances (e.g., `use;`).
                    frags![
                        Fragment::PathSegIdent,
                        TokenKind::OpenCurlyBracket,
                        TokenKind::SingleAsterisk
                    ],
                ));
            }
        })
    }

    fn parse_macro_call_item(&mut self) -> Result<ast::ItemKind<'src>> {
        // NOTE: To be kept in sync with `Self::begins_macro_item`.

        let path = self.parse_path::<ast::NoGenericArgs>(PathMode::Normal)?;
        self.parse(TokenKind::SingleBang)?;

        let binder = if let [ast::PathSeg { ident: ast::Ident!(weak::MacroRules::STR), args: () }] =
            *path.segs
        {
            self.consume_common_ident()
        } else {
            None
        };

        let (bracket, body) = self.parse_delimited_token_stream()?;

        if bracket != ast::Bracket::Curly {
            self.parse(TokenKind::Semicolon)?;
        }

        Ok(if let Some(binder) = binder {
            ast::ItemKind::MacroDef(Box::new(ast::MacroDef {
                binder,
                params: None,
                body,
                style: ast::MacroDefStyle::Old,
            }))
        } else {
            ast::ItemKind::MacroCall(Box::new(ast::MacroCall { path, bracket, stream: body }))
        })
    }

    fn parse_delimited_assoc_items(
        &mut self,
        cx: ItemCx,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<Vec<ast::AssocItem<'src>>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        self.parse_attrs_into(ast::AttrStyle::Inner, attrs)?;

        let mut items = Vec::new();

        for item in self.parse_items(cx, TokenKind::CloseCurlyBracket)? {
            items.push(ast::AssocItem {
                attrs: item.attrs,
                vis: item.vis,
                kind: match item.kind {
                    ast::ItemKind::Const(item) => ast::AssocItemKind::Const(item),
                    ast::ItemKind::Delegation(item) => ast::AssocItemKind::Delegation(item),
                    ast::ItemKind::Fn(item) => ast::AssocItemKind::Fn(item),
                    ast::ItemKind::MacroCall(item) => ast::AssocItemKind::MacroCall(item),
                    ast::ItemKind::TyAlias(item) => ast::AssocItemKind::Ty(item),
                    _ => {
                        self.error(Error::InvalidAssocItemKind(item.span));
                        continue;
                    }
                },
                span: item.span,
            });
        }

        Ok(items)
    }

    /// Finish parsing a delegation item assuming the leading `reuse` has been parsed already.
    fn fin_parse_delegation_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let (ext, mode) = self.parse_path_ext()?;
        let path = self.parse_delegation_path_tree(mode)?;
        let body = self.parse_delegation_body()?;

        Ok(ast::ItemKind::Delegation(Box::new(ast::DelegationItem { ext, path, body })))
    }

    fn parse_delegation_path_tree(
        &mut self,
        mode: PathMode,
    ) -> Result<ast::DelegationPathTree<'src>> {
        let mut path = self.parse_path_prefix(mode)?;

        match self.parse_delegation_path_tree_kind(&mut path)? {
            ast::DelegationPathTreeKind::Stump(None) => {}
            kind => return Ok(ast::DelegationPathTree { path, kind }),
        }

        while self.consume(TokenKind::DoubleColon) {
            match self.parse_delegation_path_tree_kind(&mut path)? {
                ast::DelegationPathTreeKind::Stump(None) => {}
                kind => return Ok(ast::DelegationPathTree { path, kind }),
            }
        }

        Ok(ast::DelegationPathTree { path, kind: ast::DelegationPathTreeKind::Stump(None) })
    }

    fn parse_delegation_path_tree_kind(
        &mut self,
        path: &mut ast::Path<'src, ast::ObligatorilyDisambiguatedGenericArgs>,
    ) -> Result<ast::DelegationPathTreeKind<'src>> {
        let parse_binder = |this: &mut Self| {
            this.consume(TokenKind::As).then(|| this.parse_common_ident()).transpose()
        };

        Ok(match self.token.kind {
            TokenKind::OpenCurlyBracket => {
                self.advance();
                ast::DelegationPathTreeKind::Branch(self.fin_parse_delim_seq(
                    TokenKind::CloseCurlyBracket,
                    TokenKind::Comma,
                    |this| {
                        let ast::PathSeg { ident, args: () } =
                            this.parse_path_seg::<ast::NoGenericArgs>()?;
                        let binder = parse_binder(this)?;
                        Ok((ident, binder))
                    },
                )?)
            }
            TokenKind::SingleAsterisk => {
                self.advance();
                ast::DelegationPathTreeKind::Global
            }
            PathSegIdent!() => {
                let ident = self.ident(self.token.span);
                self.advance();
                let args = ast::ObligatorilyDisambiguatedGenericArgs::parse(self)?;
                path.segs.push(ast::PathSeg { ident, args });
                let binder = parse_binder(self)?;
                ast::DelegationPathTreeKind::Stump(binder)
            }
            _ => {
                return self.fatal(Error::UnexpectedToken(
                    self.token,
                    frags![
                        Fragment::PathSegIdent,
                        TokenKind::OpenCurlyBracket,
                        TokenKind::SingleAsterisk
                    ],
                ));
            }
        })
    }

    fn parse_delegation_body(&mut self) -> Result<Option<ast::BlockExpr<'src>>> {
        if self.consume(TokenKind::OpenCurlyBracket) {
            Ok(Some(self.fin_parse_block_expr(AttrPolicy::Reject)?))
        } else {
            self.parse(TokenKind::Semicolon)?;
            Ok(None)
        }
    }

    fn parse_visibility(&mut self) -> Result<ast::Visibility<'src>> {
        // To kept in sync with `Self::begins_visibility`.

        if !self.consume(TokenKind::Pub) {
            return Ok(ast::Visibility::Inherited);
        }

        if self.begins_restriction(|_| true) {
            let path = self.parse_restriction()?;
            return Ok(ast::Visibility::Restricted(path));
        }

        Ok(ast::Visibility::Public)
    }

    fn begins_visibility(&self) -> bool {
        // To kept in sync with `Self::parse_visibility`.

        self.token.kind == TokenKind::Pub
    }

    // FIXME: Ideally this would return the herald.
    fn begins_restriction(&mut self, may_follow: fn(TokenKind) -> bool) -> bool {
        if self.token.kind != TokenKind::OpenRoundBracket {
            return false;
        }

        match self.peek(1).kind {
            TokenKind::In => true,
            TokenKind::Crate | TokenKind::Super | TokenKind::SelfLower => {
                self.peek(2).kind == TokenKind::CloseRoundBracket && may_follow(self.peek(3).kind)
            }
            _ => false,
        }
    }

    fn parse_restriction(&mut self) -> Result<ast::Path<'src, ast::NoGenericArgs>> {
        enum Herald {
            In,
            CrateSuperSelf(Span),
        }

        self.parse(TokenKind::OpenRoundBracket)?;

        let herald = match self.token.kind {
            TokenKind::Crate | TokenKind::Super | TokenKind::SelfLower => {
                Herald::CrateSuperSelf(self.token.span)
            }
            TokenKind::In => Herald::In,
            _ => {
                return self.fatal(Error::UnexpectedToken(
                    self.token,
                    frags![TokenKind::Crate, TokenKind::Super, TokenKind::SelfLower, TokenKind::In],
                ));
            }
        };

        self.advance();

        let path = match herald {
            Herald::In => self.parse_path(PathMode::Normal)?,
            Herald::CrateSuperSelf(span) => ast::Path::ident(self.ident(span)),
        };

        self.parse(TokenKind::CloseRoundBracket)?;

        Ok(path)
    }
}

impl ast::ItemKind<'_> {
    fn supports_visibility(&self) -> bool {
        match self {
            | Self::Const(_)
            | Self::Delegation(_)
            | Self::Enum(_)
            | Self::ExternBlock(_)
            | Self::ExternCrate(_)
            | Self::Fn(_)
            | Self::Impl(_)
            | Self::Mod(_)
            | Self::Static(_)
            | Self::Struct(_)
            | Self::Trait(_)
            | Self::TraitAlias(_)
            | Self::TyAlias(_)
            | Self::Union(_)
            | Self::Use(_) => true,
            // NOTE: rustc actually accepts `pub const {}` unless it's in a body (`fn f() { pub const {} }`).
            //       I don't want to further parametrize fn `parse_item` or this function. So I'll just ban it outright.
            //       I'm going to open an issue or PR upstream soon-ish.
            //       The first part *is* mentioned in the tracking issue but only under *Unresolved Questions*.
            //       And they've actually added test marked with a fixme: `tests/ui/parser/const-block-items/pub.rs`.
            Self::ConstBlock(_) | Self::MacroCall(_) => false,
            Self::MacroDef(item) => matches!(item.style, ast::MacroDefStyle::New),
        }
    }

    fn supports_explicit_override_policy(&self) -> bool {
        match self {
            Self::Const(_) | Self::Fn(_) | Self::TyAlias(_) => true,
            Self::Impl(item) => item.trait_ref.is_some(),
            | Self::ConstBlock(_)
            | Self::Delegation(_)
            | Self::Enum(_)
            | Self::ExternBlock(_)
            | Self::ExternCrate(_)
            | Self::MacroCall(_)
            | Self::MacroDef(_)
            | Self::Mod(_)
            | Self::Static(_)
            | Self::Struct(_)
            | Self::Trait(_)
            | Self::TraitAlias(_)
            | Self::Union(_)
            | Self::Use(_) => false,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum ItemCx {
    Boring,
    Trait,
}

enum Qualifier<'src> {
    Async,
    Auto(Span),
    Const(Span),
    Extern(Option<&'src str>),
    Fn,
    Gen(Span),
    Impl,
    ImplRestriction(Span, Box<Result<ast::Path<'src, ast::NoGenericArgs>>>),
    Mod,
    Reuse(Span),
    Safe,
    Static,
    Trait,
    Type(Span),
    Unsafe(Span),
}

impl<'src> Qualifier<'src> {
    fn strip_const(qualifiers: &mut [Self], gate: impl FnOnce(Span)) -> (ast::Const, &mut [Self]) {
        match qualifiers {
            [Self::Const(span), qualifiers @ ..] => {
                gate(*span);
                (ast::Const::Yes, qualifiers)
            }
            _ => (ast::Const::No, qualifiers),
        }
    }

    fn strip_unsafe(qualifiers: &mut [Self]) -> (ast::Safety, &mut [Self]) {
        match qualifiers {
            [Self::Unsafe(span), qualifiers @ ..] => (ast::Safety::Unsafe(*span), qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    fn strip_safety(qualifiers: &mut [Self]) -> (ast::Safety<()>, &mut [Self]) {
        match qualifiers {
            [Self::Unsafe(span), qualifiers @ ..] => (ast::Safety::Unsafe(*span), qualifiers),
            [Self::Safe, qualifiers @ ..] => (ast::Safety::Safe(()), qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    fn strip_extern(qualifiers: &mut [Self]) -> (ast::Extern<'src>, &mut [Self]) {
        match qualifiers {
            [Self::Extern(abi), qualifiers @ ..] => (ast::Extern::Yes(*abi), qualifiers),
            _ => (ast::Extern::No, qualifiers),
        }
    }
}

#[derive(Clone, Copy)]
enum ImplKind {
    Normal,
    Delegation(Span),
}
