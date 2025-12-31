use super::{
    ExpectedFragment, Parser, Result, TokenKind,
    common::FnParamMode,
    path::PathMode,
    weak::{self, Weak as _},
};
use crate::{
    Edition, ast,
    error::{Buffer as ErrorBuffer, Error},
    span::Span,
};

impl<'src> Parser<'_, '_, 'src> {
    /// Parse a sequence of items.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Items⟨terminator⟩ ::= Item* ⟨terminator⟩
    /// ```
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
    ///
    /// # Grammar
    ///
    /// <!-- FIXME: Add EBNF section back in -->
    pub(super) fn parse_item(&mut self, cx: ItemCx) -> Result<ast::Item<'src>> {
        // NOTE: To be kept in sync with `Self::begins_final_non_macro_call_item`.

        let start = self.token.span;

        let mut attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        let vis = self.parse_visibility()?;

        let defaultness = if self.consume(weak::Default) {
            ast::Defaultness::Default
        } else {
            ast::Defaultness::Final
        };

        let kind = self.parse_item_kind(defaultness, cx, &mut attrs)?;

        if !matches!(vis, ast::Visibility::Inherited) && !kind.supports_visibility() {
            return self.fatal(Error::VisibilityOnInvalidItem);
        }

        if let ast::Defaultness::Default = defaultness
            && !kind.supports_defaultness()
        {
            return self.fatal(Error::DefaultnessOnInvalidItem);
        }

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::Item { attrs, vis, kind, span })
    }

    // FIXME: Experiment with doing the stmt(item<->expr) disambiguation in begins_expr instead.
    // FIXME: Experiment with replacing this with an parse_item_prefix that rets Option<ItemPrefix>
    //        to be then used for fin_parse_item(prefix)
    pub(super) fn begins_final_non_macro_call_item(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_item`.

        match self.token.kind {
            | TokenKind::Enum
            | TokenKind::Macro
            | TokenKind::Struct
            | TokenKind::Trait
            | TokenKind::Type
            | TokenKind::Use => return true,
            TokenKind::CommonIdent => match self.source(self.token.span) {
                weak::Union::STR => return weak::Union.qualifies(self),
                _ => {}
            },
            _ => {}
        }

        if self.begins_outer_attr() || self.begins_visibility() {
            return true;
        }

        if self.check(weak::MacroRules) {
            return true;
        }

        let mut qualified = false;
        for (qualifier, token) in self.snapshot(&mut ErrorBuffer::Void).parse_item_qualifiers() {
            match qualifier {
                Qualifier::Async | Qualifier::Const | Qualifier::Gen | Qualifier::Static => {}
                _ => return true,
            }

            // The token sequence actually begins a block or closure expression, not an item; bail out.
            // FIXME: Also check for `|=` and `||=` for diagnostic purposes.
            if let TokenKind::OpenCurlyBracket
            | TokenKind::SinglePipe
            | TokenKind::DoublePipe
            | TokenKind::Move = token
            {
                return false;
            }

            qualified = true;
        }

        qualified
    }

    fn parse_item_kind(
        &mut self,
        defaultness: ast::Defaultness,
        cx: ItemCx,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        // NOTE: To be kept in sync with `Self::begins_final_non_macro_call_item`.

        let start = self.token.span;

        let qualifiers: Vec<_> =
            self.parse_item_qualifiers().map(|(qualifier, _)| qualifier).collect();

        // FIXME: Provide more targeted diagnostics if the qualifiers don't make sense.
        match qualifiers.as_slice() {
            [] => {}
            [Qualifier::Const] => return self.fin_parse_const_item(defaultness),
            // `crate` can't be a qualifier itself because it may also begin paths & it's not worth the look-ahead.
            [Qualifier::Extern(None)] if self.consume(TokenKind::Crate) => {
                return self.fin_parse_extern_crate_item();
            }
            [Qualifier::Reuse] => return self.fin_parse_delegation_item(),
            [qualifiers @ .., Qualifier::Mod] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_mod_item(safety, attrs);
            }
            [qualifiers @ .., Qualifier::Static] => {
                let (safety, qualifiers) = Qualifier::strip_safety(qualifiers);
                if !qualifiers.is_empty() {
                    return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_static_item(safety);
            }
            &[mut ref qualifiers @ .., Qualifier::Fn] => {
                let mut modifiers = ast::FnItemModifiers { defaultness, .. };

                (modifiers.constness, qualifiers) = Qualifier::strip_const(qualifiers);
                (modifiers.asyncness, qualifiers) = match qualifiers {
                    [Qualifier::Async, qualifiers @ ..] => (ast::Asyncness::Async, qualifiers),
                    _ => (ast::Asyncness::Not, qualifiers),
                };
                (modifiers.genness, qualifiers) = match qualifiers {
                    [Qualifier::Gen, qualifiers @ ..] => (ast::Genness::Gen, qualifiers),
                    _ => (ast::Genness::Not, qualifiers),
                };
                (modifiers.safety, qualifiers) = Qualifier::strip_safety(qualifiers);
                (modifiers.externness, qualifiers) = Qualifier::strip_extern(qualifiers);
                if !qualifiers.is_empty() {
                    return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_fn_item(modifiers, cx);
            }
            &[mut ref qualifiers @ .., Qualifier::Trait] => {
                let mut modifiers = ast::TraitItemModifiers::default();

                (modifiers.constness, qualifiers) = Qualifier::strip_const(qualifiers);
                (modifiers.safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                (modifiers.autoness, qualifiers) = match qualifiers {
                    [Qualifier::Auto, qualifiers @ ..] => (ast::Autoness::Auto, qualifiers),
                    _ => (ast::Autoness::Not, qualifiers),
                };
                if !qualifiers.is_empty() {
                    return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_trait_item(modifiers, attrs);
            }
            [qualifiers @ .., Qualifier::Impl] => {
                let (kind, qualifiers) = match qualifiers {
                    [Qualifier::Reuse, qualifiers @ ..] => (ImplKind::Delegation, qualifiers),
                    _ => (ImplKind::Normal, qualifiers),
                };
                let (constness, qualifiers) = Qualifier::strip_const(qualifiers);
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_impl_item(defaultness, kind, constness, safety, attrs);
            }
            [qualifiers @ .., Qualifier::Extern(abi)] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return self.fatal(Error::InvalidItemPrefix(start.until(self.token.span)));
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
                    let binder = self.source(self.token.span);
                    self.advance();
                    return self.fin_parse_union_item(binder);
                }
                _ => {}
            },
            TokenKind::Macro => {
                self.advance();
                return self.fin_parse_macro_def();
            }
            TokenKind::Struct => {
                self.advance();
                return self.fin_parse_struct_item();
            }
            TokenKind::Type => {
                self.advance();
                return self.fin_parse_ty_alias_item(defaultness);
            }
            TokenKind::Use => {
                self.advance();
                return self.fin_parse_use_item();
            }
            _ => {}
        }

        if self.begins_path(self.token) {
            return self.parse_macro_call_item();
        }

        self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::Item))
    }

    gen fn parse_item_qualifiers(&mut self) -> (Qualifier<'src>, TokenKind) {
        loop {
            let qualifier = match self.token.kind {
                TokenKind::Async => Qualifier::Async,
                TokenKind::Const => Qualifier::Const,
                TokenKind::Extern => {
                    self.advance();
                    yield (Qualifier::Extern(self.parse_abi_str()), self.token.kind);
                    continue;
                }
                TokenKind::Fn => Qualifier::Fn,
                TokenKind::Gen => Qualifier::Gen,
                TokenKind::CommonIdent => match self.source(self.token.span) {
                    weak::Auto::STR if weak::Auto.qualifies(self) => Qualifier::Auto,
                    weak::Reuse::STR if weak::Reuse.qualifies(self) => Qualifier::Reuse,
                    weak::Safe::STR if weak::Safe.qualifies(self) => Qualifier::Safe,
                    _ => return,
                },
                TokenKind::Impl => {
                    self.advance();
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
                TokenKind::Unsafe if self.peek(1).kind != TokenKind::OpenCurlyBracket => {
                    Qualifier::Unsafe
                }
                _ => return,
            };
            self.advance();
            yield (qualifier, self.token.kind);
        }
    }

    /// Finish parsing a constant item assuming the leading `const` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Const_Item ::=
    ///     "const" (Common_Ident | "_")
    ///     Generic_Params
    ///     ":" Ty
    ///     ("=" Expr)?
    ///     Where_Clause?
    ///     ";"
    /// ```
    fn fin_parse_const_item(
        &mut self,
        defaultness: ast::Defaultness,
    ) -> Result<ast::ItemKind<'src>> {
        let (binder, _) = self.parse_common_ident_or(TokenKind::Underscore)?;
        let params = self.parse_generic_param_list()?;
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        let preds = self.parse_where_clause()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Const(Box::new(ast::ConstItem {
            defaultness,
            binder,
            generics: ast::Generics { params, preds },
            ty,
            body,
        })))
    }

    /// Finish parsing an enumeration item assuming the leading `enum` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Enum_Item ::=
    ///     "enum" Common_Ident
    ///     Generics
    ///     "{" (Enum_Variant ("," | >"}"))* "}"
    /// Enum_Variant ::= Common_Ident
    /// ```
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
        let binder = self.parse_common_ident()?;
        let kind = self.parse_variant_kind()?;
        let discr = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        Ok(ast::Variant { attrs, vis, binder, kind, discr })
    }

    fn parse_variant_kind(&mut self) -> Result<ast::VariantKind<'src>> {
        Ok(match self.token.kind {
            TokenKind::OpenRoundBracket => {
                self.advance();
                let fields = self.fin_parse_delim_seq(
                    TokenKind::CloseRoundBracket,
                    TokenKind::Comma,
                    |this| {
                        let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
                        let vis = this.parse_visibility()?;
                        let ty = this.parse_ty()?;
                        let default = this
                            .consume(TokenKind::SingleEquals)
                            .then(|| this.parse_expr())
                            .transpose()?;
                        Ok(ast::TupleFieldDef { attrs, vis, ty, default })
                    },
                )?;
                ast::VariantKind::Tuple(fields)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                let fields = self.parse_struct_fields()?;
                ast::VariantKind::Struct(fields)
            }
            _ => ast::VariantKind::Unit,
        })
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<ast::StructFieldDef<'src>>> {
        self.fin_parse_delim_seq(TokenKind::CloseCurlyBracket, TokenKind::Comma, |this| {
            let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
            let vis = this.parse_visibility()?;
            let safety = if this.consume(TokenKind::Unsafe) {
                ast::Safety::Unsafe
            } else {
                ast::Safety::Inherited
            };
            let binder = this.parse_common_ident()?;
            let ty = this.parse_ty_annotation()?;
            let default =
                this.consume(TokenKind::SingleEquals).then(|| this.parse_expr()).transpose()?;
            Ok(ast::StructFieldDef { attrs, vis, safety, binder, ty, default })
        })
    }

    /// Finish parsing an extern block item assuming the leading `"extern" #Str_Lit?` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Extern_Block_Item ::= "unsafe"? "extern" #Str_Lit? "{" … "}"
    /// ```
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
            })
        }

        Ok(ast::ItemKind::ExternBlock(Box::new(ast::ExternBlockItem { safety, abi, body: items })))
    }

    /// Finish parsing an extern crate item assuming the leading `extern crate` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Extern_Crate_Item ::= "extern" "crate" (Common_Ident | "self") ("as" Common_Ident) ";"
    /// ```
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
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Fn_Item ::=
    ///     Fn_Modifiers
    ///     "fn" Common_Ident
    ///     Generic_Params Fn_Params
    ///     ("->" Ty)?
    ///     Where_Clause?
    ///     (Block_Expr | ";")
    /// Fn_Modifiers ::= …
    /// ```
    fn fin_parse_fn_item(
        &mut self,
        modifiers: ast::FnItemModifiers<'src>,
        cx: ItemCx,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let gen_params = self.parse_generic_param_list()?;
        let params = self.parse_fn_param_list(match (cx, self.edition) {
            (ItemCx::Trait, Edition::Rust2015) => FnParamMode::Optional,
            _ => FnParamMode::Required,
        })?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;
        let contract = self.parse_contract()?;
        let preds = self.parse_where_clause()?;

        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            Some(self.fin_parse_block_expr()?)
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
        // FIXME: Reject inner attrs inside of the block.
        let requires = self
            .consume(weak::ContractRequires)
            .then(|| self.parse_block_expr().map(Box::new))
            .transpose()?;
        let ensures = self
            .consume(weak::ContractEnsures)
            .then(|| self.parse_expr().map(Box::new))
            .transpose()?;
        Ok(ast::Contract { requires, ensures })
    }

    /// Finish parsing an implementation item assuming the leading `impl` or `impl const` has been parsed already.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    fn fin_parse_impl_item(
        &mut self,
        defaultness: ast::Defaultness,
        kind: ImplKind,
        constness: ast::Constness,
        safety: ast::Safety,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let params = if self.pick_generic_param_list_over_ext_path(0) {
            self.parse_generic_param_list()?
        } else {
            Vec::new()
        };

        let constness = if let ast::Constness::Not = constness
            && self.consume(TokenKind::Const)
        {
            ast::Constness::Const
        } else {
            constness
        };

        let polarity = if self.token.kind == TokenKind::SingleBang
            && self.peek(1).kind != TokenKind::OpenCurlyBracket
        {
            self.advance();
            ast::ImplPolarity::Negative
        } else {
            ast::ImplPolarity::Positive
        };

        let ty = self.parse_ty()?;

        let (trait_ref, self_ty) = if self.consume(TokenKind::For) {
            let self_ty = match self.consume(TokenKind::DoubleDot) {
                // Legacy syntax for auto trait impls that are still permitted if cfg'ed out.
                // FIXME: Introduce custom type, so we can pretty-print it faithfully
                true => ast::Ty::Error,
                false => self.parse_ty()?,
            };
            let ast::Ty::Path(deref!(ast::ExtPath { ext: None, path: trait_ref })) = ty else {
                return self.fatal(Error::ExpectedTraitFoundTy);
            };
            (Some(trait_ref), self_ty)
        } else {
            (None, ty)
        };

        let preds = self.parse_where_clause()?;

        let trait_ref = match trait_ref {
            Some(path) => Some(ast::ImplTraitRef { defaultness, safety, polarity, path }),
            None => {
                match polarity {
                    ast::ImplPolarity::Positive => {}
                    ast::ImplPolarity::Negative => {
                        self.error(Error::TraitImplModifierInInherentImpl("!"))
                    }
                }

                match safety {
                    ast::Safety::Inherited => {}
                    ast::Safety::Unsafe => {
                        self.error(Error::TraitImplModifierInInherentImpl("unsafe"))
                    }
                }

                None
            }
        };

        let body = match kind {
            ImplKind::Normal => {
                let items = self.parse_delimited_assoc_items(ItemCx::Boring, attrs)?;
                ast::ImplBody::Normal(items)
            }
            ImplKind::Delegation => {
                if trait_ref.is_none() {
                    self.error(Error::ReuseInherentImpl);
                }

                let body = self.parse_delegation_body()?;
                ast::ImplBody::Delegated(body)
            }
        };

        Ok(ast::ItemKind::Impl(Box::new(ast::ImplItem {
            generics: ast::Generics { params, preds },
            constness,
            trait_ref,
            self_ty,
            body,
        })))
    }

    /// Finish parsing a macro (2.0) definition assuming the leading `macro` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Macro_Def ::= "macro" Common_Ident ("(" Token_Stream ")")? "{" Token_Stream "}"
    /// ```
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
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Mod_Item ::= "unsafe"? "mod" Common_Ident ("{" … "}" | ";")
    /// ```
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
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Static_Item ::= "static" "mut"? Common_Ident ":" Ty ("=" Expr)? ";"
    /// ```
    fn fin_parse_static_item(&mut self, safety: ast::Safety<()>) -> Result<ast::ItemKind<'src>> {
        let mut_ = self.parse_mutability();
        let binder = self.parse_common_ident()?;
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Static(Box::new(ast::StaticItem { safety, mut_, binder, ty, body })))
    }

    /// Finish parsing a struct item assuming the leading `struct` has been parsed already.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    fn fin_parse_struct_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let mut generics = self.parse_generics()?;
        let kind = self.parse_variant_kind()?;
        if let ast::VariantKind::Tuple(_) = kind {
            debug_assert!(generics.preds.is_empty());
            generics.preds = self.parse_where_clause()?;
        }
        if kind.needs_semicolon() {
            self.parse(TokenKind::Semicolon)?;
        }
        Ok(ast::ItemKind::Struct(Box::new(ast::StructItem { binder, generics, kind })))
    }

    /// Finish parsing a trait item assuming the leading `trait` has been parsed already.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    fn fin_parse_trait_item(
        &mut self,
        modifiers: ast::TraitItemModifiers,
        attrs: &mut Vec<ast::Attr<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_param_list()?;

        if self.consume(TokenKind::SingleEquals) {
            return self.fin_parse_trait_alias_item(modifiers, binder, params);
        }

        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let preds = self.parse_where_clause()?;

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
        modifiers: ast::TraitItemModifiers,
        binder: ast::Ident<'src>,
        params: Vec<ast::GenericParam<'src>>,
    ) -> Result<ast::ItemKind<'src>> {
        let bounds = self.parse_bounds()?;
        let preds = self.parse_where_clause()?;

        self.parse(TokenKind::Semicolon)?;

        let ast::TraitItemModifiers { constness, safety, autoness } = modifiers;

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe => self.error(Error::UnsafeTraitAlias),
        }

        match autoness {
            ast::Autoness::Auto => self.error(Error::AutoTraitAlias),
            ast::Autoness::Not => {}
        }

        Ok(ast::ItemKind::TraitAlias(Box::new(ast::TraitAliasItem {
            constness,
            binder,
            generics: ast::Generics { params, preds },
            bounds,
        })))
    }

    /// Finish parsing a type item assuming the leading `type` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Ty_Alias_Item ::=
    ///     "type" Common_Ident
    ///     Generic_Params
    ///     (":" Bounds)?
    ///     Where_Clause?
    ///     ("=" Ty Where_Clause?)?
    ///     ";"
    fn fin_parse_ty_alias_item(
        &mut self,
        defaultness: ast::Defaultness,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_param_list()?;
        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let mut preds = self.parse_where_clause()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_ty()).transpose()?;
        if body.is_some() {
            preds.append(&mut self.parse_where_clause()?);
        }
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::TyAlias(Box::new(ast::TyAliasItem {
            defaultness,
            binder,
            generics: ast::Generics { params, preds },
            bounds,
            body,
        })))
    }

    /// Finish parsing a union item assuming the leading `"union" Common_Ident` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Union_Item ::=
    ///     "union" Common_Ident
    ///     Generics
    ///     "{" … "}"
    /// ```
    fn fin_parse_union_item(&mut self, binder: ast::Ident<'src>) -> Result<ast::ItemKind<'src>> {
        let generics = self.parse_generics()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        let fields = self.parse_struct_fields()?;

        Ok(ast::ItemKind::Union(Box::new(ast::UnionItem { binder, generics, fields })))
    }

    /// Finish parsing a use-item assuming the leading `use` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use_Item ::= "use" Path_Tree ";"
    /// Path_Tree ::= …
    /// ```
    fn fin_parse_use_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let path = self.parse_path_tree(PathMode::Normal)?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Use(Box::new(ast::UseItem { path })))
    }

    fn parse_macro_call_item(&mut self) -> Result<ast::ItemKind<'src>> {
        // NOTE: To be kept in sync with `Self::begins_macro_item`.

        let path = self.parse_path::<ast::NoGenericArgs>(PathMode::Normal)?;
        self.parse(TokenKind::SingleBang)?;

        let binder = if let [ast::PathSeg { ident: weak::MacroRules::STR, args: () }] = *path.segs {
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
        // FIXME: Permit generic args at the top level!
        let path = self.parse_path_tree(mode)?;
        let body = self.parse_delegation_body()?;

        Ok(ast::ItemKind::Delegation(Box::new(ast::DelegationItem { ext, path, body })))
    }

    fn parse_delegation_body(&mut self) -> Result<Option<ast::BlockExpr<'src>>> {
        if self.consume(TokenKind::OpenCurlyBracket) {
            // FIXME: Reject inner attrs. For some reason, they're syntactically forbidden.
            self.fin_parse_block_expr().map(Some)
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

        enum VisKeyword {
            In,
            CrateSuperSelf(Span),
        }

        // FIXME: Only do this lookahead dance for tuple struct fields. This way, we can
        // can give better errors on invalid vis restrictions in the common cases.
        if self.token.kind == TokenKind::OpenRoundBracket
            && let token = self.peek(1)
            && let Some(keyword) = match token.kind {
                TokenKind::Crate | TokenKind::Super | TokenKind::SelfLower => {
                    Some(VisKeyword::CrateSuperSelf(token.span))
                }
                TokenKind::In => Some(VisKeyword::In),
                _ => None,
            }
        {
            self.advance();
            self.advance();

            let path = match keyword {
                VisKeyword::In => self.parse_path(PathMode::Normal)?,
                VisKeyword::CrateSuperSelf(span) => ast::Path::ident(self.source(span)),
            };
            self.parse(TokenKind::CloseRoundBracket)?;
            return Ok(ast::Visibility::Restricted(path));
        }

        Ok(ast::Visibility::Public)
    }

    fn begins_visibility(&self) -> bool {
        // To kept in sync with `Self::parse_visibility`.

        self.token.kind == TokenKind::Pub
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
            Self::MacroCall(_) => false,
            Self::MacroDef(item) => matches!(item.style, ast::MacroDefStyle::New),
        }
    }

    fn supports_defaultness(&self) -> bool {
        match self {
            Self::Const(_) | Self::Fn(_) | Self::TyAlias(_) => true,
            Self::Impl(item) => item.trait_ref.is_some(),
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

#[derive(Clone, Copy)]
enum Qualifier<'src> {
    Async,
    Auto,
    Const,
    Extern(Option<&'src str>),
    Fn,
    Gen,
    Impl,
    Mod,
    Reuse,
    Safe,
    Static,
    Trait,
    Unsafe,
}

impl<'src> Qualifier<'src> {
    fn strip_const(qualifiers: &[Self]) -> (ast::Constness, &[Self]) {
        match qualifiers {
            [Self::Const, qualifiers @ ..] => (ast::Constness::Const, qualifiers),
            _ => (ast::Constness::Not, qualifiers),
        }
    }

    fn strip_unsafe(qualifiers: &[Self]) -> (ast::Safety, &[Self]) {
        match qualifiers {
            [Self::Unsafe, qualifiers @ ..] => (ast::Safety::Unsafe, qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    fn strip_safety(qualifiers: &[Self]) -> (ast::Safety<()>, &[Self]) {
        match qualifiers {
            [Self::Unsafe, qualifiers @ ..] => (ast::Safety::Unsafe, qualifiers),
            [Self::Safe, qualifiers @ ..] => (ast::Safety::Safe(()), qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    fn strip_extern(qualifiers: &[Self]) -> (ast::Externness<'src>, &[Self]) {
        match qualifiers {
            [Self::Extern(abi), qualifiers @ ..] => (ast::Externness::Extern(*abi), qualifiers),
            _ => (ast::Externness::Not, qualifiers),
        }
    }
}

enum ImplKind {
    Normal,
    Delegation,
}
