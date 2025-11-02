use super::{
    ExpectedFragment, MacroCallPolicy, Parser, Result, TokenKind,
    common::{FnParamMode, Qualifier},
    error::ParseError,
    ident::{AUTO, MACRO_RULES, SAFE, UNION},
};
use crate::{ast, edition::Edition, span::Span};

impl<'src> Parser<'_, 'src> {
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

        // We look for a delimiter instead of checking `begins_item` for better diagnostics.
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
        // NOTE: To be kept in sync with `Self::begins_item`.

        let start = self.token.span;

        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        // FIXME: Not all item-likes support `pub` (think about mac calls, impls?, mac defs?, …).
        let vis = self.parse_visibility()?;
        let kind = self.parse_item_kind(cx)?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::Item { attrs, vis, kind, span })
    }

    pub(super) fn begins_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_item`.

        if self.begins_outer_attr() || self.begins_visibility() || self.begins_macro_item(policy) {
            return true;
        }

        match self.token.kind {
            TokenKind::Async => {
                self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket)
                    // FIXME: HACK: for `async gen {`
                    && self.look_ahead(2, |t| t.kind != TokenKind::OpenCurlyBracket)
            }
            TokenKind::Const | TokenKind::Unsafe => {
                self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket)
            }
            TokenKind::Gen => self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket),
            | TokenKind::Enum
            | TokenKind::Extern
            | TokenKind::Fn
            | TokenKind::Impl
            | TokenKind::Macro
            | TokenKind::Mod
            | TokenKind::Static
            | TokenKind::Struct
            | TokenKind::Trait
            | TokenKind::Type
            | TokenKind::Use => true,
            TokenKind::Ident => match self.source(self.token.span) {
                AUTO => self.look_ahead(1, |t| t.kind == TokenKind::Trait),
                SAFE => self.look_ahead(1, |t| matches!(t.kind, TokenKind::Fn | TokenKind::Extern)),
                UNION => self.look_ahead(1, |t| t.kind == TokenKind::Ident),
                _ => false,
            },
            _ => false,
        }
    }

    fn parse_item_kind(&mut self, cx: ItemCx) -> Result<ast::ItemKind<'src>> {
        let start = self.token.span;

        // FIXME: Better span for InvalidItemPrefix
        match self.parse_qualifiers()?.as_slice() {
            [] => {}
            [Qualifier::Const] => return self.fin_parse_const_item(),
            // `crate` can't be a qualifier itself because it may also begin paths.
            [Qualifier::Extern(None)] if self.consume(TokenKind::Crate) => {
                return self.fin_parse_extern_crate_item();
            }
            &[mut ref qualifiers @ .., Qualifier::Fn] => {
                let mut modifiers = ast::FnItemModifiers::default();

                (modifiers.constness, qualifiers) = Qualifier::strip_const(qualifiers);
                (modifiers.asyncness, qualifiers) = match qualifiers {
                    [Qualifier::Async, qualifiers @ ..] => (ast::Asyncness::Async, qualifiers),
                    _ => (ast::Asyncness::Not, qualifiers),
                };
                (modifiers.genness, qualifiers) = match qualifiers {
                    [Qualifier::Gen, qualifiers @ ..] => (ast::Genness::Gen, qualifiers),
                    _ => (ast::Genness::Not, qualifiers),
                };
                (modifiers.safety, qualifiers) = match qualifiers {
                    [Qualifier::Unsafe, qualifiers @ ..] => (ast::Safety::Unsafe, qualifiers),
                    [Qualifier::Safe, qualifiers @ ..] => (ast::Safety::Safe, qualifiers),
                    _ => (ast::Safety::Inherited, qualifiers),
                };
                (modifiers.externness, qualifiers) = Qualifier::strip_extern(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
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
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_trait_item(modifiers);
            }
            [qualifiers @ .., Qualifier::Impl] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_impl_item(safety, ast::Constness::Not);
            }
            [qualifiers @ .., Qualifier::Impl, Qualifier::Const] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_impl_item(safety, ast::Constness::Const);
            }
            [qualifiers @ .., Qualifier::Extern(abi)] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_extern_block_item(safety, *abi);
            }
            [qualifiers @ .., Qualifier::Mod] => {
                let (safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_mod_item(safety);
            }
            _ => {
                return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
            }
        }

        match self.token.kind {
            TokenKind::Enum => {
                self.advance();
                return self.fin_parse_enum_item();
            }
            TokenKind::Ident => {
                if let UNION = self.source(self.token.span)
                    && self.look_ahead(1, |t| t.kind == TokenKind::Ident)
                {
                    self.advance();
                    let binder = self.source(self.token.span);
                    self.advance();
                    return self.fin_parse_union_item(binder);
                }
            }
            TokenKind::Macro => {
                self.advance();
                return self.fin_parse_macro_def();
            }
            TokenKind::Static => {
                self.advance();
                return self.fin_parse_static_item();
            }
            TokenKind::Struct => {
                self.advance();
                return self.fin_parse_struct_item();
            }
            TokenKind::Type => {
                self.advance();
                return self.fin_parse_ty_alias_item();
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

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Item))
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
    fn fin_parse_const_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_ident_or(TokenKind::Underscore)?;
        let params = self.parse_generic_params()?;
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        let preds = self.parse_where_clause()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Const(Box::new(ast::ConstItem {
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
        let binder = self.parse_ident()?;
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
        // FIXME: Parse visibility
        let binder = self.parse_ident()?;
        let kind = self.parse_variant_kind()?;
        let discr = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        Ok(ast::Variant { attrs, binder, kind, discr })
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
                        Ok(ast::TupleFieldDef { attrs, vis, ty })
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
            let binder = this.parse_ident()?;
            let ty = this.parse_ty_annotation()?;
            Ok(ast::StructFieldDef { attrs, vis, binder, ty })
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
    ) -> Result<ast::ItemKind<'src>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        let items = self
            .parse_items(ItemCx::Boring, TokenKind::CloseCurlyBracket)?
            .into_iter()
            .map(|item| {
                Ok(ast::ExternItem {
                    attrs: item.attrs,
                    vis: item.vis,
                    kind: match item.kind {
                        ast::ItemKind::Static(item) => ast::ExternItemKind::Static(item),
                        ast::ItemKind::Fn(item) => ast::ExternItemKind::Fn(item),
                        ast::ItemKind::MacroCall(item) => ast::ExternItemKind::MacroCall(item),
                        ast::ItemKind::Ty(item) => ast::ExternItemKind::Ty(item),
                        _ => return Err(ParseError::InvalidExternItemKind(item.span)),
                    },
                    span: item.span,
                })
            })
            .collect::<Result<_>>()?;

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
        let target = self.parse_ident_or(TokenKind::SelfLower)?;
        let binder = self.consume(TokenKind::As).then(|| self.parse_ident()).transpose()?;

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
        let binder = self.parse_ident()?;
        let gen_params = self.parse_generic_params()?;
        let params = self.parse_fn_params(match (cx, self.edition) {
            (ItemCx::Trait, Edition::Rust2015) => FnParamMode::Optional,
            _ => FnParamMode::Required,
        })?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;
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
            body,
        })))
    }

    /// Finish parsing an implementation item assuming the leading `impl` or `impl const` has been parsed already.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    // FIXME: Take a different kind of safety, on that's boolean, not a tristate (explicit "safe" trait is impossible)
    fn fin_parse_impl_item(
        &mut self,
        safety: ast::Safety,
        constness: ast::Constness,
    ) -> Result<ast::ItemKind<'src>> {
        // FIXME: Handle "impl<T> ::Path {}" vs. "impl <T>::Path {}"
        let params = self.parse_generic_params()?;

        let polarity = match self.consume(TokenKind::SingleBang) {
            true => ast::ImplPolarity::Negative,
            false => ast::ImplPolarity::Positive,
        };

        let ty = self.parse_ty()?;

        let (trait_ref, self_ty) = if self.consume(TokenKind::For) {
            let self_ty = match self.consume(TokenKind::DoubleDot) {
                // Legacy syntax for auto trait impls that are still permitted if cfg'ed out.
                true => ast::Ty::Error,
                false => self.parse_ty()?,
            };
            let ast::Ty::Path(deref!(ast::ExtPath { ext: None, path: trait_ref })) = ty else {
                return Err(ParseError::ExpectedTraitFoundTy);
            };
            (Some(trait_ref), self_ty)
        } else {
            (None, ty)
        };

        let preds = self.parse_where_clause()?;

        let items = self.parse_delimited_assoc_items(ItemCx::Boring)?;

        Ok(ast::ItemKind::Impl(Box::new(ast::ImplItem {
            safety,
            generics: ast::Generics { params, preds },
            constness,
            polarity,
            trait_ref,
            self_ty,
            body: items,
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
        let binder = self.parse_ident()?;
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
    fn fin_parse_mod_item(&mut self, safety: ast::Safety) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_ident()?;
        let items = if self.consume(TokenKind::OpenCurlyBracket) {
            // FIXME: Smh. merge with outer attrs?
            let _attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
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
    fn fin_parse_static_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let mut_ = self.parse_mutability();
        let binder = self.parse_ident()?;
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Static(Box::new(ast::StaticItem { mut_, binder, ty, body })))
    }

    /// Finish parsing a struct item assuming the leading `struct` has been parsed already.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    fn fin_parse_struct_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_ident()?;
        // FIXME: For tuple structs the where clause is trailing, not leading!
        let generics = self.parse_generics()?;
        let kind = self.parse_variant_kind()?;
        match kind {
            ast::VariantKind::Unit | ast::VariantKind::Tuple(_) => {
                self.parse(TokenKind::Semicolon)?;
            }
            ast::VariantKind::Struct(_) => {}
        }
        Ok(ast::ItemKind::Struct(Box::new(ast::StructItem { binder, generics, kind })))
    }

    /// Finish parsing a trait item assuming the leading `trait` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Trait_Item ::=
    ///     "const"? "unsafe"? "auto"?
    ///     "trait" Common_Ident
    ///     Generic_Params
    ///     (":" Bounds)?
    ///     Where_Clause?
    ///     "{" … "}"
    /// ```
    // FIXME: Take a different kind of safety, on that's boolean, not a tristate (explicit "safe" trait is impossible)
    fn fin_parse_trait_item(
        &mut self,
        modifiers: ast::TraitItemModifiers,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_ident()?;
        let params = self.parse_generic_params()?;

        // FIXME: Or if `=` parse a trait alias but make sure to reject unsafe trait aliases,
        //        bounds and leading where-clauses on them.

        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let preds = self.parse_where_clause()?;

        let items = self.parse_delimited_assoc_items(ItemCx::Trait)?;

        Ok(ast::ItemKind::Trait(Box::new(ast::TraitItem {
            modifiers,
            binder,
            generics: ast::Generics { params, preds },
            bounds,
            body: items,
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
    fn fin_parse_ty_alias_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_ident()?;
        let params = self.parse_generic_params()?;
        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let mut preds = self.parse_where_clause()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_ty()).transpose()?;
        if body.is_some() {
            preds.append(&mut self.parse_where_clause()?);
        }
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Ty(Box::new(ast::TyAliasItem {
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
    /// Use_Item ::= "use" Use_Path_Tree ";"
    /// Use_Path_Tree ::= …
    /// ```
    fn fin_parse_use_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let tree = self.parse_path_tree()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Use(Box::new(ast::UseItem { tree })))
    }

    fn parse_macro_call_item(&mut self) -> Result<ast::ItemKind<'src>> {
        // NOTE: To be kept in sync with `Self::begins_macro_item`.

        let path = self.parse_path::<ast::NoGenericArgs>()?;
        self.parse(TokenKind::SingleBang)?;

        let binder = if let [ast::PathSeg { ident: MACRO_RULES, args: () }] = *path.segs {
            self.consume_ident()
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

    fn begins_macro_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_macro_item`.

        match policy {
            MacroCallPolicy::Allowed => self.begins_path(self.token),
            MacroCallPolicy::Forbidden => {
                self.is_ident(MACRO_RULES)
                    && self.look_ahead(1, |t| t.kind == TokenKind::SingleBang)
                    && self.look_ahead(2, |t| t.kind == TokenKind::Ident)
            }
        }
    }

    fn parse_delimited_assoc_items(&mut self, cx: ItemCx) -> Result<Vec<ast::AssocItem<'src>>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        // FIXME: Smh. merge with outer attrs?
        let _attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        self.parse_items(cx, TokenKind::CloseCurlyBracket)?
            .into_iter()
            .map(|item| {
                Ok(ast::AssocItem {
                    attrs: item.attrs,
                    vis: item.vis,
                    kind: match item.kind {
                        ast::ItemKind::Const(item) => ast::AssocItemKind::Const(item),
                        ast::ItemKind::Fn(item) => ast::AssocItemKind::Fn(item),
                        ast::ItemKind::MacroCall(item) => ast::AssocItemKind::MacroCall(item),
                        ast::ItemKind::Ty(item) => ast::AssocItemKind::Ty(item),
                        _ => return Err(ParseError::InvalidAssocItemKind(item.span)),
                    },
                    span: item.span,
                })
            })
            .collect()
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
            && let Some(keyword) = self.look_ahead(1, |token| match token.kind {
                TokenKind::Crate | TokenKind::Super | TokenKind::SelfLower => {
                    Some(VisKeyword::CrateSuperSelf(token.span))
                }
                TokenKind::In => Some(VisKeyword::In),
                _ => None,
            })
        {
            self.advance();
            self.advance();

            let path = match keyword {
                VisKeyword::In => self.parse_path()?,
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

#[derive(Clone, Copy)]
pub(super) enum ItemCx {
    Boring,
    Trait,
}
