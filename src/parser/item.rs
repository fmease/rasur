use super::{
    ExpectedFragment, MacroCallPolicy, Parser, Result, TokenKind, error::ParseError,
    keyword::Keyword, pat::OrPolicy,
};
use crate::ast;

impl<'src> Parser<'_, 'src> {
    /// Parse a sequence of items.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Items⟨terminator⟩ ::= Item* ⟨terminator⟩
    /// ```
    pub(super) fn parse_items(&mut self, delim: TokenKind) -> Result<Vec<ast::Item<'src>>> {
        let mut items = Vec::new();

        // We look for a delimiter instead of checking `begins_item` for better diagnostics.
        while !self.consume(delim) {
            items.push(self.parse_item()?);
        }

        Ok(items)
    }

    /// Parse an item.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Item ::= Attrs⟨Outer⟩ Visibility Bare_Item
    /// Visibility ::= "pub"?
    /// Bare_Item ::=
    ///     | Const_Item
    ///     | Enum_Item
    ///     | Extern_Block_Item
    ///     | Fn_Item
    ///     | Impl_Item
    ///     | Macro_Def
    ///     | Mod_Item
    ///     | Static_Item
    ///     | Struct_Item
    ///     | Static_Item
    ///     | Trait_Item
    ///     | Ty_Item
    ///     | Union_Item
    ///     | Use_Item
    ///     | Macro_Call
    ///     | Macro_Def
    /// ```
    #[expect(clippy::too_many_lines)]
    pub(super) fn parse_item(&mut self) -> Result<ast::Item<'src>> {
        // NOTE: To be kept in sync with `Self::begins_item`.

        let start = self.token.span;

        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        // FIXME: Not all item-likes support `pub` (think about mac calls, impls?, mac defs?, …).
        let vis = self.parse_visibility()?;
        let kind = self.parse_item_kind()?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::Item { attrs, vis, kind, span })
    }

    pub(super) fn begins_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_item`.

        if self.begins_outer_attr() || self.begins_visibility() || self.begins_macro_item(policy) {
            return true;
        }

        let Ok(keyword) = self.as_keyword(self.token) else { return false };

        match keyword {
            Keyword::Async => {
                self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket)
                    // HACK: for `async gen {`
                    && self.look_ahead(2, |t| t.kind != TokenKind::OpenCurlyBracket)
            }
            Keyword::Auto => self.look_ahead(1, |t| self.as_keyword(t) == Ok(Keyword::Trait)),
            Keyword::Const | Keyword::Unsafe => {
                self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket)
            }
            Keyword::Gen => self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket),
            Keyword::Safe => self.look_ahead(1, |token| {
                matches!(self.as_keyword(token), Ok(Keyword::Fn | Keyword::Extern))
            }),
            Keyword::Union => self.look_ahead(1, |t| self.as_common_ident(t).is_some()),
            | Keyword::Enum
            | Keyword::Extern
            | Keyword::Fn
            | Keyword::Impl
            | Keyword::Macro
            | Keyword::Mod
            | Keyword::Static
            | Keyword::Struct
            | Keyword::Trait
            | Keyword::Type
            | Keyword::Use => true,
            _ => false,
        }
    }

    fn parse_item_kind(&mut self) -> Result<ast::ItemKind<'src>> {
        let start = self.token.span;

        fn parse_const<'a, 'b>(
            modifiers: &'a [ItemKeyword<'b>],
        ) -> (ast::Constness, &'a [ItemKeyword<'b>]) {
            match modifiers {
                [ItemKeyword::Const, modifiers @ ..] => (ast::Constness::Const, modifiers),
                _ => (ast::Constness::Not, modifiers),
            }
        }
        fn parse_unsafe<'a, 'b>(
            modifiers: &'a [ItemKeyword<'b>],
        ) -> (ast::Safety, &'a [ItemKeyword<'b>]) {
            match modifiers {
                [ItemKeyword::Unsafe, modifiers @ ..] => (ast::Safety::Unsafe, modifiers),
                _ => (ast::Safety::Inherited, modifiers),
            }
        }

        // FIXME: Better span for InvalidItemPrefix
        match &*self.parse_item_keyword()? {
            [] => {}
            [ItemKeyword::Const] => return self.fin_parse_const_item(),
            [ItemKeyword::Extern(None), ItemKeyword::Crate] => {
                return self.fin_parse_extern_crate_item();
            }
            [modifiers @ .., ItemKeyword::Fn] => {
                let (constness, modifiers) = parse_const(modifiers);
                let (asyncness, modifiers) = match modifiers {
                    [ItemKeyword::Async, modifiers @ ..] => (ast::Asyncness::Async, modifiers),
                    _ => (ast::Asyncness::Not, modifiers),
                };
                let (genness, modifiers) = match modifiers {
                    [ItemKeyword::Gen, modifiers @ ..] => (ast::Genness::Gen, modifiers),
                    _ => (ast::Genness::Not, modifiers),
                };
                let (safety, modifiers) = match modifiers {
                    [ItemKeyword::Unsafe, modifiers @ ..] => (ast::Safety::Unsafe, modifiers),
                    [ItemKeyword::Safe, modifiers @ ..] => (ast::Safety::Safe, modifiers),
                    _ => (ast::Safety::Inherited, modifiers),
                };
                let (externness, modifiers) = match modifiers {
                    [ItemKeyword::Extern(abi), modifiers @ ..] => {
                        (ast::Externness::Extern(*abi), modifiers)
                    }
                    _ => (ast::Externness::Not, modifiers),
                };
                if !modifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_fn_item(ast::FnModifiers {
                    constness,
                    asyncness,
                    genness,
                    safety,
                    externness,
                });
            }
            [modifiers @ .., ItemKeyword::Trait] => {
                let (constness, modifiers) = parse_const(modifiers);
                let (safety, modifiers) = parse_unsafe(modifiers);
                let (autoness, modifiers) = match modifiers {
                    [ItemKeyword::Auto, modifiers @ ..] => (ast::Autoness::Auto, modifiers),
                    _ => (ast::Autoness::Not, modifiers),
                };
                if !modifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_trait_item(constness, safety, autoness);
            }
            [modifiers @ .., ItemKeyword::Impl] => {
                let (safety, modifiers) = parse_unsafe(modifiers);
                if !modifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_impl_item(safety, ast::Constness::Not);
            }
            [modifiers @ .., ItemKeyword::Impl, ItemKeyword::Const] => {
                let (safety, modifiers) = parse_unsafe(modifiers);
                if !modifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_impl_item(safety, ast::Constness::Const);
            }
            [modifiers @ .., ItemKeyword::Extern(abi)] => {
                let (safety, modifiers) = parse_unsafe(modifiers);
                if !modifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_extern_block_item(safety, *abi);
            }
            [modifiers @ .., ItemKeyword::Mod] => {
                let (safety, modifiers) = parse_unsafe(modifiers);
                if !modifiers.is_empty() {
                    return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
                }

                return self.fin_parse_mod_item(safety);
            }
            _ => {
                return Err(ParseError::InvalidItemPrefix(start.until(self.token.span)));
            }
        }

        match self.as_keyword(self.token) {
            Ok(Keyword::Enum) => {
                self.advance();
                return self.fin_parse_enum_item();
            }
            Ok(Keyword::Macro) => {
                self.advance();
                return self.fin_parse_macro_def();
            }
            Ok(Keyword::Static) => {
                self.advance();
                return self.fin_parse_static_item();
            }
            Ok(Keyword::Struct) => {
                self.advance();
                return self.fin_parse_struct_item();
            }
            Ok(Keyword::Type) => {
                self.advance();
                return self.fin_parse_ty_alias_item();
            }
            Ok(Keyword::Union) => {
                if let Some(binder) = self.look_ahead(1, |token| self.as_common_ident(token)) {
                    self.advance();
                    self.advance();
                    return self.fin_parse_union_item(binder);
                }
            }
            Ok(Keyword::Use) => {
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

    fn parse_item_keyword(&mut self) -> Result<Vec<ItemKeyword<'src>>> {
        let mut candidates = Vec::new();

        while let Ok(keyword) = self.as_keyword(self.token) {
            candidates.push(match keyword {
                Keyword::Async
                    if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket)
                            // for async gen {
                            && self.look_ahead(2, |t| t.kind != TokenKind::OpenCurlyBracket) =>
                {
                    ItemKeyword::Async
                }
                Keyword::Auto
                    if self.look_ahead(1, |t| self.as_keyword(t) == Ok(Keyword::Trait)) =>
                {
                    ItemKeyword::Auto
                }
                Keyword::Const if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket) => {
                    ItemKeyword::Const
                }
                Keyword::Crate => ItemKeyword::Crate,
                Keyword::Extern => {
                    self.advance();
                    let token = self.token;
                    let abi = self.consume(TokenKind::StrLit).then(|| self.source(token.span));
                    candidates.push(ItemKeyword::Extern(abi));
                    continue;
                }
                Keyword::Fn => ItemKeyword::Fn,
                Keyword::Gen if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket) => {
                    ItemKeyword::Gen
                }
                Keyword::Impl => ItemKeyword::Impl,
                Keyword::Mod => ItemKeyword::Mod,
                Keyword::Safe
                    if self.look_ahead(1, |t| {
                        matches!(self.as_keyword(t), Ok(Keyword::Fn | Keyword::Extern))
                    }) =>
                {
                    ItemKeyword::Safe
                }
                Keyword::Trait => ItemKeyword::Trait,
                Keyword::Unsafe
                    if self.look_ahead(1, |t| t.kind != TokenKind::OpenCurlyBracket) =>
                {
                    ItemKeyword::Unsafe
                }
                _ => break,
            });
            self.advance();
        }

        Ok(candidates)
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
        let binder = self.parse_ident_where_common_or("_")?;
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
        // FIXME: Parse visibility
        let binder = self.parse_common_ident()?;
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
            let binder = this.parse_common_ident()?;
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
            .parse_items(TokenKind::CloseCurlyBracket)?
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
    /// Extern_Crate_Item ::= "extern crate" (Common_Ident | "self") ("as" Common_Ident) ";"
    /// ```
    fn fin_parse_extern_crate_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let target = self.parse_ident_where_common_or("self")?;
        let binder = self.consume(Keyword::As).then(|| self.parse_common_ident()).transpose()?;

        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::ExternCrate(Box::new(ast::ExternCrateItem { target, binder })))
    }

    /// Finish parsing a function item assuming the leading `Fn_Modifiers "fn"` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Fn_Item ::=
    ///     Fn_Modifiers "fn" Common_Ident
    ///     Generic_Params Fn_Params
    ///     ("->" Ty)?
    ///     Where_Clause?
    ///     (Block_Expr | ";")
    /// Fn_Modifiers ::= "const"? ("extern" #Str_Lit)?
    /// ```
    fn fin_parse_fn_item(
        &mut self,
        modifiers: ast::FnModifiers<'src>,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let gen_params = self.parse_generic_params()?;
        let params = self.parse_fn_params()?;
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

    /// Parse function parameters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Fn_Params ::= "(" (Fn_Param ("," | >")"))* ")"
    /// Fn_Param ::= Pat ":" Ty
    /// ```
    fn parse_fn_params(&mut self) -> Result<Vec<ast::FnParam<'src>>> {
        self.parse(TokenKind::OpenRoundBracket)?;

        let mut first = true;
        self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            let first = std::mem::take(&mut first);

            if let Some((ref_, mut_)) = this.probe(|this| {
                let ref_ =
                    this.consume(TokenKind::SingleAmpersand).then(|| this.parse_common_lifetime());
                let mut_ = this.parse_mutability();
                this.parse(Keyword::SelfLower).ok()?;
                Some((ref_, mut_))
            }) {
                if !first {
                    return Err(ParseError::MisplacedReceiver);
                }

                let pat = ast::Pat::Ident(ast::IdentPat {
                    mut_: match ref_ {
                        Some(_) => ast::Mutability::Not,
                        None => mut_,
                    },
                    by_ref: ast::ByRef::No,
                    ident: "self",
                });

                let self_ty = || ast::Ty::Path(Box::new(ast::ExtPath::ident("Self")));

                let ty = match ref_ {
                    Some(lt) => ast::Ty::Ref(lt?, mut_, Box::new(self_ty())),
                    None => match this.consume(TokenKind::SingleColon) {
                        true => this.parse_ty()?,
                        false => self_ty(),
                    },
                };

                return Ok(ast::FnParam { pat, ty });
            };

            let pat = this.parse_pat(OrPolicy::Allowed)?;
            let ty = this.parse_ty_annotation()?;

            Ok(ast::FnParam { pat, ty })
        })
    }

    /// Finish parsing an implementation item assuming the leading `impl` or `impl const` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Impl_Item ::=
    ///     "unsafe"? "impl" "const"?
    ///     Generic_Params
    ///     Path "for" Ty
    ///     Where_Clause? "{" … "}"
    /// ```
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

        let (trait_ref, self_ty) = if self.consume(Keyword::For) {
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

        let items = self.parse_delimited_assoc_items()?;

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
    fn fin_parse_mod_item(&mut self, safety: ast::Safety) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let items = if self.consume(TokenKind::OpenCurlyBracket) {
            // FIXME: Smh. merge with outer attrs?
            let _attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
            Some(self.parse_items(TokenKind::CloseCurlyBracket)?)
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
        let binder = self.parse_common_ident()?;
        let ty = self.parse_ty_annotation()?;
        let body = self.consume(TokenKind::SingleEquals).then(|| self.parse_expr()).transpose()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Static(Box::new(ast::StaticItem { mut_, binder, ty, body })))
    }

    /// Finish parsing a struct item assuming the leading `struct` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Struct_Item ::=
    ///     "struct" Common_Ident
    ///     Generics
    ///     ("{" (Struct_Field ("," | >"}"))* "}" | ";")
    /// Struct_Field ::= Visibility Common_Ident ":" Ty
    /// ```
    fn fin_parse_struct_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
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
        constness: ast::Constness,
        safety: ast::Safety,
        autoness: ast::Autoness,
    ) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;

        // FIXME: Or if `=` parse a trait alias but make sure to reject unsafe trait aliases,
        //        bounds and leading where-clauses on them.

        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let preds = self.parse_where_clause()?;

        let items = self.parse_delimited_assoc_items()?;

        Ok(ast::ItemKind::Trait(Box::new(ast::TraitItem {
            constness,
            safety,
            autoness,
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
        let binder = self.parse_common_ident()?;
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

        let binder = if let [ast::PathSeg { ident: "macro_rules", args: () }] = *path.segs {
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

    fn begins_macro_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_macro_item`.

        match policy {
            MacroCallPolicy::Allowed => self.begins_path(self.token),
            MacroCallPolicy::Forbidden => {
                self.as_ident(self.token) == Some("macro_rules")
                    && self.look_ahead(1, |token| token.kind == TokenKind::SingleBang)
                    && self.look_ahead(2, |token| self.as_common_ident(token).is_some())
            }
        }
    }

    fn parse_delimited_assoc_items(&mut self) -> Result<Vec<ast::AssocItem<'src>>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
        // FIXME: Smh. merge with outer attrs?
        let _attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        self.parse_items(TokenKind::CloseCurlyBracket)?
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

        if !self.consume(Keyword::Pub) {
            return Ok(ast::Visibility::Inherited);
        }

        // FIXME: Only do this lookahead dance for tuple struct fields. This way, we can
        // can give better errors on invalid vis restrictions in the common cases.
        if self.token.kind == TokenKind::OpenRoundBracket
            && let Some(keyword) = self.look_ahead(1, |token| self.as_keyword(token).ok())
        {
            let path = match keyword {
                Keyword::In => {
                    self.advance();
                    self.advance();
                    Some(self.parse_path()?)
                }
                Keyword::Crate | Keyword::Super | Keyword::SelfLower => {
                    self.advance();
                    self.advance();
                    Some(ast::Path::ident(keyword.to_str()))
                }
                _ => None,
            };
            if let Some(path) = path {
                self.parse(TokenKind::CloseRoundBracket)?;
                return Ok(ast::Visibility::Restricted(path));
            }
        }

        Ok(ast::Visibility::Public)
    }

    fn begins_visibility(&self) -> bool {
        // To kept in sync with `Self::parse_visibility`.

        self.as_keyword(self.token) == Ok(Keyword::Pub)
    }
}

enum ItemKeyword<'src> {
    Async,
    Auto,
    Const,
    Crate,
    Extern(Option<&'src str>),
    Fn,
    Gen,
    Impl,
    Mod,
    Safe,
    Trait,
    Unsafe,
}
