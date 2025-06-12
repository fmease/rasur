use super::{
    ExpectedFragment, Ident, MacroCallPolicy, ParseError, Parser, Result, Shape as _, TokenKind,
    one_of,
};
use crate::{ast, parser::expr};

impl<'src> Parser<'src> {
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
    ///     | Macro_Def // FIXME
    ///     | Mod_Item
    ///     | Static_Item
    ///     | Struct_Item
    ///     | Static_Item
    ///     | Trait_Item
    ///     | Ty_Item
    ///     | Union_Item
    ///     | Use_Item
    ///     | Macro_Call // FIXME
    ///     | Macro_Def // FIXME
    /// ```
    #[expect(clippy::too_many_lines)]
    pub(super) fn parse_item(&mut self) -> Result<ast::Item<'src>> {
        // NOTE: To be kept in sync with `Self::begins_item`.

        let start = self.token().span;

        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;

        // FIXME: Not all item-likes support `pub` (think about mac calls, impls?, mac defs?, …).
        let vis = self.parse_visibility()?;

        let kind = 'kind: {
            let token = self.token();
            if let Some(ident) = self.as_ident(token) {
                match ident {
                    "const" => {
                        if self.look_ahead(1, |token| token.kind != TokenKind::OpenCurlyBracket) {
                            self.advance();
                            break 'kind if self.consume(Ident("fn")) {
                                self.fin_parse_fn_item(
                                    ast::Constness::Const,
                                    ast::Safety::Inherited,
                                    ast::Externness::Not,
                                )
                            } else {
                                self.fin_parse_const_item()
                            };
                        }
                    }
                    "enum" => {
                        self.advance();
                        break 'kind self.fin_parse_enum_item();
                    }
                    "extern" => {
                        self.advance();

                        let token = self.token();
                        let abi = self.consume(TokenKind::StrLit).then(|| self.source(token.span));

                        let token = self.token();
                        match token.kind {
                            TokenKind::OpenCurlyBracket => {
                                self.advance();
                                break 'kind self.fin_parse_extern_block_item(abi);
                            }
                            TokenKind::Ident => match self.source(token.span) {
                                "crate" => {
                                    self.advance();

                                    break 'kind self.fin_parse_extern_crate_item();
                                }
                                "fn" => {
                                    self.advance();

                                    break 'kind self.fin_parse_fn_item(
                                        ast::Constness::Not,
                                        ast::Safety::Inherited,
                                        ast::Externness::Extern(abi),
                                    );
                                }
                                _ => {}
                            },
                            _ => {}
                        }

                        break 'kind Err(ParseError::UnexpectedToken(
                            token,
                            one_of![
                                TokenKind::OpenCurlyBracket,
                                ExpectedFragment::Raw("crate"),
                                ExpectedFragment::Raw("fn")
                            ],
                        ));
                    }
                    "fn" => {
                        self.advance();
                        break 'kind self.fin_parse_fn_item(
                            ast::Constness::Not,
                            ast::Safety::Inherited,
                            ast::Externness::Not,
                        );
                    }
                    "impl" => {
                        self.advance();
                        break 'kind self.fin_parse_impl_item(ast::Safety::Inherited);
                    }
                    "macro" => {
                        self.advance();
                        break 'kind self.fin_parse_macro_def();
                    }
                    "mod" => {
                        self.advance();
                        break 'kind self.fin_parse_mod_item();
                    }
                    "safe" => {
                        // FIXME: Or "extern"
                        if self.look_ahead(1, |token| {
                            self.as_ident(token).is_some_and(|ident| ident == "fn")
                        }) {
                            self.advance();
                            self.advance();
                            break 'kind self.fin_parse_fn_item(
                                ast::Constness::Not,
                                ast::Safety::Safe,
                                ast::Externness::Not,
                            );
                        }
                    }
                    "static" => {
                        self.advance();
                        break 'kind self.fin_parse_static_item();
                    }
                    "struct" => {
                        self.advance();
                        break 'kind self.fin_parse_struct_item();
                    }
                    "trait" => {
                        self.advance();
                        break 'kind self.fin_parse_trait_item(ast::Safety::Inherited);
                    }
                    "type" => {
                        self.advance();
                        break 'kind self.fin_parse_ty_alias_item();
                    }
                    "union" => {
                        if let Some(binder) =
                            self.look_ahead(1, |token| self.as_common_ident(token))
                        {
                            self.advance();
                            self.advance();
                            break 'kind self.fin_parse_union_item(binder);
                        }
                    }
                    "unsafe" => {
                        if self.look_ahead(1, |token| token.kind != TokenKind::OpenCurlyBracket) {
                            self.advance();

                            let token = self.token();
                            // FIXME: Doesn't account for `unsafe extern ...` (extern block, fn)
                            // FIXME: `unsafe impl`
                            match self.as_ident(token) {
                                Some("fn") => {
                                    self.advance();
                                    break 'kind self.fin_parse_fn_item(
                                        ast::Constness::Not,
                                        ast::Safety::Unsafe,
                                        ast::Externness::Not,
                                    );
                                }
                                Some("trait") => {
                                    self.advance();
                                    break 'kind self.fin_parse_trait_item(ast::Safety::Unsafe);
                                }
                                Some("impl") => {
                                    self.advance();
                                    break 'kind self.fin_parse_impl_item(ast::Safety::Unsafe);
                                }
                                _ => {
                                    return Err(ParseError::UnexpectedToken(
                                        token,
                                        one_of![
                                            ExpectedFragment::Raw("fn"),
                                            ExpectedFragment::Raw("trait"),
                                            ExpectedFragment::Raw("impl"),
                                        ],
                                    ));
                                }
                            }
                        }
                    }
                    "use" => {
                        self.advance();
                        break 'kind self.fin_parse_use_item();
                    }
                    _ => {}
                }
            }

            if self.begins_path() {
                break 'kind self.parse_macro_call_item();
            }

            Err(ParseError::UnexpectedToken(self.token(), ExpectedFragment::Item))
        }?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::Item { attrs, vis, kind, span })
    }

    pub(super) fn begins_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_item`.

        if self.begins_outer_attr() || self.begins_visibility() || self.begins_macro_item(policy) {
            return true;
        }

        let Some(ident) = self.as_ident(self.token()) else { return false };

        match ident {
            "enum" | "extern" | "fn" | "impl" | "macro" | "mod" | "static" | "struct" | "trait"
            | "type" | "use" => true,
            "const" | "unsafe" => {
                self.look_ahead(1, |token| token.kind != TokenKind::OpenCurlyBracket)
            }
            "union" => self.look_ahead(1, |token| self.as_common_ident(token).is_some()),
            "safe" => {
                // FIXME: or "extern"
                self.look_ahead(1, |token| self.as_ident(token).is_some_and(|ident| ident == "fn"))
            }
            _ => false,
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
    fn fin_parse_const_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_ident_if_common_or("_")?;
        let params = self.parse_generic_params()?;
        let ty = self.parse_ty_annotation()?;
        let body = self
            .consume(TokenKind::SingleEquals)
            .then(|| self.parse_expr(expr::StructLitPolicy::Allowed))
            .transpose()?;
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
        let variants = self.parse_delimited_sequence(
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
        let discr = self
            .consume(TokenKind::SingleEquals)
            .then(|| self.parse_expr(expr::StructLitPolicy::Allowed))
            .transpose()?;
        Ok(ast::Variant { attrs, binder, kind, discr })
    }

    fn parse_variant_kind(&mut self) -> Result<ast::VariantKind<'src>> {
        let token = self.token();
        Ok(match token.kind {
            TokenKind::OpenRoundBracket => {
                self.advance();
                let fields = self.parse_delimited_sequence(
                    TokenKind::CloseRoundBracket,
                    TokenKind::Comma,
                    |this| {
                        let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
                        let vis = this.parse_visibility()?;
                        let ty = this.parse_ty()?;
                        Ok(ast::TupleField { attrs, vis, ty })
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

    fn parse_struct_fields(&mut self) -> Result<Vec<ast::StructField<'src>>> {
        self.parse_delimited_sequence(TokenKind::CloseCurlyBracket, TokenKind::Comma, |this| {
            let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;
            let vis = this.parse_visibility()?;
            let binder = this.parse_common_ident()?;
            let ty = this.parse_ty_annotation()?;
            Ok(ast::StructField { attrs, vis, binder, ty })
        })
    }

    /// Finish parsing an extern block item assuming the leading `"extern" #Str_Lit? "{"` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Extern_Block_Item ::= "extern" #Str_Lit? "{" … "}"
    /// ```
    fn fin_parse_extern_block_item(
        &mut self,
        abi: Option<&'src str>,
    ) -> Result<ast::ItemKind<'src>> {
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

        Ok(ast::ItemKind::ExternBlock(Box::new(ast::ExternBlockItem { abi, body: items })))
    }

    /// Finish parsing an extern crate item assuming the leading `extern crate` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Extern_Crate_Item ::= "extern crate" (Common_Ident | "self") ("as" Common_Ident) ";"
    /// ```
    fn fin_parse_extern_crate_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let target = self.parse_ident_if_common_or("self")?;
        let binder = self.consume(Ident("as")).then(|| self.parse_common_ident()).transpose()?;

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
        constness: ast::Constness,
        safety: ast::Safety,
        externness: ast::Externness<'src>,
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
            constness,
            safety,
            externness,
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
        self.parse_delimited_sequence(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
            let first = std::mem::take(&mut first);

            let pat = this.parse_pat()?;

            // FIXME: Extract into "extract_shorthand_self"
            let (pat, ty) = match pat {
                // FIXME: "mut self" is actually impossible, we should parse the receiver manually.
                ast::Pat::Ident(ast::IdentPat {
                    mut_: _,
                    by_ref: ast::ByRef::No,
                    ident: "self",
                }) => {
                    if !first {
                        return Err(ParseError::MisplacedReceiver);
                    }
                    let ty = if this.consume(TokenKind::SingleColon) {
                        this.parse_ty()?
                    } else {
                        ast::Ty::Path(Box::new(ast::ExtPath::ident("Self")))
                    };
                    (pat, ty)
                }
                // FIXME: We don't support `&'a self` right now, oof!
                ast::Pat::Borrow(
                    mut_,
                    deref!(pat @ ast::Pat::Ident(ast::IdentPat { mut_: _, by_ref: ast::ByRef::No, ident: "self" })),
                ) => {
                    if !first {
                        return Err(ParseError::MisplacedReceiver);
                    }
                    let ty = ast::Ty::Ref(
                        None,
                        mut_,
                        Box::new(ast::Ty::Path(Box::new(ast::ExtPath::ident("Self")))),
                    );
                    (pat, ty)
                }

                // FIXME: Optional if in trait && edition==2015
                pat => (pat, this.parse_ty_annotation()?),
            };

            Ok(ast::FnParam { pat, ty })
        })
    }

    /// Finish parsing an implementation item assuming the leading `impl` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Impl_Item ::= "impl" Generic_Params Path for Ty Where_Clause? "{" … "}"
    /// ```
    // FIXME: Take a different kind of safety, on that's boolean, not a tristate (explicit "safe" trait is impossible)
    fn fin_parse_impl_item(&mut self, safety: ast::Safety) -> Result<ast::ItemKind<'src>> {
        // FIXME: Handle "impl<T> ::Path {}" vs. "impl <T>::Path {}"
        let params = self.parse_generic_params()?;

        let constness = match self.consume(Ident("const")) {
            true => ast::Constness::Const,
            false => ast::Constness::Not,
        };

        let polarity = match self.consume(TokenKind::SingleBang) {
            true => ast::ImplPolarity::Negative,
            false => ast::ImplPolarity::Positive,
        };

        let ty = self.parse_ty()?;

        let (trait_ref, self_ty) = if self.consume(Ident("for")) {
            let self_ty = match self.consume(TokenKind::DoubleDot) {
                // Legacy syntax for auto trait impls that are still permitted if cfg'ed out.
                true => ast::Ty::Error,
                false => self.parse_ty()?,
            };
            let ast::Ty::Path(deref!(ast::ExtPath { self_ty: None, path: trait_ref })) = ty else {
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
    /// Mod_Item ::= "mod" Common_Ident ("{" … "}" | ";")
    /// ```
    fn fin_parse_mod_item(&mut self) -> Result<ast::ItemKind<'src>> {
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

        Ok(ast::ItemKind::Mod(Box::new(ast::ModItem { binder, body: items })))
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
        let body = self
            .consume(TokenKind::SingleEquals)
            .then(|| self.parse_expr(expr::StructLitPolicy::Allowed))
            .transpose()?;
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
    ///     "trait" Common_Ident
    ///     Generic_Params
    ///     (":" Bounds)?
    ///     Where_Clause?
    ///     "{" … "}"
    /// ```
    // FIXME: Take a different kind of safety, on that's boolean, not a tristate (explicit "safe" trait is impossible)
    fn fin_parse_trait_item(&mut self, safety: ast::Safety) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;

        // FIXME: Or if `=` parse a trait alias but make sure to reject unsafe trait aliases,
        //        bounds and leading where-clauses on them.

        let bounds =
            if self.consume(TokenKind::SingleColon) { self.parse_bounds()? } else { Vec::new() };
        let preds = self.parse_where_clause()?;

        let items = self.parse_delimited_assoc_items()?;

        Ok(ast::ItemKind::Trait(Box::new(ast::TraitItem {
            safety,
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

        let path = self.parse_path::<ast::GenericArgsPolicy::Forbidden>()?;
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
            MacroCallPolicy::Allowed => self.begins_path(),
            MacroCallPolicy::Forbidden => {
                Ident("macro_rules").check(self)
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

        if !self.consume(Ident("pub")) {
            return Ok(ast::Visibility::Inherited);
        }

        // FIXME: Only do this lookahead dance for tuple struct fields. This way, we can
        // can give better errors on invalid vis restrictions in the common cases.
        if self.token().kind == TokenKind::OpenRoundBracket
            && let Some(ident) = self.look_ahead(1, |token| self.as_ident(token))
        {
            let path = match ident {
                "in" => {
                    self.advance();
                    self.advance();
                    Some(self.parse_path()?)
                }
                "crate" | "super" | "self" => {
                    self.advance();
                    self.advance();
                    Some(ast::Path::ident(ident))
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

        Ident("pub").check(self)
    }
}
