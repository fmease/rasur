use std::borrow::Cow;
use std::fmt;

use crate::ast;
use crate::edition::Edition;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

pub(crate) type Result<T, E = ParseError> = std::result::Result<T, E>;

pub(crate) fn parse(tokens: Vec<Token>, source: &str, edition: Edition) -> Result<ast::File<'_>> {
    Parser { tokens, index: 0, source, edition }.parse_file()
}

struct Parser<'src> {
    tokens: Vec<Token>,
    index: usize,
    source: &'src str,
    edition: Edition,
}

impl<'src> Parser<'src> {
    /// Parse a source file.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// File ::= Attrs⟨Inner⟩ Items⟨#End_Of_Input⟩
    /// ```
    fn parse_file(&mut self) -> Result<ast::File<'src>> {
        let start = self.token().span;

        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let items = self.parse_items(TokenKind::EndOfInput)?;

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::File { attrs, items, span })
    }

    /// Parse a sequence of items.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Items⟨terminator⟩ ::= Item* ⟨terminator⟩
    /// ```
    fn parse_items(&mut self, delim: TokenKind) -> Result<Vec<ast::Item<'src>>> {
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
    ///     | Macro_Call // FIXME
    ///     | Macro_Def // FIXME
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
    /// ```
    fn parse_item(&mut self) -> Result<ast::Item<'src>> {
        // NOTE: To be kept in sync with `Self::begins_item`.

        let start = self.token().span;

        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;

        // FIXME: Not all item-likes support `pub` (think about mac calls, impls?, mac defs?, …).
        let vis = self.parse_visibility();

        let kind = if self.begins_path() {
            self.parse_macro_call_item()?
        } else if let token = self.token()
            && let Some(ident) = self.is_ident(token)
        {
            match ident {
                "const" => {
                    self.advance();
                    if self.consume(Ident("fn")) {
                        self.fin_parse_fn_item(ast::Constness::Const)?
                    } else {
                        self.fin_parse_const_item()?
                    }
                }
                "enum" => {
                    self.advance();
                    self.fin_parse_enum_item()?
                }
                "extern" => {
                    self.advance();
                    self.fin_parse_extern_block_item()?
                }
                "fn" => {
                    self.advance();
                    self.fin_parse_fn_item(ast::Constness::Not)?
                }
                "impl" => {
                    self.advance();
                    self.fin_parse_impl_item()?
                }
                "macro" => {
                    self.advance();
                    self.fin_parse_macro_def()?
                }
                "mod" => {
                    self.advance();
                    self.fin_parse_mod_item()?
                }
                "static" => {
                    self.advance();
                    self.fin_parse_static_item()?
                }
                "struct" => {
                    self.advance();
                    self.fin_parse_struct_item()?
                }
                "trait" => {
                    self.advance();
                    self.fin_parse_trait_item()?
                }
                "type" => {
                    self.advance();
                    self.fin_parse_ty_item()?
                }
                // FIXME: Likely Needs look-ahead(Ident) bc of `fn f() { union { x: 20 } }`
                "union" => {
                    self.advance();
                    self.fin_parse_union_item()?
                }
                "use" => {
                    self.advance();
                    self.fin_parse_use_item()?
                }
                _ => return Err(ParseError::UnexpectedToken(token, ExpectedFragment::Item)),
            }
        } else {
            return Err(ParseError::UnexpectedToken(self.token(), ExpectedFragment::Item));
        };

        let span = start.to(self.prev_token().map(|token| token.span));

        Ok(ast::Item { attrs, vis, kind, span })
    }

    fn begins_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_item`.

        if self.begins_outer_attr() || self.begins_visibility() || self.begins_macro_item(policy) {
            return true;
        }

        // FIXME: look-ahead(Ident) for union bc of `fn f() { union { x: 20 } }`
        self.is_ident(self.token()).is_some_and(|ident| {
            [
                "const", "enum", "extern", "fn", "impl", "macro", "mod", "static", "struct",
                "trait", "type", "union", "use",
            ]
            .contains(&ident)
        })
    }

    /// Parse a sequence of attributes of the given style.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attrs⟨style⟩ ::= ("#" Bang⟨style⟩ "[" Attr_Path … "]" )*
    /// Bang⟨Outer⟩ ::= ""
    /// Bang⟨Inner⟩ ::= "!"
    /// ```
    fn parse_attrs(&mut self, style: ast::AttrStyle) -> Result<Vec<ast::Attr<'src>>> {
        // NOTE: To be kept in sync with `Self::begins_outer_attr`.
        // FIXME: Parse doc comments.

        let mut attrs = Vec::new();

        while self.token().kind == TokenKind::Hash {
            match style {
                ast::AttrStyle::Outer => self.advance(),
                // We don't expect(Bang) here because the caller may want to
                // parse outer attributes next.
                ast::AttrStyle::Inner => {
                    if self.look_ahead(1, |token| token.kind == TokenKind::Bang) {
                        self.advance();
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            attrs.push(self.fin_parse_attr(style)?);
        }

        Ok(attrs)
    }

    fn begins_outer_attr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_attr`.

        self.token().kind == TokenKind::Hash
    }

    fn fin_parse_attr(&mut self, style: ast::AttrStyle) -> Result<ast::Attr<'src>> {
        self.parse(TokenKind::OpenSquareBracket)?;
        let path = self.parse_path::<ParsePathArgsNo>()?;
        let token = self.token();
        let kind = match token.kind {
            TokenKind::CloseSquareBracket => ast::AttrKind::Unit,
            // FIXME: Admits `==`.
            TokenKind::Equals => {
                self.advance();
                let expr = self.parse_expr()?;
                ast::AttrKind::Assign(expr)
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Round)?;
                ast::AttrKind::Call(bracket, stream)
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Square)?;
                ast::AttrKind::Call(bracket, stream)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                let (bracket, stream) =
                    self.fin_parse_delimited_token_stream(ast::Bracket::Curly)?;
                ast::AttrKind::Call(bracket, stream)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    ExpectedFragment::OneOf(Box::new([
                        TokenKind::CloseSquareBracket,
                        TokenKind::Equals,
                        TokenKind::OpenRoundBracket,
                        TokenKind::OpenSquareBracket,
                        TokenKind::OpenCurlyBracket,
                    ])),
                ));
            }
        };

        self.parse(TokenKind::CloseSquareBracket)?;

        Ok(ast::Attr { style, path, kind })
    }

    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= "::"? Path_Seg_Ident ("::" Path_Seg_Ident)*
    /// ```
    fn parse_path<A: ParsePathArgs>(&mut self) -> Result<ast::Path<'src, A::Output<'src>>> {
        // NOTE: To be kept in sync with `Self::begins_path`.

        let hook = match self.consume(DOUBLE_COLON) {
            true => ast::PathHook::Global,
            false => ast::PathHook::Local,
        };

        let mut segs = Vec::new();
        segs.push(self.parse_path_seg_ident::<A>()?);
        while self.consume(DOUBLE_COLON) {
            segs.push(self.parse_path_seg_ident::<A>()?);
        }

        Ok(ast::Path { hook, segs })
    }

    fn begins_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        DOUBLE_COLON.check(self) || self.is_path_seg_ident().is_some()
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
        // FIXME: Allow underscore
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;
        let ty = self.parse_ty_ann()?;
        let body = self.consume(TokenKind::Equals).then(|| self.parse_expr()).transpose()?;
        let preds = self.parse_where_clause()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Const(ast::ConstItem {
            binder,
            generics: ast::Generics { params, preds },
            ty,
            body,
        }))
    }

    /// Finish parsing an enumeration item assuming the leading `enum` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Enum_Item ::=
    ///     "enum" Common_Ident
    ///     Generics
    ///     "{" … "}"
    /// ```
    fn fin_parse_enum_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let generics = self.parse_generics()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        self.parse(TokenKind::CloseCurlyBracket)?;

        Ok(ast::ItemKind::Enum(ast::EnumItem { binder, generics }))
    }

    /// Finish parsing an extern block item assuming the leading `extern` has been parsed already.
    ///
    /// # Grammar
    fn fin_parse_extern_block_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let token = self.token();
        let abi = self.consume(TokenKind::StrLit).then(|| self.source(token.span));

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
                        _ => return Err(ParseError::InvalidExternItemKind),
                    },
                    span: item.span,
                })
            })
            .collect::<Result<_>>()?;

        Ok(ast::ItemKind::ExternBlock(ast::ExternBlockItem { abi, body: items }))
    }

    /// Finish parsing a function item assuming the leading `fn` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Fn_Item ::=
    ///     "const"? "fn" Common_Ident
    ///     Generic_Params Fn_Params
    ///     ("->" Ty)?
    ///     Where_Clause?
    ///     (Block_Expr | ";")
    /// ```
    fn fin_parse_fn_item(&mut self, constness: ast::Constness) -> Result<ast::ItemKind<'src>> {
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

        Ok(ast::ItemKind::Fn(ast::FnItem {
            constness,
            binder,
            generics: ast::Generics { params: gen_params, preds },
            params,
            ret_ty,
            body,
        }))
    }

    /// Finish parsing an implementation item assuming the leading `impl` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Impl_Item ::= "impl" Generic_Params Path for Ty Where_Clause? "{" … "}"
    /// ```
    fn fin_parse_impl_item(&mut self) -> Result<ast::ItemKind<'src>> {
        // FIXME: Handle "impl<T> ::Path {}" vs. "impl <T>::Path {}"
        let params = self.parse_generic_params()?;

        let constness = match self.consume(Ident("const")) {
            true => ast::Constness::Const,
            false => ast::Constness::Not,
        };

        let polarity = match self.consume(TokenKind::Bang) {
            true => ast::ImplPolarity::Negative,
            false => ast::ImplPolarity::Positive,
        };

        let ty = self.parse_ty()?;

        let (trait_ref, self_ty) = if self.consume(Ident("for")) {
            let self_ty = match self.consume(Glued([TokenKind::Dot, TokenKind::Dot])) {
                // Legacy syntax for auto trait impls.
                true => ast::Ty::Error,
                false => self.parse_ty()?,
            };
            let trait_ref = match ty {
                ast::Ty::Path(path) => path,
                _ => return Err(ParseError::ExpectedTraitFoundTy),
            };
            (Some(trait_ref), self_ty)
        } else {
            (None, ty)
        };

        let preds = self.parse_where_clause()?;

        let items = self.parse_delimited_assoc_items()?;

        Ok(ast::ItemKind::Impl(ast::ImplItem {
            generics: ast::Generics { params, preds },
            constness,
            polarity,
            trait_ref,
            self_ty,
            body: items,
        }))
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
        Ok(ast::ItemKind::MacroDef(ast::MacroDef {
            binder,
            params,
            body,
            style: ast::MacroDefStyle::New,
        }))
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

        Ok(ast::ItemKind::Mod(ast::ModItem { binder, body: items }))
    }

    /// Finish parsing a static item assuming the leading `static` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Static_Item ::= "static" Common_Ident ":" Ty ("=" Expr)? ";"
    /// ```
    fn fin_parse_static_item(&mut self) -> Result<ast::ItemKind<'src>> {
        // FIXME: "mut"
        let binder = self.parse_common_ident()?;
        let ty = self.parse_ty_ann()?;
        let body = self.consume(TokenKind::Equals).then(|| self.parse_expr()).transpose()?;
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Static(ast::StaticItem { binder, ty, body }))
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
        let generics = self.parse_generics()?;
        // FIXME: Unit structs (where the where clause is trailing)
        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            let mut fields = Vec::new();

            const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
            while !self.consume(DELIMITER) {
                let vis = self.parse_visibility();

                let binder = self.parse_common_ident()?;
                let ty = self.parse_ty_ann()?;

                // FIXME: Can we express that nicer?
                if self.token().kind != DELIMITER {
                    self.parse(TokenKind::Comma)?;
                }

                fields.push(ast::StructField { vis, binder, ty })
            }
            ast::StructBody::Normal { fields }
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.parse(TokenKind::Semicolon)?;
            ast::StructBody::Unit
        };

        Ok(ast::ItemKind::Struct(ast::StructItem { binder, generics, body }))
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
    fn fin_parse_trait_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;

        let bounds = if self.consume(TokenKind::Colon) { self.parse_bounds()? } else { Vec::new() };
        let preds = self.parse_where_clause()?;

        let items = self.parse_delimited_assoc_items()?;

        Ok(ast::ItemKind::Trait(ast::TraitItem {
            binder,
            generics: ast::Generics { params, preds },
            bounds,
            body: items,
        }))
    }

    /// Finish parsing a type item assuming the leading `type` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Ty_Item ::=
    ///     "type" Common_Ident
    ///     Generic_Params
    ///     (":" Bounds)?
    ///     Where_Clause?
    ///     ("=" Ty)?
    ///     Where_Clause?
    ///     ";"
    fn fin_parse_ty_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;
        let bounds = if self.consume(TokenKind::Colon) { self.parse_bounds()? } else { Vec::new() };
        let mut preds = self.parse_where_clause()?;
        let body = self.consume(TokenKind::Equals).then(|| self.parse_ty()).transpose()?;
        preds.append(&mut self.parse_where_clause()?);
        self.parse(TokenKind::Semicolon)?;

        Ok(ast::ItemKind::Ty(ast::TyItem {
            binder,
            generics: ast::Generics { params, preds },
            bounds,
            body,
        }))
    }

    fn parse_delimited_assoc_items(&mut self) -> Result<Vec<ast::AssocItem<'src>>> {
        self.parse(TokenKind::OpenCurlyBracket)?;
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
                        _ => return Err(ParseError::InvalidAssocItemKind),
                    },
                    span: item.span,
                })
            })
            .collect()
    }

    /// Parse a where clause.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Where_Clause ::= ("where" Predicates)?
    /// Predicates ::= (Predicate ",")* Predicate?
    /// Predicate ::= Ty ":" Bounds
    /// ```
    fn parse_where_clause(&mut self) -> Result<Vec<ast::Predicate<'src>>> {
        let mut preds = Vec::new();

        if !self.consume(Ident("where")) {
            return Ok(preds);
        }

        while self.begins_ty() {
            let ty = self.parse_ty()?;
            self.parse(TokenKind::Colon)?;
            let bounds = self.parse_bounds()?;

            preds.push(ast::Predicate::Trait(ast::TraitPredicate { ty, bounds }));

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(preds)
    }

    /// Parse a bounds annotation if available.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Bounds ::= (Bound "+")* Bound?
    /// ```
    fn parse_bounds(&mut self) -> Result<Vec<ast::Bound<'src>>> {
        let mut bounds = Vec::new();

        while self.begins_bound() {
            bounds.push(self.parse_bound()?);

            if !self.consume(TokenKind::Plus) {
                break;
            }
        }

        Ok(bounds)
    }

    fn parse_bound(&mut self) -> Result<ast::Bound<'src>> {
        // NOTE: To be kept in sync with `Self::begins_bound`.

        Ok(ast::Bound::Trait(self.parse_path::<ParsePathArgsYes>()?))
    }

    fn begins_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_bound`.

        self.begins_path()
    }

    /// Finish parsing a union item assuming the leading `union` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Union_Item ::=
    ///     "union" Common_Ident
    ///     Generics
    ///     "{" … "}"
    /// ```
    fn fin_parse_union_item(&mut self) -> Result<ast::ItemKind<'src>> {
        let binder = self.parse_common_ident()?;
        let generics = self.parse_generics()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        self.parse(TokenKind::CloseCurlyBracket)?;

        Ok(ast::ItemKind::Union(ast::UnionItem { binder, generics }))
    }

    /// Finish parsing a use-item assuming the leading `use` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use_Item ::= "use" …
    /// ```
    fn fin_parse_use_item(&mut self) -> Result<ast::ItemKind<'src>> {
        todo!()
    }

    fn parse_macro_call_item(&mut self) -> Result<ast::ItemKind<'src>> {
        // NOTE: To be kept in sync with `Self::begins_macro_item`.

        let path = self.parse_path::<ParsePathArgsNo>()?;
        self.parse(TokenKind::Bang)?;

        let binder = if let ast::PathHook::Local = path.hook
            && let [ast::PathSeg { ident: "macro_rules", args: () }] = *path.segs
        {
            self.consume_common_ident()
        } else {
            None
        };

        let (bracket, body) = self.parse_delimited_token_stream()?;

        if bracket != ast::Bracket::Curly {
            self.parse(TokenKind::Semicolon)?;
        }

        Ok(match binder {
            Some(binder) => ast::ItemKind::MacroDef(ast::MacroDef {
                binder,
                params: None,
                body,
                style: ast::MacroDefStyle::Old,
            }),
            None => ast::ItemKind::MacroCall(ast::MacroCall { path, bracket, stream: body }),
        })
    }

    fn begins_macro_item(&self, policy: MacroCallPolicy) -> bool {
        // NOTE: To be kept in sync with `Self::parse_macro_item`.

        match policy {
            MacroCallPolicy::Allowed => self.begins_path(),
            MacroCallPolicy::Forbidden => {
                Ident("macro_rules").check(self)
                    && self.look_ahead(1, |token| token.kind == TokenKind::Bang)
                    && self.look_ahead(2, |token| self.is_common_ident(token).is_some())
            }
        }
    }

    fn parse_common_ident(&mut self) -> Result<ast::Ident<'src>> {
        self.consume_common_ident()
            .ok_or_else(|| ParseError::UnexpectedToken(self.token(), ExpectedFragment::CommonIdent))
    }

    fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        self.is_common_ident(self.token()).inspect(|_| self.advance())
    }

    fn is_common_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        self.is_ident(token).filter(|ident| self.ident_is_common(ident))
    }

    fn ident_is_common(&self, ident: &str) -> bool {
        !is_reserved(ident, self.edition)
    }

    fn is_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        matches!(token.kind, TokenKind::Ident).then(|| self.source(token.span))
    }

    fn parse_path_seg_ident<A: ParsePathArgs>(
        &mut self,
    ) -> Result<ast::PathSeg<'src, A::Output<'src>>> {
        let ident = self.is_path_seg_ident().ok_or_else(|| {
            ParseError::UnexpectedToken(self.token(), ExpectedFragment::PathSegIdent)
        })?;
        self.advance();
        let args = A::parse(self)?;
        Ok(ast::PathSeg { ident, args })
    }

    fn is_path_seg_ident(&self) -> Option<ast::Ident<'src>> {
        self.is_ident(self.token())
            .filter(|ident| is_path_seg_keyword(ident) || self.ident_is_common(ident))
    }

    fn consume_lifetime(&mut self) -> Option<ast::Lifetime<'src>> {
        let apo = self.token();
        if apo.kind == TokenKind::Apostrophe
            && let Some(ident) =
                self.look_ahead(1, |ident| self.is_ident(ident).filter(|_| apo.touches(ident)))
            && (ident == "_" || ident == "static" || self.ident_is_common(ident))
        {
            self.advance(); // Apostrophe
            self.advance(); // Ident
            return Some(ast::Lifetime(ident));
        }
        None
    }

    /// Parse generics.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Generics ::= Generic_Params Where_Clause?
    /// ```
    fn parse_generics(&mut self) -> Result<ast::Generics<'src>> {
        let params = self.parse_generic_params()?;
        let preds = self.parse_where_clause()?;
        Ok(ast::Generics { params, preds })
    }

    fn parse_generic_params(&mut self) -> Result<Vec<ast::GenericParam<'src>>> {
        let mut params = Vec::new();

        if self.consume(TokenKind::OpenAngleBracket) {
            const DELIMITER: TokenKind = TokenKind::CloseAngleBracket;
            // FIXME: This is so hideously structured! We need better primitives!
            while !self.consume(DELIMITER) {
                let token = self.token();
                let (binder, kind) = if let Some(ast::Lifetime(lifetime)) = self.consume_lifetime()
                {
                    // FIXME: Outlives-bounds
                    // FIXME: `'a+` (bare trait object type with leading lifetime)
                    (lifetime, ast::GenericParamKind::Lifetime)
                } else {
                    match self.is_ident(token) {
                        Some("const") => {
                            self.advance();
                            let binder = self.parse_common_ident()?;
                            let ty = self.parse_ty_ann()?;
                            (binder, ast::GenericParamKind::Const(ty))
                        }
                        Some(ident) if self.ident_is_common(ident) => {
                            self.advance();
                            let bounds = if self.consume(TokenKind::Colon) {
                                self.parse_bounds()?
                            } else {
                                Vec::new()
                            };
                            (ident, ast::GenericParamKind::Ty(bounds))
                        }
                        _ => {
                            // FIXME: OneOf(Comma, ClosingAngleBracket, …)
                            return Err(ParseError::UnexpectedToken(
                                token,
                                ExpectedFragment::GenericParam,
                            ));
                        }
                    }
                };

                // FIXME: Is there a nicer way to do this?
                if self.token().kind != DELIMITER {
                    self.parse(TokenKind::Comma)?;
                }

                params.push(ast::GenericParam { binder, kind })
            }
        }

        Ok(params)
    }

    /// Parse function parameters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Fn_Params ::= "(" (Fn_Param ("," | >")"))* ")"
    /// Fn_Param ::= Pat ":" Ty
    /// ```
    fn parse_fn_params(&mut self) -> Result<Vec<ast::Param<'src>>> {
        let mut params = Vec::new();

        self.parse(TokenKind::OpenRoundBracket)?;
        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        while !self.consume(DELIMITER) {
            let pat = self.parse_pat()?;
            // FIXME: Optional if in trait and edition 2015
            let ty = self.parse_ty_ann()?;

            // FIXME: Is there a nicer way to do this?
            if self.token().kind != DELIMITER {
                self.parse(TokenKind::Comma)?;
            }

            params.push(ast::Param { pat, ty })
        }

        Ok(params)
    }

    /// Parse a type.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Ty ::=
    ///     | Path
    ///     | Inferred_Ty
    ///     | Dyn_Trait_Ty
    ///     | Fn_Ptr_Ty
    ///     | Impl_Trait_Ty
    ///     | Never_Ty
    ///     | Ref_Ty
    ///     | Paren_Or_Tuple_Ty
    /// Inferred_Ty ::= "_"
    /// Fn_Ptr_Ty ::= "fn" "(" ")" ("->" Ty)?
    /// Never_Ty ::= "!"
    /// Ref_Ty ::= "&" Lifetime? "mut"? Ty
    /// Paren_Or_Tuple_Ty ::= "(" (Ty ("," | >")"))* ")"
    /// ```
    fn parse_ty(&mut self) -> Result<ast::Ty<'src>> {
        // NOTE: To be kept in sync with `Self::begins_ty`.

        if self.begins_path() {
            return Ok(ast::Ty::Path(self.parse_path::<ParsePathArgsYes>()?));
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => match self.source(token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Ty::Inferred);
                }
                // In Rust 2015, we would have already taken `parse_path`, so all is good.
                "dyn" => {
                    self.advance();
                    // FIXME: Actually parse the bounds.
                    return Ok(ast::Ty::DynTrait);
                }
                "fn" => {
                    self.advance();
                    self.parse(TokenKind::OpenRoundBracket)?;
                    // FIXME: Parse type params (opt name)
                    self.parse(TokenKind::CloseRoundBracket)?;
                    let ret_ty = self
                        .consume(TokenKind::ThinArrow)
                        .then(|| self.parse_ty().map(Box::new))
                        .transpose()?;
                    return Ok(ast::Ty::FnPtr((), ret_ty));
                }
                "impl" => {
                    self.advance();
                    // FIXME: Actually parse the bounds.
                    return Ok(ast::Ty::ImplTrait);
                }
                _ => {}
            },
            TokenKind::Bang => {
                self.advance();
                return Ok(ast::Ty::Never);
            }
            TokenKind::Ampersand => {
                self.advance();
                let lt = self.consume_lifetime();
                let mut_ = self.parse_mutability();
                let ty = self.parse_ty()?;
                return Ok(ast::Ty::Ref(lt, mut_, Box::new(ty)));
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                let ty = self.parse_ty()?;
                let len =
                    self.consume(TokenKind::Semicolon).then(|| self.parse_expr()).transpose()?;
                self.parse(TokenKind::CloseSquareBracket)?;
                return Ok(match len {
                    Some(len) => ast::Ty::Array(Box::new(ty), len),
                    None => ast::Ty::Slice(Box::new(ty)),
                });
            }
            TokenKind::OpenRoundBracket => {
                self.advance();

                return self.fin_parse_parenthesized_or_tuple(Self::parse_ty, ast::Ty::Tup);
            }
            _ => {}
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Ty))
    }

    fn begins_ty(&self) -> bool {
        // FIXME: To be kept in sync with `Self::parse_ty`.

        if self.begins_path() {
            return true;
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => matches!(self.source(token.span), "_" | "dyn" | "fn" | "impl"),
            TokenKind::Bang
            | TokenKind::Ampersand
            | TokenKind::OpenSquareBracket
            | TokenKind::OpenRoundBracket => true,
            _ => false,
        }
    }

    fn parse_ty_ann(&mut self) -> Result<ast::Ty<'src>> {
        self.parse(TokenKind::Colon)?;
        self.parse_ty()
    }

    /// Finish parsing a block expression assuming the leading `{` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Block_Expr ::= "{" Attrs⟨Inner⟩* Stmt* "}"
    /// ```
    fn fin_parse_block_expr(&mut self) -> Result<ast::Expr<'src>> {
        let attrs = self.parse_attrs(ast::AttrStyle::Inner)?;
        let mut stmts = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
        while !self.consume(DELIMITER) {
            stmts.push(self.parse_stmt(DELIMITER)?);
        }

        Ok(ast::Expr::Block(Box::new(ast::BlockExpr { attrs, stmts })))
    }

    /// Parse a statement.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Stmt ::=
    ///     | Item\Macro_Call
    ///     | Let_Stmt
    ///     | Expr ";" // FIXME: Not entirely factual
    ///     | ";"
    /// Let_Stmt ::= "let" Pat (":" Ty) ("=" Expr) ";"
    /// ```
    // NOTE: Contrary to rustc and syn, at the time of writing we represent "macro stmts" as
    //       "macro expr stmts". I think the difference only matters if we were to perform
    //       macro expansion.
    fn parse_stmt(&mut self, delimiter: TokenKind) -> Result<ast::Stmt<'src>> {
        // FIXME: Outer attrs on let stmt
        if self.begins_item(MacroCallPolicy::Forbidden) {
            Ok(ast::Stmt::Item(self.parse_item()?))
        } else if self.consume(Ident("let")) {
            let pat = self.parse_pat()?;
            let ty = self.consume(TokenKind::Colon).then(|| self.parse_ty()).transpose()?;
            let body = self.consume(TokenKind::Equals).then(|| self.parse_expr()).transpose()?;
            self.parse(TokenKind::Semicolon)?;
            Ok(ast::Stmt::Let(ast::LetStmt { pat, ty, body }))
        } else if self.begins_expr() {
            let expr = self.parse_expr()?;
            // FIXME: Should we replace the delimiter check with some sort of `begins_stmt` check?
            let semi = if expr.has_trailing_block(ast::TrailingBlockMode::Normal)
                || self.token().kind == delimiter
            {
                match self.consume(TokenKind::Semicolon) {
                    true => ast::Semicolon::Yes,
                    false => ast::Semicolon::No,
                }
            } else {
                self.parse(TokenKind::Semicolon)?;
                ast::Semicolon::Yes
            };
            Ok(ast::Stmt::Expr(expr, semi))
        } else {
            let token = self.token();
            match token.kind {
                TokenKind::Semicolon => {
                    self.advance();
                    Ok(ast::Stmt::Empty)
                }
                _ => Err(ParseError::UnexpectedToken(token, ExpectedFragment::Stmt)),
            }
        }
    }

    /// Parse an expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Expr ::=
    ///     | Path
    ///     | Macro_Call
    ///     | Wildcard_Expr
    ///     | Match_Expr
    ///     | #Num_Lit
    ///     | #Str_Lit
    ///     | Borrow_Expr
    ///     | Block_Expr
    ///     | Paren_Or_Tuple_Expr
    /// Wildcard_Expr ::= "_"
    /// # FIXME: Doesn't include trailing-block logic
    /// Match_Expr ::= "match" Expr "{" (Pat "=>" Expr ("," | >"}"))* "}"
    /// Borrow_Expr ::= "&" "mut"? Expr
    /// Paren_Or_Tuple_Expr ::= "(" (Expr ("," | >")"))* ")"
    /// ```
    fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_expr`.

        if self.begins_path() {
            // FIXME: gen args disamb only
            let path = self.parse_path::<ParsePathArgsYes>()?;

            if self.consume(TokenKind::Bang) {
                let (bracket, stream) = self.parse_delimited_token_stream()?;
                return Ok(ast::Expr::MacroCall(ast::MacroCall { path, bracket, stream }));
            }

            return Ok(ast::Expr::Path(path));
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => match self.source(token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Expr::Wildcard);
                }
                "match" => {
                    self.advance();

                    let scrutinee = self.parse_expr()?;
                    let mut arms = Vec::new();

                    self.parse(TokenKind::OpenCurlyBracket)?;

                    const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                    while !self.consume(DELIMITER) {
                        let pat = self.parse_pat()?;
                        self.parse(TokenKind::WideArrow)?;

                        let body = self.parse_expr()?;

                        if body.has_trailing_block(ast::TrailingBlockMode::Match)
                            || self.token().kind == DELIMITER
                        {
                            self.consume(TokenKind::Comma);
                        } else {
                            self.parse(TokenKind::Comma)?;
                        };

                        arms.push(ast::MatchArm { pat, body })
                    }

                    return Ok(ast::Expr::Match { scrutinee: Box::new(scrutinee), arms });
                }
                _ => {}
            },
            TokenKind::NumLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Expr::NumLit(lit));
            }
            TokenKind::StrLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Expr::StrLit(lit));
            }
            TokenKind::Ampersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let expr = self.parse_expr()?;
                return Ok(ast::Expr::Borrow(mut_, Box::new(expr)));
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return self.fin_parse_block_expr();
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_parenthesized_or_tuple(Self::parse_expr, ast::Expr::Tup);
            }
            _ => {}
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Expr))
    }

    fn begins_expr(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_expr`.

        if self.begins_path() {
            return true;
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => matches!(self.source(token.span), "_" | "match"),
            TokenKind::NumLit
            | TokenKind::StrLit
            | TokenKind::Ampersand
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenCurlyBracket => true,
            _ => false,
        }
    }

    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Pat ::=
    ///     | Path
    ///     | Macro_Call
    ///     | Wildcard_Pat
    ///     | #Num_Lit
    ///     | #Str_Lit
    ///     | Borrow_Pat
    ///     | Paren_Or_Tup_Pat
    /// Wildcard_Pat ::= "_"
    /// Borrow_Pat ::= "&" "mut"? Pat
    /// Paren_Or_Tup_Pat ::= "(" (Pat ("," | >")"))* ")"
    /// ```
    fn parse_pat(&mut self) -> Result<ast::Pat<'src>> {
        if self.begins_path() {
            // FIXME: gen args disamb only
            let path = self.parse_path::<ParsePathArgsYes>()?;

            if self.consume(TokenKind::Bang) {
                let (bracket, stream) = self.parse_delimited_token_stream()?;
                return Ok(ast::Pat::MacroCall(ast::MacroCall { path, bracket, stream }));
            }

            return Ok(ast::Pat::Path(path));
        }

        let token = self.token();
        match token.kind {
            TokenKind::Ident => match self.source(token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Pat::Wildcard);
                }
                _ => {}
            },
            TokenKind::NumLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Pat::NumLit(lit));
            }
            TokenKind::StrLit => {
                let lit = self.source(token.span);
                self.advance();
                return Ok(ast::Pat::StrLit(lit));
            }
            TokenKind::Ampersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let pat = self.parse_pat()?;
                return Ok(ast::Pat::Borrow(mut_, Box::new(pat)));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_parenthesized_or_tuple(Self::parse_pat, ast::Pat::Tup);
            }
            _ => {}
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Pat))
    }

    fn fin_parse_parenthesized_or_tuple<T>(
        &mut self,
        parse: impl Fn(&mut Self) -> Result<T>,
        tuple: impl FnOnce(Vec<T>) -> T,
    ) -> Result<T> {
        let mut nodes = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        while !self.consume(DELIMITER) {
            let node = parse(self)?;

            // FIXME: Is there a better way to express this?
            if self.token().kind == DELIMITER {
                if nodes.is_empty() {
                    // This is actually a parenthesized node, not a tuple.
                    self.advance();
                    return Ok(node);
                }
            } else {
                self.parse(TokenKind::Comma)?;
            }

            nodes.push(node);
        }

        Ok(tuple(nodes))
    }

    fn parse_delimited_token_stream(&mut self) -> Result<(ast::Bracket, ast::TokenStream)> {
        let bracket = self.token();
        match bracket.kind {
            TokenKind::OpenRoundBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Round)
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Square)
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                self.fin_parse_delimited_token_stream(ast::Bracket::Curly)
            }
            _ => Err(ParseError::UnexpectedToken(
                bracket,
                ExpectedFragment::OneOf(Box::new([
                    TokenKind::OpenRoundBracket,
                    TokenKind::OpenSquareBracket,
                    TokenKind::OpenCurlyBracket,
                ])),
            )),
        }
    }

    fn fin_parse_delimited_token_stream(
        &mut self,
        bracket: ast::Bracket,
    ) -> Result<(ast::Bracket, ast::TokenStream)> {
        let stream = self.parse_token_strean(bracket)?;
        self.parse(match bracket {
            ast::Bracket::Round => TokenKind::CloseRoundBracket,
            ast::Bracket::Square => TokenKind::CloseSquareBracket,
            ast::Bracket::Curly => TokenKind::CloseCurlyBracket,
        })?;
        Ok((bracket, stream))
    }

    fn parse_token_strean(&mut self, exp_delim: ast::Bracket) -> Result<ast::TokenStream> {
        let mut tokens = Vec::new();
        let mut stack = Vec::new();
        let mut is_delimited = false;

        loop {
            let token = self.token();

            let act_delim = {
                use ast::Bracket::*;
                use ast::Orientation::*;
                match token.kind {
                    TokenKind::OpenRoundBracket => Some((Round, Open)),
                    TokenKind::OpenSquareBracket => Some((Square, Open)),
                    TokenKind::OpenCurlyBracket => Some((Curly, Open)),
                    TokenKind::CloseRoundBracket => Some((Round, Close)),
                    TokenKind::CloseSquareBracket => Some((Square, Close)),
                    TokenKind::CloseCurlyBracket => Some((Curly, Close)),
                    TokenKind::EndOfInput => break,
                    _ => None,
                }
            };

            if let Some((act_delim, orient)) = act_delim {
                if stack.is_empty()
                    && act_delim == exp_delim
                    && let ast::Orientation::Close = orient
                {
                    is_delimited = true;
                    break;
                }

                match orient {
                    ast::Orientation::Open => stack.push(act_delim),
                    ast::Orientation::Close => {
                        let close_delim = act_delim;
                        match stack.pop() {
                            Some(open_delim) if open_delim == close_delim => {}
                            // FIXME: Better error.
                            _ => return Err(ParseError::InvalidDelimiter),
                        }
                    }
                }
            }

            tokens.push(token);
            self.advance();
        }

        if is_delimited && stack.is_empty() {
            Ok(tokens)
        } else {
            // FIXME: Better error.
            Err(ParseError::InvalidDelimiter)
        }
    }

    fn parse_visibility(&mut self) -> ast::Visibility {
        // To kept in sync with `Self::begins_visibility`.

        match self.consume(Ident("pub")) {
            true => ast::Visibility::Public,
            false => ast::Visibility::Inherited,
        }
    }

    fn begins_visibility(&self) -> bool {
        // To kept in sync with `Self::parse_visibility`.

        Ident("pub").check(self)
    }

    fn parse_mutability(&mut self) -> ast::Mutability {
        match self.consume(Ident("mut")) {
            true => ast::Mutability::Mut,
            false => ast::Mutability::Imm,
        }
    }

    fn consume<S: Shape>(&mut self, shape: S) -> bool {
        if shape.check(self) {
            S::advance(self);
            true
        } else {
            false
        }
    }

    // FIXME: Take S: Shape
    fn parse(&mut self, kind: TokenKind) -> Result<()> {
        let token = self.token();
        if token.kind == kind {
            self.advance();
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::OneOf(Box::new([kind]))))
    }

    fn prev_token(&self) -> Option<Token> {
        Some(self.tokens[self.index.checked_sub(1)?])
    }

    fn token(&self) -> Token {
        self.tokens[self.index]
    }

    fn look_ahead<T: Default>(&self, amount: usize, pred: impl FnOnce(Token) -> T) -> T {
        if let Some(index) = self.index.checked_add(amount)
            && let Some(&token) = self.tokens.get(index)
        {
            pred(token)
        } else {
            T::default()
        }
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }
}

enum MacroCallPolicy {
    #[expect(dead_code)] // FIXME
    Allowed,
    Forbidden,
}

trait ParsePathArgs {
    type Output<'src>;

    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Output<'src>>;
}

// FIXME: Add one for `::<` only

enum ParsePathArgsYes {} // FIXME: Temp name
enum ParsePathArgsNo {} // FIXME: Temp name

impl ParsePathArgs for ParsePathArgsNo {
    type Output<'src> = ();

    fn parse<'src>(_: &mut Parser<'src>) -> Result<Self::Output<'src>> {
        Ok(())
    }
}

impl ParsePathArgs for ParsePathArgsYes {
    type Output<'src> = Vec<ast::GenericArg<'src>>;

    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Output<'src>> {
        let args = Vec::new();
        if parser.consume(TokenKind::OpenAngleBracket) {
            // FIXME: Args
            parser.parse(TokenKind::CloseAngleBracket)?;
        }
        Ok(args)
    }
}

// FIXME: Or should we move most "glued" token detection into the lexer?
//        We can't move everything though.
const DOUBLE_COLON: Glued<2, TokenKind> = Glued([TokenKind::Colon, TokenKind::Colon]);

// FIXME: Check master if this is still up to date
fn is_reserved(ident: &str, edition: Edition) -> bool {
    #[rustfmt::skip]
    fn is_used_keyword(ident: &str) -> bool {
        matches!(
            ident,
            | "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern" | "false" | "fn"
            | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" | "mut"
            | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct" | "super" | "trait" | "true"
            | "type" | "unsafe" | "use" | "where" | "while"
        )
    }

    #[rustfmt::skip]
    fn is_unused_keyword(ident: &str) -> bool {
        matches!(
            ident,
            | "abstract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv" | "typeof" | "unsized"
            | "virtual" | "yield"
        )
    }

    fn is_used_keyword_if(ident: &str, edition: Edition) -> bool {
        edition >= Edition::Rust2018 && matches!(ident, "async" | "await" | "dyn")
    }

    fn is_unused_keyword_if(ident: &str, edition: Edition) -> bool {
        edition >= Edition::Rust2018 && matches!(ident, "try")
            || edition >= Edition::Rust2024 && matches!(ident, "gen")
    }

    ident == "_"
        || is_used_keyword(ident)
        || is_unused_keyword(ident)
        || is_used_keyword_if(ident, edition)
        || is_unused_keyword_if(ident, edition)
}

fn is_path_seg_keyword(ident: &str) -> bool {
    matches!(ident, "_" | "self" | "Self" | "super" | "crate")
}

pub(crate) enum ParseError {
    UnexpectedToken(Token, ExpectedFragment),
    // FIXME: Temporary
    InvalidDelimiter,
    InvalidAssocItemKind,
    InvalidExternItemKind,
    ExpectedTraitFoundTy,
}

impl ParseError {
    pub(crate) fn print(&self, source: &str) {
        eprint!("error: ");
        match self {
            Self::UnexpectedToken(token, expected) => {
                let found = token.to_diag_str(Some(source));
                eprint!("{:?}: found {found} but expected {expected}", token.span)
            }
            Self::InvalidDelimiter => eprint!("invalid delimiter"),
            Self::InvalidAssocItemKind => eprint!("invalid associated item kind"),
            Self::InvalidExternItemKind => eprint!("invalid extern item kind"),
            Self::ExpectedTraitFoundTy => eprint!("found type expected trait"),
        }
        eprintln!();
    }
}

impl Token {
    fn to_diag_str(self, source: Option<&str>) -> Cow<'static, str> {
        // FIXME: Say "`{source}` (U+NNNN)" on TokenKind::Error | invalid tokens.
        match (self.kind, source) {
            (TokenKind::Ident, Some(source)) => {
                let ident = &source[self.span.range()];
                return Cow::Owned(format!("identifier `{ident}`"));
            }
            _ => Cow::Borrowed(self.kind.to_diag_str()),
        }
    }
}

impl TokenKind {
    fn to_diag_str(self) -> &'static str {
        match self {
            Self::Ampersand => "`&`",
            Self::Apostrophe => "`'`",
            Self::Bang => "`!`",
            Self::CloseAngleBracket => "`>`",
            Self::CloseCurlyBracket => "`}`",
            Self::CloseRoundBracket => "`)`",
            Self::CloseSquareBracket => "`]`",
            Self::Colon => "`:`",
            Self::Comma => "`,`",
            Self::Dot => "`.`",
            Self::EndOfInput => "end of input",
            Self::Equals => "`=`",
            Self::Error => "error",
            Self::Hash => "`#`",
            Self::Hyphen => "-",
            Self::Ident => "identifier",
            Self::NumLit => "number literal",
            Self::OpenAngleBracket => "`<`",
            Self::OpenCurlyBracket => "`{`",
            Self::OpenRoundBracket => "`(`",
            Self::OpenSquareBracket => "`[`",
            Self::Pipe => "`|`",
            Self::Plus => "`+`",
            Self::Semicolon => "`;`",
            Self::Slash => "`/`",
            Self::Star => "`*`",
            Self::StrLit => "string literal",
            Self::ThinArrow => "`->`",
            Self::WideArrow => "`=>`",
        }
    }
}

pub(crate) enum ExpectedFragment {
    CommonIdent,
    Expr,
    GenericParam,
    Item,
    OneOf(Box<[TokenKind]>),
    PathSegIdent,
    Stmt,
    Ty,
    Pat,
}

impl fmt::Display for ExpectedFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::CommonIdent => "identifier",
            Self::Expr => "expression",
            Self::GenericParam => "generic parameter",
            Self::Item => "item",
            Self::OneOf(tokens) => {
                let tokens = tokens
                    .iter()
                    .map(|token| token.to_diag_str())
                    .intersperse(" or ")
                    .collect::<String>();
                return write!(f, "{tokens}");
            }
            Self::PathSegIdent => "path segment",
            Self::Stmt => "statement",
            Self::Ty => "type",
            Self::Pat => "pattern",
        })
    }
}

trait Shape: Copy {
    const LENGTH: usize;

    fn check(self, parser: &Parser<'_>) -> bool;

    fn advance(parser: &mut Parser<'_>) {
        for _ in 0..Self::LENGTH {
            parser.advance();
        }
    }
}

impl Shape for TokenKind {
    const LENGTH: usize = 1;

    fn check(self, parser: &Parser<'_>) -> bool {
        // FIXME: This permits `==` if `=` is requested. This is not okay
        parser.token().kind == self
    }
}

#[derive(Clone, Copy)]
struct Ident<'src>(&'src str);

impl Shape for Ident<'_> {
    const LENGTH: usize = 1;

    fn check(self, parser: &Parser<'_>) -> bool {
        let actual = parser.token();
        actual.kind == TokenKind::Ident && parser.source(actual.span) == self.0
    }
}

#[derive(Clone, Copy)]
struct Glued<const N: usize, T>([T; N]);

impl Shape for Glued<2, TokenKind> {
    const LENGTH: usize = 2;

    fn check(self, parser: &Parser<'_>) -> bool {
        let Self([expected0, expected1]) = self;
        let actual0 = parser.token();
        actual0.kind == expected0
            && parser.look_ahead(1, |actual1| actual1.kind == expected1 && actual0.touches(actual1))
    }
}
