use super::{ExpectedFragment, ParseError, Parser, Result, TokenKind, one_of};
use crate::ast;

impl<'src> Parser<'_, 'src> {
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
    ///     | Ptr_Ty
    ///     | Paren_Or_Tuple_Ty
    /// Inferred_Ty ::= "_"
    /// Dyn_Trait_Ty ::= "dyn" Bounds
    /// Fn_Ptr_Ty ::= "fn" "(" ")" ("->" Ty)?
    /// Impl_Trait_Ty ::= "impl" Bounds
    /// Never_Ty ::= "!"
    /// Ref_Ty ::= "&" Lifetime? "mut"? Ty
    /// Ptr_Ty ::= "*" ("const" | "mut") Ty
    /// Paren_Or_Tuple_Ty ::= "(" (Ty ("," | >")"))* ")"
    /// ```
    pub(super) fn parse_ty(&mut self) -> Result<ast::Ty<'src>> {
        // NOTE: To be kept in sync with `Self::begins_ty`.

        match self.token.kind {
            TokenKind::Ident => match self.source(self.token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Ty::Inferred);
                }
                // In Rust 2015, we would have already taken `parse_path`, so all is good.
                "dyn" => {
                    self.advance();
                    let bounds = self.parse_bounds()?;
                    return Ok(ast::Ty::DynTrait(bounds));
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
                    let bounds = self.parse_bounds()?;
                    return Ok(ast::Ty::ImplTrait(bounds));
                }
                _ => {}
            },
            TokenKind::SingleBang => {
                self.advance();
                return Ok(ast::Ty::Never);
            }
            TokenKind::SingleAmpersand => {
                self.advance();
                let lt = self.consume_common_lifetime()?;
                let mut_ = self.parse_mutability();
                let ty = self.parse_ty()?;
                return Ok(ast::Ty::Ref(lt, mut_, Box::new(ty)));
            }
            TokenKind::SingleAsterisk => {
                self.advance();
                let mut_ = match self.as_ident(self.token) {
                    Some("mut") => {
                        self.advance();
                        ast::Mutability::Mut
                    }
                    Some("const") => {
                        self.advance();
                        ast::Mutability::Not
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            self.token,
                            one_of![ExpectedFragment::Raw("mut"), ExpectedFragment::Raw("const")],
                        ));
                    }
                };
                let ty = self.parse_ty()?;
                return Ok(ast::Ty::Ptr(mut_, Box::new(ty)));
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

                return self.fin_parse_grouped_or_tuple(
                    Self::parse_ty,
                    ast::Ty::Grouped,
                    ast::Ty::Tup,
                );
            }
            _ => {}
        }

        if self.begins_ext_path() {
            let path = self.parse_ext_path::<ast::UnambiguousGenericArgs>()?;

            if self.consume(TokenKind::SingleBang) {
                let ast::ExtPath { ext: None, path } = path else {
                    return Err(ParseError::TyRelMacroCall);
                };
                let (bracket, stream) = self.parse_delimited_token_stream()?;
                return Ok(ast::Ty::MacroCall(ast::MacroCall { path, bracket, stream }));
            }

            return Ok(ast::Ty::Path(Box::new(path)));
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Ty))
    }

    pub(super) fn begins_ty(&self) -> bool {
        // FIXME: To be kept in sync with `Self::parse_ty`.

        if self.begins_ext_path() {
            return true;
        }

        match self.token.kind {
            TokenKind::Ident => matches!(self.source(self.token.span), "_" | "dyn" | "fn" | "impl"),
            TokenKind::SingleBang
            | TokenKind::SingleAmpersand
            | TokenKind::SingleAsterisk
            | TokenKind::OpenSquareBracket
            | TokenKind::OpenRoundBracket => true,
            _ => false,
        }
    }

    pub(super) fn parse_ty_annotation(&mut self) -> Result<ast::Ty<'src>> {
        self.parse(TokenKind::SingleColon)?;
        self.parse_ty()
    }

    /// Parse generics.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Generics ::= Generic_Params Where_Clause?
    /// ```
    pub(super) fn parse_generics(&mut self) -> Result<ast::Generics<'src>> {
        let params = self.parse_generic_params()?;
        let preds = self.parse_where_clause()?;
        Ok(ast::Generics { params, preds })
    }

    /// Parse a list of generic parameters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Generic_Params ::= "<" (Generic_Param ("," | >">"))* ">"
    /// Generic_Param ::=
    ///     | Lifetime
    ///     | "const" Common_Ident ":" Type ("=" Const_Arg)?
    ///     | Common_Ident (":" Bounds)? ("=" Ty)?
    /// ```
    pub(super) fn parse_generic_params(&mut self) -> Result<Vec<ast::GenericParam<'src>>> {
        if !self.consume_single_less_than() {
            return Ok(Vec::new());
        }

        const SEPARATOR: TokenKind = TokenKind::Comma;
        self.fin_parse_delim_seq_with(
            Self::consume_single_greater_than,
            Self::begins_single_greater_than,
            SEPARATOR,
            |this| {
                let (binder, kind) =
                    if let Some(ast::Lifetime(lifetime)) = this.consume_common_lifetime()? {
                        let bounds = if this.consume(TokenKind::SingleColon) {
                            this.parse_outlives_bounds()?
                        } else {
                            Vec::new()
                        };
                        (lifetime, ast::GenericParamKind::Lifetime(bounds))
                    } else {
                        match this.as_ident(this.token) {
                            Some("const") => {
                                this.advance();
                                let binder = this.parse_common_ident()?;
                                let ty = this.parse_ty_annotation()?;
                                let default = this
                                    .consume(TokenKind::SingleEquals)
                                    .then(|| this.parse_const_arg())
                                    .transpose()?;
                                (binder, ast::GenericParamKind::Const { ty, default })
                            }
                            Some(ident) if this.ident_is_common(ident) => {
                                this.advance();
                                let bounds = if this.consume(TokenKind::SingleColon) {
                                    this.parse_bounds()?
                                } else {
                                    Vec::new()
                                };
                                let default = this
                                    .consume(TokenKind::SingleEquals)
                                    .then(|| this.parse_ty())
                                    .transpose()?;
                                (ident, ast::GenericParamKind::Ty { bounds, default })
                            }
                            _ => {
                                return Err(ParseError::UnexpectedToken(
                                    this.token,
                                    one_of![
                                        ExpectedFragment::GenericParam,
                                        SEPARATOR,
                                        TokenKind::SingleGreaterThan
                                    ],
                                ));
                            }
                        }
                    };

                Ok(ast::GenericParam { binder, kind })
            },
        )
    }

    /// Parse a where clause.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Where_Clause ::= ("where" Predicates)?
    /// # FIXME: Traling comma
    /// Predicates ::= (Predicate ",")* Predicate?
    /// Predicate ::=
    ///     | Ty ":" Bounds
    /// ```
    pub(super) fn parse_where_clause(&mut self) -> Result<Vec<ast::Predicate<'src>>> {
        let mut preds = Vec::new();

        if !self.consume_ident_if("where") {
            return Ok(preds);
        }

        while self.begins_predicate() {
            preds.push(self.parse_predicate()?);

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(preds)
    }

    fn parse_predicate(&mut self) -> Result<ast::Predicate<'src>> {
        // NOTE: To be kept in sync with `Self::begins_predicate`.

        // FIXME: for<>

        if self.begins_ty() {
            let ty = self.parse_ty()?;
            self.parse(TokenKind::SingleColon)?;
            let bounds = self.parse_bounds()?;
            return Ok(ast::Predicate::Trait(ast::TraitPredicate { ty, bounds }));
        }
        if let Some(lt) = self.consume_common_lifetime()? {
            self.parse(TokenKind::SingleColon)?;
            let bounds = self.parse_outlives_bounds()?;
            return Ok(ast::Predicate::Outlives(ast::OutlivesPredicate { lt, bounds }));
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Predicate))
    }

    fn begins_predicate(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_predicate`.

        self.begins_ty() || matches!(self.token.kind, TokenKind::Lifetime)
    }

    /// Parse a bounds annotation if available.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Bounds ::= (Bound "+")* Bound?
    /// ```
    pub(super) fn parse_bounds(&mut self) -> Result<Vec<ast::Bound<'src>>> {
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

        // FIXME: Bound modifiers
        // FIXME: for<>

        let mods = self.parse_trait_bound_modifiers();

        if self.begins_path() {
            let path = self.parse_path::<ast::UnambiguousGenericArgs>()?;
            return Ok(ast::Bound::Trait(mods, path));
        }

        if let Some(lt) = self.consume_common_lifetime()? {
            if let ast::TraitBoundModifiers::NONE = mods {
                return Ok(ast::Bound::Outlives(lt));
            }
            return Err(ParseError::ModifierOnOutlivesBound);
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Bound))
    }

    fn begins_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_bound`.

        self.begins_trait_bound_modifiers()
            || self.begins_path()
            || matches!(self.token.kind, TokenKind::Lifetime)
    }

    fn parse_trait_bound_modifiers(&mut self) -> ast::TraitBoundModifiers {
        // NOTE: To be kept in sync with `Self::begins_trait_bound_modifiers`.

        let polarity = match self.token.kind {
            TokenKind::SingleBang => {
                self.advance();
                ast::BoundPolarity::Negative
            }
            TokenKind::QuestionMark => {
                self.advance();
                ast::BoundPolarity::Maybe
            }
            _ => ast::BoundPolarity::Positive,
        };

        ast::TraitBoundModifiers { polarity }
    }

    fn begins_trait_bound_modifiers(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_trait_bound_modifiers`.

        matches!(self.token.kind, TokenKind::SingleBang | TokenKind::QuestionMark)
    }

    fn parse_outlives_bounds(&mut self) -> Result<Vec<ast::Lifetime<'src>>> {
        let mut bounds = Vec::new();

        while let Some(lt) = self.consume_common_lifetime()? {
            bounds.push(lt);

            if !self.consume(TokenKind::Plus) {
                break;
            }
        }

        Ok(bounds)
    }
}
