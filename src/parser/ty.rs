use super::{ExpectedFragment, Parser, Result, TokenKind, error::ParseError, one_of};
use crate::{ast, span::Span};

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
                // FIXME: In Rust 2015, we want to take `parse_ext_path` instead unless
                //        `dyn` is followed by some specific tokens.
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
                return self.fin_parse_ref_ty();
            }
            TokenKind::DoubleAmpersand => {
                self.advance();
                let inner_ty = self.fin_parse_ref_ty()?;
                return Ok(ast::Ty::Ref(None, ast::Mutability::Not, Box::new(inner_ty)));
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
            | TokenKind::DoubleAmpersand
            | TokenKind::SingleAsterisk
            | TokenKind::OpenSquareBracket
            | TokenKind::OpenRoundBracket => true,
            _ => false,
        }
    }

    fn fin_parse_ref_ty(&mut self) -> Result<ast::Ty<'src>> {
        let lt = self.consume_common_lifetime()?;
        let mut_ = self.parse_mutability();
        let ty = self.parse_ty()?;
        Ok(ast::Ty::Ref(lt, mut_, Box::new(ty)))
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

        let bound_vars = self.parse_higher_ranked_binder()?;

        if bound_vars.is_some() || self.begins_ty() {
            let ty = self.parse_ty()?;
            self.parse(TokenKind::SingleColon)?;
            let bounds = self.parse_bounds()?;
            return Ok(ast::Predicate::Trait(ast::TraitPredicate {
                bound_vars: bound_vars.map_or(Vec::new(), |(vars, _)| vars),
                ty,
                bounds,
            }));
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

        self.as_ident(self.token).is_some_and(|ident| ident == "for")
            || self.begins_ty()
            || matches!(self.token.kind, TokenKind::Lifetime)
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

        // We parse these for all bound kinds to reject them afterwards with a better diagnostic.
        let bound_vars = self.parse_higher_ranked_binder()?;
        let modifiers = self.parse_trait_bound_modifiers()?;

        if let Some(lt) = self.consume_common_lifetime()? {
            if let Some((_, span)) = bound_vars {
                return Err(ParseError::HigherRankedBinderOnOutlivesBound(span));
            }
            if modifiers != ast::TraitBoundModifiers::NONE {
                // FIXME: Span
                return Err(ParseError::ModifiersOnOutlivesBound);
            }

            return Ok(ast::Bound::Outlives(lt));
        }

        if self.consume_ident_if("use") {
            self.parse(TokenKind::SingleLessThan)?;
            let captures =
                self.fin_parse_delim_seq(TokenKind::SingleGreaterThan, TokenKind::Comma, |this| {
                    if let Some(ast::Lifetime(lifetime)) = this.consume_common_lifetime()? {
                        return Ok(lifetime);
                    }
                    if let Some(ident) = this.as_ident(this.token)
                        && (this.ident_is_common(ident) || ident == "Self")
                    {
                        return Ok(ident);
                    }
                    Err(ParseError::UnexpectedToken(this.token, ExpectedFragment::GenericParam))
                })?;

            if let Some((_, span)) = bound_vars {
                return Err(ParseError::HigherRankedBinderOnUseBound(span));
            }
            if modifiers != ast::TraitBoundModifiers::NONE {
                // FIXME: Span
                return Err(ParseError::ModifiersOnUseBound);
            }

            return Ok(ast::Bound::Use(captures));
        }

        // FIXME: Also support parentheses around trait bounds
        if self.begins_path() {
            let trait_ref = self.parse_path::<ast::UnambiguousGenericArgs>()?;
            return Ok(ast::Bound::Trait {
                bound_vars: bound_vars.map_or(Vec::new(), |(vars, _)| vars),
                modifiers,
                trait_ref,
            });
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Bound))
    }

    fn begins_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_bound`.

        // FIXME: Intro `begins_trait_bound` abstracting over for<>, TBMs, path
        matches!(self.as_ident(self.token), Some("for" | "use"))
            || self.begins_trait_bound_modifiers()
            || self.begins_path()
            || matches!(self.token.kind, TokenKind::Lifetime) // FIXME: swap about with begins_outlives_bound
    }

    fn parse_trait_bound_modifiers(&mut self) -> Result<ast::TraitBoundModifiers> {
        // NOTE: To be kept in sync with `Self::begins_trait_bound_modifiers`.

        let constness = match self.token.kind {
            TokenKind::Ident if let "const" = self.source(self.token.span) => {
                self.advance();
                ast::BoundConstness::Always
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                self.parse_ident_where("const")?;
                self.parse(TokenKind::CloseSquareBracket)?;
                ast::BoundConstness::Maybe
            }
            _ => ast::BoundConstness::Never,
        };

        // FIXME: asyncness

        // FIMXE: Make polarity incompatible with higher-ranked binders, constness & asyncness
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

        Ok(ast::TraitBoundModifiers { constness, polarity })
    }

    fn begins_trait_bound_modifiers(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_trait_bound_modifiers`.

        match self.token.kind {
            TokenKind::Ident => self.source(self.token.span) == "const",
            TokenKind::OpenSquareBracket => {
                self.look_ahead(1, |token| {
                    self.as_ident(token).is_some_and(|ident| ident == "const")
                }) && self.look_ahead(2, |token| token.kind == TokenKind::CloseSquareBracket)
            }
            TokenKind::SingleBang | TokenKind::QuestionMark => true,
            _ => false,
        }
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

    fn parse_higher_ranked_binder(
        &mut self,
    ) -> Result<Option<(Vec<ast::GenericParam<'src>>, Span)>> {
        let start = self.token.span;

        if !self.consume_ident_if("for") {
            return Ok(None);
        }

        let bound_vars = self.parse_generic_params()?;

        // FIXME: Better span
        Ok(Some((bound_vars, start.until(self.token.span))))
    }
}
