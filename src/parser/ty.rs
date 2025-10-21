use super::{
    ExpectedFragment, Parser, Result, TokenKind, TokenPrefix, error::ParseError, keyword::Keyword,
    one_of,
};
use crate::{ast, edition::Edition, span::Span, token::Token};

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
                let mut_ = match self.as_keyword(self.token) {
                    Ok(Keyword::Mut) => {
                        self.advance();
                        ast::Mutability::Mut
                    }
                    Ok(Keyword::Const) => {
                        self.advance();
                        ast::Mutability::Not
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            self.token,
                            one_of![Keyword::Mut, Keyword::Const],
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

        match self.as_keyword(self.token) {
            Ok(Keyword::Underscore) => {
                self.advance();
                return Ok(ast::Ty::Inferred);
            }
            Ok(Keyword::Dyn) => {
                self.advance();
                return self.fin_parse_dyn_trait_object_ty();
            }
            Err(Some("dyn"))
                if self.edition == Edition::Rust2015
                    && self.look_ahead(1, |t| self.begins_2015_dyn_bound(t)) =>
            {
                self.advance();
                return self.fin_parse_dyn_trait_object_ty();
            }
            Ok(Keyword::Fn) => {
                self.advance();
                return self.fin_parse_fn_ptr_ty(Vec::new());
            }
            Ok(Keyword::For) => {
                self.advance();
                let bound_vars = self.parse_generic_params()?;

                // FIXME: Expect bare trait object types, too.
                self.parse(Keyword::Fn)?;
                return self.fin_parse_fn_ptr_ty(bound_vars);
            }
            Ok(Keyword::Impl) => {
                self.advance();
                let bounds = self.parse_bounds()?;
                return Ok(ast::Ty::ImplTrait(bounds));
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

    // FIXME: Find ways to get rid of this function or make it return something richer that
    //        can then be used inside `parse_ty` to perform less work / avoid prefix rechecking.
    pub(super) fn begins_ty(&self) -> bool {
        // FIXME: To be kept in sync with `Self::parse_ty`.

        match self.token.kind {
            TokenKind::SingleBang
            | TokenKind::SingleAmpersand
            | TokenKind::DoubleAmpersand
            | TokenKind::SingleAsterisk
            | TokenKind::OpenSquareBracket
            | TokenKind::OpenRoundBracket => return true,
            _ => (),
        }

        if let Ok(Keyword::Underscore | Keyword::Dyn | Keyword::Fn | Keyword::For | Keyword::Impl) =
            self.as_keyword(self.token)
        {
            return true;
        }

        if self.begins_ext_path() {
            return true;
        }

        false
    }

    fn begins_2015_dyn_bound(&self, token: Token) -> bool {
        if let TokenKind::Lifetime | TokenKind::QuestionMark | TokenKind::OpenRoundBracket =
            token.kind
        {
            return true;
        }

        match self.as_keyword(token) {
            Ok(keyword) => keyword == Keyword::For || keyword.is_path_seg(),
            Err(ident) => ident.is_some(),
        }
    }

    fn fin_parse_dyn_trait_object_ty(&mut self) -> Result<ast::Ty<'src>> {
        Ok(ast::Ty::DynTrait(self.parse_bounds()?))
    }

    fn fin_parse_fn_ptr_ty(
        &mut self,
        bound_vars: Vec<ast::GenericParam<'src>>,
    ) -> Result<ast::Ty<'src>> {
        self.parse(TokenKind::OpenRoundBracket)?;
        // FIXME: Actually parse the parameters using `Self::parse_fn_params`
        //        to capture the full grammar (for that, the functions needs to
        //        be able to parse optional parameter *patterns* (!)).
        let params =
            self.fin_parse_delim_seq(TokenKind::CloseRoundBracket, TokenKind::Comma, |this| {
                this.parse_ty()
            })?;
        let ret_ty = self
            .consume(TokenKind::ThinArrow)
            .then(|| self.parse_ty().map(Box::new))
            .transpose()?;
        return Ok(ast::Ty::FnPtr(bound_vars, params, ret_ty));
    }

    fn fin_parse_ref_ty(&mut self) -> Result<ast::Ty<'src>> {
        let lt = self.parse_common_lifetime()?;
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
        if !self.consume(TokenPrefix::LessThan) {
            return Ok(Vec::new());
        }

        const SEPARATOR: TokenKind = TokenKind::Comma;
        self.fin_parse_delim_seq_with(
            |this| this.consume(TokenPrefix::GreaterThan),
            |this| TokenPrefix::GreaterThan.matches(this.token.kind),
            SEPARATOR,
            |this| {
                let (binder, kind) =
                    if let Some(ast::Lifetime(lifetime)) = this.parse_common_lifetime()? {
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

        if !self.consume(Keyword::Where) {
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
        if let Some(lt) = self.parse_common_lifetime()? {
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

            if !self.consume(TokenPrefix::Plus) {
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

        if let Some(lt) = self.parse_common_lifetime()? {
            if let Some((_, span)) = bound_vars {
                return Err(ParseError::HigherRankedBinderOnOutlivesBound(span));
            }
            if modifiers != ast::TraitBoundModifiers::NONE {
                // FIXME: Span
                return Err(ParseError::ModifiersOnOutlivesBound);
            }

            return Ok(ast::Bound::Outlives(lt));
        }

        if self.consume(Keyword::Use) {
            self.parse(TokenKind::SingleLessThan)?;
            let captures =
                self.fin_parse_delim_seq(TokenKind::SingleGreaterThan, TokenKind::Comma, |this| {
                    if let Some(ast::Lifetime(lifetime)) = this.parse_common_lifetime()? {
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
        if self.begins_path(self.token) {
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
            || self.begins_path(self.token)
            || matches!(self.token.kind, TokenKind::Lifetime) // FIXME: swap about with begins_outlives_bound
    }

    fn parse_trait_bound_modifiers(&mut self) -> Result<ast::TraitBoundModifiers> {
        // NOTE: To be kept in sync with `Self::begins_trait_bound_modifiers`.

        let constness = match self.token.kind {
            TokenKind::Ident if let Ok(Keyword::Const) = self.as_keyword(self.token) => {
                self.advance();
                ast::BoundConstness::Always
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                self.parse(Keyword::Const)?;
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
            TokenKind::Ident => self.as_keyword(self.token) == Ok(Keyword::Const),
            TokenKind::OpenSquareBracket => {
                self.look_ahead(1, |token| self.as_keyword(token) == Ok(Keyword::Const))
                    && self.look_ahead(2, |token| token.kind == TokenKind::CloseSquareBracket)
            }
            TokenKind::SingleBang | TokenKind::QuestionMark => true,
            _ => false,
        }
    }

    fn parse_outlives_bounds(&mut self) -> Result<Vec<ast::Lifetime<'src>>> {
        let mut bounds = Vec::new();

        while let Some(lt) = self.parse_common_lifetime()? {
            bounds.push(lt);

            if !self.consume(TokenKind::SinglePlus) {
                break;
            }
        }

        Ok(bounds)
    }

    fn parse_higher_ranked_binder(
        &mut self,
    ) -> Result<Option<(Vec<ast::GenericParam<'src>>, Span)>> {
        let start = self.token.span;

        if !self.consume(Keyword::For) {
            return Ok(None);
        }

        let bound_vars = self.parse_generic_params()?;

        // FIXME: Better span
        Ok(Some((bound_vars, start.until(self.token.span))))
    }
}
