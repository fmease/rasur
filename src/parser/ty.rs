use super::{
    ExpectedFragment, Parser, PathSegIdent, Result, TokenKind, TokenPrefix, common::FnParamMode,
    error::ParseError, ident::DYN, one_of,
};
use crate::{ast, edition::Edition, span::Span, token::Token};
use std::mem;

impl<'src> Parser<'_, 'src> {
    /// Parse a type.
    ///
    /// <!-- FIXME: Add an EBNF section back in -->
    pub(super) fn parse_ty(&mut self) -> Result<ast::Ty<'src>> {
        // NOTE: To be kept in sync with `Self::begins_ty`.

        let start = self.token.span;

        // FIXME: Provide more targeted diagnostics if the qualifiers don't make sense.
        match self.parse_ty_qualifiers()?.as_mut_slice() {
            [] => {}
            [qualifiers @ .., Qualifier::Fn] => {
                let mut modifiers = ast::FnPtrTyModifiers::default();

                let (bound_vars, mut qualifiers) = match qualifiers {
                    [Qualifier::For(bound_vars), qualifiers @ ..] => {
                        (mem::take(bound_vars), &*qualifiers)
                    }
                    _ => (Vec::new(), &*qualifiers),
                };
                (modifiers.safety, qualifiers) = Qualifier::strip_unsafe(qualifiers);
                (modifiers.externness, qualifiers) = Qualifier::strip_extern(qualifiers);
                if !qualifiers.is_empty() {
                    return Err(ParseError::InvalidFnPtrTyPrefix(start.until(self.token.span)));
                }
                return self.fin_parse_fn_ptr_ty(bound_vars, modifiers);
            }
            _ => return Err(ParseError::InvalidFnPtrTyPrefix(start.until(self.token.span))),
        }

        match self.token.kind {
            TokenKind::DoubleAmpersand => {
                self.advance();
                let inner_ty = self.fin_parse_ref_ty()?;
                return Ok(ast::Ty::Ref(None, ast::Mutability::Not, Box::new(inner_ty)));
            }
            TokenKind::Dyn => {
                self.advance();
                return self.fin_parse_dyn_trait_object_ty();
            }
            TokenKind::Ident
                if let DYN = self.source(self.token.span)
                    && self.edition == Edition::Rust2015
                    && self.look_ahead(1, |t| self.begins_2015_dyn_bound(t)) =>
            {
                self.advance();
                return self.fin_parse_dyn_trait_object_ty();
            }
            TokenKind::Impl => {
                self.advance();
                return Ok(ast::Ty::ImplTrait(self.parse_bounds()?));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();

                return self.fin_parse_grouped_or_tuple(
                    Self::parse_ty,
                    ast::Ty::Grouped,
                    ast::Ty::Tuple,
                );
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
            TokenKind::SingleAmpersand => {
                self.advance();
                return self.fin_parse_ref_ty();
            }
            TokenKind::SingleBang => {
                self.advance();
                return Ok(ast::Ty::Never);
            }
            TokenKind::SingleAsterisk => {
                self.advance();
                let mut_ = match self.token.kind {
                    TokenKind::Const => {
                        self.advance();
                        ast::Mutability::Not
                    }
                    TokenKind::Mut => {
                        self.advance();
                        ast::Mutability::Mut
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            self.token,
                            one_of![TokenKind::Mut, TokenKind::Const],
                        ));
                    }
                };
                let ty = self.parse_ty()?;
                return Ok(ast::Ty::Ptr(mut_, Box::new(ty)));
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::Ty::Inferred);
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
            | TokenKind::DoubleAmpersand
            | TokenKind::Dyn
            | TokenKind::Extern
            | TokenKind::Fn
            | TokenKind::For
            | TokenKind::Impl
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::SingleAmpersand
            | TokenKind::SingleAsterisk
            | TokenKind::SingleBang
            | TokenKind::Underscore
            | TokenKind::Unsafe => return true,
            _ => {}
        }

        if self.begins_ext_path() {
            return true;
        }

        false
    }

    fn parse_ty_qualifiers(&mut self) -> Result<Vec<Qualifier<'src>>> {
        std::iter::from_fn(|| self.parse_ty_qualifier()).collect()
    }

    fn parse_ty_qualifier(&mut self) -> Option<Result<Qualifier<'src>>> {
        let qualifier = match self.token.kind {
            TokenKind::Extern => {
                self.advance();
                let span = self.token.span;
                let abi = self.consume(TokenKind::StrLit).then(|| self.source(span));
                return Some(Ok(Qualifier::Extern(abi)));
            }
            TokenKind::Fn => Qualifier::Fn,
            TokenKind::For => {
                self.advance();
                return Some(self.parse_generic_param_list().map(Qualifier::For));
            }
            TokenKind::Unsafe => Qualifier::Unsafe,
            _ => return None,
        };
        self.advance();
        Some(Ok(qualifier))
    }

    fn begins_2015_dyn_bound(&self, token: Token) -> bool {
        matches!(
            token.kind,
            PathSegIdent!()
                | TokenKind::For
                | TokenKind::Lifetime
                | TokenKind::OpenRoundBracket
                | TokenKind::QuestionMark
        )
    }

    fn fin_parse_dyn_trait_object_ty(&mut self) -> Result<ast::Ty<'src>> {
        Ok(ast::Ty::DynTrait(self.parse_bounds()?))
    }

    fn fin_parse_fn_ptr_ty(
        &mut self,
        bound_vars: Vec<ast::GenericParam<'src>>,
        modifiers: ast::FnPtrTyModifiers<'src>,
    ) -> Result<ast::Ty<'src>> {
        let inputs = self.parse_fn_param_list(FnParamMode::Optional)?;
        let output = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;

        return Ok(ast::Ty::FnPtr(Box::new(ast::FnPtrTy {
            bound_vars,
            modifiers,
            inputs,
            output,
        })));
    }

    fn fin_parse_ref_ty(&mut self) -> Result<ast::Ty<'src>> {
        let lt = self.parse_lifetime()?;
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
    /// Generics ::= Generic_Param_List Where_Clause?
    /// ```
    pub(super) fn parse_generics(&mut self) -> Result<ast::Generics<'src>> {
        let params = self.parse_generic_param_list()?;
        let preds = self.parse_where_clause()?;
        Ok(ast::Generics { params, preds })
    }

    /// Parse a list of generic parameters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Generic_Param_List ::= "<" (Generic_Param ("," | >">"))* ">"
    /// Generic_Param_List ::=
    ///     | Lifetime
    ///     | "const" Common_Ident ":" Type ("=" Const_Arg)?
    ///     | Common_Ident (":" Bounds)? ("=" Ty)?
    /// ```
    pub(super) fn parse_generic_param_list(&mut self) -> Result<Vec<ast::GenericParam<'src>>> {
        if !self.consume(TokenPrefix::LessThan) {
            return Ok(Vec::new());
        }

        const SEPARATOR: TokenKind = TokenKind::Comma;
        self.fin_parse_delim_seq_with(
            |this| this.consume(TokenPrefix::GreaterThan),
            |this| TokenPrefix::GreaterThan.matches(this.token.kind),
            SEPARATOR,
            |this| {
                let (binder, kind) = if let Some(ast::Lifetime(lifetime)) = this.parse_lifetime()? {
                    let bounds = if this.consume(TokenKind::SingleColon) {
                        this.parse_outlives_bounds()?
                    } else {
                        Vec::new()
                    };
                    (lifetime, ast::GenericParamKind::Lifetime(bounds))
                } else {
                    match this.token.kind {
                        TokenKind::Const => {
                            this.advance();
                            let binder = this.parse_ident()?;
                            let ty = this.parse_ty_annotation()?;
                            let default = this
                                .consume(TokenKind::SingleEquals)
                                .then(|| this.parse_const_arg())
                                .transpose()?;
                            (binder, ast::GenericParamKind::Const { ty, default })
                        }
                        TokenKind::Ident => {
                            let ident = this.source(this.token.span);
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

        if !self.consume(TokenKind::Where) {
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
        if let Some(lt) = self.parse_lifetime()? {
            self.parse(TokenKind::SingleColon)?;
            let bounds = self.parse_outlives_bounds()?;
            return Ok(ast::Predicate::Outlives(ast::OutlivesPredicate { lt, bounds }));
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Predicate))
    }

    fn begins_predicate(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_predicate`.

        matches!(self.token.kind, TokenKind::Lifetime | TokenKind::For) || self.begins_ty()
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

        // We parse the trait bound "frontmatter" for all bound kinds to
        // reject them afterwards with a better diagnostic.
        let grouped = self.consume(TokenKind::OpenRoundBracket);
        let bound_vars = self.parse_higher_ranked_binder()?;
        let modifiers = self.parse_trait_bound_modifiers()?;

        if let Some(lt) = self.parse_lifetime()? {
            self.reject_trait_bound_frontmatter(grouped, bound_vars, modifiers)?;
            return Ok(ast::Bound::Outlives(lt));
        }

        if self.consume(TokenKind::Use) {
            self.parse(TokenKind::SingleLessThan)?;
            let captures =
                self.fin_parse_delim_seq(TokenKind::SingleGreaterThan, TokenKind::Comma, |this| {
                    if let Some(ast::Lifetime(lifetime)) = this.parse_lifetime()? {
                        return Ok(lifetime);
                    }
                    match this.token.kind {
                        TokenKind::Ident | TokenKind::SelfUpper => Ok(this.source(this.token.span)),
                        _ => Err(ParseError::UnexpectedToken(
                            this.token,
                            ExpectedFragment::GenericParam,
                        )),
                    }
                })?;

            self.reject_trait_bound_frontmatter(grouped, bound_vars, modifiers)?;

            return Ok(ast::Bound::Use(captures));
        }

        if self.begins_path(self.token) {
            let trait_ref = self.parse_path::<ast::UnambiguousGenericArgs>()?;

            if grouped {
                self.parse(TokenKind::CloseRoundBracket)?;
            }

            return Ok(ast::Bound::Trait {
                bound_vars: bound_vars.map_or(Vec::new(), |(vars, _)| vars),
                modifiers,
                trait_ref,
            });
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Bound))
    }

    fn reject_trait_bound_frontmatter(
        &mut self,
        grouped: bool,
        bound_vars: Option<(Vec<ast::GenericParam<'src>>, Span)>,
        modifiers: ast::TraitBoundModifiers,
    ) -> Result<()> {
        if grouped {
            self.parse(TokenKind::CloseRoundBracket)?;
            // FIXME: Span
            return Err(ParseError::InvalidParenthesizedBound);
        }

        if let Some((_, span)) = bound_vars {
            return Err(ParseError::HigherRankedBinderOnInvalidBound(span));
        }

        if modifiers != ast::TraitBoundModifiers::NONE {
            // FIXME: Span
            return Err(ParseError::ModifiersOnInvalidBound);
        }

        Ok(())
    }

    fn begins_bound(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_bound`.

        match self.token.kind {
            TokenKind::Lifetime | TokenKind::For | TokenKind::Use | TokenKind::OpenRoundBracket => {
                return true;
            }
            _ => {}
        }

        self.begins_trait_bound_modifiers() || self.begins_path(self.token)
    }

    fn parse_trait_bound_modifiers(&mut self) -> Result<ast::TraitBoundModifiers> {
        // NOTE: To be kept in sync with `Self::begins_trait_bound_modifiers`.

        let constness = match self.token.kind {
            TokenKind::Const => {
                self.advance();
                ast::BoundConstness::Always
            }
            TokenKind::OpenSquareBracket => {
                self.advance();
                self.parse(TokenKind::Const)?;
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
            TokenKind::Const | TokenKind::QuestionMark | TokenKind::SingleBang => true,
            TokenKind::OpenSquareBracket => {
                self.look_ahead(1, |t| t.kind == TokenKind::Const)
                    && self.look_ahead(2, |t| t.kind == TokenKind::CloseSquareBracket)
            }
            _ => false,
        }
    }

    fn parse_outlives_bounds(&mut self) -> Result<Vec<ast::Lifetime<'src>>> {
        let mut bounds = Vec::new();

        while let Some(lt) = self.parse_lifetime()? {
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

        if !self.consume(TokenKind::For) {
            return Ok(None);
        }

        let bound_vars = self.parse_generic_param_list()?;

        // FIXME: Better span
        Ok(Some((bound_vars, start.until(self.token.span))))
    }
}

enum Qualifier<'src> {
    Extern(Option<&'src str>),
    Fn,
    For(Vec<ast::GenericParam<'src>>),
    Unsafe,
}

impl<'src> Qualifier<'src> {
    fn strip_unsafe(qualifiers: &[Self]) -> (ast::Safety, &[Self]) {
        match qualifiers {
            [Self::Unsafe, qualifiers @ ..] => (ast::Safety::Unsafe, qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    fn strip_extern(qualifiers: &[Self]) -> (ast::Externness<'src>, &[Self]) {
        match qualifiers {
            [Qualifier::Extern(abi), qualifiers @ ..] => {
                (ast::Externness::Extern(*abi), qualifiers)
            }
            _ => (ast::Externness::Not, qualifiers),
        }
    }
}
