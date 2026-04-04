use super::{
    Fragment, Result, TokenKind, TokenPrefix, common::FnParamMode, frags, path::PathMode, weak,
};
use crate::{
    ast,
    error::Error,
    feature::Feature,
    parser::weak::Weak,
    span::Span,
    token::{PathSegIdent, Token},
};
use std::mem;

impl<'src> super::Parser<'_, '_, 'src> {
    /// Parse a type.
    pub(super) fn parse_ty(&mut self) -> Result<ast::Ty<'src>> {
        self.parse_ty_where(PlusPolicy::Parse)
    }

    // FIXME: Find ways to get rid of this function or make it return something richer that
    //        can then be used inside `parse_ty` to perform less work / avoid prefix rechecking.
    pub(super) fn begins_ty(&self, offset: usize) -> bool {
        // NOTE: To be kept in sync with `Self::parse_ty`.

        match self.peek(offset).kind {
            | TokenKind::DoubleAmpersand
            | TokenKind::Dyn
            | TokenKind::Extern
            | TokenKind::Fn
            | TokenKind::For
            | TokenKind::Impl
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
            // NB: `?` is the only eligible trait bound modifier here!
            | TokenKind::QuestionMark
            | TokenKind::SingleAmpersand
            | TokenKind::SingleAsterisk
            | TokenKind::SingleBang
            | TokenKind::Underscore
            | TokenKind::Unsafe => true,
            TokenKind::TickedIdent => self.matches(TokenPrefix::Plus, self.peek(offset + 1)),
            _ => self.begins_ext_path(offset),
        }
    }

    pub(super) fn parse_ty_where(&mut self, p_policy: PlusPolicy) -> Result<ast::Ty<'src>> {
        // NOTE: To be kept in sync with `Self::begins_ty`.

        let start = self.token.span;

        // FIXME: Provide more targeted diagnostics if the qualifiers don't make sense.
        match self.parse_ty_qualifiers()?.as_mut_slice() {
            [] => {}
            [qualifiers @ .., Qualifier::Fn] => {
                let mut modifiers = ast::FnPtrTyModifiers::default();

                let (bound_vars, mut qualifiers) = match qualifiers {
                    [Qualifier::ForBinder(bound_vars), qualifiers @ ..] => {
                        (mem::take(bound_vars), &*qualifiers)
                    }
                    _ => (Vec::new(), &*qualifiers),
                };
                (modifiers.safety, qualifiers) = Qualifier::strip_safety(qualifiers);
                (modifiers.extern_, qualifiers) = Qualifier::strip_extern(qualifiers);
                if !qualifiers.is_empty() {
                    self.error(Error::InvalidTyPrefix(start.until(self.token.span)));
                }
                return self.fin_parse_fn_ptr_ty(bound_vars, modifiers);
            }
            [Qualifier::ForBinder(bound_vars)] => {
                let path = self.parse_path::<ast::UnambiguousGenericArgs>(PathMode::Normal)?;
                let mut bounds = vec![ast::Bound::Trait {
                    bound_vars: mem::take(bound_vars),
                    modifiers: ast::TraitBoundModifiers::NONE,
                    path,
                }];
                if let PlusPolicy::Parse = p_policy
                    && self.consume(TokenPrefix::Plus)
                {
                    // NB: Indeed, we're not meant to elevate the plus policy here.
                    self.parse_bounds_into(p_policy.maintain(), &mut bounds)?;
                }
                return Ok(ast::Ty::DynTrait(ast::DynKind::Bare, bounds));
            }
            _ => return self.fatal(Error::InvalidTyPrefix(start.until(self.token.span))),
        }

        match self.token.kind {
            TokenKind::CommonIdent => match () {
                () if self.check(weak::Builtin) => {
                    self.advance();
                    return self.fin_parse_builtin_ty(start);
                }
                () if self.check(weak::Dyn) => {
                    self.advance();
                    return self.fin_parse_dyn_trait_object_ty(p_policy);
                }
                () => {}
            },
            TokenKind::DoubleAmpersand => {
                self.advance();
                let pointee = self.fin_parse_ref_ty()?;
                let view = self.parse_view()?;
                return Ok(ast::Ty::Ref(Box::new(ast::RefTy {
                    lt: None,
                    kind: ast::BorrowKind::Ref,
                    mut_: ast::Mut::No,
                    pointee,
                    view,
                })));
            }
            TokenKind::Dyn => {
                self.advance();
                return self.fin_parse_dyn_trait_object_ty(p_policy);
            }
            TokenKind::Impl => {
                self.advance();
                let bounds = self.parse_bounds_where(p_policy.elevate())?;
                return Ok(ast::Ty::ImplTrait(bounds));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_grouped_or_tuple_or_bare_trait_object_ty(p_policy);
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
            TokenKind::SingleAsterisk => {
                self.advance();
                let mut_ = match self.token.kind {
                    TokenKind::Const => {
                        self.advance();
                        ast::Mut::No
                    }
                    TokenKind::Mut => {
                        self.advance();
                        ast::Mut::Yes
                    }
                    _ => {
                        return self.fatal(Error::UnexpectedToken(
                            self.token,
                            frags![TokenKind::Mut, TokenKind::Const],
                        ));
                    }
                };
                let ty = self.parse_ty_where(PlusPolicy::Yield)?;
                return Ok(ast::Ty::Ptr(mut_, Box::new(ty)));
            }
            TokenKind::SingleBang => {
                self.advance();
                return Ok(ast::Ty::Never);
            }
            // NB: We're indeed committing to parsing a trait object type if the lifetime is
            //     followed by a `+` while completely disregarding the plus policy! It means
            //     we're briefly counting the plus as belonging to this lifetime bound but
            //     then when it comes to actually parsing the list of bounds, we're involving
            //     the policy again. It means we accept `fn f<T: Fn() -> 'a + B>() {}`. Moreover,
            //     we interpret the bounds like `(Fn() -> 'a+) + B`, not even `Fn() -> ('a + B)`!
            //     Clearly, this is an upstream bug. Such pluses should be flagged ambiguous.
            TokenKind::TickedIdent => {
                if !self.matches(TokenPrefix::Plus, self.peek(1)) {
                    self.error(Error::LifetimeObjectTyWithoutPlus(start));
                }

                let bounds = self.parse_bounds_where(p_policy.maintain())?;
                return Ok(ast::Ty::DynTrait(ast::DynKind::Bare, bounds));
            }
            TokenKind::Underscore => {
                self.advance();
                return Ok(ast::Ty::Inferred);
            }
            TokenKind::Unsafe => {
                self.feature(Feature::unsafe_binders, self.token.span);
                self.advance();
                self.parse(TokenPrefix::LessThan)?;
                let bound_vars = self.fin_parse_generic_param_list()?;
                let ty = self.parse_ty()?;
                return Ok(ast::Ty::UnsafeBinder(bound_vars, Box::new(ty)));
            }
            _ => {}
        }

        if self.begins_ext_path(0) {
            let path = self.parse_ext_path::<ast::UnambiguousGenericArgs>()?;

            if self.consume(TokenKind::SingleBang) {
                if path.ext.is_some() {
                    self.error(Error::TyRelMacroCall(start.until(self.token.span)));
                }
                let (bracket, stream) = self.parse_delimited_token_stream()?;
                return Ok(ast::Ty::MacroCall(ast::MacroCall { path: path.path, bracket, stream }));
            }

            if path.ext.is_none()
                && let PlusPolicy::Parse = p_policy
                && self.consume(TokenPrefix::Plus)
            {
                let mut bounds = vec![ast::Bound::from(path.path)];
                self.parse_bounds_into(p_policy.maintain(), &mut bounds)?;
                return Ok(ast::Ty::DynTrait(ast::DynKind::Bare, bounds));
            }

            return Ok(ast::Ty::Path(Box::new(path)));
        }

        // NB: Indeed, `[const] Trait` won't reach here. Upstream has the same problem.
        if self.begins_bound(0) {
            let bounds = self.parse_bounds_where(p_policy.maintain())?;
            return Ok(ast::Ty::DynTrait(ast::DynKind::Bare, bounds));
        }

        self.fatal(Error::UnexpectedToken(self.token, frags![Fragment::Ty]))
    }

    fn parse_ty_qualifiers(&mut self) -> Result<Vec<Qualifier<'src>>> {
        let mut qualifiers = Vec::new();

        loop {
            let qualifier = match self.token.kind {
                TokenKind::CommonIdent => match self.source(self.token.span) {
                    weak::Safe::STR if weak::Safe.qualifies(self) => Qualifier::Safe,
                    _ => break,
                },
                TokenKind::Extern => {
                    self.advance();
                    qualifiers.push(Qualifier::Extern(self.parse_abi_str()));
                    continue;
                }
                TokenKind::Fn => Qualifier::Fn,
                TokenKind::For => {
                    self.advance();
                    self.parse(TokenPrefix::LessThan)?;
                    let bound_vars = self.fin_parse_generic_param_list()?;
                    qualifiers.push(Qualifier::ForBinder(bound_vars));
                    continue;
                }
                TokenKind::Unsafe if self.peek(1).kind != TokenKind::SingleLessThan => {
                    Qualifier::Unsafe
                }
                _ => break,
            };
            self.advance();
            qualifiers.push(qualifier);
        }

        Ok(qualifiers)
    }

    fn fin_parse_grouped_or_tuple_or_bare_trait_object_ty(
        &mut self,
        p_policy: PlusPolicy,
    ) -> Result<ast::Ty<'src>> {
        let mut tys = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        const SEPARATOR: TokenKind = TokenKind::Comma;
        while !self.consume(DELIMITER) {
            let mut ty = self.parse_ty()?;

            if self.token.kind == DELIMITER {
                if tys.is_empty() {
                    let trailing_plus =
                        self.prev_token().is_some_and(|t| self.matches(TokenPrefix::Plus, t));

                    self.advance();

                    if !trailing_plus
                        && let PlusPolicy::Parse = p_policy
                        && self.check(TokenPrefix::Plus)
                        && let Some(ty) =
                            self.extract_fin_parse_bare_paren_trait_object_ty(&mut ty, p_policy)?
                    {
                        return Ok(ty);
                    }

                    return Ok(ast::Ty::Grouped(Box::new(ty)));
                }
            } else {
                self.parse(SEPARATOR)?;
            }

            tys.push(ty);
        }

        Ok(ast::Ty::Tuple(tys))
    }

    fn extract_fin_parse_bare_paren_trait_object_ty(
        &mut self,
        inner_ty: &mut ast::Ty<'src>,
        p_policy: PlusPolicy,
    ) -> Result<Option<ast::Ty<'src>>> {
        const EMPTY<'src>: ast::Path<'src, ast::UnambiguousGenericArgs> =
            ast::Path { segs: Vec::new() };

        let bound = match inner_ty {
            ast::Ty::Path(deref!(ast::ExtPath { ext: None, path })) => {
                ast::Bound::from(mem::replace(path, EMPTY))
            }
            ast::Ty::DynTrait(ast::DynKind::Bare, deref!([bound])) => {
                match bound {
                    ast::Bound::Outlives(_) => return Ok(None),
                    // NOTE: I'm not happy about this since use-bounds can't be parenthesized "normally".
                    ast::Bound::Use(captures) => ast::Bound::Use(mem::take(captures)),
                    ast::Bound::Trait { bound_vars, modifiers, path } => ast::Bound::Trait {
                        bound_vars: mem::take(bound_vars),
                        modifiers: *modifiers,
                        path: mem::replace(path, EMPTY),
                    },
                }
            }
            _ => return Ok(None),
        };

        self.parse_unchecked(TokenPrefix::Plus);

        let mut bounds = vec![bound];
        self.parse_bounds_into(p_policy.maintain(), &mut bounds)?;

        Ok(Some(ast::Ty::DynTrait(ast::DynKind::Bare, bounds)))
    }

    fn fin_parse_builtin_ty(&mut self, start: Span) -> Result<ast::Ty<'src>> {
        self.fin_parse_builtin_syntax(start, ast::Ty::Error, |this, name| match name {
            weak::FieldOf::STR => {
                let ty = this.parse_ty()?;
                this.parse(TokenKind::Comma)?;
                let fields = this.fin_parse_delimited_field_seq()?;

                let (variant, field) = match *fields {
                    [] => unreachable!(),
                    [field] => (None, field),
                    [variant, field, ref extra @ ..] => {
                        let extra = match extra {
                            [] => None,
                            [single] => Some(single.span),
                            [first, .., last] => Some(first.span.to(last.span)),
                        };
                        if let Some(span) = extra {
                            this.error(Error::InvalidExtraFieldProjections(span));
                        }

                        (Some(variant), field)
                    }
                };

                Ok(Some(ast::Ty::FieldOf(Box::new(ty), variant, field)))
            }
            _ => Ok(None),
        })
    }

    fn fin_parse_dyn_trait_object_ty(&mut self, p_policy: PlusPolicy) -> Result<ast::Ty<'src>> {
        Ok(ast::Ty::DynTrait(ast::DynKind::Dyn, self.parse_bounds_where(p_policy.elevate())?))
    }

    fn fin_parse_fn_ptr_ty(
        &mut self,
        bound_vars: Vec<ast::GenericParam<'src>>,
        modifiers: ast::FnPtrTyModifiers<'src>,
    ) -> Result<ast::Ty<'src>> {
        let inputs = self.parse_fn_param_list(FnParamMode::Optional)?;
        let output = self
            .consume(TokenKind::ThinArrow)
            .then(|| self.parse_ty_where(PlusPolicy::Yield))
            .transpose()?;
        Ok(ast::Ty::FnPtr(Box::new(ast::FnPtrTy { bound_vars, modifiers, inputs, output })))
    }

    fn fin_parse_ref_ty(&mut self) -> Result<ast::Ty<'src>> {
        let lt = self.parse_lifetime();
        let (kind, mut_) = self.parse_borrow_kind_and_mutability();
        let pointee = self.parse_ty_where(PlusPolicy::Yield)?;
        let view = self.parse_view()?;
        Ok(ast::Ty::Ref(Box::new(ast::RefTy { lt, kind, mut_, pointee, view })))
    }

    fn parse_view(&mut self) -> Result<Option<Vec<ast::Ident<'src>>>> {
        if let span = self.token.span
            && self.consume(TokenKind::SingleDot)
        {
            self.feature(Feature::view_types, span);
            self.parse(TokenKind::OpenCurlyBracket)?;
            return self
                .fin_parse_delim_seq(TokenKind::CloseCurlyBracket, TokenKind::Comma, |this| {
                    this.parse_common_ident()
                })
                .map(Some);
        }

        Ok(None)
    }

    /// Optionally parse generics (generic parameter list followed by a where-clause).
    pub(super) fn parse_generics(&mut self) -> Result<ast::Generics<'src>> {
        let params = self.parse_generic_param_list()?.unwrap_or_default();
        let preds = self.parse_where_clause()?.unwrap_or_default();
        Ok(ast::Generics { params, preds })
    }

    /// Optionally parse a list of generic parameters.
    pub(super) fn parse_generic_param_list(
        &mut self,
    ) -> Result<Option<Vec<ast::GenericParam<'src>>>> {
        if !self.consume(TokenPrefix::LessThan) {
            return Ok(None);
        }
        self.fin_parse_generic_param_list().map(Some)
    }

    pub(super) fn fin_parse_generic_param_list(&mut self) -> Result<Vec<ast::GenericParam<'src>>> {
        const SEPARATOR: TokenKind = TokenKind::Comma;
        self.fin_parse_delim_seq(TokenPrefix::GreaterThan, SEPARATOR, |this| {
            let attrs = this.parse_attrs(ast::AttrStyle::Outer)?;

            let (binder, kind) = if let Some(ast::Lifetime(lt)) = this.parse_lifetime() {
                let bounds = if this.consume(TokenKind::SingleColon) {
                    this.parse_outlives_bounds()
                } else {
                    Vec::new()
                };
                (lt, ast::GenericParamKind::Lifetime(bounds))
            } else {
                match this.token.kind {
                    TokenKind::Const => {
                        this.advance();
                        let binder = this.parse_common_ident()?;
                        let ty = this.parse_ty_annotation()?;
                        let default = this
                            .consume(TokenKind::SingleEquals)
                            .then(|| this.parse_const_arg())
                            .transpose()?;
                        (binder, ast::GenericParamKind::Const { ty, default })
                    }
                    TokenKind::CommonIdent => {
                        let ident = this.ident(this.token.span);
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
                        return this.fatal(Error::UnexpectedToken(
                            this.token,
                            frags![Fragment::GenericParam, SEPARATOR, TokenKind::SingleGreaterThan],
                        ));
                    }
                }
            };

            Ok(ast::GenericParam { attrs, binder, kind })
        })
    }

    /// Optionally parse a where-clause.
    pub(super) fn parse_where_clause(&mut self) -> Result<Option<Vec<ast::Predicate<'src>>>> {
        if !self.consume(TokenKind::Where) {
            return Ok(None);
        }

        if self.pick_generic_param_list_over_ext_path(0) {
            let start = self.token.span;
            let _bound_vars = self.parse_generic_param_list()?;
            self.error(Error::ParametrizedWhereClause(start.until(self.token.span)));
        }

        let mut preds = Vec::new();

        while self.begins_predicate() {
            preds.push(self.parse_predicate()?);

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(Some(preds))
    }

    fn parse_predicate(&mut self) -> Result<ast::Predicate<'src>> {
        // NOTE: To be kept in sync with `Self::begins_predicate`.

        let attrs = self.parse_attrs(ast::AttrStyle::Outer)?;
        if let Some(attr) = attrs.first() {
            let span = attr.span.to(attrs.last().unwrap().span);
            self.feature(Feature::where_clause_attrs, span);
        }

        let bound_vars = self.parse_for_binder()?;

        let kind = if bound_vars.is_some() || self.begins_ty(0) {
            let ty = self.parse_ty()?;
            self.parse(TokenKind::SingleColon)?;
            let bounds = self.parse_bounds()?;
            ast::PredicateKind::Trait(ast::TraitPredicate {
                bound_vars: bound_vars.map_or(Vec::new(), |(vars, _)| vars),
                ty,
                bounds,
            })
        } else if let Some(lt) = self.parse_lifetime() {
            self.parse(TokenKind::SingleColon)?;
            let bounds = self.parse_outlives_bounds();
            ast::PredicateKind::Outlives(ast::OutlivesPredicate { lt, bounds })
        } else {
            return self.fatal(Error::UnexpectedToken(self.token, frags![Fragment::Predicate]));
        };

        Ok(ast::Predicate { attrs, kind })
    }

    fn begins_predicate(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_predicate`.

        matches!(self.token.kind, TokenKind::TickedIdent | TokenKind::For)
            || self.begins_ty(0)
            || self.begins_outer_attr()
    }

    /// Parse a bounds annotation if available.
    pub(super) fn parse_bounds(&mut self) -> Result<Vec<ast::Bound<'src>>> {
        self.parse_bounds_where(PlusPolicy::Parse)
    }

    fn parse_bounds_where(&mut self, p_policy: PlusPolicy<()>) -> Result<Vec<ast::Bound<'src>>> {
        let mut bounds = Vec::new();
        self.parse_bounds_into(p_policy, &mut bounds)?;
        Ok(bounds)
    }

    fn parse_bounds_into(
        &mut self,
        p_policy: PlusPolicy<()>,
        bounds: &mut Vec<ast::Bound<'src>>,
    ) -> Result<()> {
        while self.begins_bound(0) {
            bounds.push(self.parse_bound()?);

            let span = self.token.span;
            if matches!(p_policy, PlusPolicy::Yield) || !self.consume(TokenPrefix::Plus) {
                break;
            }
            if let PlusPolicy::Reject(()) = p_policy {
                self.error(Error::AmbiguousPlus(span));
            }
        }

        Ok(())
    }

    fn parse_bound(&mut self) -> Result<ast::Bound<'src>> {
        // NOTE: To be kept in sync with `Self::begins_bound`.

        // We parse the trait bound "frontmatter" for all bound kinds to
        // reject them afterwards with a better diagnostic.
        let grouped = self.consume(TokenKind::OpenRoundBracket);
        let bound_vars = self.parse_for_binder()?;
        let modifiers = self.parse_trait_bound_modifiers(bound_vars.as_ref())?;

        if let Some(lt) = self.parse_lifetime() {
            self.reject_trait_bound_frontmatter(grouped, bound_vars, modifiers)?;
            return Ok(ast::Bound::Outlives(lt));
        }

        if self.consume(TokenKind::Use) {
            self.parse(TokenKind::SingleLessThan)?;
            let captures =
                self.fin_parse_delim_seq(TokenPrefix::GreaterThan, TokenKind::Comma, |this| {
                    if let Some(lt) = this.parse_lifetime() {
                        return Ok(ast::Capture::Lifetime(lt));
                    }
                    match this.token.kind {
                        TokenKind::CommonIdent | TokenKind::SelfUpper => {
                            let param = this.ident(this.token.span);
                            this.advance();
                            Ok(ast::Capture::TyOrConst(param))
                        }
                        _ => this.fatal(Error::UnexpectedToken(
                            this.token,
                            frags![Fragment::GenericParam],
                        )),
                    }
                })?;

            self.reject_trait_bound_frontmatter(grouped, bound_vars, modifiers)?;

            return Ok(ast::Bound::Use(captures));
        }

        if self.begins_path(0) {
            let path = self.parse_path::<ast::UnambiguousGenericArgs>(PathMode::Normal)?;

            if grouped {
                self.parse(TokenKind::CloseRoundBracket)?;
            }

            return Ok(ast::Bound::Trait {
                bound_vars: bound_vars.map_or(Vec::new(), |(vars, _)| vars),
                modifiers,
                path,
            });
        }

        self.fatal(Error::UnexpectedToken(self.token, frags![Fragment::Bound]))
    }

    #[allow(clippy::needless_pass_by_value)] // the callers want to dispose of the bad binder
    fn reject_trait_bound_frontmatter(
        &mut self,
        grouped: bool,
        bound_vars: Option<(Vec<ast::GenericParam<'src>>, Span)>,
        modifiers: ast::TraitBoundModifiers,
    ) -> Result<()> {
        if grouped {
            self.parse(TokenKind::CloseRoundBracket)?;
            // FIXME: (Multi)Span
            self.error(Error::InvalidParenthesizedBound);
        }

        if let Some((_, span)) = bound_vars {
            self.error(Error::HigherRankedBinderOnInvalidBound(span));
        }

        if modifiers != ast::TraitBoundModifiers::NONE {
            // FIXME: Span
            self.error(Error::ModifiersOnInvalidBound);
        }

        Ok(())
    }

    fn begins_bound(&self, offset: usize) -> bool {
        // NOTE: To be kept in sync with `Self::parse_bound`.

        match self.peek(offset).kind {
            | TokenKind::TickedIdent
            | TokenKind::For
            | TokenKind::Use
            | TokenKind::OpenRoundBracket => true,
            _ => self.begins_trait_bound_modifiers(offset) || self.begins_path(offset),
        }
    }

    #[expect(clippy::unused_self)] // keeping `begins_bound` & this fn as siblings for better discoverability
    pub(super) fn begins_2015_dyn_bound(&self, token: Token) -> bool {
        matches!(
            token.kind,
            PathSegIdent!()
                | TokenKind::For
                | TokenKind::TickedIdent
                | TokenKind::OpenRoundBracket
                | TokenKind::QuestionMark
        )
    }

    fn parse_trait_bound_modifiers(
        &mut self,
        bound_vars: Option<&(Vec<ast::GenericParam<'src>>, Span)>,
    ) -> Result<ast::TraitBoundModifiers> {
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
            TokenKind::Tilde => {
                self.advance();
                self.parse(TokenKind::Const)?;
                ast::BoundConstness::Maybe
            }
            _ => ast::BoundConstness::Never,
        };
        if let ast::BoundConstness::Always | ast::BoundConstness::Maybe = constness {
            self.feature_no_span_fixme(Feature::const_trait_impl);
        }

        let asyncness = if let span = self.token.span
            && self.consume(TokenKind::Async)
        {
            self.feature(Feature::async_trait_bounds, span);
            ast::BoundAsyncness::Always
        } else {
            ast::BoundAsyncness::Never
        };

        // FIXME: Find a nicer way to impl / expr this
        let polarity = if bound_vars.is_none()
            && let ast::BoundConstness::Never = constness
            && let ast::BoundAsyncness::Never = asyncness
        {
            match self.token.kind {
                TokenKind::SingleBang => {
                    self.feature(Feature::negative_bounds, self.token.span);
                    self.advance();
                    ast::BoundPolarity::Negative
                }
                TokenKind::QuestionMark => {
                    self.advance();
                    ast::BoundPolarity::Maybe
                }
                _ => ast::BoundPolarity::Positive,
            }
        } else {
            ast::BoundPolarity::Positive
        };

        Ok(ast::TraitBoundModifiers { constness, asyncness, polarity })
    }

    fn begins_trait_bound_modifiers(&self, offset: usize) -> bool {
        // NOTE: To be kept in sync with `Self::parse_trait_bound_modifiers`.

        match self.peek(offset).kind {
            | TokenKind::Async
            | TokenKind::Const
            | TokenKind::QuestionMark
            | TokenKind::SingleBang
            | TokenKind::Tilde => true,
            TokenKind::OpenSquareBracket => {
                self.peek(offset + 1).kind == TokenKind::Const
                    && self.peek(offset + 2).kind == TokenKind::CloseSquareBracket
            }
            _ => false,
        }
    }

    fn parse_outlives_bounds(&mut self) -> Vec<ast::Lifetime<'src>> {
        let mut bounds = Vec::new();

        while let Some(lt) = self.parse_lifetime() {
            bounds.push(lt);

            if !self.consume(TokenKind::SinglePlus) {
                break;
            }
        }

        bounds
    }

    fn parse_for_binder(&mut self) -> Result<Option<(Vec<ast::GenericParam<'src>>, Span)>> {
        let start = self.token.span;

        if !self.consume(TokenKind::For) {
            return Ok(None);
        }

        self.parse(TokenPrefix::LessThan)?;
        let bound_vars = self.fin_parse_generic_param_list()?;

        // FIXME: Better span
        Ok(Some((bound_vars, start.until(self.token.span))))
    }

    /// Optionally parse a lifetime.
    pub(super) fn parse_lifetime(&mut self) -> Option<ast::Lifetime<'src>> {
        Some(ast::Lifetime(self.parse_ticked_ident(
            |kind| {
                matches!(kind, TokenKind::CommonIdent | TokenKind::Underscore | TokenKind::Static)
            },
            Error::ReservedLifetime,
        )?))
    }
}

#[derive(Clone, Copy)]
pub(super) enum PlusPolicy<X = !> {
    Parse,
    Yield,
    Reject(X),
}

impl PlusPolicy {
    fn maintain(self) -> PlusPolicy<()> {
        match self {
            Self::Parse => PlusPolicy::Parse,
            Self::Yield => PlusPolicy::Yield,
        }
    }

    fn elevate(self) -> PlusPolicy<()> {
        match self {
            Self::Parse => PlusPolicy::Parse,
            Self::Yield => PlusPolicy::Reject(()),
        }
    }
}

enum Qualifier<'src> {
    Extern(Option<&'src str>),
    Fn,
    ForBinder(Vec<ast::GenericParam<'src>>),
    Safe,
    Unsafe,
}

impl<'src> Qualifier<'src> {
    fn strip_safety(qualifiers: &[Self]) -> (ast::Safety<()>, &[Self]) {
        match qualifiers {
            [Self::Unsafe, qualifiers @ ..] => (ast::Safety::Unsafe, qualifiers),
            [Self::Safe, qualifiers @ ..] => (ast::Safety::Safe(()), qualifiers),
            _ => (ast::Safety::Inherited, qualifiers),
        }
    }

    fn strip_extern(qualifiers: &[Self]) -> (ast::Extern<'src>, &[Self]) {
        match qualifiers {
            [Qualifier::Extern(abi), qualifiers @ ..] => (ast::Extern::Yes(*abi), qualifiers),
            _ => (ast::Extern::No, qualifiers),
        }
    }
}
