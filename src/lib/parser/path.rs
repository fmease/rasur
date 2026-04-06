use super::{
    ExpectedFragment, Parser, Result, TokenKind, TokenPrefix, expr::AttrPolicy, one_of,
    ty::PlusPolicy,
};
use crate::{ast, error::Error, feature::Feature, token::PathSegIdent};

impl<'src> Parser<'_, '_, 'src> {
    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= "::"? Path_Seg_Ident ("::" Path_Seg_Ident)*
    /// ```
    pub(super) fn parse_path<M: GenericArgsMode>(
        &mut self,
        mode: PathMode,
    ) -> Result<ast::Path<'src, M>> {
        // NOTE: To be kept in sync with `Self::begins_path`.

        let mut path = self.parse_path_prefix(mode)?;

        path.segs.push(self.parse_path_seg::<M>()?);

        while self.consume(TokenKind::DoubleColon) {
            path.segs.push(self.parse_path_seg::<M>()?);
        }

        Ok(path)
    }

    pub(super) fn begins_path(&self, offset: usize) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        matches!(self.peek(offset).kind, TokenKind::DoubleColon | PathSegIdent!())
    }

    pub(super) fn parse_path_prefix<M: GenericArgsMode>(
        &mut self,
        mode: PathMode,
    ) -> Result<ast::Path<'src, M>> {
        let mut path = ast::Path { segs: Vec::new() };

        match mode {
            PathMode::Normal => {
                let span = self.token.span;
                if self.consume(TokenKind::DoubleColon) {
                    let ident = ast::Ident::new("", span.start.into());
                    path.segs.push(ast::PathSeg::ident(ident));
                }
            }
            PathMode::Suffix => self.parse(TokenKind::DoubleColon)?,
        }

        Ok(path)
    }

    /// Parse an extended path.
    pub(super) fn parse_ext_path<S: GenericArgsStyle>(&mut self) -> Result<ast::ExtPath<'src, S>> {
        // NOTE: To be kept in sync with `Self::begins_ext_path`.

        let (ext, mode) = self.parse_path_ext()?;
        let path = self.parse_path(mode)?;

        Ok(ast::ExtPath { ext, path })
    }

    pub(super) fn parse_path_ext(&mut self) -> Result<(Option<ast::PathExt<'src>>, PathMode)> {
        // NOTE: To be kept in sync with `Self::begins_ext_path`.

        if !self.consume(TokenPrefix::LessThan) {
            return Ok((None, PathMode::Normal));
        }
        let self_ty = self.parse_ty()?;
        // We're in a "type context" now and can parse generic args unambiguously.
        let trait_ref = self
            .consume(TokenKind::As)
            .then(|| self.parse_path::<ast::UnambiguousGenericArgs>(PathMode::Normal))
            .transpose()?;
        self.parse(TokenKind::SingleGreaterThan)?; // no need to account for DoubleGreaterThan

        Ok((Some(ast::PathExt { self_ty, trait_ref }), PathMode::Suffix))
    }

    pub(super) fn begins_ext_path(&self, offset: usize) -> bool {
        // NOTE: To be kept in sync with `Self::parse_ext_path`.

        self.matches(TokenPrefix::LessThan, self.peek(offset)) || self.begins_path(offset)
    }

    pub(super) fn parse_path_seg<M: GenericArgsMode>(&mut self) -> Result<ast::PathSeg<'src, M>> {
        match self.token.kind {
            PathSegIdent!() => {
                let ident = self.ident(self.token.span);
                self.advance();
                Ok(ast::PathSeg { ident, args: M::parse(self)? })
            }
            _ => self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::PathSegIdent)),
        }
    }

    fn parse_generic_args(
        &mut self,
        ambiguity: GenericArgsAmbiguity,
    ) -> Result<Option<ast::GenericArgs<'src>>> {
        let disambiguated = if let TokenKind::DoubleColon = self.token.kind
            && let token = self.peek(1)
            && (token.kind == TokenKind::OpenRoundBracket
                || self.matches(TokenPrefix::LessThan, token))
        {
            self.advance();
            true
        } else {
            false
        };

        if disambiguated || ambiguity == GenericArgsAmbiguity::No {
            if self.consume(TokenPrefix::LessThan) {
                return self.fin_parse_angle_generic_args().map(Some);
            } else if self.consume(TokenKind::OpenRoundBracket) {
                return self.fin_parse_paren_generic_args().map(Some);
            }
        }

        Ok(None)
    }

    fn fin_parse_angle_generic_args(&mut self) -> Result<ast::GenericArgs<'src>> {
        const SEPARATOR: TokenKind = TokenKind::Comma;

        Ok(ast::GenericArgs::Angle(self.fin_parse_delim_seq(
            TokenPrefix::GreaterThan,
            SEPARATOR,
            |this: &mut Self| {
                let mut arg = if this.begins_ty(0) {
                    let ty = this.parse_ty()?;
                    ast::AngleGenericArg::Ty(ty)
                } else if let Some(lt) = this.parse_lifetime() {
                    ast::AngleGenericArg::Lifetime(lt)
                } else if this.begins_const_arg() {
                    let expr = this.parse_const_arg()?;
                    ast::AngleGenericArg::Const(expr)
                } else {
                    return this.fatal(Error::UnexpectedToken(
                        this.token,
                        one_of![
                            ExpectedFragment::GenericArg,
                            SEPARATOR,
                            /*delimiter*/ TokenKind::SingleGreaterThan
                        ],
                    ));
                };

                let separator = this.token;
                let arg = if let TokenKind::SingleColon | TokenKind::SingleEquals = separator.kind
                    && let Some((ident, args)) = extract_assoc_item_seg(&mut arg)
                {
                    this.advance();

                    let kind = match separator.kind {
                        TokenKind::SingleColon => {
                            ast::AssocItemConstraintKind::Bound(this.parse_bounds()?)
                        }
                        TokenKind::SingleEquals => {
                            ast::AssocItemConstraintKind::Equality(this.parse_term()?)
                        }
                        _ => unreachable!(),
                    };

                    ast::AngleGenericArg::Constraint(ast::AssocItemConstraint { ident, args, kind })
                } else {
                    arg
                };

                Ok(arg)
            },
        )?))
    }

    fn fin_parse_paren_generic_args(&mut self) -> Result<ast::GenericArgs<'src>> {
        if self.consume(TokenKind::DoubleDot) {
            self.feature_no_span_fixme(Feature::ReturnTypeNotation);
            self.parse(TokenKind::CloseRoundBracket)?;

            return Ok(ast::GenericArgs::ParenElided);
        }

        let inputs = self.fin_parse_delim_seq(
            TokenKind::CloseRoundBracket,
            TokenKind::Comma,
            Self::parse_ty,
        )?;
        let output = if self.consume(TokenKind::ThinArrow) {
            Some(self.parse_ty_where(PlusPolicy::Yield)?)
        } else {
            None
        };

        Ok(ast::GenericArgs::Paren(inputs, output))
    }

    fn parse_term(&mut self) -> Result<ast::Term<'src>> {
        if self.begins_ty(0) {
            Ok(ast::Term::Ty(self.parse_ty()?))
        } else if self.begins_const_arg() {
            self.feature(Feature::MinGenericConstArgs, self.token.span);
            Ok(ast::Term::Const(self.parse_const_arg()?))
        } else {
            self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::Term))
        }
    }

    pub(super) fn parse_const_arg(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_const_arg`.

        if let Some((sign, lit)) = self.opt_parse_negatable_lit()? {
            let expr = ast::ExprKind::Lit(lit).into();
            return Ok(match sign {
                ast::Sign::None => expr,
                ast::Sign::Neg => ast::ExprKind::UnOp(ast::UnOp::Neg, Box::new(expr)).into(),
            });
        }

        match self.token.kind {
            // NB: Only reachable when parsing terms. FIXME: We should make this a
            //     policy param for clarity.
            TokenKind::CommonIdent => {
                let ident = self.ident(self.token.span);
                self.advance();
                Ok(ast::ExprKind::Path(Box::new(ast::ExtPath::ident(ident))).into())
            }
            TokenKind::Const => {
                self.feature(Feature::MinGenericConstArgs, self.token.span);
                self.advance();
                let mut attrs = Vec::new();
                let block = self.parse_block_expr(AttrPolicy::Parse(&mut attrs))?;
                let kind =
                    ast::ExprKind::SpecialBlock(ast::SpecialBlockKind::Const, Box::new(block));
                Ok(ast::Expr { attrs, kind })
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                let mut attrs = Vec::new();
                let block = self.fin_parse_block_expr(AttrPolicy::Parse(&mut attrs))?;
                let kind = ast::ExprKind::Block(None, Box::new(block));
                Ok(ast::Expr { attrs, kind })
            }
            _ => self.fatal(Error::UnexpectedToken(self.token, ExpectedFragment::ConstArg)),
        }
    }

    // NB: Intentionally excludes common idents. FIXME: This should be made more obvious.
    fn begins_const_arg(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_const_arg`.

        matches!(self.token.kind, TokenKind::OpenCurlyBracket | TokenKind::Const)
            || self.begins_negatable_lit()
    }
}

#[derive(Clone, Copy)]
pub(super) enum PathMode {
    Normal,
    Suffix,
}

pub(super) trait GenericArgsMode: ast::GenericArgsMode {
    fn parse<'src>(parser: &mut Parser<'_, '_, 'src>) -> Result<Self::Args<'src>>;
}

impl GenericArgsMode for ast::NoGenericArgs {
    fn parse<'src>(_: &mut Parser<'_, '_, 'src>) -> Result<Self::Args<'src>> {
        Ok(())
    }
}

impl GenericArgsMode for ast::UnambiguousGenericArgs {
    fn parse<'src>(parser: &mut Parser<'_, '_, 'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(GenericArgsAmbiguity::No)
    }
}

impl GenericArgsMode for ast::ObligatorilyDisambiguatedGenericArgs {
    fn parse<'src>(parser: &mut Parser<'_, '_, 'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(GenericArgsAmbiguity::Yes)
    }
}

pub(super) trait GenericArgsStyle: ast::GenericArgsStyle + GenericArgsMode {}

impl GenericArgsStyle for ast::UnambiguousGenericArgs {}
impl GenericArgsStyle for ast::ObligatorilyDisambiguatedGenericArgs {}

#[derive(PartialEq, Eq, Clone, Copy)]
enum GenericArgsAmbiguity {
    Yes,
    No,
}

fn extract_assoc_item_seg<'src>(
    arg: &mut ast::AngleGenericArg<'src>,
) -> Option<(ast::Ident<'src>, Option<ast::GenericArgs<'src>>)> {
    if let ast::AngleGenericArg::Ty(ty) = arg
        && let ast::Ty::Path(deref!(path)) = ty
        && let ast::ExtPath { ext: None, path } = path
        && let ast::Path { segs: deref!([seg]) } = path
    {
        Some((seg.ident, seg.args.take()))
    } else {
        None
    }
}
