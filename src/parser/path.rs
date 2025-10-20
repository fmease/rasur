use super::{
    ExpectedFragment, Parser, Result, Token, TokenKind, error::ParseError, is_path_seg_keyword,
    is_reserved, one_of,
};
use crate::ast;

impl<'src> Parser<'_, 'src> {
    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= "::"? Path_Seg_Ident ("::" Path_Seg_Ident)*
    /// ```
    pub(super) fn parse_path<M: GenericArgsMode>(&mut self) -> Result<ast::Path<'src, M>> {
        // NOTE: To be kept in sync with `Self::begins_path`.

        let mut path = ast::Path { segs: Vec::new() };

        if self.consume(TokenKind::DoubleColon) {
            path.segs.push(ast::PathSeg::ident(""));
        }

        path.segs.push(self.parse_path_seg::<M>()?);

        while self.consume(TokenKind::DoubleColon) {
            path.segs.push(self.parse_path_seg::<M>()?);
        }

        Ok(path)
    }

    pub(super) fn begins_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        self.token.kind == TokenKind::DoubleColon || self.as_path_seg_ident().is_some()
    }

    /// Parse an extended path.
    pub(super) fn parse_ext_path<S: GenericArgsStyle>(&mut self) -> Result<ast::ExtPath<'src, S>> {
        // NOTE: To be kept in sync with `Self::begins_ext_path`.

        let mut path = ast::Path { segs: Vec::new() };

        // FIXME: Add `<` to list of expected tokens
        let self_ty = if self.consume_relaxed(TokenKind::SingleLessThan) {
            let ty = self.parse_ty()?;
            // We're in a "type context" now and can parse generic args unambiguously.
            let trait_ref = self
                .consume_ident("as")
                .then(|| self.parse_path::<ast::UnambiguousGenericArgs>())
                .transpose()?;
            self.parse(TokenKind::SingleGreaterThan)?; // no need to account for DoubleGreaterThan
            self.parse(TokenKind::DoubleColon)?;
            Some(ast::PathExt { self_ty: ty, trait_ref })
        } else {
            None
        };

        path.segs.push(self.parse_path_seg::<S>()?);

        while self.consume(TokenKind::DoubleColon) {
            path.segs.push(self.parse_path_seg::<S>()?);
        }

        Ok(ast::ExtPath { ext: self_ty, path })
    }

    pub(super) fn begins_ext_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_ext_path`.

        matches!(
            self.token.kind,
            TokenKind::SingleLessThan
                | TokenKind::DoubleLessThan
                | TokenKind::LessThanEquals
                | TokenKind::DoubleLessThanEquals
        ) || self.begins_path()
    }

    fn parse_path_seg<M: GenericArgsMode>(&mut self) -> Result<ast::PathSeg<'src, M>> {
        let ident = self.as_path_seg_ident().inspect(|_| self.advance()).ok_or_else(|| {
            ParseError::UnexpectedToken(self.token, ExpectedFragment::PathSegIdent)
        })?;
        let args = M::parse(self)?;
        Ok(ast::PathSeg { ident, args })
    }

    pub(super) fn as_path_seg_ident(&self) -> Option<ast::Ident<'src>> {
        self.as_ident(self.token)
            .filter(|ident| is_path_seg_keyword(ident) || self.ident_is_common(ident))
    }

    fn parse_generic_args(
        &mut self,
        ambiguity: GenericArgsAmbiguity,
    ) -> Result<Option<ast::GenericArgs<'src>>> {
        let disambiguated = if self.token.kind == TokenKind::DoubleColon
            && self.look_ahead(1, |token| {
                matches!(
                    token.kind,
                    TokenKind::SingleLessThan
                        | TokenKind::DoubleLessThan
                        | TokenKind::OpenRoundBracket
                )
            }) {
            self.advance();
            true
        } else {
            false
        };

        if disambiguated || ambiguity == GenericArgsAmbiguity::No {
            return Ok(match self.token.kind {
                TokenKind::SingleLessThan => {
                    self.advance();
                    Some(self.fin_parse_angle_generic_args()?)
                }
                TokenKind::DoubleLessThan => {
                    self.modify_in_place(TokenKind::SingleLessThan);
                    Some(self.fin_parse_angle_generic_args()?)
                }
                TokenKind::OpenRoundBracket => {
                    self.advance();
                    Some(self.fin_parse_paren_generic_args()?)
                }
                _ => None,
            });
        }

        Ok(None)
    }

    fn fin_parse_angle_generic_args(&mut self) -> Result<ast::GenericArgs<'src>> {
        const SEPARATOR: TokenKind = TokenKind::Comma;

        Ok(ast::GenericArgs::Angle(self.fin_parse_delim_seq_with(
            |this| this.consume_relaxed(TokenKind::SingleGreaterThan),
            Self::begins_single_greater_than,
            SEPARATOR,
            |this: &mut Self| {
                let mut arg = if this.begins_ty() {
                    let ty = this.parse_ty()?;
                    ast::GenericArg::Ty(ty)
                } else if let Some(lt) = this.consume_common_lifetime()? {
                    ast::GenericArg::Lifetime(lt)
                } else if this.begins_const_arg() {
                    let expr = this.parse_const_arg()?;
                    ast::GenericArg::Const(expr)
                } else {
                    return Err(ParseError::UnexpectedToken(
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
                    ast::AngleGenericArg::Argument(arg)
                };

                Ok(arg)
            },
        )?))
    }

    fn fin_parse_paren_generic_args(&mut self) -> Result<ast::GenericArgs<'src>> {
        if self.consume(TokenKind::DoubleDot) {
            self.parse(TokenKind::CloseRoundBracket)?;

            return Ok(ast::GenericArgs::ParenElided);
        }

        let inputs = self.fin_parse_delim_seq(
            TokenKind::CloseRoundBracket,
            TokenKind::Comma,
            Self::parse_ty,
        )?;
        let output = if self.consume(TokenKind::ThinArrow) { Some(self.parse_ty()?) } else { None };

        Ok(ast::GenericArgs::Paren { inputs, output })
    }

    fn parse_term(&mut self) -> Result<ast::Term<'src>> {
        if self.begins_ty() {
            Ok(ast::Term::Ty(self.parse_ty()?))
        } else if self.begins_const_arg() {
            Ok(ast::Term::Const(self.parse_const_arg()?))
        } else {
            Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Term))
        }
    }

    pub(crate) fn parse_const_arg(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_const_arg`.

        // FIXME: Leading dash (unary minus)
        match self.token.kind {
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::Expr::Lit(ast::Lit::Num(lit)));
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::Expr::Lit(ast::Lit::Str(lit)));
            }
            TokenKind::CharLit => {
                let lit = self.source(self.token.span);
                self.advance();
                // FIXME: Validate the char lit.
                return Ok(ast::Expr::Lit(ast::Lit::Char(lit)));
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return Ok(ast::Expr::Block(
                    ast::BlockKind::Bare,
                    Box::new(self.fin_parse_block_expr()?),
                ));
            }
            TokenKind::Ident => match self.source(self.token.span) {
                "false" => {
                    self.advance();
                    return Ok(ast::Expr::Lit(ast::Lit::Bool(false)));
                }
                "true" => {
                    self.advance();
                    return Ok(ast::Expr::Lit(ast::Lit::Bool(true)));
                }
                _ => {}
            },
            _ => {}
        }

        // FIXME: Proper fragment
        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Expr))
    }

    fn begins_const_arg(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_const_arg`.

        // FIXME: Leading dash (unary minus)
        match self.token.kind {
            TokenKind::OpenCurlyBracket
            | TokenKind::StrLit
            | TokenKind::NumLit
            | TokenKind::CharLit => true,
            TokenKind::Ident => matches!(self.source(self.token.span), "false" | "true"),
            _ => false,
        }
    }

    pub(super) fn parse_path_tree(&mut self) -> Result<ast::PathTree<'src>> {
        let mut path = ast::Path { segs: Vec::new() };

        if self.consume(TokenKind::DoubleColon) {
            path.segs.push(ast::PathSeg::ident(""));
        }

        match self.parse_path_tree_kind(&mut path)? {
            ast::PathTreeKind::Stump(None) => {}
            kind => return Ok(ast::PathTree { path, kind }),
        }

        while self.consume(TokenKind::DoubleColon) {
            match self.parse_path_tree_kind(&mut path)? {
                ast::PathTreeKind::Stump(None) => {}
                kind => return Ok(ast::PathTree { path, kind }),
            }
        }

        Ok(ast::PathTree { path, kind: ast::PathTreeKind::Stump(None) })
    }

    fn parse_path_tree_kind(
        &mut self,
        path: &mut ast::Path<'src, ast::NoGenericArgs>,
    ) -> Result<ast::PathTreeKind<'src>> {
        Ok(match self.token.kind {
            TokenKind::OpenCurlyBracket => {
                self.advance();
                ast::PathTreeKind::Branch(self.fin_parse_delim_seq(
                    TokenKind::CloseCurlyBracket,
                    TokenKind::Comma,
                    Self::parse_path_tree,
                )?)
            }
            TokenKind::SingleAsterisk => {
                self.advance();
                ast::PathTreeKind::Global
            }
            _ if let Some(ident) = self.as_path_seg_ident() => {
                self.advance();
                path.segs.push(ast::PathSeg::ident(ident));
                let binder = self
                    .consume_ident("as")
                    .then(|| self.parse_ident_where_common_or("_"))
                    .transpose()?;
                ast::PathTreeKind::Stump(binder)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.token,
                    // FIXME: Technically also DoubleColon under certain circumstances (e.g., `use;`).
                    one_of![
                        ExpectedFragment::PathSegIdent,
                        TokenKind::OpenCurlyBracket,
                        TokenKind::SingleAsterisk
                    ],
                ));
            }
        })
    }

    pub(super) fn parse_ident(&mut self, expected: &'static str) -> Result<()> {
        if self.as_ident(self.token) == Some(expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(self.token, one_of![ExpectedFragment::Raw(expected)]))
        }
    }

    pub(super) fn parse_ident_where_common_or(
        &mut self,
        exception: &'static str,
    ) -> Result<ast::Ident<'src>> {
        if let Some(ident) = self.as_ident(self.token)
            && (ident == exception || self.ident_is_common(ident))
        {
            self.advance();
            Ok(ident)
        } else {
            Err(ParseError::UnexpectedToken(
                self.token,
                one_of![ExpectedFragment::CommonIdent, ExpectedFragment::Raw(exception)],
            ))
        }
    }

    pub(super) fn consume_ident(&mut self, expected: &str) -> bool {
        if self.as_ident(self.token) == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(super) fn as_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        matches!(token.kind, TokenKind::Ident).then(|| self.source(token.span))
    }

    pub(super) fn parse_common_ident(&mut self) -> Result<ast::Ident<'src>> {
        self.consume_common_ident()
            .ok_or_else(|| ParseError::UnexpectedToken(self.token, ExpectedFragment::CommonIdent))
    }

    pub(super) fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        self.as_common_ident(self.token).inspect(|_| self.advance())
    }

    pub(super) fn as_common_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        self.as_ident(token).filter(|ident| self.ident_is_common(ident))
    }

    pub(super) fn ident_is_common(&self, ident: &str) -> bool {
        !is_reserved(ident, self.edition)
    }
}

pub(super) trait GenericArgsMode: ast::GenericArgsMode {
    fn parse<'src>(parser: &mut Parser<'_, 'src>) -> Result<Self::Args<'src>>;
}

impl GenericArgsMode for ast::NoGenericArgs {
    fn parse<'src>(_: &mut Parser<'_, 'src>) -> Result<Self::Args<'src>> {
        Ok(())
    }
}

impl GenericArgsMode for ast::UnambiguousGenericArgs {
    fn parse<'src>(parser: &mut Parser<'_, 'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(GenericArgsAmbiguity::No)
    }
}

impl GenericArgsMode for ast::ObligatorilyDisambiguatedGenericArgs {
    fn parse<'src>(parser: &mut Parser<'_, 'src>) -> Result<Self::Args<'src>> {
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
    arg: &mut ast::GenericArg<'src>,
) -> Option<(ast::Ident<'src>, Option<ast::GenericArgs<'src>>)> {
    if let ast::GenericArg::Ty(ty) = arg
        && let ast::Ty::Path(path) = ty
        && let ast::ExtPath { ext: None, path } = path
        && let ast::Path { segs: deref!([seg]) } = path
    {
        Some((seg.ident, seg.args.take()))
    } else {
        None
    }
}
