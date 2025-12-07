use super::{
    ExpectedFragment, LessThan, Parser, PathSegIdent, Result, Token, TokenKind, TokenPrefix,
    error::ParseError, one_of,
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

        self.parse_path_where::<M>(PathMode::Normal)
    }

    pub(super) fn begins_path(&self, token: Token) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        matches!(token.kind, TokenKind::DoubleColon | PathSegIdent!())
    }

    fn parse_path_where<M: GenericArgsMode>(
        &mut self,
        mode: PathMode,
    ) -> Result<ast::Path<'src, M>> {
        // NOTE: To be kept in sync with `Self::begins_path`.

        let mut path = ast::Path { segs: Vec::new() };

        match mode {
            PathMode::Normal => {
                if self.consume(TokenKind::DoubleColon) {
                    path.segs.push(ast::PathSeg::ident(""))
                }
            }
            PathMode::Suffix => self.parse(TokenKind::DoubleColon)?,
        }

        path.segs.push(self.parse_path_seg::<M>()?);

        while self.consume(TokenKind::DoubleColon) {
            path.segs.push(self.parse_path_seg::<M>()?);
        }

        Ok(path)
    }

    /// Parse an extended path.
    pub(super) fn parse_ext_path<S: GenericArgsStyle>(&mut self) -> Result<ast::ExtPath<'src, S>> {
        // NOTE: To be kept in sync with `Self::begins_ext_path`.

        // FIXME: Add `<` to list of expected tokens
        let (ext, mode) = if self.consume(TokenPrefix::LessThan) {
            let self_ty = self.parse_ty()?;
            // We're in a "type context" now and can parse generic args unambiguously.
            let trait_ref = self
                .consume(TokenKind::As)
                .then(|| self.parse_path::<ast::UnambiguousGenericArgs>())
                .transpose()?;
            self.parse(TokenKind::SingleGreaterThan)?; // no need to account for DoubleGreaterThan

            (Some(ast::PathExt { self_ty, trait_ref }), PathMode::Suffix)
        } else {
            (None, PathMode::Normal)
        };

        let path = self.parse_path_where(mode)?;

        Ok(ast::ExtPath { ext, path })
    }

    pub(super) fn begins_ext_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_ext_path`.

        matches!(self.token.kind, LessThan!()) || self.begins_path(self.token)
    }

    fn parse_path_seg<M: GenericArgsMode>(&mut self) -> Result<ast::PathSeg<'src, M>> {
        match self.token.kind {
            PathSegIdent!() => {
                let ident = self.source(self.token.span);
                self.advance();
                Ok(ast::PathSeg { ident, args: M::parse(self)? })
            }
            _ => Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::PathSegIdent)),
        }
    }

    fn parse_generic_args(
        &mut self,
        ambiguity: GenericArgsAmbiguity,
    ) -> Result<Option<ast::GenericArgs<'src>>> {
        // FIXME: Use TokenCategory/TokenPrefix API
        let disambiguated = if self.token.kind == TokenKind::DoubleColon
            && self.look_ahead(1, |token| {
                matches!(token.kind, LessThan!() | TokenKind::OpenRoundBracket)
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
            |this| this.consume(TokenPrefix::GreaterThan),
            |this| TokenPrefix::GreaterThan.matches(this.token.kind),
            SEPARATOR,
            |this: &mut Self| {
                let mut arg = if this.begins_ty() {
                    let ty = this.parse_ty()?;
                    ast::GenericArg::Ty(ty)
                } else if let Some(lt) = this.parse_lifetime()? {
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

    // FIXME: Move into mod expr or a new expr::const_arg
    pub(crate) fn parse_const_arg(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_const_arg`.

        // FIXME: Leading dash (unary minus)
        match self.token.kind {
            TokenKind::CharLit => {
                let lit = self.source(self.token.span);
                self.advance();
                // FIXME: Validate the char lit.
                return Ok(ast::ExprKind::Lit(ast::Lit::Char(lit)).into());
            }
            TokenKind::False => {
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Bool(false)).into());
            }
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Num(lit)).into());
            }
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return Ok(
                    ast::ExprKind::Block(None, Box::new(self.fin_parse_block_expr()?)).into()
                );
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Str(lit)).into());
            }
            TokenKind::True => {
                self.advance();
                return Ok(ast::ExprKind::Lit(ast::Lit::Bool(true)).into());
            }
            _ => {}
        }

        // FIXME: Proper fragment
        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Expr))
    }

    fn begins_const_arg(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_const_arg`.

        // FIXME: Leading dash (unary minus)
        match self.token.kind {
            | TokenKind::CharLit
            | TokenKind::False
            | TokenKind::NumLit
            | TokenKind::OpenCurlyBracket
            | TokenKind::StrLit
            | TokenKind::True => true,
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
            PathSegIdent!() => {
                path.segs.push(ast::PathSeg::ident(self.source(self.token.span)));
                self.advance();
                let binder = if self.consume(TokenKind::As) {
                    let (binder, _) = self.parse_common_ident_or(TokenKind::Underscore)?;
                    Some(binder)
                } else {
                    None
                };
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
}

enum PathMode {
    Normal,
    Suffix,
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
