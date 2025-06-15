use super::{
    ExpectedFragment, Ident, ParseError, Parser, Result, Token, TokenKind, is_path_seg_keyword,
    is_reserved, one_of,
};
use crate::ast;

impl<'src> Parser<'src> {
    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= "::"? Path_Seg_Ident ("::" Path_Seg_Ident)*
    /// ```
    pub(super) fn parse_path<A: ParseGenericArgs>(&mut self) -> Result<ast::Path<'src, A>> {
        // NOTE: To be kept in sync with `Self::begins_path`.

        let mut path = ast::Path { segs: Vec::new() };

        if self.consume(TokenKind::DoubleColon) {
            path.segs.push(ast::PathSeg::ident(""));
        }

        path.segs.push(self.parse_path_seg::<A>()?);

        while self.consume(TokenKind::DoubleColon) {
            path.segs.push(self.parse_path_seg::<A>()?);
        }

        Ok(path)
    }

    pub(super) fn begins_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        self.token().kind == TokenKind::DoubleColon || self.as_path_seg_ident().is_some()
    }

    pub(super) fn begins_ext_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_ty_rel_path`.

        // FIXME: Or DoubleLessThan
        self.token().kind == TokenKind::SingleLessThan || self.begins_path()
    }

    pub(super) fn parse_ext_path<A: ParseGenericArgs>(&mut self) -> Result<ast::ExtPath<'src, A>> {
        let mut path = ast::Path { segs: Vec::new() };

        // FIXME: Deal with DoubleLessThan, too (`<<T>::P>::P`).
        let self_ty = if self.consume(TokenKind::SingleLessThan) {
            let ty = self.parse_ty()?;
            if self.consume(Ident("as")) {
                path = self.parse_path::<A>()?;
            }
            self.parse(TokenKind::SingleGreaterThan)?;
            self.parse(TokenKind::DoubleColon)?;
            Some(ast::SelfTy { ty, offset: path.segs.len() })
        } else {
            None
        };

        // FIXME: Add `<`` to list of expected tokens

        path.segs.push(self.parse_path_seg::<A>()?);

        while self.consume(TokenKind::DoubleColon) {
            path.segs.push(self.parse_path_seg::<A>()?);
        }

        Ok(ast::ExtPath { self_ty, path })
    }

    fn parse_path_seg<A: ParseGenericArgs>(&mut self) -> Result<ast::PathSeg<'src, A>> {
        let ident = self.as_path_seg_ident().inspect(|_| self.advance()).ok_or_else(|| {
            ParseError::UnexpectedToken(self.token(), ExpectedFragment::PathSegIdent)
        })?;
        let args = A::parse(self)?;
        Ok(ast::PathSeg { ident, args })
    }

    pub(super) fn as_path_seg_ident(&self) -> Option<ast::Ident<'src>> {
        self.as_ident(self.token())
            .filter(|ident| is_path_seg_keyword(ident) || self.ident_is_common(ident))
    }

    fn parse_generic_args(
        &mut self,
        requires_disambiguation: RequiresDisambiguation,
    ) -> Result<Option<ast::GenericArgs<'src>>> {
        let disambiguated = if self.token().kind == TokenKind::DoubleColon
            && self.look_ahead(1, |token| {
                matches!(token.kind, TokenKind::SingleLessThan | TokenKind::OpenRoundBracket)
            }) {
            self.advance();
            true
        } else {
            false
        };

        if disambiguated || requires_disambiguation == RequiresDisambiguation::No {
            return Ok(match self.token().kind {
                TokenKind::SingleLessThan => {
                    self.advance();
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
        const DELIMITER: TokenKind = TokenKind::SingleGreaterThan;
        const SEPARATOR: TokenKind = TokenKind::Comma;
        self.parse_delimited_sequence(DELIMITER, SEPARATOR, |this| {
            let mut arg = if this.begins_ty() {
                let ty = this.parse_ty()?;
                ast::GenericArg::Ty(ty)
            } else if let Some(lt) = this.consume_lifetime()? {
                ast::GenericArg::Lifetime(lt)
            } else if this.begins_const_arg() {
                let expr = this.parse_const_arg()?;
                ast::GenericArg::Const(expr)
            } else {
                return Err(ParseError::UnexpectedToken(
                    this.token(),
                    one_of![ExpectedFragment::GenericArg, SEPARATOR, DELIMITER],
                ));
            };

            let token = this.token();
            let arg = if let TokenKind::SingleColon | TokenKind::SingleEquals = token.kind
                && let Some((ident, args)) = extract_assoc_item_seg(&mut arg)
            {
                this.advance();

                let kind = match token.kind {
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
        })
        .map(ast::GenericArgs::Angle)
    }

    fn fin_parse_paren_generic_args(&mut self) -> Result<ast::GenericArgs<'src>> {
        if self.consume(TokenKind::DoubleDot) {
            self.parse(TokenKind::CloseRoundBracket)?;

            return Ok(ast::GenericArgs::ParenElided);
        }

        let inputs = self.parse_delimited_sequence(
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
            Err(ParseError::UnexpectedToken(self.token(), ExpectedFragment::Term))
        }
    }

    fn parse_const_arg(&mut self) -> Result<ast::Expr<'src>> {
        // NOTE: To be kept in sync with `Self::begins_const_arg`.

        // FIXME: Leading dash (unary minus)
        let token = self.token();
        match token.kind {
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
            TokenKind::OpenCurlyBracket => {
                self.advance();
                return Ok(ast::Expr::Block(Box::new(self.fin_parse_block_expr()?)));
            }
            TokenKind::Ident => match self.source(token.span) {
                "false" => {
                    self.advance();
                    return Ok(ast::Expr::BoolLit(false));
                }
                "true" => {
                    self.advance();
                    return Ok(ast::Expr::BoolLit(true));
                }
                _ => {}
            },
            _ => {}
        }

        // FIXME: Proper fragment
        Err(ParseError::UnexpectedToken(token, ExpectedFragment::Expr))
    }

    fn begins_const_arg(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_const_arg`.

        // FIXME: Leading dash (unary minus)
        let token = self.token();
        match token.kind {
            TokenKind::OpenCurlyBracket | TokenKind::StrLit | TokenKind::NumLit => true,
            TokenKind::Ident => matches!(self.source(token.span), "false" | "true"),
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
        path: &mut ast::Path<'src, ast::GenericArgsPolicy::Forbidden>,
    ) -> Result<ast::PathTreeKind<'src>> {
        let token = self.token();
        Ok(match token.kind {
            TokenKind::OpenCurlyBracket => {
                self.advance();
                ast::PathTreeKind::Branch(self.parse_delimited_sequence(
                    TokenKind::CloseCurlyBracket,
                    TokenKind::Comma,
                    Self::parse_path_tree,
                )?)
            }
            TokenKind::Asterisk => {
                self.advance();
                ast::PathTreeKind::Global
            }
            _ if let Some(ident) = self.as_path_seg_ident() => {
                self.advance();
                path.segs.push(ast::PathSeg::ident(ident));
                let binder =
                    self.consume(Ident("as")).then(|| self.parse_common_ident()).transpose()?;
                ast::PathTreeKind::Stump(binder)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    // FIXME: Technically also DoubleColon under certain circumstances (e.g., `use;`).
                    one_of![
                        ExpectedFragment::PathSegIdent,
                        TokenKind::OpenCurlyBracket,
                        TokenKind::Asterisk
                    ],
                ));
            }
        })
    }

    pub(super) fn parse_ident_if_common_or(
        &mut self,
        exception: &'static str,
    ) -> Result<ast::Ident<'src>> {
        let token = self.token();
        self.as_ident(token)
            .filter(|&ident| ident == exception || self.ident_is_common(ident))
            .inspect(|_| self.advance())
            .ok_or_else(|| {
                ParseError::UnexpectedToken(
                    token,
                    one_of![ExpectedFragment::CommonIdent, ExpectedFragment::Raw(exception)],
                )
            })
    }

    pub(super) fn parse_common_ident(&mut self) -> Result<ast::Ident<'src>> {
        self.consume_common_ident()
            .ok_or_else(|| ParseError::UnexpectedToken(self.token(), ExpectedFragment::CommonIdent))
    }

    pub(super) fn consume_common_ident(&mut self) -> Option<ast::Ident<'src>> {
        self.as_common_ident(self.token()).inspect(|_| self.advance())
    }

    pub(super) fn as_common_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        self.as_ident(token).filter(|ident| self.ident_is_common(ident))
    }

    pub(super) fn ident_is_common(&self, ident: &str) -> bool {
        !is_reserved(ident, self.edition)
    }

    pub(super) fn as_ident(&self, token: Token) -> Option<ast::Ident<'src>> {
        matches!(token.kind, TokenKind::Ident).then(|| self.source(token.span))
    }
}

pub(super) trait ParseGenericArgs: ast::GenericArgsPolicy::Kind {
    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Args<'src>>;
}

impl ParseGenericArgs for ast::GenericArgsPolicy::Forbidden {
    fn parse<'src>(_: &mut Parser<'src>) -> Result<Self::Args<'src>> {
        Ok(())
    }
}

impl ParseGenericArgs for ast::GenericArgsPolicy::Allowed {
    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(RequiresDisambiguation::No)
    }
}

impl ParseGenericArgs for ast::GenericArgsPolicy::DisambiguatedOnly {
    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(RequiresDisambiguation::Yes)
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum RequiresDisambiguation {
    Yes,
    No,
}

fn extract_assoc_item_seg<'src>(
    arg: &mut ast::GenericArg<'src>,
) -> Option<(ast::Ident<'src>, Option<ast::GenericArgs<'src>>)> {
    if let ast::GenericArg::Ty(ty) = arg
        && let ast::Ty::Path(path) = ty
        && let ast::ExtPath { self_ty: None, path } = path
        && let ast::Path { segs: deref!([seg]) } = path
    {
        Some((seg.ident, seg.args.take()))
    } else {
        None
    }
}
