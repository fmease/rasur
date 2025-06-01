use super::{
    DOUBLE_COLON, DoubleColon, ExpectedFragment, Ident, ParseError, Parser, Result, Shape as _,
    TokenKind, is_path_seg_keyword,
};
use crate::{
    ast,
    parser::{Glued, one_of},
};

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

        if self.consume(DOUBLE_COLON) {
            path.segs.push(ast::PathSeg::ident(""));
        }

        path.segs.push(self.parse_path_seg::<A>()?);

        while self.consume(DOUBLE_COLON) {
            path.segs.push(self.parse_path_seg::<A>()?);
        }

        Ok(path)
    }

    pub(super) fn begins_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        DOUBLE_COLON.check(self) || self.as_path_seg_ident().is_some()
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
        let disambiguated = if DOUBLE_COLON.check(self)
            && self.look_ahead(DoubleColon::LENGTH, |token| {
                matches!(token.kind, TokenKind::OpenAngleBracket | TokenKind::OpenRoundBracket)
            }) {
            DoubleColon::advance(self);
            true
        } else {
            false
        };

        if disambiguated || requires_disambiguation == RequiresDisambiguation::No {
            return Ok(match self.token().kind {
                TokenKind::OpenAngleBracket => {
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
        let mut args = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseAngleBracket;
        const SEPARATOR: TokenKind = TokenKind::Comma;
        while !self.consume(DELIMITER) {
            let mut arg = if self.begins_ty() {
                let ty = self.parse_ty()?;
                ast::GenericArg::Ty(ty)
            } else if let Some(lt) = self.consume_lifetime() {
                ast::GenericArg::Lifetime(lt)
            } else if self.begins_const_arg() {
                let expr = self.parse_expr()?;
                ast::GenericArg::Const(expr)
            } else {
                return Err(ParseError::UnexpectedToken(
                    self.token(),
                    one_of![ExpectedFragment::GenericArg, SEPARATOR, DELIMITER],
                ));
            };

            let token = self.token();
            let arg = if let TokenKind::Colon | TokenKind::Equals = token.kind
                && let Some((ident, args)) = extract_assoc_item_seg(&mut arg)
            {
                self.advance();

                let kind = match token.kind {
                    TokenKind::Colon => ast::AssocItemConstraintKind::Bound(self.parse_bounds()?),
                    TokenKind::Equals => ast::AssocItemConstraintKind::Equality(self.parse_term()?),
                    _ => unreachable!(),
                };

                ast::AngleGenericArg::Constraint(ast::AssocItemConstraint { ident, args, kind })
            } else {
                ast::AngleGenericArg::Argument(arg)
            };

            args.push(arg);

            // FIXME: Is there a better way to express this?
            if self.token().kind != DELIMITER {
                self.parse(SEPARATOR)?;
            }
        }

        Ok(ast::GenericArgs::Angle(args))
    }

    fn fin_parse_paren_generic_args(&mut self) -> Result<ast::GenericArgs<'src>> {
        if self.consume(Glued([TokenKind::Dot, TokenKind::Dot])) {
            self.parse(TokenKind::CloseRoundBracket)?;

            return Ok(ast::GenericArgs::ParenElided);
        }

        let mut inputs = Vec::new();

        const DELIMITER: TokenKind = TokenKind::CloseRoundBracket;
        const SEPARATOR: TokenKind = TokenKind::Comma;
        while !self.consume(DELIMITER) {
            inputs.push(self.parse_ty()?);

            // FIXME: Is there a better way to express this?
            if self.token().kind != DELIMITER {
                self.parse(SEPARATOR)?;
            }
        }

        let output = if self.consume(TokenKind::ThinArrow) { Some(self.parse_ty()?) } else { None };

        Ok(ast::GenericArgs::Paren { inputs, output })
    }

    fn parse_term(&mut self) -> Result<ast::Term<'src>> {
        if self.begins_ty() {
            Ok(ast::Term::Ty(self.parse_ty()?))
        } else if self.begins_const_arg() {
            Ok(ast::Term::Const(self.parse_expr()?))
        } else {
            Err(ParseError::UnexpectedToken(self.token(), ExpectedFragment::Term))
        }
    }

    fn begins_const_arg(&self) -> bool {
        let token = self.token();

        // FIXME: Leading dash (unary minus)
        match token.kind {
            TokenKind::OpenCurlyBracket | TokenKind::StrLit | TokenKind::NumLit => true,
            TokenKind::Ident => matches!(self.source(token.span), "false" | "true"),
            _ => false,
        }
    }

    pub(super) fn parse_path_tree(&mut self) -> Result<ast::PathTree<'src>> {
        let mut path = ast::Path { segs: Vec::new() };

        if self.consume(DOUBLE_COLON) {
            path.segs.push(ast::PathSeg::ident(""));
        }

        match self.parse_path_tree_kind(&mut path)? {
            ast::PathTreeKind::Stump(None) => {}
            kind => return Ok(ast::PathTree { path, kind }),
        }

        while self.consume(DOUBLE_COLON) {
            match self.parse_path_tree_kind(&mut path)? {
                ast::PathTreeKind::Stump(None) => {}
                kind => return Ok(ast::PathTree { path, kind }),
            };
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
                let mut trees = Vec::new();

                const DELIMITER: TokenKind = TokenKind::CloseCurlyBracket;
                const SEPARATOR: TokenKind = TokenKind::Comma;
                while !self.consume(DELIMITER) {
                    trees.push(self.parse_path_tree()?);

                    if self.token().kind != DELIMITER {
                        self.parse(SEPARATOR)?;
                    }
                }

                ast::PathTreeKind::Branch(trees)
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

#[derive(PartialEq, Eq)]
enum RequiresDisambiguation {
    Yes,
    No,
}

fn extract_assoc_item_seg<'src>(
    arg: &mut ast::GenericArg<'src>,
) -> Option<(ast::Ident<'src>, Option<ast::GenericArgs<'src>>)> {
    if let ast::GenericArg::Ty(ty) = arg
        && let ast::Ty::Path(ast::Path { segs: deref!([seg]) }) = ty
    {
        Some((seg.ident, seg.args.take()))
    } else {
        None
    }
}
