use super::{
    DOUBLE_COLON, DoubleColon, ExpectedFragment, ParseError, Parser, Result, Shape as _, TokenKind,
    is_path_seg_keyword,
};
use crate::{ast, parser::one_of};

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

        let hook = match self.consume(DOUBLE_COLON) {
            true => ast::PathHook::Global,
            false => ast::PathHook::Local,
        };

        let mut segs = Vec::new();
        segs.push(self.parse_path_seg::<A>()?);
        while self.consume(DOUBLE_COLON) {
            segs.push(self.parse_path_seg::<A>()?);
        }

        Ok(ast::Path { hook, segs })
    }

    pub(super) fn begins_path(&self) -> bool {
        // NOTE: To be kept in sync with `Self::parse_path`.

        DOUBLE_COLON.check(self) || self.is_path_seg_ident().is_some()
    }

    fn parse_path_seg<A: ParseGenericArgs>(&mut self) -> Result<ast::PathSeg<'src, A>> {
        let ident = self.is_path_seg_ident().inspect(|_| self.advance()).ok_or_else(|| {
            ParseError::UnexpectedToken(self.token(), ExpectedFragment::PathSegIdent)
        })?;
        let args = A::parse(self)?;
        Ok(ast::PathSeg { ident, args })
    }

    fn is_path_seg_ident(&self) -> Option<ast::Ident<'src>> {
        self.as_ident(self.token())
            .filter(|ident| is_path_seg_keyword(ident) || self.ident_is_common(ident))
    }

    fn parse_generic_args(
        &mut self,
        requires_disambiguation: bool,
    ) -> Result<Option<ast::GenericArgs<'src>>> {
        // FIXME: Support parenthesized args

        let disambiguated = if DOUBLE_COLON.check(self)
            && self.look_ahead(DoubleColon::LENGTH, |token| {
                matches!(token.kind, TokenKind::OpenAngleBracket | TokenKind::OpenRoundBracket)
            }) {
            DoubleColon::advance(self);
            true
        } else {
            false
        };

        if disambiguated || !requires_disambiguation {
            let token = self.token();
            match token.kind {
                TokenKind::OpenAngleBracket => {
                    self.advance();

                    let mut args = Vec::new();

                    const DELIMITER: TokenKind = TokenKind::CloseAngleBracket;
                    const SEPARATOR: TokenKind = TokenKind::Comma;
                    while !self.consume(DELIMITER) {
                        // FIXME: Parse const args
                        // FIXME: Parse assoc item constraints
                        args.push(if self.begins_ty() {
                            let ty = self.parse_ty()?;
                            ast::AngleGenericArg::Ty(ty)
                        } else if let Some(lt) = self.consume_lifetime() {
                            ast::AngleGenericArg::Lifetime(lt)
                        } else if self.begins_const_arg() {
                            let expr = self.parse_expr()?;
                            ast::AngleGenericArg::Const(expr)
                        } else {
                            return Err(ParseError::UnexpectedToken(
                                self.token(),
                                one_of![ExpectedFragment::GenericArg, SEPARATOR, DELIMITER],
                            ));
                        });

                        // FIXME: Is there a better way to express this?
                        if self.token().kind != DELIMITER {
                            self.parse(SEPARATOR)?;
                        }
                    }

                    return Ok(Some(ast::GenericArgs::Angle(args)));
                }
                // FIXME: Support RTN
                TokenKind::OpenRoundBracket => {
                    self.advance();

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

                    // FIXME: And don't parse it if it isn't RTN
                    let output = if self.consume(TokenKind::ThinArrow) {
                        Some(self.parse_ty()?)
                    } else {
                        None
                    };

                    return Ok(Some(ast::GenericArgs::Paren { inputs, output }));
                }
                _ => {}
            }
        }

        Ok(None)
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
}

pub(super) trait ParseGenericArgs: ast::GenericArgsPolicy::Kind {
    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Args<'src>>;
}

impl ParseGenericArgs for ast::GenericArgsPolicy::Disallowed {
    fn parse<'src>(_: &mut Parser<'src>) -> Result<Self::Args<'src>> {
        Ok(())
    }
}

impl ParseGenericArgs for ast::GenericArgsPolicy::Allowed {
    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(false)
    }
}

impl ParseGenericArgs for ast::GenericArgsPolicy::DisambiguatedOnly {
    fn parse<'src>(parser: &mut Parser<'src>) -> Result<Self::Args<'src>> {
        parser.parse_generic_args(true)
    }
}
