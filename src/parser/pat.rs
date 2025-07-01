use super::{ExpectedFragment, ParseError, Parser, Result, TokenKind};
use crate::{ast, parser::one_of};

impl<'src> Parser<'_, 'src> {
    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Pat ::=
    ///     | Wildcard_Pat
    ///     | Ident_Pat
    ///     | #Num_Lit
    ///     | #Str_Lit
    ///     | Borrow_Pat
    ///     | Paren_Or_Tup_Pat
    ///     | Ext_Path
    ///     | Macro_Call
    /// Wildcard_Pat ::= "_"
    /// Ident_Pat ::= "mut"? ("ref" "mut"?)? Common_Ident
    /// Borrow_Pat ::= "&" "mut"? Pat
    /// Paren_Or_Tup_Pat ::= "(" (Pat ("," | >")"))* ")"
    /// ```
    pub(super) fn parse_pat(&mut self) -> Result<ast::Pat<'src>> {
        match self.token.kind {
            TokenKind::Ident => match self.source(self.token.span) {
                "_" => {
                    self.advance();
                    return Ok(ast::Pat::Wildcard);
                }
                "mut" => {
                    self.advance();
                    return match self.as_ident(self.token) {
                        Some("ref") => {
                            self.advance();
                            self.fin_parse_by_ref_ident_pat(ast::Mutability::Mut)
                        }
                        Some(ident) if self.ident_is_common(ident) => {
                            self.advance();
                            Ok(ast::Pat::Ident(ast::IdentPat {
                                mut_: ast::Mutability::Mut,
                                by_ref: ast::ByRef::No,
                                ident,
                            }))
                        }
                        _ => Err(ParseError::UnexpectedToken(
                            self.token,
                            one_of![ExpectedFragment::Raw("ref"), ExpectedFragment::CommonIdent,],
                        )),
                    };
                }
                "ref" => {
                    self.advance();
                    return self.fin_parse_by_ref_ident_pat(ast::Mutability::Not);
                }
                _ => {}
            },
            TokenKind::NumLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::Pat::NumLit(lit));
            }
            TokenKind::StrLit => {
                let lit = self.source(self.token.span);
                self.advance();
                return Ok(ast::Pat::StrLit(lit));
            }
            // FIXME: Also DoubleAmpersand
            TokenKind::SingleAmpersand => {
                self.advance();
                let mut_ = self.parse_mutability();
                let pat = self.parse_pat()?;
                return Ok(ast::Pat::Borrow(mut_, Box::new(pat)));
            }
            TokenKind::OpenRoundBracket => {
                self.advance();
                return self.fin_parse_grouped_or_tuple(
                    Self::parse_pat,
                    ast::Pat::Grouped,
                    ast::Pat::Tup,
                );
            }
            _ => {}
        }

        if self.begins_ext_path() {
            let path = self.parse_ext_path::<ast::GenericArgsPolicy::DisambiguatedOnly>()?;

            match self.token.kind {
                TokenKind::SingleBang => {
                    let ast::ExtPath { self_ty: None, path } = path else {
                        return Err(ParseError::TyRelMacroCall);
                    };
                    let (bracket, stream) = self.parse_delimited_token_stream()?;
                    return Ok(ast::Pat::MacroCall(ast::MacroCall { path, bracket, stream }));
                }
                TokenKind::OpenRoundBracket => {
                    self.advance();
                    let fields = self.fin_parse_delim_seq(
                        TokenKind::CloseRoundBracket,
                        TokenKind::Comma,
                        |this| this.parse_pat(),
                    )?;
                    return Ok(ast::Pat::TupleStruct(Box::new(ast::TupleStructPat {
                        path,
                        fields,
                    })));
                }
                _ => {}
            }

            return Ok(match path {
                ast::ExtPath {
                    self_ty: None,
                    path: ast::Path { segs: deref!([ast::PathSeg { ident, args: None }]) },
                } => ast::Pat::Ident(ast::IdentPat {
                    by_ref: ast::ByRef::No,
                    mut_: ast::Mutability::Not,
                    ident,
                }),
                _ => ast::Pat::Path(Box::new(path)),
            });
        }

        Err(ParseError::UnexpectedToken(self.token, ExpectedFragment::Pat))
    }

    fn fin_parse_by_ref_ident_pat(&mut self, mut_: ast::Mutability) -> Result<ast::Pat<'src>> {
        let ref_mut = self.parse_mutability();
        let ident = self.parse_common_ident()?;
        Ok(ast::Pat::Ident(ast::IdentPat { by_ref: ast::ByRef::Yes(ref_mut), mut_, ident }))
    }
}
