use std::fmt;

use crate::ast;
use crate::edition::Edition;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

pub(crate) type Result<T, E = ParseError> = std::result::Result<T, E>;

pub(crate) fn parse(tokens: Vec<Token>, source: &str, edition: Edition) -> Result<ast::File<'_>> {
    Parser { tokens, index: 0, source, edition }.parse_file()
}

struct Parser<'src> {
    tokens: Vec<Token>,
    index: usize,
    source: &'src str,
    edition: Edition,
}

impl<'src> Parser<'src> {
    /// Parse a source file.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// File ::= Attrs⟨Inner⟩ Items⟨#End_Of_Input⟩
    /// ```
    fn parse_file(&mut self) -> Result<ast::File<'src>> {
        let attrs = self.parse_attrs(AttrStyle::Inner)?;
        let items = self.parse_items(TokenKind::EndOfInput)?;

        Ok(ast::File { attrs, items })
    }

    /// Parse a sequence of items.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Items⟨terminator⟩ ::= Item* ⟨terminator⟩
    /// Item ::= Attrs⟨Outer⟩ Visibility (Enum | Fn | Mod | Struct | Trait)
    /// Visibility ::= "pub"?
    /// ```
    fn parse_items(&mut self, terminator: TokenKind) -> Result<Vec<ast::Item<'src>>> {
        let mut items = Vec::new();

        loop {
            if self.consume(terminator) {
                break;
            }

            let attrs = self.parse_attrs(AttrStyle::Outer)?;

            // FIXME: Not all item-likes support `pub` (think about mac calls, impls?, mac defs?, …).
            let vis = match self.consume(Keyword("pub")) {
                true => ast::Visibility::Public,
                false => ast::Visibility::Inherited,
            };

            let token = self.peek();
            let kind = match token.kind {
                TokenKind::Ident => match self.source(token.span) {
                    "enum" => {
                        self.advance();
                        self.fin_parse_enum()?
                    }
                    "fn" => {
                        self.advance();
                        self.fin_parse_fn()?
                    }
                    "mod" => {
                        self.advance();
                        self.fin_parse_mod()?
                    }
                    "struct" => {
                        self.advance();
                        self.fin_parse_struct()?
                    }
                    "trait" => {
                        self.advance();
                        self.fin_parse_trait()?
                    }
                    _ => return Err(ParseError::UnexpectedToken(token, ExpectedFragment::Item)),
                },
                _ => return Err(ParseError::UnexpectedToken(token, ExpectedFragment::Item)),
            };

            items.push(ast::Item { attrs, vis, kind });
        }

        Ok(items)
    }

    /// Parse a sequence of attributes of the given style.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attrs⟨style⟩ ::= ("#" Bang⟨style⟩ "[" Attr_Path … "]" )*
    /// Bang⟨Outer⟩ ::= ""
    /// Bang⟨Inner⟩ ::= "!"
    /// ```
    fn parse_attrs(&mut self, style: AttrStyle) -> Result<Vec<ast::Attr<'src>>> {
        let mut attrs = Vec::new();

        while self.peek().kind == TokenKind::Hash {
            match style {
                AttrStyle::Outer => self.advance(),
                // We don't expect(Bang) here because the caller may want to
                // parse outer attributes next.
                AttrStyle::Inner => {
                    if self.look_ahead(1, |token| token.kind == TokenKind::Bang) {
                        self.advance();
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            attrs.push(self.fin_parse_attr()?);
        }

        Ok(attrs)
    }

    fn fin_parse_attr(&mut self) -> Result<ast::Attr<'src>> {
        self.parse(TokenKind::OpenSquareBracket)?;
        let path = self.parse_attr_path()?;
        let token = self.peek();
        let kind = match token.kind {
            TokenKind::CloseSquareBracket => ast::AttrKind::Unit,
            // FIXME: Admits `==`.
            TokenKind::Equals => {
                self.advance();
                let expr = self.parse_expr()?;
                ast::AttrKind::Assign(expr)
            }
            TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::OpenCurlyBracket => {
                self.advance();
                let bracket = match token.kind {
                    TokenKind::OpenRoundBracket => ast::Bracket::Round,
                    TokenKind::OpenSquareBracket => ast::Bracket::Square,
                    TokenKind::OpenCurlyBracket => ast::Bracket::Curly,
                    _ => unreachable!(),
                };
                let stream = self.parse_token_strean(bracket)?;
                self.parse(match bracket {
                    ast::Bracket::Round => TokenKind::CloseRoundBracket,
                    ast::Bracket::Square => TokenKind::CloseSquareBracket,
                    ast::Bracket::Curly => TokenKind::CloseCurlyBracket,
                })?;
                ast::AttrKind::Call(bracket, stream)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    ExpectedFragment::OneOf(Box::new([
                        TokenKind::CloseSquareBracket,
                        TokenKind::Equals,
                        TokenKind::OpenRoundBracket,
                        TokenKind::OpenSquareBracket,
                        TokenKind::OpenCurlyBracket,
                    ])),
                ));
            }
        };

        self.parse(TokenKind::CloseSquareBracket)?;

        Ok(ast::Attr { path, kind })
    }

    /// Parse an attribute path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= "::"? Path_Seg_Ident ("::" Path_Seg_Ident)*
    /// ```
    fn parse_attr_path(&mut self) -> Result<ast::Path<'src>> {
        // FIXME: Or should we move most "glued" token detection into the lexer?
        //        We can't move everything though.
        const DOUBLE_COLON: Glued<2, TokenKind> = Glued([TokenKind::Colon, TokenKind::Colon]);

        let locality = match self.consume(DOUBLE_COLON) {
            true => ast::PathLocality::Global,
            false => ast::PathLocality::Local,
        };

        let mut segs = Vec::new();

        segs.push(self.parse_path_seg_ident()?);

        while self.consume(DOUBLE_COLON) {
            segs.push(self.parse_path_seg_ident()?);
        }

        Ok(ast::Path { locality, segs })
    }

    /// Finish parsing an enum item assuming the leading `enum` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Enum ::= "enum" Common_Ident Generic_Params "{" "}"
    /// ```
    fn fin_parse_enum(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        self.parse(TokenKind::CloseCurlyBracket)?;

        Ok(ast::ItemKind::Enum(ast::Enum { name, generics: ast::Generics { params } }))
    }

    /// Finish parsing a function item assuming the leading `fn` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Fn ::=
    ///     "fn" Common_Ident
    ///     Generic_Params Params
    ///     ("->" Ty)?
    ///     (Block_Expr | ";")
    /// ```
    fn fin_parse_fn(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let gen_params = self.parse_generic_params()?;
        let params = self.parse_params()?;
        let ret_ty = self.consume(TokenKind::ThinArrow).then(|| self.parse_ty()).transpose()?;
        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            Some(self.fin_parse_block_expr()?)
        } else {
            self.parse(TokenKind::Semicolon)?;
            None
        };
        Ok(ast::ItemKind::Fn(ast::Fn {
            name,
            generics: ast::Generics { params: gen_params },
            params,
            ret_ty,
            body,
        }))
    }

    fn fin_parse_mod(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let items = if self.consume(TokenKind::OpenCurlyBracket) {
            Some(self.parse_items(TokenKind::CloseCurlyBracket)?)
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.parse(TokenKind::Semicolon)?;
            None
        };
        Ok(ast::ItemKind::Mod(ast::Mod { name, items }))
    }

    /// Finish parsing a struct item assuming the leading `struct` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Struct ::=
    ///     "struct" Common_Ident
    ///     Generic_Params
    ///     ("{" (Common_Ident ":" Ty ("," | >"}"))* "}" | ";")
    /// ```
    fn fin_parse_struct(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let gen_params = self.parse_generic_params()?;
        let body = if self.consume(TokenKind::OpenCurlyBracket) {
            let mut fields = Vec::new();

            loop {
                if self.consume(TokenKind::CloseCurlyBracket) {
                    break;
                }

                let ident = self.parse_common_ident()?;
                self.parse(TokenKind::Colon)?;
                let ty = self.parse_ty()?;

                // FIXME: Can we express that nicer?
                if self.peek().kind != TokenKind::CloseCurlyBracket {
                    self.parse(TokenKind::Comma)?;
                }

                fields.push((ident, ty))
            }
            ast::StructBody::Normal { fields }
        } else {
            // FIXME: Should this really be inside parse_fn or rather inside parse_item?
            self.parse(TokenKind::Semicolon)?;
            ast::StructBody::Unit
        };
        Ok(ast::ItemKind::Struct(ast::Struct {
            name,
            generics: ast::Generics { params: gen_params },
            body,
        }))
    }

    /// Finish parsing a trait item assuming the leading `trait` has been parsed already.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Trait ::= "trait" Common_Ident Generic_Params "{" "}"
    /// ```
    fn fin_parse_trait(&mut self) -> Result<ast::ItemKind<'src>> {
        let name = self.parse_common_ident()?;
        let params = self.parse_generic_params()?;

        self.parse(TokenKind::OpenCurlyBracket)?;
        self.parse(TokenKind::CloseCurlyBracket)?;

        Ok(ast::ItemKind::Trait(ast::Trait { name, generics: ast::Generics { params } }))
    }

    fn parse_common_ident(&mut self) -> Result<&'src str> {
        let token = self.peek();

        if let TokenKind::Ident = token.kind
            && let ident = self.source(token.span)
            && !is_reserved(ident, self.edition)
        {
            self.advance();
            return Ok(ident);
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::CommonIdent))
    }

    fn parse_path_seg_ident(&mut self) -> Result<&'src str> {
        let token = self.peek();

        if let TokenKind::Ident = token.kind
            && let ident = self.source(token.span)
            && (is_path_seg_keyword(ident) || !is_reserved(ident, self.edition))
        {
            self.advance();
            return Ok(ident);
        }

        Err(ParseError::UnexpectedToken(token, ExpectedFragment::PathSegIdent))
    }

    fn parse_generic_params(&mut self) -> Result<Vec<ast::GenParam<'src>>> {
        let mut params = Vec::new();

        if self.consume(TokenKind::OpenAngleBracket) {
            loop {
                if self.consume(TokenKind::CloseAngleBracket) {
                    break;
                }

                let ident = self.parse_common_ident()?;

                self.consume(TokenKind::Comma);

                params.push(ast::GenParam { name: ident })
            }
        }

        Ok(params)
    }

    fn parse_params(&mut self) -> Result<Vec<ast::Param<'src>>> {
        let mut params = Vec::new();

        if self.consume(TokenKind::OpenRoundBracket) {
            loop {
                if self.consume(TokenKind::CloseRoundBracket) {
                    break;
                }

                let ident = self.parse_common_ident()?;
                let ty = self.parse_opt_ty_ann()?;
                self.consume(TokenKind::Comma);

                params.push(ast::Param { name: ident, ty })
            }
        }

        Ok(params)
    }

    fn parse_ty(&mut self) -> Result<ast::Ty<'src>> {
        let token = self.peek();
        match token.kind {
            TokenKind::Ident
                if let ident = self.source(token.span)
                    && !is_reserved(ident, self.edition) =>
            {
                self.advance();
                Ok(ast::Ty::Ident(ident))
            }
            _ => return Err(ParseError::UnexpectedToken(token, ExpectedFragment::Ty)),
        }
    }

    fn parse_opt_ty_ann(&mut self) -> Result<Option<ast::Ty<'src>>> {
        if self.consume(TokenKind::Colon) { self.parse_ty().map(Some) } else { Ok(None) }
    }

    /// Finish parsing a block expression assuming the leading `{` has already been parsed.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Block_Expr ::= "{" Expr* "}"
    /// ```
    fn fin_parse_block_expr(&mut self) -> Result<ast::Expr<'src>> {
        // FIXME: Inner attributes.
        // FIXME: Statements.

        let expr = if self.consume(TokenKind::CloseCurlyBracket) {
            None
        } else {
            let expr = self.parse_expr()?;
            self.parse(TokenKind::CloseCurlyBracket)?;
            Some(expr)
        };

        Ok(ast::Expr::Block(Box::new(ast::BlockExpr { expr })))
    }

    /// Parse an expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Expr ::=
    ///     | #Num_Lit
    ///     | #Str_Lit
    ///     | Common_Ident
    /// ```
    fn parse_expr(&mut self) -> Result<ast::Expr<'src>> {
        let token = self.peek();

        match token.kind {
            TokenKind::NumLit => {
                let lit = self.source(token.span);
                self.advance();
                Ok(ast::Expr::NumLit(lit))
            }
            TokenKind::StrLit => {
                let lit = self.source(token.span);
                self.advance();
                Ok(ast::Expr::StrLit(lit))
            }
            TokenKind::Ident
                if let ident = self.source(token.span)
                    && !is_reserved(ident, self.edition) =>
            {
                self.advance();
                Ok(ast::Expr::Ident(ident))
            }
            _ => return Err(ParseError::UnexpectedToken(token, ExpectedFragment::Expr)),
        }
    }

    fn parse_token_strean(&mut self, exp_delim: ast::Bracket) -> Result<ast::TokenStream> {
        let mut tokens = Vec::new();
        let mut stack = Vec::new();
        let mut is_delimited = false;

        enum Orientation {
            Open,
            Close,
        }

        loop {
            let token = self.peek();

            let act_delim = match token.kind {
                TokenKind::OpenRoundBracket => Some((ast::Bracket::Round, Orientation::Open)),
                TokenKind::OpenSquareBracket => Some((ast::Bracket::Square, Orientation::Open)),
                TokenKind::OpenCurlyBracket => Some((ast::Bracket::Curly, Orientation::Open)),
                TokenKind::CloseRoundBracket => Some((ast::Bracket::Round, Orientation::Close)),
                TokenKind::CloseSquareBracket => Some((ast::Bracket::Square, Orientation::Close)),
                TokenKind::CloseCurlyBracket => Some((ast::Bracket::Curly, Orientation::Close)),
                TokenKind::EndOfInput => break,
                _ => None,
            };

            if let Some((act_delim, orient)) = act_delim {
                if stack.is_empty()
                    && act_delim == exp_delim
                    && let Orientation::Close = orient
                {
                    is_delimited = true;
                    break;
                }

                match orient {
                    Orientation::Open => stack.push(act_delim),
                    Orientation::Close => {
                        let close_delim = act_delim;
                        match stack.pop() {
                            Some(open_delim) if open_delim == close_delim => {}
                            // FIXME: Better error.
                            _ => return Err(ParseError::InvalidDelimiter),
                        }
                    }
                }
            }

            tokens.push(token.kind);
            self.advance();
        }

        if is_delimited && stack.is_empty() {
            Ok(tokens)
        } else {
            // FIXME: Better error.
            Err(ParseError::InvalidDelimiter)
        }
    }

    fn consume<S: Shape>(&mut self, shape: S) -> bool {
        if shape.check(self) {
            S::advance(self);
            true
        } else {
            false
        }
    }

    // FIXME: Take S: Shape
    fn parse(&mut self, kind: TokenKind) -> Result<()> {
        let token = self.peek();
        if token.kind == kind {
            self.advance();
            return Ok(());
        }
        Err(ParseError::UnexpectedToken(token, ExpectedFragment::OneOf(Box::new([kind]))))
    }

    fn peek(&self) -> Token {
        self.tokens[self.index]
    }

    fn look_ahead(&self, amount: usize, pred: impl FnOnce(&Token) -> bool) -> bool {
        if let Some(index) = self.index.checked_add(amount)
            && let Some(token) = self.tokens.get(index)
        {
            pred(token)
        } else {
            false
        }
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }
}

// FIXME: Check master if this is still up to date
fn is_reserved(ident: &str, edition: Edition) -> bool {
    #[rustfmt::skip]
    fn is_used_keyword(ident: &str) -> bool {
        matches!(
            ident,
            | "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern" | "false" | "fn"
            | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" | "mut"
            | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct" | "super" | "trait" | "true"
            | "type" | "unsafe" | "use" | "where" | "while"
        )
    }

    #[rustfmt::skip]
    fn is_unused_keyword(ident: &str) -> bool {
        matches!(
            ident,
            | "abstract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv" | "typeof" | "unsized"
            | "virtual" | "yield"
        )
    }

    fn is_used_keyword_if(ident: &str, edition: Edition) -> bool {
        edition >= Edition::Rust2018 && matches!(ident, "async" | "await" | "dyn")
    }

    fn is_unused_keyword_if(ident: &str, edition: Edition) -> bool {
        edition >= Edition::Rust2018 && matches!(ident, "try")
            || edition >= Edition::Rust2024 && matches!(ident, "gen")
    }

    ident == "_"
        || is_used_keyword(ident)
        || is_unused_keyword(ident)
        || is_used_keyword_if(ident, edition)
        || is_unused_keyword_if(ident, edition)
}

fn is_path_seg_keyword(ident: &str) -> bool {
    matches!(ident, "_" | "self" | "Self" | "super" | "crate")
}

pub(crate) enum ParseError {
    UnexpectedToken(Token, ExpectedFragment),
    // FIXME: Temporary
    InvalidDelimiter,
}

impl ParseError {
    pub(crate) fn print(&self, source: &str) {
        eprint!("error: ");
        match self {
            Self::UnexpectedToken(token, expected) => {
                let found = match token.kind {
                    TokenKind::Ident => format!("ident `{}`", &source[token.span.range()]),
                    kind => format!("{kind:?}"),
                };
                eprint!("{:?}: found {found} but expected {expected}", token.span)
            }
            Self::InvalidDelimiter => eprint!("invalid delimiter"),
        }
        eprintln!();
    }
}

pub(crate) enum ExpectedFragment {
    OneOf(Box<[TokenKind]>),
    CommonIdent,
    PathSegIdent,
    Item,
    Ty,
    Expr,
}

impl fmt::Display for ExpectedFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::OneOf(tokens) => {
                let tokens = tokens
                    .iter()
                    .map(|token| format!("{token:?}"))
                    .intersperse_with(|| String::from(" "))
                    .collect::<String>();
                return write!(f, "{tokens}");
            }
            Self::CommonIdent => "common ident",
            Self::PathSegIdent => "path-seg ident",
            Self::Item => "item",
            Self::Ty => "ty",
            Self::Expr => "expr",
        })
    }
}

#[derive(PartialEq, Eq)]
enum AttrStyle {
    Inner,
    Outer,
}

trait Shape: Copy {
    const LENGTH: usize;

    fn check(self, parser: &Parser<'_>) -> bool;

    fn advance(parser: &mut Parser<'_>) {
        for _ in 0..Self::LENGTH {
            parser.advance();
        }
    }
}

impl Shape for TokenKind {
    const LENGTH: usize = 1;

    fn check(self, parser: &Parser<'_>) -> bool {
        // FIXME: This permits `==` if `=` is requested. This is not okay
        parser.peek().kind == self
    }
}

#[derive(Clone, Copy)]
struct Keyword<'src>(&'src str);

impl Shape for Keyword<'_> {
    const LENGTH: usize = 1;

    fn check(self, parser: &Parser<'_>) -> bool {
        let actual = parser.peek();
        actual.kind == TokenKind::Ident && parser.source(actual.span) == self.0
    }
}

#[derive(Clone, Copy)]
struct Glued<const N: usize, T>([T; N]);

impl Shape for Glued<2, TokenKind> {
    const LENGTH: usize = 2;

    fn check(self, parser: &Parser<'_>) -> bool {
        let Self([exp0, exp1]) = self;
        let act0 = parser.peek();
        act0.kind == exp0
            && parser.look_ahead(1, |act1| act1.kind == exp1 && act0.span.end == act1.span.start)
    }
}
