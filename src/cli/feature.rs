use crate::{
    Diag,
    diagnostics::{IntoDiag, RenderCx, ToDiagStr},
};
use rasur::{
    ast::{self, Token, TokenKind},
    edition::Edition,
    feature::Feature,
    parser::Parser,
    span::Span,
    store::Store,
};
use std::{borrow::Cow, collections::HashSet, fmt, ops::ControlFlow};
use utility::{List1, list1};

pub fn enabled_features<'src>(
    file: &ast::File<'src>,
    source: &'src str,
    edition: Edition,
) -> (HashSet<Feature>, Vec<Error<'src>>) {
    let store = Store::default();
    let mut p = EarlyAttrParser::new(source, edition, &store);

    for attr in &file.attrs {
        let ast::AttrKind::Regular(meta) = &attr.kind else { continue };

        match p.parse_attr(meta) {
            Ok(ControlFlow::Continue(())) | Err(()) => {}
            Ok(ControlFlow::Break(())) => break,
        }
    }

    let mut features = p.features;

    if features.contains(&Feature::coroutines) || features.contains(&Feature::gen_blocks) {
        features.insert(Feature::yield_expr);
    }

    let mut errors = p.errors;
    errors.extend(store.errors.into_inner().into_iter().map(Error::Parse));

    // FIXME: This might become reachable in the future, pave the way.
    debug_assert!(store.features.into_inner().is_empty());

    (features, errors)
}

struct EarlyAttrParser<'tok, 'sto, 'src> {
    p: Parser<'tok, 'sto, 'src>,
    errors: Vec<Error<'src>>,
    features: HashSet<Feature>,
}

impl<'tok, 'sto, 'src> EarlyAttrParser<'tok, 'sto, 'src> {
    fn new(source: &'src str, edition: Edition, store: &'sto Store) -> Self {
        let tokens = const { &[Token { kind: TokenKind::EndOfInput, span: Span::default() }] };

        Self {
            p: Parser::new(tokens, source, edition, store),
            errors: Vec::new(),
            features: HashSet::default(),
        }
    }

    fn parse_attr(&mut self, meta: &ast::Meta<'src>) -> Result<ControlFlow<()>, ()> {
        let ast::Path { segs: [ast::PathSeg { ident, .. }] } = meta.path else {
            return Ok(ControlFlow::Continue(()));
        };

        match ident.name {
            "cfg" => {
                let cfg = self.parse_cfg_attr(meta)?;

                if !cfg.eval() {
                    return Ok(ControlFlow::Break(()));
                }
            }
            "cfg_attr" => return self.parse_cfg_attr_attr(meta),
            "feature" => self.parse_feature_attr(meta)?,
            _ => {}
        }

        Ok(ControlFlow::Continue(()))
    }

    fn parse_cfg_attr(&mut self, attr: &ast::Meta<'src>) -> Result<Cfg<'src>, ()> {
        if let ast::Safety::Unsafe(span) = attr.safety {
            self.error(Error::UnsafeOnSafeAttr(span, EarlyAttrName::Cfg));
        }

        let ast::MetaArgs::Call(ast::Bracket::Round, tokens) = &attr.args else {
            self.error(Error::MalformedAttr(EarlyAttrName::Cfg));
            return Err(());
        };

        self.with(tokens, |this| this.parse_cfg())
    }

    fn parse_cfg(&mut self) -> Result<Cfg<'src>, ()> {
        Err(match self.p.token.kind {
            TokenKind::False => {
                self.p.advance();
                return Ok(Cfg::False);
            }
            TokenKind::True => {
                self.p.advance();
                return Ok(Cfg::True);
            }
            TokenKind::CommonIdent => {
                let source = self.p.source(self.p.token.span);
                self.p.advance();
                return Ok(match source {
                    "not" => {
                        self.p.parse(TokenKind::OpenRoundBracket)?;
                        let cfg = self.parse_cfg()?;
                        self.p.parse(TokenKind::CloseRoundBracket)?;
                        Cfg::Not(Box::new(cfg))
                    }
                    "any" => Cfg::Any(self.parse_delim_cfg_list()?),
                    "all" => Cfg::All(self.parse_delim_cfg_list()?),
                    // FIXME: Support key="value"
                    // FIXME: Support the version predicate
                    _ => {
                        let key = source;
                        let value = if self.p.consume(TokenKind::SingleEquals) {
                            match self.p.token.kind {
                                // FIXME: I'm pretty sure we need to reject certain kinds of string literals
                                TokenKind::StrLit => {
                                    let source = self.p.source(self.p.token.span);
                                    self.p.advance();
                                    Some(source)
                                }
                                _ => {
                                    self.p
                                        .unexpected(self.p.token, list1![TokenKind::StrLit.into()]);
                                    return Err(());
                                }
                            }
                        } else {
                            None
                        };

                        Cfg::Var(key, value)
                    }
                });
            }
            _ => self.error(Error::UnexpectedToken(self.p.token, list1![Fragment::Configuration])),
        })
    }

    fn parse_delim_cfg_list(&mut self) -> Result<Vec<Cfg<'src>>, ()> {
        self.p.parse(TokenKind::OpenRoundBracket)?;

        let mut cfgs = Vec::new();
        while !self.p.consume(TokenKind::CloseRoundBracket) {
            cfgs.push(self.parse_cfg()?);
            if self.p.token.kind != TokenKind::CloseRoundBracket {
                self.p.parse(TokenKind::Comma)?;
            }
        }
        Ok(cfgs)
    }

    fn parse_cfg_attr_attr(&mut self, attr: &ast::Meta<'src>) -> Result<ControlFlow<()>, ()> {
        if let ast::Safety::Unsafe(span) = attr.safety {
            self.error(Error::UnsafeOnSafeAttr(span, EarlyAttrName::CfgAttr));
        }

        let ast::MetaArgs::Call(ast::Bracket::Round, tokens) = &attr.args else {
            self.error(Error::MalformedAttr(EarlyAttrName::CfgAttr));
            return Err(());
        };

        self.with(tokens, |this| {
            let cfg = this.parse_cfg()?;

            this.p.parse(TokenKind::Comma)?;

            let metas = this.p.fin_parse_delim_seq(
                TokenKind::EndOfInput,
                TokenKind::Comma,
                Parser::parse_meta,
            )?;

            if !cfg.eval() {
                return Ok(ControlFlow::Continue(()));
            }

            for meta in metas {
                match this.parse_attr(&meta)? {
                    ControlFlow::Continue(()) => {}
                    ControlFlow::Break(()) => return Ok(ControlFlow::Break(())),
                }
            }

            Ok(ControlFlow::Continue(()))
        })
    }

    fn parse_feature_attr(&mut self, attr: &ast::Meta<'src>) -> Result<(), ()> {
        if let ast::Safety::Unsafe(span) = attr.safety {
            self.error(Error::UnsafeOnSafeAttr(span, EarlyAttrName::Feature));
        }

        let ast::MetaArgs::Call(ast::Bracket::Round, tokens) = &attr.args else {
            self.error(Error::MalformedAttr(EarlyAttrName::Feature));
            return Ok(());
        };

        self.with(tokens, |this| {
            let idents = this.p.fin_parse_delim_seq(
                TokenKind::EndOfInput,
                TokenKind::Comma,
                Parser::parse_common_ident,
            )?;

            for ident in idents {
                match ident.name.parse::<Feature>() {
                    Ok(feature) if this.features.insert(feature) => {}
                    Ok(feature) => this.error(Error::FeatureAlreadyEnabled(feature, ident.span)),
                    Err(()) => this.error(Error::UnknownFeature(ident.name)),
                }
            }

            Err(())
        })
    }

    fn with<'tmptok, T>(
        &mut self,
        tokens: &'tmptok [Token],
        perform: impl FnOnce(&mut EarlyAttrParser<'tmptok, 'sto, 'src>) -> T,
    ) -> T {
        let p = Parser::new(tokens, self.p.source, self.p.edition, self.p.store);

        let mut this = EarlyAttrParser {
            p,
            errors: std::mem::take(&mut self.errors),
            features: std::mem::take(&mut self.features),
        };

        let result = perform(&mut this);

        self.errors = this.errors;
        self.features = this.features;

        result
    }

    fn error(&mut self, error: Error<'src>) {
        self.errors.push(error);
    }
}

#[derive(Debug)]
enum Cfg<'src> {
    False,
    True,
    Not(Box<Self>),
    Any(Vec<Self>),
    All(Vec<Self>),
    Var(#[expect(dead_code)] &'src str, #[expect(dead_code)] Option<&'src str>),
}

impl Cfg<'_> {
    fn eval(self) -> bool {
        match self {
            // FIXME: Properly support variables once CLI has `--cfg`.
            Self::False | Self::Var(..) => false,
            Self::True => true,
            Self::Not(cfg) => !cfg.eval(),
            Self::Any(cfgs) => cfgs.into_iter().any(Self::eval),
            Self::All(cfgs) => cfgs.into_iter().all(Self::eval),
        }
    }
}

// FIXME: All of these need a span
// FIXME: These are placeholder errors, improve them significantly
#[derive(Debug)]
pub(super) enum Error<'src> {
    UnsafeOnSafeAttr(Span, EarlyAttrName),
    MalformedAttr(EarlyAttrName),
    UnexpectedToken(Token, List1<Fragment>),
    FeatureAlreadyEnabled(Feature, Span),
    UnknownFeature(&'src str),
    Parse(rasur::error::Error),
}

impl IntoDiag for Error<'_> {
    fn into_diag(self, cx: &RenderCx<'_>) -> Diag {
        match self {
            Self::UnsafeOnSafeAttr(span, name) => {
                Diag::error(format!("`{name}` is not an unsafe attribute")).span(span)
            }
            Self::MalformedAttr(name) => Diag::error(format!("attribute `{name}` is malformed")),
            Self::UnexpectedToken(actual, expected) => {
                let span = actual.span;
                let actual = actual.to_diag_str(cx.file.as_ref().map(|file| file.source));
                let expected = expected.to_diag_str(());
                Diag::error(format!("found {actual} but expected {expected}"))
                    .span(span)
                    .label("unexpected token")
            }
            Self::FeatureAlreadyEnabled(feature, span) => {
                Diag::error(format!("feature `{feature}` is already enabled")).span(span)
            }
            Self::UnknownFeature(name) => Diag::error(format!("unknown feature `{name}`")),
            Self::Parse(error) => error.into_diag(cx),
        }
    }
}

#[derive(Debug)]
pub(super) enum Fragment {
    Token(TokenKind),
    Configuration,
}

impl ToDiagStr for Fragment {
    type Cx<'a> = ();

    fn to_diag_str(&self, _: ()) -> Cow<'static, str> {
        match self {
            Self::Token(token) => token.to_diag_str(()),
            Self::Configuration => "configuration".into(),
        }
    }
}

impl From<TokenKind> for Fragment {
    fn from(kind: TokenKind) -> Self {
        Self::Token(kind)
    }
}

#[derive(Debug)]
pub(super) enum EarlyAttrName {
    Cfg,
    CfgAttr,
    Feature,
}

impl fmt::Display for EarlyAttrName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Cfg => "cfg",
            Self::CfgAttr => "cfg_attr",
            Self::Feature => "feature",
        })
    }
}
