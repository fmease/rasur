use super::ExpectedFragment;
use crate::{span::Span, token::Token};
use annotate_snippets as ann;
use std::{borrow::Cow, path::Path};

#[cfg_attr(test, derive(Debug))]
pub(crate) enum ParseError {
    ExpectedTraitFoundTy,
    GenericArgsOnFieldExpr(Span),
    InvalidAssocItemKind(Span),
    InvalidDelimiter,
    InvalidExternItemKind(Span),
    MisplacedReceiver,
    ModifiersOnOutlivesBound,
    // FIXME: &'static str over String
    OpCannotBeChained(String),
    ReservedLifetime(Span),
    TyRelMacroCall,
    UnexpectedToken(Token, ExpectedFragment),
    HigherRankedBinderOnOutlivesBound,
}

impl ParseError {
    pub(crate) fn print(self, source: &str, path: &Path) {
        let diag = match self {
            Self::UnexpectedToken(token, expected) => {
                let found = token.to_diag_str(Some(source));
                Diag::new(format!("found {found} but expected {expected}"))
                    .highlight(token.span, "unexpected token")
            }
            Self::InvalidAssocItemKind(span) => {
                Diag::new("invalid associated item kind").unlabeled_highlight(span)
            }
            Self::InvalidDelimiter => Diag::new("invalid delimiter"),
            Self::InvalidExternItemKind(span) => {
                Diag::new("invalid extern item kind").unlabeled_highlight(span)
            }
            Self::ExpectedTraitFoundTy => Diag::new("found type expected trait"),
            Self::ModifiersOnOutlivesBound => Diag::new("outlives-bounds may not have modifiers"),
            Self::HigherRankedBinderOnOutlivesBound => {
                Diag::new("outlives-bounds may not have a binder")
            }
            Self::MisplacedReceiver => Diag::new("misplaced receiver"),
            Self::OpCannotBeChained(op) => Diag::new(format!("operator `{op}` cannot be chained")),
            Self::TyRelMacroCall => Diag::new("type-relative macro call"),
            Self::ReservedLifetime(span) => {
                Diag::new("reserved lifetime").unlabeled_highlight(span)
            }
            Self::GenericArgsOnFieldExpr(span) => {
                Diag::new("generic args on field expression").unlabeled_highlight(span)
            }
        };
        eprintln!("{}", diag.render(source, path));
    }
}

struct Diag {
    title: Cow<'static, str>,
    highlight: Option<(Span, Option<Cow<'static, str>>)>,
}

impl Diag {
    fn new(title: impl Into<Cow<'static, str>>) -> Self {
        Self { title: title.into(), highlight: None }
    }

    fn highlight(mut self, span: Span, label: impl Into<Cow<'static, str>>) -> Self {
        self.highlight = Some((span, Some(label.into())));
        self
    }

    fn unlabeled_highlight(mut self, span: Span) -> Self {
        self.highlight = Some((span, None));
        self
    }

    fn render(self, source: &str, path: &Path) -> String {
        let group = ann::Group::with_title(ann::Level::ERROR.title(self.title));
        let group = match self.highlight {
            Some((span, label)) => {
                // FIXME: Being forced to use to_string_lossy is sad :(
                super let path = path.to_string_lossy();
                let annotation = ann::AnnotationKind::Primary.span(span.range());
                let annotation = match label {
                    Some(label) => annotation.label(label),
                    None => annotation,
                };
                group.element(ann::Snippet::source(source).path(&path).annotation(annotation))
            }
            None => group,
        };
        ann::Renderer::styled().render(&[group])
    }
}
