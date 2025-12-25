use super::ExpectedFragment;
use crate::{span::Span, token::Token};
use annotate_snippets as ann;
use std::{borrow::Cow, path::Path};

#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub enum Error {
    AutoTraitAlias,
    DefaultnessOnInvalidItem,
    ExpectedTraitFoundTy,
    GenericArgsOnFieldExpr(Span),
    HigherRankedBinderOnInvalidBound(Span),
    InvalidAssocItemKind(Span),
    InvalidExprPrefix(Span),
    InvalidExternItemKind(Span),
    InvalidTyPrefix(Span),
    InvalidItemPrefix(Span),
    InvalidOpAfterCast,
    InvalidParenthesizedBound,
    MisplacedReceiver,
    MissingClosingDelimiters(Span),
    ModifiersOnInvalidBound,
    OpCannotBeChained(String),
    ParametrizedWhereClause,
    ReservedLabel(Span),
    ReservedLifetime(Span),
    ReservedPrefix(Span),
    TraitImplModifierInInherentImpl(&'static str),
    TyRelMacroCall,
    UnexpectedClosingDelimiter(Token),
    UnexpectedToken(Token, ExpectedFragment),
    UnknownBuiltInSyntax,
    UnsafeTraitAlias,
    VisibilityOnInvalidItem,
    InvalidLetChain,
    ReuseInherentImpl,
    InvalidRawTickedIdent,
    InvalidRawIdent,
}

impl Error {
    // FIXME: Move into binary crate?
    pub fn print(self, cx: RenderCx<'_>) {
        let diag = match self {
            Self::AutoTraitAlias => Diag::new("trait aliases cannot be marked `auto`"),
            Self::DefaultnessOnInvalidItem => {
                Diag::new("this item kind may not be marked with `default`")
            }
            Self::UnexpectedToken(actual, expected) => {
                let span = actual.span;
                let actual = actual.to_diag_str(Some(cx.source));
                Diag::new(format!("found {actual} but expected {expected}"))
                    .highlight(span, "unexpected token")
            }
            Self::InvalidAssocItemKind(span) => {
                Diag::new("invalid associated item kind").unlabeled_highlight(span)
            }
            Self::MissingClosingDelimiters(span) => {
                Diag::new("missing closing delimiter(s)").highlight(span, "missing delimiter(s)")
            }
            Self::UnexpectedClosingDelimiter(actual) => {
                let span = actual.span;
                let actual = actual.to_diag_str(Some(cx.source));
                Diag::new(format!("found unexpected closing delimiter {actual}"))
                    .highlight(span, "unexpected delimiter")
            }
            Self::InvalidExternItemKind(span) => {
                Diag::new("invalid extern item kind").unlabeled_highlight(span)
            }
            Self::ExpectedTraitFoundTy => Diag::new("found type expected trait"),
            Self::ModifiersOnInvalidBound => Diag::new("this bound kind may not have modifiers"),
            Self::HigherRankedBinderOnInvalidBound(span) => {
                Diag::new("this bound kind may not have a binder").unlabeled_highlight(span)
            }
            Self::MisplacedReceiver => Diag::new("misplaced receiver"),
            Self::OpCannotBeChained(op) => Diag::new(format!("operator `{op}` cannot be chained")),
            Self::TyRelMacroCall => Diag::new("type-relative macro call"),
            Self::ReservedLabel(span) => Diag::new("reserved label").unlabeled_highlight(span),
            Self::ReservedLifetime(span) => {
                Diag::new("reserved lifetime").unlabeled_highlight(span)
            }
            Self::ReservedPrefix(span) => Diag::new("reserved prefix").unlabeled_highlight(span),
            Self::GenericArgsOnFieldExpr(span) => {
                Diag::new("generic args on field expression").unlabeled_highlight(span)
            }
            Self::InvalidItemPrefix(span) => {
                Diag::new(format!("invalid item modifiers")).unlabeled_highlight(span)
            }
            Self::InvalidTyPrefix(span) => {
                Diag::new(format!("invalid type modifiers")).unlabeled_highlight(span)
            }
            Self::InvalidExprPrefix(span) => {
                Diag::new(format!("invalid expression modifiers")).unlabeled_highlight(span)
            }
            Self::TraitImplModifierInInherentImpl(modifier) => {
                Diag::new(format!("trait impl modifier `{modifier}` in inherent impl"))
            }
            Self::UnsafeTraitAlias => Diag::new("trait aliases cannot be marked `unsafe`"),
            Self::InvalidParenthesizedBound => {
                Diag::new("this bound kind may not be parenthesized")
            }
            Self::VisibilityOnInvalidItem => {
                Diag::new("this item kind may not be marked with visibility")
            }
            Self::ParametrizedWhereClause => {
                Diag::new("generic parameter lists on where-clauses are reserved")
            }
            Self::InvalidOpAfterCast => Diag::new("invalid operator following a cast"),
            Self::UnknownBuiltInSyntax => Diag::new("unknown built-in syntax"),
            Self::InvalidLetChain => Diag::new("invalid let-chain"),
            Self::ReuseInherentImpl => Diag::new("inherent impls cannot be reused"),
            Self::InvalidRawTickedIdent => Diag::new("invalid raw ticked identifier"),
            Self::InvalidRawIdent => Diag::new("invalid raw identifier"),
        };
        eprintln!("{}", diag.render(cx));
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

    fn render(self, cx: RenderCx<'_>) -> String {
        let group = ann::Group::with_title(ann::Level::ERROR.title(self.title));
        let group = match self.highlight {
            Some((span, label)) => {
                // FIXME: Being forced to use to_string_lossy is sad :(
                super let path = cx.path.to_string_lossy();
                let annotation = ann::AnnotationKind::Primary.span(span.range());
                let annotation = match label {
                    Some(label) => annotation.label(label),
                    None => annotation,
                };
                group.element(ann::Snippet::source(cx.source).path(&path).annotation(annotation))
            }
            None => group,
        };
        ann::Renderer::styled().short_message(cx.short).render(&[group])
    }
}

#[derive(Clone, Copy)]
pub struct RenderCx<'a> {
    pub source: &'a str,
    pub path: &'a Path,
    pub short: bool,
}
