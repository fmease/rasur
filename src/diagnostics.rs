use annotate_snippets as ann;
use rasur::{
    error::Error,
    parser::ExpectedFragment,
    span::Span,
    token::{Repr, Token, TokenKind},
};
use std::{borrow::Cow, path::Path};

pub(crate) fn print(error: Error, cx: RenderCx<'_>) {
    let diag = match error {
        Error::AutoTraitAlias => Diag::new("trait aliases cannot be marked `auto`"),
        Error::DefaultnessOnInvalidItem => {
            Diag::new("this item kind may not be marked with `default`")
        }
        Error::UnexpectedToken(actual, expected) => {
            let span = actual.span;
            let actual = actual.to_diag_str(Some(cx.source));
            Diag::new(format!("found {actual} but expected {}", expected.to_diag_str(())))
                .highlight(span, "unexpected token")
        }
        Error::InvalidAssocItemKind(span) => {
            Diag::new("invalid associated item kind").unlabeled_highlight(span)
        }
        Error::MissingClosingDelimiters(span) => {
            Diag::new("missing closing delimiter(s)").highlight(span, "missing delimiter(s)")
        }
        Error::UnexpectedClosingDelimiter(actual) => {
            let span = actual.span;
            let actual = actual.to_diag_str(Some(cx.source));
            Diag::new(format!("found unexpected closing delimiter {actual}"))
                .highlight(span, "unexpected delimiter")
        }
        Error::InvalidExternItemKind(span) => {
            Diag::new("invalid extern item kind").unlabeled_highlight(span)
        }
        Error::ExpectedTraitFoundTy => Diag::new("found type expected trait"),
        Error::ModifiersOnInvalidBound => Diag::new("this bound kind may not have modifiers"),
        Error::HigherRankedBinderOnInvalidBound(span) => {
            Diag::new("this bound kind may not have a binder").unlabeled_highlight(span)
        }
        Error::MisplacedReceiver => Diag::new("misplaced receiver"),
        Error::OpCannotBeChained(op) => Diag::new(format!("operator `{op}` cannot be chained")),
        Error::TyRelMacroCall => Diag::new("type-relative macro call"),
        Error::ReservedLabel(span) => Diag::new("reserved label").unlabeled_highlight(span),
        Error::ReservedLifetime(span) => Diag::new("reserved lifetime").unlabeled_highlight(span),
        Error::ReservedPrefix(span) => Diag::new("reserved prefix").unlabeled_highlight(span),
        Error::GenericArgsOnFieldExpr(span) => {
            Diag::new("generic args on field expression").unlabeled_highlight(span)
        }
        Error::InvalidItemPrefix(span) => {
            Diag::new(format!("invalid item modifiers")).unlabeled_highlight(span)
        }
        Error::InvalidTyPrefix(span) => {
            Diag::new(format!("invalid type modifiers")).unlabeled_highlight(span)
        }
        Error::InvalidExprPrefix(span) => {
            Diag::new(format!("invalid expression modifiers")).unlabeled_highlight(span)
        }
        Error::TraitImplModifierInInherentImpl(modifier) => {
            Diag::new(format!("trait impl modifier `{modifier}` in inherent impl"))
        }
        Error::UnsafeTraitAlias => Diag::new("trait aliases cannot be marked `unsafe`"),
        Error::InvalidParenthesizedBound => Diag::new("this bound kind may not be parenthesized"),
        Error::VisibilityOnInvalidItem => {
            Diag::new("this item kind may not be marked with visibility")
        }
        Error::ParametrizedWhereClause => {
            Diag::new("generic parameter lists on where-clauses are reserved")
        }
        Error::InvalidOpAfterCast => Diag::new("invalid operator following a cast"),
        Error::UnknownBuiltInSyntax => Diag::new("unknown built-in syntax"),
        Error::InvalidLetChain => Diag::new("invalid let-chain"),
        Error::ReuseInherentImpl => Diag::new("inherent impls cannot be reused"),
        Error::InvalidRawTickedIdent => Diag::new("invalid raw ticked identifier"),
        Error::InvalidRawIdent => Diag::new("invalid raw identifier"),
    };
    eprintln!("{}", diag.render(cx));
}

trait ToDiagStr {
    type Cx<'a>;

    fn to_diag_str(&self, cx: Self::Cx<'_>) -> Cow<'static, str>;
}

impl ToDiagStr for Token {
    type Cx<'a> = Option<&'a str>;

    fn to_diag_str(&self, source: Option<&str>) -> Cow<'static, str> {
        // FIXME: Say "`{source}` (U+NNNN)" on TokenKind::Error | invalid tokens.
        match (self.kind, source) {
            (TokenKind::CommonIdent, Some(source)) => {
                let ident = &source[self.span.range()];
                format!("identifier `{ident}`").into()
            }
            _ => self.kind.to_diag_str(()),
        }
    }
}

impl ToDiagStr for TokenKind {
    type Cx<'a> = ();

    fn to_diag_str(&self, _: ()) -> Cow<'static, str> {
        match self.repr() {
            Repr::Src(src) => format!("`{src}`").into(),
            Repr::Tag(tag) => tag.into(),
        }
    }
}

impl ToDiagStr for ExpectedFragment {
    type Cx<'a> = ();

    fn to_diag_str(&self, _: ()) -> Cow<'static, str> {
        match self {
            Self::Bound => "bound",
            Self::CommonIdent => "common identifier",
            Self::ConstArg => "const argument",
            Self::Expr => "expression",
            Self::ExtPath => "extended path",
            Self::GenericArg => "generic argument",
            Self::GenericParam => "generic parameter",
            Self::Item => "item",
            Self::Literal => "literal",
            Self::OneOf(frags) => {
                return frags
                    .iter()
                    .map(|frag| frag.to_diag_str(()))
                    .intersperse(Cow::Borrowed(" or "))
                    .collect::<String>()
                    .into();
            }
            Self::Pat => "pattern",
            Self::PathSegIdent => "path segment",
            Self::Predicate => "predicate",
            Self::Stmt => "statement",
            Self::Term => "type or const argument",
            Self::Token(token) => return token.to_diag_str(()),
            Self::Ty => "type",
        }
        .into()
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
pub(crate) struct RenderCx<'a> {
    pub(crate) source: &'a str,
    pub(crate) path: &'a Path,
    pub(crate) short: bool,
}
