use annotate_snippets as ann;
use rasur::{
    error::{Error, UnchainableExprOp},
    parser::ExpectedFragment,
    span::Span,
    token::{Repr, Token, TokenKind},
};
use std::{borrow::Cow, path::Path};

pub(crate) fn eprint(error: Error, cx: RenderCx<'_>) {
    let diag = match error {
        Error::AutoTraitAlias => Diag::new("trait aliases cannot be marked `auto`"),
        Error::DefaultnessOnInvalidItem(span) => {
            Diag::new("this item kind may not be marked with `default`").highlight(span)
        }
        Error::UnexpectedToken(actual, expected) => {
            let span = actual.span;
            let actual = actual.to_diag_str(Some(cx.source));
            Diag::new(format!("found {actual} but expected {}", expected.to_diag_str(())))
                .labeled_highlight(span, "unexpected token")
        }
        Error::InvalidAssocItemKind(span) => {
            Diag::new("invalid associated item kind").highlight(span)
        }
        Error::MissingClosingDelimiters(span) => Diag::new("missing closing delimiter(s)")
            .labeled_highlight(span, "missing delimiter(s)"),
        Error::UnexpectedClosingDelimiter(actual) => {
            let span = actual.span;
            let actual = actual.to_diag_str(Some(cx.source));
            Diag::new(format!("found unexpected closing delimiter {actual}"))
                .labeled_highlight(span, "unexpected delimiter")
        }
        Error::InvalidExternItemKind(span) => Diag::new("invalid extern item kind").highlight(span),
        Error::ExpectedTraitFoundTy(span) => Diag::new("found type expected trait").highlight(span),
        Error::ModifiersOnInvalidBound => Diag::new("this bound kind may not have modifiers"),
        Error::HigherRankedBinderOnInvalidBound(span) => {
            Diag::new("this bound kind may not have a binder").highlight(span)
        }
        Error::MisplacedReceiver(span) => Diag::new("misplaced receiver").highlight(span),
        Error::UnchainableExprOp(op, span) => {
            let kind = match op {
                UnchainableExprOp::Compare => "comparison",
                UnchainableExprOp::Range => "range",
            };
            Diag::new(format!("{kind} operators cannot be chained")).highlight(span)
        }
        Error::TyRelMacroCall(span) => Diag::new("type-relative macro call").highlight(span),
        Error::ReservedLabel(span) => Diag::new("reserved label").highlight(span),
        Error::ReservedLifetime(span) => Diag::new("reserved lifetime").highlight(span),
        Error::ReservedPrefix(span) => Diag::new("reserved prefix").highlight(span),
        Error::GenericArgsOnFieldExpr(span) => {
            Diag::new("generic args on field expression").highlight(span)
        }
        Error::InvalidItemPrefix(span) => {
            Diag::new(format!("invalid item modifiers")).highlight(span)
        }
        Error::InvalidTyPrefix(span) => {
            Diag::new(format!("invalid type modifiers")).highlight(span)
        }
        Error::InvalidExprPrefix(span) => {
            Diag::new(format!("invalid expression modifiers")).highlight(span)
        }
        Error::TraitImplModifierInInherentImpl(modifier) => {
            Diag::new(format!("trait impl modifier `{modifier}` in inherent impl"))
        }
        Error::UnsafeTraitAlias => Diag::new("trait aliases cannot be marked `unsafe`"),
        Error::InvalidParenthesizedBound => Diag::new("this bound kind may not be parenthesized"),
        Error::VisibilityOnInvalidItem(span) => {
            Diag::new("this item kind may not be marked with visibility").highlight(span)
        }
        Error::ParametrizedWhereClause(span) => {
            Diag::new("generic parameter lists on where-clauses are reserved").highlight(span)
        }
        Error::InvalidOpAfterCast(span) => {
            Diag::new("invalid operator following a cast").highlight(span)
        }
        Error::UnknownBuiltInSyntax(span) => Diag::new("unknown built-in syntax").highlight(span),
        Error::InvalidLetChain => Diag::new("invalid let-chain"),
        Error::ReuseInherentImpl => Diag::new("inherent impls cannot be reused"),
        Error::InvalidRawTickedIdent(span) => {
            Diag::new("invalid raw ticked identifier").highlight(span)
        }
        Error::InvalidRawIdent(span) => Diag::new("invalid raw identifier").highlight(span),
        Error::UnterminatedBlockComment(span) => {
            Diag::new("unterminated block comment").highlight(span)
        }
        Error::UnterminatedCharLit(span) => Diag::new("unterminated char literal").highlight(span),
        Error::UnterminatedStrLit(span) => Diag::new("unterminated string literal").highlight(span),
        Error::StrLitGuardTooLarge(span) => {
            Diag::new("string literal guard too large").highlight(span)
        }
        Error::ReservedMultiHash(span) => Diag::new("reserved multi-hash").highlight(span),
        Error::InvalidEscapeSequence(span) => Diag::new("invalid escape sequence").highlight(span),
        Error::EmptyCharLit(span) => Diag::new("empty char literal").highlight(span),
        Error::MultiScalarCharLit(span) => Diag::new("multi-scalar char literal").highlight(span),
        Error::InvalidToken(char, span) => {
            Diag::new(format!("invalid token U+{:04X}", char as u32)).highlight(span)
        }
        Error::InvalidStrLitDelim(span) => {
            Diag::new("invalid string literal delimiter").highlight(span)
        }
        Error::EmptyNumLit(span) => Diag::new("empty number literal").highlight(span),
        Error::InvalidDigit(span) => Diag::new("invalid digit").highlight(span),
        Error::InvalidAbiStr(span) => Diag::new("invalid ABI string").highlight(span),
        Error::InvalidLitSuffix(span) => Diag::new("invalid literal suffix").highlight(span),
        Error::InvalidScalarInLit(span) => Diag::new("invalid scalar in literal").highlight(span),
        Error::NonDecFloatLit(span) => Diag::new("non-decimal float literal").highlight(span),
        Error::ParenthesizedGuardedPatInMatch => {
            Diag::new("parenthesized guarded pattern in match expression")
        }
        Error::EmptyExponent(span) => Diag::new("empty exponent").highlight(span),
        Error::InvalidFrontmatterInfostring(span) => {
            Diag::new("invalid frontmatter infostring").highlight(span)
        }
        Error::FrontmatterOpeningTooLarge(span) => {
            Diag::new("frontmatter opening too large").highlight(span)
        }
        Error::UnterminatedFrontmatter(span) => {
            Diag::new("unterminated frontmatter").highlight(span)
        }
        Error::FrontmatterClosingTrailer(span) => {
            Diag::new("extra characters after frontmatter closing").highlight(span)
        }
        Error::ForbiddenInnerAttrs => Diag::new("inner attributes are forbidden in this context"),
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

    fn labeled_highlight(mut self, span: Span, label: impl Into<Cow<'static, str>>) -> Self {
        self.highlight = Some((span, Some(label.into())));
        self
    }

    fn highlight(mut self, span: Span) -> Self {
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
