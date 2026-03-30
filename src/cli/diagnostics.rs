use annotate_snippets as ann;
use rasur::{
    error::{Error, InvalidScalarPlace},
    lexer::IdentKind,
    parser::ExpectedFragment,
    span::Span,
    token::{Repr, Token, TokenKind},
};
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

fn convert(error: Error, cx: &RenderCx<'_>) -> Diag {
    match error {
        Error::AmbiguousPlus(span) => Diag::error("ambiguous `+`").highlight(span),
        Error::AutoTraitAlias => Diag::error("trait aliases cannot be marked `auto`"),
        Error::DefaultOnInvalidItem(span) => {
            Diag::error("this item kind may not be marked with `default`").highlight(span)
        }
        Error::FinalOnInvalidItem(span) => {
            Diag::error("this item kind may not be marked with `final`").highlight(span)
        }
        Error::UnexpectedToken(actual, expected) => {
            let span = actual.span;
            let actual = actual.to_diag_str(cx.file.as_ref().map(|file| file.source));
            Diag::error(format!("found {actual} but expected {}", expected.to_diag_str(())))
                .labeled_highlight(span, "unexpected token")
        }
        Error::InvalidAssocItemKind(span) => {
            Diag::error("invalid associated item kind").highlight(span)
        }
        Error::MissingClosingDelimiters(span) => Diag::error("missing closing delimiter(s)")
            .labeled_highlight(span, "missing delimiter(s)"),
        Error::UnexpectedClosingDelimiter(actual) => {
            let span = actual.span;
            let actual = actual.to_diag_str(cx.file.as_ref().map(|file| file.source));
            Diag::error(format!("found unexpected closing delimiter {actual}"))
                .labeled_highlight(span, "unexpected delimiter")
        }
        Error::InvalidExternItemKind(span) => {
            Diag::error("invalid extern item kind").highlight(span)
        }
        Error::LifetimeObjectTyWithoutPlus(span) => {
            Diag::error("lifetime object type without plus").highlight(span)
        }
        Error::ExpectedTraitFoundTy(span) => {
            Diag::error("found type expected trait").highlight(span)
        }
        Error::ModifiersOnInvalidBound => Diag::error("this bound kind may not have modifiers"),
        Error::HigherRankedBinderOnInvalidBound(span) => {
            Diag::error("this bound kind may not have a binder").highlight(span)
        }
        Error::MisplacedReceiver(span) => Diag::error("misplaced receiver").highlight(span),
        Error::ChainedComparison(span) => {
            Diag::error("comparison operators cannot be chained").highlight(span)
        }
        Error::TyRelMacroCall(span) => Diag::error("type-relative macro call").highlight(span),
        Error::InvalidExtraFieldProjections(span) => {
            Diag::error("invalid extra field accesses").highlight(span)
        }
        Error::ReservedLabel(span) => Diag::error("reserved label").highlight(span),
        Error::ReservedLifetime(span) => Diag::error("reserved lifetime").highlight(span),
        Error::ReservedPrefix(span) => Diag::error("reserved prefix").highlight(span),
        Error::GenericArgsOnFieldExpr(span) => {
            Diag::error("generic args on field expression").highlight(span)
        }
        Error::InvalidItemPrefix(span) => Diag::error("invalid item modifiers").highlight(span),
        Error::InvalidTyPrefix(span) => Diag::error("invalid type modifiers").highlight(span),
        Error::InvalidExprPrefix(span) => {
            Diag::error("invalid expression modifiers").highlight(span)
        }
        Error::TraitImplModifierInInherentImpl(modifier) => {
            Diag::error(format!("trait impl modifier `{modifier}` in inherent impl"))
        }
        Error::UnsafeTraitAlias => Diag::error("trait aliases cannot be marked `unsafe`"),
        Error::InvalidParenthesizedBound => Diag::error("this bound kind may not be parenthesized"),
        Error::VisibilityOnInvalidItem(span) => {
            Diag::error("this item kind may not be marked with visibility").highlight(span)
        }
        Error::ParametrizedWhereClause(span) => {
            Diag::error("generic parameter lists on where-clauses are reserved").highlight(span)
        }
        Error::InvalidOpAfterCast(span) => {
            Diag::error("invalid operator following a cast").highlight(span)
        }
        Error::InvalidOpAfterBoundary(span) => {
            Diag::error("invalid operator following a boundary").highlight(span)
        }
        Error::UnknownBuiltinSyntax(span) => Diag::error("unknown built-in syntax").highlight(span),
        Error::InvalidLetChain(span) => Diag::error("invalid let-chain").highlight(span),
        Error::ReuseInherentImpl => Diag::error("inherent impls cannot be reused"),
        Error::InvalidRawIdent(IdentKind::Normal, span) => {
            Diag::error("invalid raw identifier").highlight(span)
        }
        Error::InvalidRawIdent(IdentKind::Ticked, span) => {
            Diag::error("invalid raw ticked identifier").highlight(span)
        }
        Error::UnterminatedBlockComment(span) => {
            Diag::error("unterminated block comment").highlight(span)
        }
        Error::UnterminatedCharLit(span) => {
            Diag::error("unterminated char literal").highlight(span)
        }
        Error::UnterminatedStrLit(span) => {
            Diag::error("unterminated string literal").highlight(span)
        }
        Error::StrLitGuardTooLarge(span) => {
            Diag::error("string literal guard too large").highlight(span)
        }
        Error::ReservedMultiHash(span) => Diag::error("reserved multi-hash").highlight(span),
        Error::ImplRestrictedTraitAlias => Diag::error("trait aliases cannot be impl-restricted"),
        Error::InvalidEscapeSequence(span) => {
            Diag::error("invalid escape sequence").highlight(span)
        }
        Error::EmptyCharLit(span) => Diag::error("empty char literal").highlight(span),
        Error::MultiScalarCharLit(span) => Diag::error("multi-scalar char literal").highlight(span),
        Error::InvalidScalar(char, place, span) => {
            let place = match place {
                InvalidScalarPlace::File => "",
                InvalidScalarPlace::FrontmatterBody => " in frontmatter body",
                InvalidScalarPlace::DocComment => " in doc comment",
                InvalidScalarPlace::Lit => " in literal",
            };
            Diag::error(format!("invalid scalar U+{:04X}{place}", char as u32)).highlight(span)
        }
        Error::InvalidStrLitDelimiter(span) => {
            Diag::error("invalid string literal delimiter").highlight(span)
        }
        Error::EmptyNumLit(span) => Diag::error("empty number literal").highlight(span),
        Error::InvalidDigit(span) => Diag::error("invalid digit").highlight(span),
        Error::InvalidAbiStr(span) => Diag::error("invalid ABI string").highlight(span),
        Error::InvalidLitSuffix(span) => Diag::error("invalid literal suffix").highlight(span),
        Error::NonDecFloatLit(span) => Diag::error("non-decimal float literal").highlight(span),
        Error::ParenthesizedGuardedPatInMatch => {
            Diag::error("parenthesized guarded pattern in match expression")
        }
        Error::EmptyExponent(span) => Diag::error("empty exponent").highlight(span),
        Error::InvalidFrontmatterInfostring(span) => {
            Diag::error("invalid frontmatter infostring").highlight(span)
        }
        Error::FrontmatterOpeningTooLarge(span) => {
            Diag::error("frontmatter opening too large").highlight(span)
        }
        Error::UnterminatedFrontmatter(span) => {
            Diag::error("unterminated frontmatter").highlight(span)
        }
        Error::InvalidFrontmatterTrailer(span) => {
            Diag::error("extra characters after frontmatter closing").highlight(span)
        }
        Error::ForbiddenInnerAttrs => Diag::error("inner attributes are forbidden in this context"),
        Error::ForbiddenOuterAttrs => Diag::error("outer attributes are forbidden in this context"),
        Error::InvalidNumericIdent(span) => {
            Diag::error("invalid numeric identifier").highlight(span)
        }
        Error::AbiStrSuffix(span) => Diag::error("suffix on ABI string").highlight(span),
        Error::TickFollowingRawTickedIdent(span) => {
            Diag::error("tick immediately following raw ticked identifier").highlight(span)
        }
    }
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
            Self::Lit => "literal",
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

pub(super) struct Diag {
    level: ann::Level<'static>,
    title: Cow<'static, str>,
    highlight: Option<(Span, Option<Cow<'static, str>>)>,
}

impl Diag {
    pub(super) fn new(level: ann::Level<'static>, title: impl Into<Cow<'static, str>>) -> Self {
        Self { level, title: title.into(), highlight: None }
    }

    pub(super) fn error(title: impl Into<Cow<'static, str>>) -> Self {
        Self::new(ann::Level::ERROR, title)
    }

    fn labeled_highlight(mut self, span: Span, label: impl Into<Cow<'static, str>>) -> Self {
        self.highlight = Some((span, Some(label.into())));
        self
    }

    pub(super) fn highlight(mut self, span: Span) -> Self {
        self.highlight = Some((span, None));
        self
    }
}

impl RenderExt for Diag {
    fn render(self, cx: &RenderCx<'_>) {
        let group = ann::Group::with_title(self.level.title(self.title));
        let group = match self.highlight {
            Some((span, label)) => {
                let file = cx.file.as_ref().expect("highlight requested but no source provided");

                super let path = match file.path {
                    // FIXME: Being forced to use to_string_lossy is sad :(
                    SourcePath::Real(path) => path.to_string_lossy(),
                    SourcePath::Anon => "<anon>".into(),
                };

                let annotation = ann::AnnotationKind::Primary.span(span.range());
                let annotation = match label {
                    Some(label) => annotation.label(label),
                    None => annotation,
                };
                group.element(ann::Snippet::source(file.source).path(&path).annotation(annotation))
            }
            None => group,
        };
        let renderer = if cx.colorize { ann::Renderer::styled() } else { ann::Renderer::plain() };
        let diag = renderer.short_message(cx.short).render(&[group]);
        eprintln!("{diag}");
    }
}

impl RenderExt for Error {
    fn render(self, cx: &RenderCx<'_>) {
        convert(self, cx).render(cx);
    }
}

pub(super) trait RenderExt {
    fn render(self, cx: &RenderCx<'_>);
}

pub(crate) struct RenderCx<'a> {
    colorize: bool,
    short: bool,
    file: Option<SourceFile<'a>>,
}

impl<'a> RenderCx<'a> {
    pub(crate) fn new(short: bool) -> Self {
        let colorize = painter::colorize(&std::io::stderr());

        Self { colorize, short, file: None }
    }

    pub(crate) fn file(self, path: SourcePath<'a>, source: &'a str) -> Self {
        Self { file: Some(SourceFile { path, source }), ..self }
    }
}

pub struct SourceFile<'a> {
    path: SourcePath<'a>,
    source: &'a str,
}

pub(crate) enum SourcePath<'a> {
    Real(&'a Path),
    Anon,
}

pub(crate) enum SourcePathBuf {
    Real(PathBuf),
    Anon,
}

impl SourcePathBuf {
    pub(super) fn as_ref(&self) -> SourcePath<'_> {
        match self {
            Self::Real(path) => SourcePath::Real(path),
            Self::Anon => SourcePath::Anon,
        }
    }
}
