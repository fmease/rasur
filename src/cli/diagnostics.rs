use annotate_snippets as ann;
use rasur::{
    error::{Error, ErrorKind, InvalidScalarPlace, List1},
    lexer::{IdentKind, IdentMode},
    parser::Fragment,
    span::{At as _, Span},
    token::{Repr, Token, TokenKind},
};
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

impl IntoDiag for Error {
    fn into_diag(self, cx: &RenderCx<'_>) -> Diag {
        let diag = Diag::new(Level::Error).span(self.span);

        match self.kind {
            ErrorKind::AbiStrSuffix => diag.title("suffix on ABI string"),
            ErrorKind::AmbiguousPlus => diag.title("ambiguous `+`"),
            ErrorKind::AutoTraitAlias => diag.title("trait aliases cannot be marked `auto`"),
            ErrorKind::BareLifetimeInTy => diag.title("bare lifetime in type"),
            ErrorKind::ChainedComparison => diag.title("comparison operators cannot be chained"),
            ErrorKind::DefaultOnInvalidItem => {
                diag.title("this item kind may not be marked with `default`")
            }
            ErrorKind::EmptyCharLit => diag.title("empty char literal"),
            ErrorKind::EmptyExponent => diag.title("empty exponent"),
            ErrorKind::EmptyNumLit => diag.title("empty number literal"),
            ErrorKind::ExpectedTraitFoundTy => diag.title("found type expected trait"),
            ErrorKind::FinalOnInvalidItem => {
                diag.title("this item kind may not be marked with `final`")
            }
            ErrorKind::ForbiddenCVariadics => {
                diag.title("C-variadic parameters are forbidden in this context")
            }
            ErrorKind::ForbiddenInnerAttrs => {
                diag.title("inner attributes are forbidden in this context")
            }
            ErrorKind::ForbiddenOuterAttrs => {
                diag.title("outer attributes are forbidden in this context")
            }
            ErrorKind::ForbiddenSelfParams => {
                diag.title("self parameters are forbidden in this context")
            }
            ErrorKind::FrontmatterOpeningTooLarge => diag.title("frontmatter opening too large"),
            ErrorKind::GenericArgsOnFieldExpr => diag.title("generic args on field expression"),
            ErrorKind::ImplRestrictedTraitAlias => {
                diag.title("trait aliases cannot be impl-restricted")
            }
            ErrorKind::InvalidAbiStr => diag.title("invalid ABI string"),
            ErrorKind::InvalidAssocItemKind => diag.title("invalid associated item kind"),
            ErrorKind::InvalidDigit => diag.title("invalid digit"),
            ErrorKind::InvalidEscapeSequence => diag.title("invalid escape sequence"),
            ErrorKind::InvalidExprPrefix => diag.title("invalid expression modifiers"),
            ErrorKind::InvalidExternItemKind => diag.title("invalid extern item kind"),
            ErrorKind::InvalidExtraFieldProjections => diag.title("invalid extra field accesses"),
            ErrorKind::InvalidFrontmatterInfostring => diag.title("invalid frontmatter infostring"),
            ErrorKind::InvalidFrontmatterTrailer => {
                diag.title("extra characters after frontmatter closing")
            }
            ErrorKind::InvalidIdent(IdentKind::Normal, IdentMode::Raw) => {
                diag.title("invalid raw identifier")
            }
            ErrorKind::InvalidIdent(IdentKind::Normal, IdentMode::Keyword) => {
                diag.title("invalid stropped keyword")
            }
            ErrorKind::InvalidIdent(IdentKind::Ticked, IdentMode::Raw) => {
                diag.title("invalid raw ticked identifier")
            }
            ErrorKind::InvalidIdent(IdentKind::Ticked, IdentMode::Keyword) => {
                diag.title("invalid stropped ticked keyword")
            }
            ErrorKind::InvalidItemPrefix => diag.title("invalid item modifiers"),
            ErrorKind::InvalidLetChain => diag.title("invalid let-chain"),
            ErrorKind::InvalidLitSuffix => diag.title("invalid literal suffix"),
            ErrorKind::InvalidNumericIdent => diag.title("invalid numeric identifier"),
            ErrorKind::InvalidOpAfterBoundary => {
                diag.title("invalid operator following a boundary")
            }
            ErrorKind::InvalidOpAfterCast => diag.title("invalid operator following a cast"),
            ErrorKind::InvalidScalar(char, place) => {
                let place = match place {
                    InvalidScalarPlace::File => "",
                    InvalidScalarPlace::FrontmatterBody => " in frontmatter body",
                    InvalidScalarPlace::DocComment => " in doc comment",
                    InvalidScalarPlace::Lit => " in literal",
                };
                diag.title(format!("invalid scalar U+{:04X}{place}", char as u32))
            }
            ErrorKind::InvalidStrLitDelimiter => diag.title("invalid string literal delimiter"),
            ErrorKind::InvalidTraitBoundModifier => diag.title("invalid trait bound modifier"),
            ErrorKind::InvalidTyPrefix => diag.title("invalid type modifiers"),
            ErrorKind::MisplacedReceiver => diag.title("misplaced receiver"),
            ErrorKind::MissingClosingDelimiters => {
                diag.title("missing closing delimiter(s)").label("missing delimiter(s)")
            }
            ErrorKind::MultiScalarCharLit => diag.title("multi-scalar char literal"),
            ErrorKind::NonDecFloatLit => diag.title("non-decimal float literal"),
            ErrorKind::ParametrizedWhereClause => {
                diag.title("generic parameter lists on where-clauses are reserved")
            }
            ErrorKind::ParenthesizedGuardedPatInMatch => {
                diag.title("parenthesized guarded pattern in match expression")
            }
            ErrorKind::ReservedLabel => diag.title("reserved label"),
            ErrorKind::ReservedLifetime => diag.title("reserved lifetime"),
            ErrorKind::ReservedMultiHash => diag.title("reserved multi-hash"),
            ErrorKind::ReservedPrefix => diag.title("reserved prefix"),
            ErrorKind::ReuseInherentImpl => diag.title("inherent impls cannot be reused"),
            ErrorKind::StrLitGuardTooLarge => diag.title("string literal guard too large"),
            ErrorKind::TickFollowingRawTickedIdent => {
                diag.title("tick immediately following raw ticked identifier")
            }
            ErrorKind::TraitImplModifierInInherentImpl(modifier) => {
                diag.title(format!("trait impl modifier `{modifier}` in inherent impl"))
            }
            ErrorKind::TyRelMacroCall => diag.title("type-relative macro call"),
            ErrorKind::UnexpectedClosingDelimiter(actual) => {
                let actual = Token { kind: actual, span: self.span }
                    .to_diag_str(cx.file.as_ref().map(|file| file.source));
                diag.title(format!("found unexpected closing delimiter {actual}"))
                    .label("unexpected delimiter")
            }
            ErrorKind::UnexpectedToken(actual, expected) => {
                let actual = Token { kind: actual, span: self.span }
                    .to_diag_str(cx.file.as_ref().map(|file| file.source));
                let expected = expected.to_diag_str(());
                diag.title(format!("found {actual} but expected {expected}"))
                    .label("unexpected token")
            }
            ErrorKind::UnknownBuiltinSyntax => diag.title("unknown built-in syntax"),
            ErrorKind::UnsafeTraitAlias => diag.title("trait aliases cannot be marked `unsafe`"),
            ErrorKind::UnterminatedBlockComment => diag.title("unterminated block comment"),
            ErrorKind::UnterminatedCharLit => diag.title("unterminated char literal"),
            ErrorKind::UnterminatedFrontmatter => diag.title("unterminated frontmatter"),
            ErrorKind::UnterminatedStrLit => diag.title("unterminated string literal"),
            ErrorKind::VisibilityOnInvalidItem => {
                diag.title("this item kind may not be marked with visibility")
            }
        }
    }
}

pub(super) trait IntoDiag {
    fn into_diag(self, cx: &RenderCx<'_>) -> Diag;
}

pub(super) trait ToDiagStr {
    type Cx<'a>;

    fn to_diag_str(&self, cx: Self::Cx<'_>) -> Cow<'static, str>;
}

impl ToDiagStr for Token {
    type Cx<'a> = Option<&'a str>;

    fn to_diag_str(&self, source: Option<&str>) -> Cow<'static, str> {
        match (self.kind, source) {
            (TokenKind::CommonIdent, Some(source)) => {
                format!("identifier `{}`", source.at(self.span)).into()
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

impl<T: for<'a> ToDiagStr<Cx<'a> = ()>> ToDiagStr for List1<T> {
    type Cx<'a> = ();

    fn to_diag_str(&self, _: ()) -> Cow<'static, str> {
        self.iter()
            .map(|frag| frag.to_diag_str(()))
            .intersperse(Cow::Borrowed(" or "))
            .collect::<String>()
            .into()
    }
}

impl ToDiagStr for Fragment {
    type Cx<'a> = ();

    fn to_diag_str(&self, _: ()) -> Cow<'static, str> {
        match self {
            Self::Bound => "bound",
            Self::ConstArg => "const argument",
            Self::Expr => "expression",
            Self::ExtPath => "extended path",
            Self::GenericArg => "generic argument",
            Self::GenericParam => "generic parameter",
            Self::Item => "item",
            Self::Lit => "literal",
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
    level: Level,
    title: Option<Cow<'static, str>>,
    span: Option<(Span, Option<Cow<'static, str>>)>,
    subs: Vec<(ann::Level<'static>, Cow<'static, str>)>,
}

impl Diag {
    pub(super) fn new(level: Level) -> Self {
        Self { level, title: None, span: None, subs: Vec::new() }
    }

    pub(super) fn error(title: impl Into<Cow<'static, str>>) -> Self {
        Self::new(Level::Error).title(title)
    }

    pub(super) fn title(mut self, title: impl Into<Cow<'static, str>>) -> Self {
        self.title = Some(title.into());
        self
    }

    pub(super) fn span(mut self, span: Span) -> Self {
        self.span = Some((span, None));
        self
    }

    pub(super) fn label(mut self, label: impl Into<Cow<'static, str>>) -> Self {
        if let Some((_, label_)) = &mut self.span {
            *label_ = Some(label.into());
            self
        } else {
            self.note(label)
        }
    }

    pub(super) fn note(mut self, note: impl Into<Cow<'static, str>>) -> Self {
        self.subs.push((ann::Level::NOTE, note.into()));
        self
    }

    pub(super) fn help(mut self, message: impl Into<Cow<'static, str>>) -> Self {
        self.subs.push((ann::Level::HELP, message.into()));
        self
    }

    pub(super) fn render(self, cx: &RenderCx<'_>) {
        let level = match self.level {
            Level::Error => ann::Level::ERROR,
            Level::Warning => ann::Level::WARNING,
        };

        let mut group = match self.title {
            Some(title) => ann::Group::with_title(level.title(title)),
            None => ann::Group::with_level(level),
        };

        let path;
        if let Some((span, label)) = self.span {
            let file = cx.file.as_ref().expect("highlight requested but no source provided");

            path = match file.path {
                // FIXME: Being forced to use to_string_lossy is sad :(
                SourcePath::Real(path) => path.to_string_lossy(),
                SourcePath::Anon => "<anon>".into(),
            };

            let annotation = ann::AnnotationKind::Primary.span(span.into());
            let annotation = match label {
                Some(label) => annotation.label(label),
                None => annotation,
            };
            group =
                group.element(ann::Snippet::source(file.source).path(&path).annotation(annotation));
        }

        for (level, message) in self.subs {
            group = group.element(level.message(message));
        }

        let renderer = if cx.colorize { ann::Renderer::styled() } else { ann::Renderer::plain() };
        let diag = renderer.short_message(cx.short).render(&[group]);
        eprintln!("{diag}");
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Level {
    Error,
    Warning,
}

impl Level {
    pub(crate) fn apply<T>(self, result: &mut Result<T, ()>) {
        match self {
            Self::Error => *result = Err(()),
            Self::Warning => {}
        }
    }
}

pub(crate) struct RenderCx<'a> {
    colorize: bool,
    short: bool,
    pub(crate) file: Option<SourceFile<'a>>,
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
    pub(crate) source: &'a str,
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
