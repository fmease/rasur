use crate::{lexer::IdentKind, parser::Fragment, span::Span, token::TokenKind};
pub use utility::List1;

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Error {
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

// FIXME: Overhaul this error type; most of the variants are just placeholders.
#[derive(Clone, Debug)]
pub enum ErrorKind {
    AbiStrSuffix,
    AmbiguousPlus,
    AutoTraitAlias,
    ChainedComparison,
    DefaultOnInvalidItem,
    EmptyCharLit,
    EmptyExponent,
    EmptyNumLit,
    ExpectedTraitFoundTy,
    FinalOnInvalidItem,
    ForbiddenCVariadics,
    ForbiddenInnerAttrs,
    ForbiddenOuterAttrs,
    ForbiddenSelfParams,
    FrontmatterOpeningTooLarge,
    GenericArgsOnFieldExpr,
    ImplRestrictedTraitAlias,
    InvalidAbiStr,
    InvalidAssocItemKind,
    InvalidDigit,
    InvalidEscapeSequence,
    InvalidExprPrefix,
    InvalidExternItemKind,
    InvalidExtraFieldProjections,
    InvalidFrontmatterInfostring,
    InvalidFrontmatterTrailer,
    InvalidItemPrefix,
    InvalidLetChain,
    InvalidLitSuffix,
    InvalidNumericIdent,
    InvalidOpAfterBoundary,
    InvalidOpAfterCast,
    InvalidRawIdent(IdentKind),
    InvalidScalar(char, InvalidScalarPlace),
    InvalidStrLitDelimiter,
    InvalidTraitBoundModifier,
    InvalidTyPrefix,
    LifetimeObjectTyWithoutPlus,
    MisplacedReceiver,
    MissingClosingDelimiters,
    MultiScalarCharLit,
    NonDecFloatLit,
    ParametrizedWhereClause,
    ParenthesizedGuardedPatInMatch,
    ReservedLabel,
    ReservedLifetime,
    ReservedMultiHash,
    ReservedPrefix,
    ReuseInherentImpl,
    StrLitGuardTooLarge,
    TraitImplModifierInInherentImpl(&'static str),
    TyRelMacroCall,
    UnexpectedClosingDelimiter(TokenKind),
    UnexpectedToken(TokenKind, List1<Fragment>),
    UnknownBuiltinSyntax,
    UnsafeTraitAlias,
    UnterminatedBlockComment,
    UnterminatedCharLit,
    UnterminatedFrontmatter,
    UnterminatedStrLit,
    VisibilityOnInvalidItem,
    TickFollowingRawTickedIdent,
}

#[derive(Clone, Copy, Debug)]
pub enum InvalidScalarPlace {
    DocComment,
    File,
    FrontmatterBody,
    Lit,
}
