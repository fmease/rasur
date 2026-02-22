use crate::{parser::ExpectedFragment, span::Span, token::Token};

pub enum Buffer {
    Void,
    Hold(Vec<Error>),
}

impl Buffer {
    #[must_use]
    pub fn non_empty(self) -> Option<Vec<Error>> {
        match self {
            Self::Void => None,
            Self::Hold(errors) if errors.is_empty() => None,
            Self::Hold(errors) => Some(errors),
        }
    }

    pub(crate) fn add(&mut self, error: Error) {
        match self {
            Self::Void => {}
            Self::Hold(errors) => errors.push(error),
        }
    }

    pub(crate) fn extend(&mut self, other: Buffer) {
        if let (Self::Hold(this), Self::Hold(mut other)) = (self, other) {
            this.append(&mut other);
        }
    }
}

// FIXME: Overhaul this error type; most of the variants are just placeholders.
// FIXME: All errors should have spans
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub enum Error {
    AmbiguousPlus(Span),
    AutoTraitAlias,
    ChainedComparison(Span),
    DefaultOnInvalidItem(Span),
    EmptyCharLit(Span),
    EmptyExponent(Span),
    EmptyNumLit(Span),
    ExpectedTraitFoundTy(Span),
    FinalOnInvalidItem(Span),
    ForbiddenInnerAttrs,
    ForbiddenOuterAttrs,
    FrontmatterOpeningTooLarge(Span),
    GenericArgsOnFieldExpr(Span),
    HigherRankedBinderOnInvalidBound(Span),
    ImplRestrictedTraitAlias,
    InvalidAbiStr(Span),
    InvalidAssocItemKind(Span),
    InvalidDigit(Span),
    InvalidEscapeSequence(Span),
    InvalidExprPrefix(Span),
    InvalidExternItemKind(Span),
    InvalidFrontmatterInfostring(Span),
    InvalidFrontmatterTrailer(Span),
    InvalidItemPrefix(Span),
    InvalidLetChain,
    InvalidLitSuffix(Span),
    InvalidOpAfterCast(Span),
    InvalidParenthesizedBound,
    InvalidRawIdent(Span),
    InvalidRawTickedIdent(Span),
    InvalidScalar(char, InvalidScalarPlace, Span),
    InvalidStrLitDelim(Span),
    InvalidTyPrefix(Span),
    LifetimeObjectTyWithoutPlus(Span),
    MisplacedReceiver(Span),
    MissingClosingDelimiters(Span),
    ModifiersOnInvalidBound,
    MultiScalarCharLit(Span),
    NonDecFloatLit(Span),
    ParametrizedWhereClause(Span),
    ParenthesizedGuardedPatInMatch,
    ReservedLabel(Span),
    ReservedLifetime(Span),
    ReservedMultiHash(Span),
    ReservedPrefix(Span),
    ReuseInherentImpl,
    StrLitGuardTooLarge(Span),
    TraitImplModifierInInherentImpl(&'static str),
    TyRelMacroCall(Span),
    UnexpectedClosingDelimiter(Token),
    UnexpectedToken(Token, ExpectedFragment),
    UnknownBuiltinSyntax(Span),
    UnsafeTraitAlias,
    UnterminatedBlockComment(Span),
    UnterminatedCharLit(Span),
    UnterminatedFrontmatter(Span),
    UnterminatedStrLit(Span),
    VisibilityOnInvalidItem(Span),
}

#[derive(Clone, Copy, Debug)]
pub enum InvalidScalarPlace {
    DocComment,
    File,
    FrontmatterBody,
    Lit,
}
