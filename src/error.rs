use crate::{parser::ExpectedFragment, span::Span, token::Token};

pub enum Buffer {
    Void,
    Hold(Vec<Error>),
}

impl Buffer {
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
    AutoTraitAlias,
    DefaultnessOnInvalidItem(Span),
    EmptyCharLit(Span),
    EmptyExponent(Span),
    EmptyNumLit(Span),
    ExpectedTraitFoundTy,
    FrontmatterClosingTrailer(Span),
    FrontmatterOpeningTooLarge(Span),
    GenericArgsOnFieldExpr(Span),
    HigherRankedBinderOnInvalidBound(Span),
    InvalidAbiStr(Span),
    InvalidAssocItemKind(Span),
    InvalidDigit(Span),
    InvalidEscapeSequence(Span),
    InvalidExprPrefix(Span),
    InvalidExternItemKind(Span),
    InvalidFrontmatterInfostring(Span),
    InvalidItemPrefix(Span),
    InvalidLetChain,
    InvalidLitSuffix(Span),
    InvalidOpAfterCast,
    InvalidParenthesizedBound,
    InvalidRawIdent(Span),
    InvalidRawTickedIdent(Span),
    InvalidScalarInLit(Span),
    InvalidStrLitDelim(Span),
    InvalidToken(char, Span),
    InvalidTyPrefix(Span),
    MisplacedReceiver,
    MissingClosingDelimiters(Span),
    ModifiersOnInvalidBound,
    MultiScalarCharLit(Span),
    NonDecFloatLit(Span),
    OpCannotBeChained(String),
    ParametrizedWhereClause,
    ParenthesizedGuardedPatInMatch,
    ReservedLabel(Span),
    ReservedLifetime(Span),
    ReservedMultiHash(Span),
    ReservedPrefix(Span),
    ReuseInherentImpl,
    StrLitGuardTooLarge(Span),
    TraitImplModifierInInherentImpl(&'static str),
    TyRelMacroCall,
    UnexpectedClosingDelimiter(Token),
    UnexpectedToken(Token, ExpectedFragment),
    UnknownBuiltInSyntax(Span),
    UnsafeTraitAlias,
    UnterminatedBlockComment(Span),
    UnterminatedCharLit(Span),
    UnterminatedFrontmatter(Span),
    UnterminatedStrLit(Span),
    VisibilityOnInvalidItem(Span),
}
