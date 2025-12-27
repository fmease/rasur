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
    DefaultnessOnInvalidItem,
    ExpectedTraitFoundTy,
    GenericArgsOnFieldExpr(Span),
    HigherRankedBinderOnInvalidBound(Span),
    InvalidAssocItemKind(Span),
    InvalidExprPrefix(Span),
    InvalidExternItemKind(Span),
    InvalidItemPrefix(Span),
    InvalidLetChain,
    InvalidOpAfterCast,
    InvalidParenthesizedBound,
    InvalidRawIdent(Span),
    InvalidRawTickedIdent(Span),
    InvalidTyPrefix(Span),
    MisplacedReceiver,
    MissingClosingDelimiters(Span),
    ModifiersOnInvalidBound,
    OpCannotBeChained(String),
    ParametrizedWhereClause,
    ReservedLabel(Span),
    ReservedLifetime(Span),
    ReservedPrefix(Span),
    ReuseInherentImpl,
    TraitImplModifierInInherentImpl(&'static str),
    TyRelMacroCall,
    UnexpectedClosingDelimiter(Token),
    UnexpectedToken(Token, ExpectedFragment),
    UnknownBuiltInSyntax,
    UnsafeTraitAlias,
    UnterminatedBlockComment(Span),
    UnterminatedCharLit(Span),
    UnterminatedStrLit(Span),
    VisibilityOnInvalidItem,
    StrLitGuardTooLarge(Span),
    ReservedMultiHash(Span),
}
