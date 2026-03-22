use crate::{lexer::IdentKind, parser::ExpectedFragment, span::Span, token::Token};
use Default::default;
use std::cell::RefCell;

pub struct Buffer {
    raw: RawBuffer,
}

impl Buffer {
    pub const fn sealed() -> Self {
        Self { raw: RawBuffer::Seal }
    }

    pub(crate) fn add(&self, error: Error) {
        match &self.raw {
            RawBuffer::Seal => {}
            RawBuffer::Hold(errors) => errors.borrow_mut().push(error),
        }
    }

    pub(crate) fn extend(&self, other: Buffer) {
        let RawBuffer::Hold(this) = &self.raw else { return };
        let RawBuffer::Hold(that) = &other.raw else { return };
        this.borrow_mut().append(&mut *that.borrow_mut());
    }

    pub fn into_inner(self) -> Vec<Error> {
        match self.raw {
            RawBuffer::Seal => Vec::new(),
            RawBuffer::Hold(errors) => errors.into_inner(),
        }
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self { raw: RawBuffer::Hold(default()) }
    }
}

enum RawBuffer {
    Seal,
    Hold(RefCell<Vec<Error>>),
}

// FIXME: Overhaul this error type; most of the variants are just placeholders.
// FIXME: All errors should have spans
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub enum Error {
    AbiStrSuffix(Span),
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
    InvalidExtraFieldProjections(Span),
    InvalidFrontmatterInfostring(Span),
    InvalidFrontmatterTrailer(Span),
    InvalidItemPrefix(Span),
    InvalidLetChain(Span),
    InvalidLitSuffix(Span),
    InvalidNumericIdent(Span),
    InvalidOpAfterBoundary(Span),
    InvalidOpAfterCast(Span),
    InvalidParenthesizedBound,
    InvalidRawIdent(IdentKind, Span),
    InvalidScalar(char, InvalidScalarPlace, Span),
    InvalidStrLitDelimiter(Span),
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
    TickFollowingRawTickedIdent(Span),
}

#[derive(Clone, Copy, Debug)]
pub enum InvalidScalarPlace {
    DocComment,
    File,
    FrontmatterBody,
    Lit,
}
