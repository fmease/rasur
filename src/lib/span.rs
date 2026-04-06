use std::{fmt, ops::Range};

type RawByteIndex = u32;

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct ByteIndex {
    raw: RawByteIndex,
}

impl ByteIndex {
    pub const fn new(index: usize) -> Self {
        debug_assert!(index as u64 <= RawByteIndex::MAX as u64);
        #[expect(clippy::cast_possible_truncation)]
        Self { raw: index as _ }
    }

    pub const fn value(self) -> usize {
        self.raw as _
    }
}

impl fmt::Debug for ByteIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

impl std::ops::Add for ByteIndex {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self { raw: self.raw + other.raw }
    }
}

impl std::ops::AddAssign for ByteIndex {
    fn add_assign(&mut self, rhs: Self) {
        self.raw += rhs.raw;
    }
}

// FIXME: Make the fields fully private.
#[derive(Clone, Copy, Default)]
pub struct Span {
    pub(crate) start: ByteIndex,
    pub(crate) end: ByteIndex,
}

impl Span {
    pub const fn new(start: ByteIndex, end: ByteIndex) -> Self {
        debug_assert!(start.raw <= end.raw);

        Self { start, end }
    }

    pub fn to(self, other: Self) -> Self {
        Self::new(self.start, other.end)
    }

    pub fn until(self, other: Span) -> Span {
        Self::new(self.start, other.start)
    }

    #[must_use]
    pub const fn range(self) -> Range<usize> {
        self.start.raw as usize..self.end.raw as usize
    }

    pub fn len(self) -> usize {
        (self.end.raw - self.start.raw) as _
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    pub const fn start(self) -> ByteIndex {
        self.start
    }

    pub const fn end(self) -> ByteIndex {
        self.end
    }
}

impl From<ByteIndex> for Span {
    fn from(index: ByteIndex) -> Self {
        Self { start: index, end: index }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

pub struct Spanned<T> {
    pub bare: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub const fn new(bare: T, span: Span) -> Self {
        Self { bare, span }
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.bare, self.span)
    }
}
