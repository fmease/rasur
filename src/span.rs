use std::{fmt, ops::Range};

type RawByteIndex = u32;

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct ByteIndex {
    raw: RawByteIndex,
}

impl ByteIndex {
    pub(crate) const fn from(index: usize) -> Self {
        debug_assert!(index as u64 <= RawByteIndex::MAX as u64);
        Self { raw: index as _ }
    }

    pub(crate) fn into(self) -> usize {
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

#[derive(Clone, Copy)]
pub struct Span {
    pub(crate) start: ByteIndex,
    pub(crate) end: ByteIndex,
}

impl Span {
    pub(crate) const fn new(start: ByteIndex, end: ByteIndex) -> Self {
        Self { start, end }
    }

    pub(crate) fn to(self, other: impl Into<Option<Self>>) -> Self {
        Self { end: other.into().unwrap_or(self).end, ..self }
    }

    pub(crate) fn until(self, other: Span) -> Span {
        Self { end: other.start, ..self }
    }

    pub fn range(self) -> Range<usize> {
        self.start.raw as usize..self.end.raw as usize
    }

    pub(crate) fn len(self) -> u32 {
        self.end.raw - self.start.raw
    }

    pub(crate) fn start(self) -> Span {
        Self { end: self.start, ..self }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}
