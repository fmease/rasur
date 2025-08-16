use std::{fmt, ops::Range};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct ByteIndex(u32);

impl ByteIndex {
    pub(crate) fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }
}

impl std::ops::AddAssign<u32> for ByteIndex {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Span {
    pub(crate) start: ByteIndex,
    pub(crate) end: ByteIndex,
}

impl Span {
    pub(crate) const fn new(start: ByteIndex, end: ByteIndex) -> Self {
        Self { start, end }
    }
}

impl Span {
    pub(crate) fn to(self, other: impl Into<Option<Self>>) -> Self {
        Self { end: other.into().unwrap_or(self).end, ..self }
    }

    pub(crate) fn until(self, other: Span) -> Span {
        Self { end: other.start, ..self }
    }

    pub(crate) fn range(self) -> Range<usize> {
        self.start.0 as usize..self.end.0 as usize
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start.0, self.end.0)
    }
}
