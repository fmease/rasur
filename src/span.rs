use std::{fmt, ops::Range};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct ByteIndex(u32);

impl ByteIndex {
    pub(crate) fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Span {
    pub(crate) start: ByteIndex,
    pub(crate) end: ByteIndex,
}

impl Span {
    pub(crate) fn range(self) -> Range<usize> {
        self.start.0 as usize..self.end.0 as usize
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start.0, self.end.0)
    }
}
