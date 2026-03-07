use crate::span::{ByteIndex, Span};
use std::str::Chars;

pub(crate) struct Cutter<'src> {
    chars: Chars<'src>,
    index: ByteIndex,
}

impl<'src> Cutter<'src> {
    pub(crate) fn new(source: &'src str, offset: ByteIndex) -> Self {
        Self { chars: source[offset.value()..].chars(), index: offset }
    }

    pub(crate) fn advance(&mut self) -> Option<(ByteIndex, char)> {
        self.chars.next().map(|char| {
            let index = self.index();
            self.index += ByteIndex::new(char.len_utf8());
            (index, char)
        })
    }

    pub(crate) fn advance_while(&mut self, predicate: impl Fn(char) -> bool) {
        while let Some(char) = self.peek()
            && predicate(char)
        {
            self.advance();
        }
    }

    pub(crate) fn snapshot(&self) -> impl Iterator<Item = char> + use<'src> {
        self.chars.clone()
    }

    pub(crate) fn peek(&mut self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next()
    }

    pub(crate) fn index(&self) -> ByteIndex {
        self.index
    }

    pub(crate) fn span(&self, start: ByteIndex) -> Span {
        Span::new(start, self.index())
    }
}
