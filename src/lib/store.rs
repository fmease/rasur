use crate::{error::Error, feature::Feature, span::Span};
use Default::default;
use std::cell::RefCell;

#[derive(Default)]
pub struct Store {
    pub errors: Buffer<Error>,
    pub features: Buffer<(Feature, Span)>,
}

impl Store {
    pub const fn sealed() -> Self {
        Self { errors: Buffer::sealed(), features: Buffer::sealed() }
    }
}

pub struct Buffer<T> {
    raw: RawBuffer<T>,
}

impl<T> Buffer<T> {
    pub const fn sealed() -> Self {
        Self { raw: RawBuffer::Seal }
    }

    pub(crate) fn add(&self, error: T) {
        match &self.raw {
            RawBuffer::Seal => {}
            RawBuffer::Hold(errors) => errors.borrow_mut().push(error),
        }
    }

    pub(crate) fn extend(&self, other: Self) {
        let RawBuffer::Hold(this) = &self.raw else { return };
        let RawBuffer::Hold(that) = &other.raw else { return };
        this.borrow_mut().append(&mut *that.borrow_mut());
    }

    pub fn into_inner(self) -> Vec<T> {
        match self.raw {
            RawBuffer::Seal => Vec::new(),
            RawBuffer::Hold(errors) => errors.into_inner(),
        }
    }
}

impl<T> Default for Buffer<T> {
    fn default() -> Self {
        Self { raw: RawBuffer::Hold(default()) }
    }
}

impl<T> IntoIterator for Buffer<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_inner().into_iter()
    }
}

enum RawBuffer<T> {
    Seal,
    // FIXME: Can we get rid of the `RefCell` again? It definitely used to be necessary once.
    Hold(RefCell<Vec<T>>),
}
