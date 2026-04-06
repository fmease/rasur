//! "`SmallVec<T, 1>` at home".
#![feature(decl_macro, const_default, const_trait_impl, deref_pure_trait)]

use std::fmt;

pub macro list1 {
    () => { List1::default() },
    ($item:expr $(,)?) => { List1::single($item) },
    ($( $item:expr ),+ $(,)?) => { List1::multi(vec![$($item),+]) },
}

#[derive(Clone)]
pub struct List1<T> {
    raw: RawList1<T>,
}

impl<T> List1<T> {
    #[doc(hidden)]
    pub const fn single(item: T) -> Self {
        Self { raw: RawList1::Inline(Some(item)) }
    }

    #[doc(hidden)]
    pub fn multi(items: Vec<T>) -> Self {
        Self { raw: RawList1::OutOfLine(items) }
    }

    pub fn push(&mut self, item: T) {
        match &mut self.raw {
            RawList1::Inline(place) => match place.take() {
                None => *place = Some(item),
                Some(first) => self.raw = RawList1::OutOfLine(vec![first, item]),
            },
            RawList1::OutOfLine(items) => items.push(item),
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        match &mut self.raw {
            RawList1::Inline(place) => place.take(),
            RawList1::OutOfLine(items) => items.pop(),
        }
    }

    pub fn as_slice(&self) -> &[T] {
        match &self.raw {
            RawList1::Inline(None) => &[],
            RawList1::Inline(Some(item)) => std::slice::from_ref(item),
            RawList1::OutOfLine(items) => items,
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.as_slice().iter()
    }
}

impl<T> const Default for List1<T> {
    fn default() -> Self {
        Self { raw: RawList1::Inline(None) }
    }
}

impl<T> std::ops::Deref for List1<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

unsafe impl<T> std::ops::DerefPure for List1<T> {}

impl<T: fmt::Debug> fmt::Debug for List1<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

#[derive(Clone, Debug)]
enum RawList1<T> {
    Inline(Option<T>),
    OutOfLine(Vec<T>),
}
