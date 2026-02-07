// issue: <https://github.com/rust-lang/rust/issues/130516>
//: accept

type Ty = unsafe<> fn();
type Ty = unsafe<'a> &'a ();
type Ty = unsafe<T: Bound<()>> unsafe<const N: usize> unsafe fn();
type Ty = unsafe < #[a] 'a >;
