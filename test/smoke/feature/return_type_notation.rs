// issue: <https://github.com/rust-lang/rust/issues/109417>
//: accept
#![rustfmt::skip]

type Ty = f(..);
type Ty = f::(..);
type Ty = T::f(..)::g(..)::h(..);
type Ty = Ty<f(..), f(..):, f::(..), f::(..):>;
type Ty = dyn Trait<f(..): Bound>;
type Ty = impl Trait<f(..): 'a + use<'_>>;

fn func() {
    let f::(..) = f::(..);
    let f(..) = f(..); // not RTN
}
