// issue: <https://github.com/rust-lang/rust/issues/41517>
//: accept
#![rustfmt::skip]

trait Trait =;
trait Trait = Bound;
trait Trait = where;
trait Trait<> =;
const trait Trait = where (): Bound;
pub trait Trait = 'a + use<> + B +;
