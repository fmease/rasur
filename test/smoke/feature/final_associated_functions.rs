// issue: <https://github.com/rust-lang/rust/issues/118212>
//: accept
#![rustfmt::skip]

final fn func() {}
extern { final fn func() {} }
trait Trait { final fn func(); }
impl Type { final fn func() {} }
fn func() { final fn func() {} }

pub final const fn func() {}
