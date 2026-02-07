// issue: <https://github.com/rust-lang/rust/issues/68318>
//: accept
#![rustfmt::skip]

impl !Trait for () {}
impl<> !Trait for () {}
impl const !Trait for () {}
impl !::Trait for () {}


impl ! {} // not a negative impl
r#impl! {} // likewise
