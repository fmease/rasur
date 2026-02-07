// issue: <https://github.com/rust-lang/rust/issues/39412>
//: accept

macro makro {}
pub macro makro { @ @ @ }
macro makro() { @ @ @ }
