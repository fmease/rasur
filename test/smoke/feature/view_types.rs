// issue: <https://github.com/rust-lang/rust/issues/155938>
//: accept

type Ty = &().{};
type Ty = &'static mut [_].{ a, b, c };
type Ty = & &().{}.{};
type Ty = &&().{}.{}; // DoubleAmpersand
type Ty = &mut !.{ e, };
