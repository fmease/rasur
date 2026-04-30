// issue: <https://github.com/rust-lang/rust/issues/139076>
//: accept

fn main() {
    super let _;
    super let v = ();
    #[a] super let ();

    super::r#let();
    super let super;
}
