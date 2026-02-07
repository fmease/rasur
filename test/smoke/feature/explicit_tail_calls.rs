// issue: <https://github.com/rust-lang/rust/issues/112788>
//: accept
#![rustfmt::skip]

const _: () = become 0;

fn func() {
    become();
    become become func;
    _ = if become {} {}
}
