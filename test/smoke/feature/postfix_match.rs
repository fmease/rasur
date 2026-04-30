// issue: <https://github.com/rust-lang/rust/issues/121618>
//: accept
#![rustfmt::skip]

const _: () = ().match {};
const _: () = ().match {}.match {};

fn func() {
    ().match {}
    0
}

fn func() {
    match ().match {} {}
    (match () {}).match {}
}

fn func() {
    ().match {
        #![r#match]
        () => {}.match {}
    }
    [].r#match
}
