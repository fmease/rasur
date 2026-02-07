// issue: <https://github.com/rust-lang/rust/issues/43122>
//: accept
#![rustfmt::skip]

fn scope() {
    let _ = static || {};
    static |_| ();
    static move | | [];
    _ = static move || ();
}
