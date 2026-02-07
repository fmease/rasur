// issue: <https://github.com/rust-lang/rust/issues/106003>
//: accept
#![rustfmt::skip]

fn func() {
    let _ = const || {};
    let _ = const |_| ();
    let _ = for<> const || {};
    let _ = const move || {};
    let _ = const use || {};
    const | | {};
}
