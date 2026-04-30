// issue: <https://github.com/rust-lang/rust/issues/31436>
//: (2018->) accept (2015) reject
#![rustfmt::skip]

fn func() {
    try {}
    try { #![a] }
    let _ = try { let _; () };
    r#try {}
}
