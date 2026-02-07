// issue: <https://github.com/rust-lang/rust/issues/96373>
//: accept
#![rustfmt::skip]

fn func() {
    do yeet;
    do yeet 0;
    do yeet::it;
    let _ = do yeet ();
    _ = if do yeet {} {};
    let r#do;
    let yeet;
}
