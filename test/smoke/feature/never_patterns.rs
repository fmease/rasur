// issue: <https://github.com/rust-lang/rust/issues/118155>
//: accept

fn func() {
    let !;
    let!;
    let(!);
    match () { () }
    match () {
        |!|!,
        (!,),
        (m!()),
        Some(_)
    }
}
