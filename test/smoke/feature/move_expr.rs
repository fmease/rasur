// issue: <https://github.com/rust-lang/rust/issues/155050>.
//: accept

fn func() {
    move(0);
    move(move(0));
    move(move || 0);
    move || move(0);
    move(0) || 0;
    let _ = move(0);
    let _ = move || move(0);
    let _ = move(0) || 0;
    let _ = move(|| 0);
}
