// issue: <https://github.com/rust-lang/rust/issues/43122>
//: accept

fn func() {
    yield;
    yield ();
    yield yield;
    yield *yield;
    ().yield;
    ().yield.yield;
    _ = if yield {} {};
    _ = if {}.yield {};
}
