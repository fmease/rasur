// In Rust 2015, the `await (x)` is just tuple struct pattern.
//: (2015) accept (2018->) reject

fn func() {
    for await (x) in [] {}
    struct await(i32);
}
