// issue: <https://github.com/rust-lang/rust/issues/118898>
//: (2018->) accept (2015) reject
#![rustfmt::skip]

fn func() {
    for await x in xs {}
    for await _ in xs.await {}
    for await () in xs {}
    for await! in xs {}
    for await[0] in xs {}
}
