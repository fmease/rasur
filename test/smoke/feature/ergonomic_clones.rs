// issue: <https://github.com/rust-lang/rust/issues/132290>
//: (2018->) accept (2015) reject
#![rustfmt::skip]

fn func() {
    {}.use;
    {}.use.use;
    async use {};
    gen use {};
    use || ();
    use | | ();
    if async gen use {} {};

    use {}; // not an ergonomic clone, just an empty import
}
