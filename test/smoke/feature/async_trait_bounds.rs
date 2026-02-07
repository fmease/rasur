// issue: <https://github.com/rust-lang/rust/issues/62290>
//: (2018->) accept (2015) reject
#![rustfmt::skip]

fn func<T: async Trait>(_: dyn async Trait) -> impl async Trait
where
    T: async Trait,
    (): const async Trait,
    for<> async Trait(): for<> async Trait() -> (),
{
}
