// issue: <https://github.com/rust-lang/rust/issues/143874>
//: accept
#![rustfmt::skip]

const trait Trait {}
const unsafe trait Trait {}
const auto trait Trait {}
const unsafe auto trait Trait {}

impl const Trait for () {}
impl const !Trait for () {}
impl const () {}
impl const impl Trait {}

const impl Trait for () {}
const impl !Trait for () {}
const impl () {}
const impl impl Trait {}

fn func<T: const Trait, U: [const] Trait, V: ~const Trait>()
where
    (): const Trait + [const] Trait + ~const Trait,
    for<> const Trait: for<> const Trait() -> (),
    for<> ~const Trait: for<> ~const Trait() -> (),
{}
