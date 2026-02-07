// issue: <https://github.com/rust-lang/rust/issues/149226>
//: accept

const {}

const {
    scope();
    loop {}
}

#[cfg(false)]
const { "..." }

const fn func() {}
