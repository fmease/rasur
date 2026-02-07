// issue: <https://github.com/rust-lang/rust/issues/132922>
//: accept

struct Ty {
    unsafe field: unsafe fn(),
    #[a]
    unsafe r#unsafe: (),
}

enum Ty {
    Variant { unsafe field: () },
}

struct Ty(
    unsafe fn(), // not an unsafe field actually
);
