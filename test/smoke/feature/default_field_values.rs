// issue: <https://github.com/rust-lang/rust/issues/132162>
//: accept

struct Type {
    x: i32 = 0,
    pub x: i32 = { 1 + 2 },
    unsafe x: i32 = unsafe { 0 },
    #[a] x: Trait() -> Trait += (),
}

struct Type(i32 = 0, pub i32 = {}, #[a] [[[_]]] = ..);

fn func() {
    // This was already syntactically legal before the introduction of DFVs.
    // Still, I'm listing it here because it's related.
    _ = Type { .. };
    _ = Type { x, x: x, .. };
}
