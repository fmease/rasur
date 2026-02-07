// issue: <https://github.com/rust-lang/rust/issues/97362>
//: accept
#![rustfmt::skip]

fn func() {
    for<> || ();
    let _ = for<'a> |_| {};
    let _ = for<T> |_| {};
    let _ = for<const N: usize> |_| {};
    let _ = for<#[a] T> |_| {};
    let _ = for<#[a] const N: usize> |_| {};
    for<'a: 'static, 'b, T, U = (), T = (),> |_| {};

    // "lookalikes" (extended paths):
    for<T as Trait>::Ty in .. {}
    for<() as Trait>::Ty in .. {}
    for<(T)>::Ty in .. {}
}
