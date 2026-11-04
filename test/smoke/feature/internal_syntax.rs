// issue: <https://github.com/rust-lang/rust/issues/110680>
//: (2021->) accept (2015->2018)
fn func() {
    let k#deref(0);
    let k#deref(x | y);
    let _ = k#type_ascribe(!0, !);
    let _ = k#offset_of(Ty, x.0.1.y.z.2);
    let _ = k#wrap_binder(0 * 1);
    let _ = k#unwrap_binder(*&());
    let _: k#field_of((), 0);
}
