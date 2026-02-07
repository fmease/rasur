// issue: <https://github.com/rust-lang/rust/issues/110680>
//: accept

fn func() {
    let builtin # deref(0);
    let builtin # deref(x | y);
    let _ = builtin # type_ascribe(!0, !);
    let _ = builtin # offset_of(Ty, x.0.1.y.z.2);
    let _ = builtin # wrap_binder(0 * 1);
    let _ = builtin # unwrap_binder(*&());
    let _: builtin # field_of((), 0);
}
