// issue: <https://github.com/rust-lang/rust/issues/123076>
//: accept

fn func(mut ref v: (), mut ref mut w: ()) {
    let mut ref v @ ();
    let mut ref mut w @ ();
    let mut ref pin mut v;
    let mut ref pin const w;
}
