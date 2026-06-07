// issue: <https://github.com/rust-lang/rust/issues/130494>
//: accept

type Ty = &pin const ();
type Ty = &pin mut ();
type Ty = &'static pin const ();
type Ty = &&pin mut *mut ();
type Ty = &&pin const *const ();

fn func() {
    let &pin const 0;
    let &pin mut 0;
    let _ = &pin const {};
    let _ = &pin const const {};
    let _ = &pin mut 0;
    let _ = &pin const || ();
    let _ = &pin const const || ();
    let ref pin const x;
    let ref pin mut x;
    let mut ref pin const x;
    let mut ref pin mut x;
}

fn func(&pin const self) {}
fn func(&pin mut self) {}
fn func(&'static pin mut self) {}
fn func(&'static pin const self) {}
