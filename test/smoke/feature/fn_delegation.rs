// issue: <https://github.com/rust-lang/rust/issues/118212>
//: accept

reuse a::b::c;
reuse <Ty>::a::b;
reuse a::<>::b::<_>;
reuse crate::{a, b, c} { d * e }
reuse impl Trait for () {}
reuse unsafe impl Trait for () {}
reuse x as y;
reuse self::{x as y};
reuse a { reuse a { reuse a; } }

// not delegation
reuse!();
reuse::m!();

fn func() {
    // See also <https://github.com/rust-lang/rust/issues/148238>.
    reuse < reuse >::P;
    reuse << reuse >::P >::P;

    // not delegation
    reuse {};
    let _ = reuse {};
}
