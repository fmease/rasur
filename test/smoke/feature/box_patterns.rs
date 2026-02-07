// issue: <https://github.com/rust-lang/rust/issues/29641>
//: accept
#![rustfmt::skip]

fn func(box (): Ty) {
    let box _;
    let box x;
    let box ref x;
    let box mut ref x;
    let box [];
    let Ty { x: box _ };
    let Ty { box x };
    let Ty { box ref x };
    let Ty { box mut ref x };

    match () {
        |box 0| box 1 => |box 0| 1,
    }
}
