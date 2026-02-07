// issue: <https://github.com/rust-lang/rust/issues/128044>
//: accept
#![rustfmt::skip]

fn func() contract_requires {} {}
fn func() contract_ensures () {}
fn func() contract_ensures {};
fn func() contract_requires { 0 } contract_ensures 0 {}
fn func() -> () contract_requires { 0 } contract_ensures 0 {}
fn func()
contract_requires { fn func(); }
contract_ensures 1 + 2 * 3;

// "lookalikes" (paths):
fn func() -> contract_requires { 0 }
fn func() -> contract_ensures { 0 }
