use super::super::Fragment;
use super::{parse_expr, parse_file, parse_item, parse_stmt, t};
use crate::{
    ast,
    edition::Edition::*,
    error::Error,
    token::{Token, TokenKind},
};
use deref as r;

#[test]
fn tuple_struct_field_visibility() {
    t!(
        parse_item,
        Rust2015,
        "struct T(pub([i32; 2]));",
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Public,
                    ty: ast::Ty::Grouped(r!(ast::Ty::Array(..))),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(crate)[i32]);",
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Restricted(ast::Path {
                        segs: r!([ast::PathSeg { ident: ast::Ident!("crate"), .. }])
                    }),
                    ty: ast::Ty::Slice(_),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(self)&());",
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Restricted(ast::Path {
                        segs: r!([ast::PathSeg { ident: ast::Ident!("self"), .. }])
                    }),
                    ty: ast::Ty::Ref(_),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    // issue: <https://github.com/fmease/rasur/issues/21>
    t!(
        parse_item,
        Rust2015,
        "struct T(pub(super::U));",
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Public,
                    ty: ast::Ty::Grouped(r!(ast::Ty::Path(ast::ExtPath {
                        ext: None,
                        path: ast::Path {
                            segs: r!([
                                ast::PathSeg { ident: ast::Ident!("super"), .. },
                                ast::PathSeg { ident: ast::Ident!("U"), .. },
                            ])
                        }
                    }))),
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(super::U)impl);",
        Err(r!([Error::UnexpectedToken(Token { kind: TokenKind::Impl, .. }, _)])),
    );

    t!(
        parse_item,
        Rust2015,
        "struct T(pub(in super::U)!);",
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(r!(ast::StructItem {
                kind: ast::VariantKind::Tuple(r!([ast::TupleFieldDef {
                    vis: ast::Visibility::Restricted(ast::Path {
                        segs: r!([
                            ast::PathSeg { ident: ast::Ident!("super"), .. },
                            ast::PathSeg { ident: ast::Ident!("U"), .. },
                        ])
                    }),
                    ty: ast::Ty::Never,
                    ..
                }])),
                ..
            })),
            ..
        })
    );

    t!(
        parse_item,
        Rust2015,
        "struct T(pub);",
        Err(r!([Error::UnexpectedToken(
            Token { kind: TokenKind::CloseRoundBracket, .. },
            r!([Fragment::Ty])
        )]))
    );
}

// FIXME: More extensively test receivers & fn params! Below are just temporary smoke tests.
#[test]
fn method_receivers() {
    t!(parse_item, Rust2015, "fn f(&self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&'a self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&'a mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&'a pin mut self);", Ok(_));
    t!(parse_item, Rust2015, "fn f(&pin const self);", Ok(_));

    // issue: <https://github.com/fmease/rasur/issues/18>
    t!(parse_item, Rust2015, "fn f(self::T: ());", Ok(_));
    t!(parse_item, Rust2015, "fn f(&self::T: ());", Ok(_));
    t!(parse_item, Rust2015, "fn f(&mut self::T: ());", Ok(_));
}

#[test]
fn item_modifiers_in_item_ctxt() {
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    t!(
        parse_file,
        Rust2024, // for `async` and `gen`
        r#"
async extern fn f() {}
async fn f() {}
async gen fn f() {}
async gen safe fn f() {}
async gen unsafe fn f() {}
async safe extern fn f() {}
async safe fn f() {}
async unsafe extern fn f() {}
async unsafe fn f() {}
auto impl(crate) trait Trait {}
auto impl(in crate) trait Trait {}
auto trait Trait {}
const F: () = ();
const async fn f() {}
const async gen safe extern "C" fn f() {}
const async gen safe fn f() {}
const async safe extern fn f() {}
const async safe fn f() {}
const async unsafe extern fn f() {}
const async unsafe fn f() {}
const auto trait Trait {}
const auto: () = (); // [!]
const extern "C" fn f() {}
const extern fn f() {}
const gen fn f() {}
const impl !Trait for () {}
const impl () {}
const impl Trait for () {}
const safe extern fn f() {} // [***]
const safe fn f() {} // [***]
const safe: () = (); // [!]
const trait Trait {}
const unsafe auto trait Trait {} // [***]
const unsafe extern "C" fn f() {}
const unsafe impl Trait for () {} // [***]
const unsafe trait Trait {} // [***]
default const F: ();
default fn f();
default type T;
extern "C" fn f() {}
extern "C" {}
extern crate krate;
extern fn f() {}
extern {}
final fn f();
final type T;
fn f() {}
fn wrap() { safe fn f() {} } // [***]
gen extern fn f() {}
gen fn f() {}
gen unsafe fn f() {}
impl !Trait for () {}
impl Trait for () {}
impl const Trait for () {}
pub const extern "C" fn f() {}
pub const fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
pub default const async gen unsafe extern "C" fn f();
pub final const async gen unsafe extern "C" fn f();
pub fn f() {}
reuse const impl Trait for () {}
reuse const unsafe impl !Trait for () {} // [***]
reuse f;
reuse impl Trait for () {}
reuse unsafe impl Trait for () {}
safe extern "C" fn f() {}
safe extern fn f() {}
safe fn f() {}
safe static X: ();
static safe: ();
trait Trait {}
type const F: ();
type const safe: (); // [!]
unsafe auto trait Trait {}
unsafe extern "C" fn f() {}
unsafe extern "C" {}
unsafe extern {}
unsafe fn f() {}
unsafe impl Trait for () {}
unsafe impl const !Trait for () {}
unsafe impl const Trait for () {}
unsafe mod m;
unsafe static X: ();
unsafe trait Trait {}
use f;
use {self::*, self::{}};
"#,
        Ok(_) // just a smoke test
    );
}

#[test]
fn item_modifiers_in_stmt_ctxt() {
    // FIXME: Re-audit:
    // NOTE: Test cases marked `[***]` actually get rejected by rustc
    //       but they should compile in my opinion.
    //       See also <https://github.com/rust-lang/rust/issues/146122>.

    // NOTE: Commented-out "test cases" marked `[???]` don't get accepted
    //       by either rustc or rasur but I feel like they should be
    //       supported "logically" or for consistency.

    t!(
        parse_expr,
        Rust2024, // for `async` and `gen`
        r#"{
async extern fn f() {}
async fn f() {}
async gen fn f() {}
async gen safe fn f() {}
async gen unsafe fn f() {}
async safe extern fn f() {}
async safe fn f() {}
async unsafe extern fn f() {}
async unsafe fn f() {}
auto impl(crate) trait Trait {}
auto impl(in crate) trait Trait {}
auto trait Trait {}
const F: () = ();
const async fn f() {}
const async gen safe extern "C" fn f() {}
const async gen safe fn f() {}
const async safe extern fn f() {}
const async safe fn f() {}
const async unsafe extern fn f() {}
const async unsafe fn f() {}
const auto trait Trait {}
const auto: () = (); // [!]
const extern "C" fn f() {}
const extern fn f() {}
const gen fn f() {}
const impl !Trait for () {}
const impl () {}
const impl Trait for () {}
const safe extern fn f() {} // [***]
const safe fn f() {} // [***]
const safe: () = (); // [!]
const trait Trait {}
const unsafe auto trait Trait {} // [***]
const unsafe extern "C" fn f() {}
const unsafe impl Trait for () {} // [***]
const unsafe trait Trait {} // [***]
// default const F: (); // [???]
// default fn f(); // [???]
// default type T; // [???]
extern "C" fn f() {}
extern "C" {}
extern crate krate;
extern fn f() {}
extern {}
final fn f(); // issue: <https://github.com/fmease/rasur/issues/26>
final type T;
fn f() {}
fn wrap() { safe fn f() {} } // [***]
gen extern fn f() {}
gen fn f() {}
gen unsafe fn f() {}
impl !Trait for () {}
impl Trait for () {}
impl const Trait for () {}
pub const extern "C" fn f() {}
pub const fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
// pub default const async gen unsafe extern "C" fn f(); // [???]
pub final const async gen unsafe extern "C" fn f();
pub fn f() {}
reuse const impl Trait for () {}
reuse const unsafe impl !Trait for () {} // [***]
reuse f;
reuse impl Trait for () {}
reuse unsafe impl Trait for () {}
safe extern "C" fn f() {}
safe extern fn f() {}
safe fn f() {}
safe static X: ();
static safe: ();
trait Trait {}
type const F: ();
type const safe: (); // [!]
unsafe auto trait Trait {}
unsafe extern "C" fn f() {}
unsafe extern "C" {}
unsafe extern {}
unsafe fn f() {}
unsafe impl Trait for () {}
unsafe impl const !Trait for () {}
unsafe impl const Trait for () {}
unsafe mod m;
unsafe static X: ();
unsafe trait Trait {}
use f;
use {self::*, self::{}};
}"#,
        Ok(_) // just a smoke test
    );

    // Make sure that we don't consider these weak / context-dependent keywords as item modifiers:
    t!(
        parse_stmt,
        Rust2015,
        "auto as _",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::Cast(
                    r!(ast::Expr {
                        kind: ast::ExprKind::Path(r!(ast::ExtPath {
                            ext: None,
                            path: ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("auto"), .. }]
                            }
                        })),
                        ..
                    }),
                    _
                ),
                ..
            },
            _
        ))
    );

    t!(
        parse_stmt,
        Rust2015,
        "default as _",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::Cast(
                    r!(ast::Expr {
                        kind: ast::ExprKind::Path(r!(ast::ExtPath {
                            ext: None,
                            path: ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("default"), .. }]
                            }
                        })),
                        ..
                    }),
                    _
                ),
                ..
            },
            _
        ))
    );

    t!(
        parse_stmt,
        Rust2015,
        "safe as _",
        Ok(ast::Stmt::Expr(
            ast::Expr {
                kind: ast::ExprKind::Cast(
                    r!(ast::Expr {
                        kind: ast::ExprKind::Path(r!(ast::ExtPath {
                            ext: None,
                            path: ast::Path {
                                segs: [ast::PathSeg { ident: ast::Ident!("safe"), .. }]
                            }
                        })),
                        ..
                    }),
                    _
                ),
                ..
            },
            _
        ))
    );
}

#[test]
fn delegation() {
    // FIXME: This is just a smoke test, convert to proper tests.
    // See also <https://github.com/fmease/rasur/issues/30>

    t!(parse_item, Rust2015, "reuse it;", Ok(_));
    t!(parse_item, Rust2015, "reuse self;", Ok(_));
    t!(parse_item, Rust2015, "reuse path::<>::to::<_>::something::();", Ok(_));
    t!(parse_item, Rust2015, "reuse it as that;", Ok(_));
    t!(parse_item, Rust2015, "reuse it::*;", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{};", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{f, g, h};", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{f as f, g as g};", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{self, super, crate};", Ok(_));
    t!(parse_item, Rust2015, "reuse it {}", Ok(_));
    t!(parse_item, Rust2015, "reuse it { 1 + 2 * 3}", Ok(_));
    t!(parse_item, Rust2015, "reuse it::{} {}", Ok(_));
    t!(parse_item, Rust2015, "reuse <()>::it;", Ok(_));
    t!(parse_item, Rust2015, "reuse <() as Trait>::it;", Ok(_));

    // Contrary to its sibling, the use-item, these are not accepted:
    t!(parse_item, Rust2015, "reuse *;", Err(_));
    t!(parse_item, Rust2015, "reuse {};", Err(_));
    t!(parse_item, Rust2015, "reuse ::it;", Err(_));
    t!(parse_item, Rust2015, "reuse it as _;", Err(_));
    t!(parse_item, Rust2015, "reuse it::{*};", Err(_));
    t!(parse_item, Rust2015, "reuse it::{f::g::h};", Err(_));
    t!(parse_item, Rust2015, "reuse it::{f::{g::{h}}};", Err(_));

    // Some other invalid forms:
    t!(parse_item, Rust2015, "reuse it<i32>;", Err(_));
    t!(parse_item, Rust2015, "reuse it::f<i32>;", Err(_));
}
