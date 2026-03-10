use super::{parse_file, parse_pat, t};
use crate::{ast, edition::Edition::*};
use deref as r;

#[test]
fn mut_ref_mut() {
    t!(
        parse_pat,
        Rust2015,
        "mut ref mut x",
        Ok(ast::Pat::Binding(r!(ast::BindingPat {
            mut_: ast::Mutability::Mut,
            by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mutability::Mut),
            binder: ast::Ident!("x"),
            pat: None,
        })))
    );
}

#[test]
fn binding_modes() {
    t!(
        parse_file,
        Rust2015,
        "
fn main() {
    let x = ();
    let mut x = ();
    let ref x = ();
    let mut ref x = ();
    let mut ref mut x = ();
    let &(mut x) = ();
    let &(ref mut x) = ();
    let &(mut ref x) = ();
    let &(mut ref mut x) = ();
    let &mut x = ();
    let &mut mut x = ();
    let &mut ref x = ();
    let &mut ref mut x = ();
    let &mut mut ref mut x = ();
}
",
        Ok(_) // just a smoke test
    );
}

#[test]
fn pseudo_field_binding_mode_box() {
    // issue: <https://github.com/fmease/rasur/issues/19>

    t!(
        parse_pat,
        Rust2015,
        "X { box mut ref mut x }",
        Ok(ast::Pat::Struct(r!(ast::StructPat {
            fields: r!([ast::StructPatField {
                attrs: _,
                binder: None,
                body: ast::Pat::Box(r!(ast::Pat::Binding(r!(ast::BindingPat {
                    mut_: ast::Mutability::Mut,
                    by_ref: ast::ByRef::Yes(ast::BorrowKind::Ref, ast::Mutability::Mut),
                    binder: ast::Ident!("x"),
                    pat: None,
                }))))
            }]),
            ..
        })))
    );
}
