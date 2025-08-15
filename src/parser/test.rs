use crate::{
    ast,
    edition::Edition::{self, *},
    lexer::lex,
};
use std::assert_matches::assert_matches;

fn parse_file(source: &str, edition: Edition) -> super::Result<ast::File<'_>> {
    super::parse(&lex(source), source, edition)
}

fn parse_via<'src, T>(
    source: &'src str,
    edition: Edition,
    parse: impl FnOnce(&mut super::Parser<'_, 'src>) -> super::Result<T>,
) -> super::Result<T> {
    let tokens = lex(source);
    let mut parser = super::Parser::new(&tokens, source, edition);
    let result = parse(&mut parser)?;
    assert_eq!(parser.token.kind, super::TokenKind::EndOfInput);
    Ok(result)
}

fn parse_expr(source: &str, edition: Edition) -> super::Result<ast::Expr<'_>> {
    parse_via(source, edition, |this| this.parse_expr())
}

fn parse_pat(source: &str, edition: Edition) -> super::Result<ast::Pat<'_>> {
    parse_via(source, edition, |this| this.parse_pat(super::pat::OrPolicy::Allowed))
}

#[test]
fn empty() {
    assert_matches!(
        parse_file("", Rust2015),
        Ok(ast::File { attrs: deref!([]), items: deref!([]), span: _ })
    );
}

#[test]
fn double_borrow_and_double_borrow() {
    assert_matches!(
        parse_expr("&&0&&&&1", Rust2015),
        Ok(ast::Expr::BinOp(
            ast::BinOp::And,
            deref!(ast::Expr::Borrow(
                ast::Mutability::Not,
                deref!(ast::Expr::Borrow(ast::Mutability::Not, _))
            )),
            deref!(ast::Expr::Borrow(
                ast::Mutability::Not,
                deref!(ast::Expr::Borrow(ast::Mutability::Not, _))
            ))
        ))
    );
}

#[test]
fn or_nullary_closure() {
    assert_matches!(
        parse_expr("()||||()", Rust2015),
        Ok(ast::Expr::BinOp(
            ast::BinOp::Or,
            deref!(ast::Expr::Tup(deref!([]))),
            deref!(ast::Expr::Closure(deref!(ast::ClosureExpr {
                params: deref!([]),
                ret_ty: None,
                body: ast::Expr::Tup(deref!([]))
            })))
        ))
    );
}

// Unstable feature: `mut_ref` <https://github.com/rust-lang/rust/issues/123076>.
#[test]
fn mut_ref_mut() {
    assert_matches!(
        parse_pat("mut ref mut x", Rust2015),
        Ok(ast::Pat::Ident(ast::IdentPat {
            mut_: ast::Mutability::Mut,
            by_ref: ast::ByRef::Yes(ast::Mutability::Mut),
            ident: "x"
        }))
    );
}

#[test]
fn expr_pat_path_paren_gen_args_arrow() {
    assert_matches!(
        parse_expr("x::()->()", Rust2015),
        Ok(ast::Expr::Path(deref!(ast::ExtPath {
            self_ty: None,
            path: ast::Path {
                segs: deref!([ast::PathSeg {
                    ident: "x",
                    args: Some(ast::GenericArgs::Paren {
                        inputs: deref!([]),
                        output: Some(ast::Ty::Tup(deref!([]))),
                    })
                }])
            }
        })))
    );

    assert_matches!(
        parse_pat("x::()->!::X", Rust2015),
        Ok(ast::Pat::Path(deref!(ast::ExtPath {
            self_ty: None,
            path: ast::Path {
                segs: deref!([
                    ast::PathSeg {
                        ident: "x",
                        args: Some(ast::GenericArgs::Paren {
                            inputs: deref!([]),
                            output: Some(ast::Ty::Never),
                        })
                    },
                    ast::PathSeg { ident: "X", args: None }
                ])
            }
        })))
    );
}

// FIXME: macro call expr with args
// FIXME: macro_rules! in stmt pos (-> item not stmt)
// FIXME: const {  } vs const item
// FIXME: ops
// FIXME: structs in ifs etc.
// FIXME: almost-assoc-item-constraint due to (  )
// FIXME: ranges!! exprs, pats
// FIXME: A bunch of negative behavior tests!

#[test]
fn smoke_binding_modes() {
    assert_matches!(
        parse_file(
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
            Rust2015
        ),
        Ok(_)
    );
}

// FIXME: Expand this smoke test.
#[test]
#[ignore] // FIXME: Make it work.
fn smoke_item_modifiers() {
    assert_matches!(
        parse_file(
            r#"
pub const unsafe extern "C" fn f() {}
const unsafe extern "C" fn f() {}
unsafe extern "C" fn f() {}
extern "C" fn f() {}
fn f() {}
pub const unsafe extern "C" fn f() {}
pub const unsafe fn f() {}
pub const fn f() {}
pub fn f() {}
pub const extern "C" fn f() {}
const extern "C" fn f() {}
"#,
            Rust2015
        ),
        Ok(_)
    );
}
