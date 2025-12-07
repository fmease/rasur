// Features
#![feature(assert_matches)]
#![feature(const_default)]
#![feature(const_trait_impl)]
#![feature(decl_macro)]
#![feature(default_field_values)]
#![feature(deref_patterns)]
#![feature(derive_const)]
#![feature(gen_blocks)]
#![feature(if_let_guard)]
#![feature(import_trait_associated_functions)]
#![feature(iter_intersperse)]
#![feature(mut_ref)]
#![feature(negative_impls)]
#![feature(never_type)]
#![feature(super_let)]
// Lints
#![expect(incomplete_features, reason = "deref_patterns")]
#![deny(unused_must_use, rust_2018_idioms)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::items_after_statements)]
#![allow(clippy::match_bool)]
#![allow(clippy::option_option)]

pub mod ast;
pub mod fmter;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Edition {
    #[default]
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024,
    Future,
}
