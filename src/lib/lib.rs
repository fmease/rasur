// Features
#![feature(const_cmp)]
#![feature(const_clone)]
#![feature(const_default)]
#![feature(const_trait_impl)]
#![feature(decl_macro)]
#![feature(default_field_values)]
#![feature(deref_patterns)]
#![feature(derive_const)]
#![feature(gen_blocks)]
#![feature(generic_const_items)]
#![feature(import_trait_associated_functions)]
#![feature(macro_metavar_expr)]
#![feature(mut_ref)]
#![feature(negative_impls)]
#![feature(never_type)]
#![feature(step_trait)]
#![feature(type_alias_impl_trait)]
#![feature(type_changing_struct_update)]
// Lints
#![expect(incomplete_features, reason = "deref_patterns")]
#![deny(unused_must_use, rust_2018_idioms)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::default_trait_access)] // too opinionated
#![allow(clippy::empty_enums)] // type-level programming
#![allow(clippy::if_not_else)] // too opinionated
#![allow(clippy::items_after_statements)] // too opinionated
#![allow(clippy::match_bool)] // too opinionated
#![allow(clippy::option_option)] // too opinionated
#![allow(clippy::too_many_lines)] // too opinionated
#![allow(clippy::unnested_or_patterns)] // <https://github.com/rust-lang/rust-clippy/issues/9899>

pub mod ast;
pub mod edition;
pub mod error;
pub mod feature;
pub mod fmter;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod store;
pub mod token;
