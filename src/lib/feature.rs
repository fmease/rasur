use std::fmt;

macro_rules! features {
    ($( $name:ident ),+ $(,)?)  => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
        #[allow(nonstandard_style)]
        pub enum Feature {
            $( $name ),+
        }

        impl Feature {
            pub const fn name(self) -> &'static str {
                match self {
                    $( Self::$name => stringify!($name) ),+
                }
            }
        }

        impl std::str::FromStr for Feature {
            type Err = ();

            fn from_str(source: &str) -> Result<Self, Self::Err> {
                Ok(match source {
                    $( stringify!($name) => Self::$name,)+
                    _ => return Err(()),
                })
            }
        }
    }
}

features! {
    // <https://github.com/rust-lang/rust/issues/118898>.
    async_for_loop,
    // <https://github.com/rust-lang/rust/issues/62290>.
    async_trait_bounds,
    // <https://github.com/rust-lang/rust/issues/13231>.
    auto_traits,
    // <https://github.com/rust-lang/rust/issues/29641>.
    box_patterns,
    // <https://github.com/rust-lang/rust/issues/110680>.
    builtin_syntax,
    // <https://github.com/rust-lang/rust/issues/97362>.
    closure_lifetime_binder,
    // <https://github.com/rust-lang/rust/issues/149226>.
    const_block_items,
    // <https://github.com/rust-lang/rust/issues/106003>.
    const_closures,
    // <https://github.com/rust-lang/rust/issues/143874>.
    const_trait_impl,
    // <https://github.com/rust-lang/rust/issues/128044>.
    contract_internals,
    // <https://github.com/rust-lang/rust/issues/43122>.
    coroutines,
    // <https://github.com/rust-lang/rust/issues/39412>.
    decl_macro,
    // <https://github.com/rust-lang/rust/issues/132162>.
    default_field_values,
    // <https://github.com/rust-lang/rust/issues/132290>.
    ergonomic_clones,
    // <https://github.com/rust-lang/rust/issues/112788>.
    explicit_tail_calls,
    // <https://github.com/rust-lang/rust/issues/131179>.
    final_associated_functions,
    // <https://github.com/rust-lang/rust/issues/118212>.
    fn_delegation,
    // <https://github.com/rust-lang/rust/issues/136889>.
    frontmatter,
    // <https://github.com/rust-lang/rust/issues/117078>.
    gen_blocks,
    // <https://github.com/rust-lang/rust/issues/113521>.
    generic_const_items,
    // <https://github.com/rust-lang/rust/issues/129967>.
    guard_patterns,
    // <https://github.com/rust-lang/rust/issues/105077>.
    impl_restriction,
    // <https://github.com/rust-lang/rust/issues/132980>.
    min_generic_const_args,
    // <https://github.com/rust-lang/rust/issues/31844>.
    min_specialization,
    // <https://github.com/rust-lang/rust/issues/86935>.
    more_qualified_paths,
    // <https://github.com/rust-lang/rust/issues/155050>.
    move_expr,
    // <https://github.com/rust-lang/rust/issues/123076>.
    mut_ref,
    // (internal)
    negative_bounds,
    // <https://github.com/rust-lang/rust/issues/68318>.
    negative_impls,
    // <https://github.com/rust-lang/rust/issues/118155>.
    never_patterns,
    // <https://github.com/rust-lang/rust/issues/130494>.
    pin_ergonomics,
    // <https://github.com/rust-lang/rust/issues/121618>.
    postfix_match,
    // <https://github.com/rust-lang/rust/issues/109417>.
    return_type_notation,
    // <https://github.com/rust-lang/rust/issues/31844>.
    specialization,
    // <https://github.com/rust-lang/rust/issues/139076>.
    super_let,
    // <https://github.com/rust-lang/rust/issues/41517>.
    trait_alias,
    // <https://github.com/rust-lang/rust/issues/31436>.
    try_blocks,
    // <https://github.com/rust-lang/rust/issues/149488>.
    try_blocks_heterogeneous,
    // <https://github.com/rust-lang/rust/issues/130516>.
    unsafe_binders,
    // <https://github.com/rust-lang/rust/issues/132922>.
    unsafe_fields,
    // <https://github.com/rust-lang/rust/issues/155938>.
    view_types,
    // <https://github.com/rust-lang/rust/issues/115590>.
    where_clause_attrs,
    // <https://github.com/rust-lang/rust/issues/96373>.
    yeet_expr,
    // <https://github.com/rust-lang/rust/issues/43122>.
    yield_expr,
}

impl Feature {
    // See also <https://github.com/rust-lang/rust/issues/154045>.
    pub const fn protected(self) -> bool {
        match self {
            | Self::auto_traits
            | Self::box_patterns
            | Self::decl_macro
            | Self::min_specialization
            | Self::negative_impls
            | Self::specialization
            | Self::trait_alias
            | Self::try_blocks => false,
            _ => true,
        }
    }
}

impl fmt::Display for Feature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
