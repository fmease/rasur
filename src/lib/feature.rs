use std::fmt;

macro_rules! features {
    ($( $name:ident $( #$issue:literal )? ),+ $(,)?)  => {
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

            pub const fn tracking_issue(self) -> Option<u32> {
                match self {
                    $( Self::$name => features!(@issue $( $issue )?) ),+
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
    };
    (@issue) => { None };
    (@issue $issue:literal) => { Some($issue) };
}

features! {
    async_for_loop #118898,
    async_trait_bounds #62290,
    auto_traits #13231,
    box_patterns #29641,
    builtin_syntax #110680,
    closure_lifetime_binder #97362,
    const_block_items #149226,
    const_closures #106003,
    const_trait_impl #143874,
    contract_internals #128044,
    coroutines #43122,
    decl_macro #39412,
    default_field_values #132162,
    ergonomic_clones #132290,
    explicit_tail_calls #112788,
    final_associated_functions #131179,
    fn_delegation #118212,
    frontmatter #136889,
    gen_blocks #117078,
    generic_const_items #113521,
    guard_patterns #129967,
    impl_restriction #105077,
    min_generic_const_args #132980,
    min_specialization #31844,
    more_qualified_paths #86935,
    move_expr #155050,
    mut_ref #123076,
    mut_restriction #105077,
    negative_bounds,
    negative_impls #68318,
    never_patterns #118155,
    pin_ergonomics #130494,
    postfix_match #121618,
    return_type_notation #109417,
    specialization #31844,
    super_let #139076,
    trait_alias #41517,
    try_blocks #31436,
    try_blocks_heterogeneous #149488,
    unnamed_enum_variants #156628,
    unsafe_binders #130516,
    unsafe_fields #132922,
    where_clause_attrs #115590,
    yeet_expr #96373,
    yield_expr #43122,
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

    pub const fn kind(self) -> FeatureKind {
        match self {
            Self::builtin_syntax | Self::contract_internals => FeatureKind::Internal,
            Self::negative_bounds => FeatureKind::SuperInternal,
            _ => FeatureKind::Experimental,
        }
    }
}

impl fmt::Display for Feature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

pub enum FeatureKind {
    Experimental,
    Internal,
    SuperInternal,
}

impl fmt::Display for FeatureKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Experimental => "experimental",
            Self::Internal => "internal",
            Self::SuperInternal => "super internal",
        })
    }
}
