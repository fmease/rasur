use std::fmt;

#[derive(Clone, Copy)]
pub enum Feature {
    /// <https://github.com/rust-lang/rust/issues/118898>.
    AsyncForLoop,
    /// <https://github.com/rust-lang/rust/issues/62290>.
    AsyncTraitBounds,
    /// <https://github.com/rust-lang/rust/issues/13231>.
    AutoTraits,
    /// <https://github.com/rust-lang/rust/issues/29641>.
    BoxPatterns,
    /// <https://github.com/rust-lang/rust/issues/110680>.
    BuiltinSyntax,
    /// <https://github.com/rust-lang/rust/issues/97362>.
    ClosureLifetimeBinder,
    /// <https://github.com/rust-lang/rust/issues/149226>.
    ConstBlockItems,
    /// <https://github.com/rust-lang/rust/issues/106003>.
    ConstClosures,
    /// <https://github.com/rust-lang/rust/issues/143874>.
    ConstTraitImpl,
    /// <https://github.com/rust-lang/rust/issues/128044>.
    ContractInternals,
    /// <https://github.com/rust-lang/rust/issues/43122>.
    Coroutines,
    /// <https://github.com/rust-lang/rust/issues/39412>.
    DeclMacro,
    /// <https://github.com/rust-lang/rust/issues/132162>.
    DefaultFieldValues,
    /// <https://github.com/rust-lang/rust/issues/132290>.
    ErgonomicClones,
    /// <https://github.com/rust-lang/rust/issues/112788>.
    ExplicitTailCalls,
    /// <https://github.com/rust-lang/rust/issues/131179>.
    FinalAssociatedFunctions,
    /// <https://github.com/rust-lang/rust/issues/118212>.
    FnDelegation,
    /// <https://github.com/rust-lang/rust/issues/136889>.
    Frontmatter,
    /// <https://github.com/rust-lang/rust/issues/117078>.
    GenBlocks,
    /// <https://github.com/rust-lang/rust/issues/129967>.
    GuardPatterns,
    /// <https://github.com/rust-lang/rust/issues/113521>.
    GenericConstItems,
    /// <https://github.com/rust-lang/rust/issues/105077>.
    ImplRestriction,
    /// <https://github.com/rust-lang/rust/issues/132980>.
    MinGenericConstArgs,
    /// <https://github.com/rust-lang/rust/issues/31844>.
    MinSpecialization,
    /// <https://github.com/rust-lang/rust/issues/86935>.
    MoreQualifiedPaths,
    /// <https://github.com/rust-lang/rust/issues/123076>.
    MutRef,
    NegativeBounds,
    /// <https://github.com/rust-lang/rust/issues/68318>.
    NegativeImpls,
    /// <https://github.com/rust-lang/rust/issues/118155>.
    NeverPatterns,
    /// <https://github.com/rust-lang/rust/issues/130494>.
    PinErgonomics,
    /// <https://github.com/rust-lang/rust/issues/121618>.
    PostfixMatch,
    /// <https://github.com/rust-lang/rust/issues/109417>.
    ReturnTypeNotation,
    /// <https://github.com/rust-lang/rust/issues/31844>.
    Specialization,
    /// <https://github.com/rust-lang/rust/issues/41517>.
    TraitAlias,
    /// <https://github.com/rust-lang/rust/issues/31436>.
    TryBlocks,
    /// <https://github.com/rust-lang/rust/issues/149488>.
    TryBlocksHeterogeneous,
    /// <https://github.com/rust-lang/rust/issues/130516>.
    UnsafeBinders,
    /// <https://github.com/rust-lang/rust/issues/132922>.
    UnsafeFields,
    /// <https://github.com/rust-lang/rust/issues/115590>.
    WhereClauseAttrs,
    /// <https://github.com/rust-lang/rust/issues/96373>.
    YeetExpr,
    /// <https://github.com/rust-lang/rust/issues/43122>.
    YieldExpr,
}

impl Feature {
    pub const fn name(self) -> &'static str {
        match self {
            Self::AsyncForLoop => "async_for_loop",
            Self::AsyncTraitBounds => "async_trait_bounds",
            Self::AutoTraits => "auto_traits",
            Self::BoxPatterns => "box_patterns",
            Self::BuiltinSyntax => "builtin_syntax",
            Self::ClosureLifetimeBinder => "closure_lifetime_binder",
            Self::ConstBlockItems => "const_block_items",
            Self::ConstClosures => "const_closures",
            Self::ConstTraitImpl => "const_trait_impl",
            Self::ContractInternals => "contract_internals",
            Self::Coroutines => "coroutines",
            Self::DeclMacro => "decl_macro",
            Self::DefaultFieldValues => "default_field_values",
            Self::ErgonomicClones => "ergonomic_clones",
            Self::ExplicitTailCalls => "explicit_tail_calls",
            Self::FinalAssociatedFunctions => "final_associated_functions",
            Self::FnDelegation => "fn_delegation",
            Self::Frontmatter => "frontmatter",
            Self::GenBlocks => "gen_blocks",
            Self::GenericConstItems => "generic_const_items",
            Self::GuardPatterns => "guard_patterns",
            Self::ImplRestriction => "impl_restriction",
            Self::MinGenericConstArgs => "min_generic_const_args",
            Self::MinSpecialization => "min_specialization",
            Self::MoreQualifiedPaths => "more_qualified_paths",
            Self::MutRef => "mut_ref",
            Self::NegativeBounds => "negative_bounds",
            Self::NegativeImpls => "negative_impls",
            Self::NeverPatterns => "never_patterns",
            Self::PinErgonomics => "pin_ergonomics",
            Self::PostfixMatch => "postfix_match",
            Self::ReturnTypeNotation => "return_type_notation",
            Self::Specialization => "specialization",
            Self::TraitAlias => "trait_alias",
            Self::TryBlocks => "try_blocks",
            Self::TryBlocksHeterogeneous => "try_blocks_heterogeneous",
            Self::UnsafeBinders => "unsafe_binders",
            Self::UnsafeFields => "unsafe_fields",
            Self::WhereClauseAttrs => "where_clause_attrs",
            Self::YeetExpr => "yeet_expr",
            Self::YieldExpr => "yield_expr",
        }
    }

    /// See also <https://github.com/rust-lang/rust/issues/154045>.
    pub const fn protected(self) -> bool {
        match self {
            | Self::AutoTraits
            | Self::BoxPatterns
            | Self::DeclMacro
            | Self::MinSpecialization
            | Self::NegativeImpls
            | Self::Specialization
            | Self::TraitAlias
            | Self::TryBlocks => false,
            _ => true,
        }
    }
}

impl fmt::Display for Feature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
