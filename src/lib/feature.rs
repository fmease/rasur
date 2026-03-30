use std::fmt;

#[derive(Clone, Copy)]
pub enum Feature {
    // <https://github.com/rust-lang/rust/issues/136889>.
    Frontmatter,
    // <https://github.com/rust-lang/rust/issues/68318>.
    NegativeImpls,
}

impl Feature {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Frontmatter => "frontmatter",
            Self::NegativeImpls => "negative_impls",
        }
    }

    /// See also <https://github.com/rust-lang/rust/issues/154045>.
    pub const fn protected(self) -> bool {
        !matches!(self, Self::NegativeImpls)
    }
}

impl fmt::Display for Feature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
