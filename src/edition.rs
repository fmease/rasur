#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub(crate) enum Edition {
    #[default]
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024,
    Future,
}
