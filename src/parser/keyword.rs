// pub(crate) fn is_path_seg(self) -> bool {
// matches!(self, Self::Crate | Self::Super | Self::SelfLower | Self::SelfUpper)
// }

pub(crate) mod soft {
    pub(crate) const AUTO: &str = "auto";
    pub(crate) const DYN: &str = "dyn"; // in Rust 2015
    pub(crate) const SAFE: &str = "safe";
    pub(crate) const UNION: &str = "union";
}
