#[derive(Copy, Hash, Debug)]
#[derive_const(Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Edition {
    #[default]
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024,
    Future,
}

impl Edition {
    pub const ALL: std::ops::RangeInclusive<Self> = Self::default()..=Self::MAX;
    const MAX: Self = Self::Future;

    pub const fn to_str(self) -> &'static str {
        match self {
            Self::Rust2015 => "2015",
            Self::Rust2018 => "2018",
            Self::Rust2021 => "2021",
            Self::Rust2024 => "2024",
            Self::Future => "future",
        }
    }

    fn via(tag: usize) -> Option<Self> {
        Some(match tag {
            0 => Self::Rust2015,
            1 => Self::Rust2018,
            2 => Self::Rust2021,
            3 => Self::Rust2024,
            4 => Self::Future,
            _ => return None,
        })
    }
}

impl std::fmt::Display for Edition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl std::str::FromStr for Edition {
    type Err = ();

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        Ok(match source {
            "2015" => Self::Rust2015,
            "2018" => Self::Rust2018,
            "2021" => Self::Rust2021,
            "2024" => Self::Rust2024,
            "future" => Self::Future,
            _ => return Err(()),
        })
    }
}

impl std::iter::Step for Edition {
    fn steps_between(&start: &Self, &end: &Self) -> (usize, Option<usize>) {
        if start > end {
            return (0, None);
        }
        let steps = end as usize - start as usize;
        (steps, Some(steps))
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        (start as usize).checked_add(count).and_then(Self::via)
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        (start as usize).checked_sub(count).and_then(Self::via)
    }
}
