use clap::{Arg, ArgAction::SetTrue, Command, builder::EnumValueParser};
use rasur::edition::Edition;
use std::path::PathBuf;

// FIXME: Ideally, we would be using something more lightweight than `clap`.

pub(crate) fn opts() -> Opts {
    let mut matches = Command::new("rasur")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(
            Arg::new(id::PATH)
                .value_parser(clap::builder::ValueParser::path_buf())
                .required_unless_present(id::SOURCE)
                .help("Path to the source file"),
        )
        .arg(
            Arg::new(id::SOURCE)
                .short(':')
                .long("source")
                .conflicts_with(id::PATH)
                .help("Provide the source code"),
        )
        .arg(
            Arg::new(id::EDITION)
                .short('e')
                .long("edition")
                .value_parser(parse_edition)
                .default_value(Edition::default().to_str())
                .help("Set the edition of the source file"),
        )
        .arg(
            Arg::new(id::EMIT)
                .long("emit")
                .value_name("TYPE")
                .value_parser(parse_artifact_type)
                .help("Emit the given artifact type"),
        )
        .arg(
            Arg::new(id::LEX_ONLY)
                .long("lex-only")
                .action(SetTrue)
                .help("Halt after lexing the source file"),
        )
        .arg(
            Arg::new(id::NO_STRIP_FRONTMATTER)
                .long("no-strip-frontmatter")
                .action(SetTrue)
                .help("Don't strip frontmatter"),
        )
        .arg(
            Arg::new(id::NO_STRIP_SHEBANG)
                .long("no-strip-shebang")
                .action(SetTrue)
                .help("Don't strip shebang"),
        )
        .arg(
            Arg::new(id::GATEKEEP)
                .short('G')
                .long("gatekeep")
                .action(SetTrue)
                .help("Forbid the use of unstable features"),
        )
        .arg(
            Arg::new(id::SKIP_MARKER)
                .long("skip-marker")
                .value_parser(parse_skip_marker)
                .value_name("MARKER")
                .help("Set the skip markers the pretty-printer should look out for"),
        )
        .arg(
            Arg::new(id::SHORT)
                .long("short")
                .action(SetTrue)
                .help("Use a terser format for diagnostics"),
        )
        .arg(
            Arg::new(id::COLOR)
                .long("color")
                .value_name("WHEN")
                .default_value("auto")
                .value_parser(EnumValueParser::<clap::ColorChoice>::new())
                .help("Control when to use color"),
        )
        .get_matches();

    let source = matches
        .remove_one(id::PATH)
        .map(|path| if &path == "-" { Source::Stdin } else { Source::Path(path) })
        .xor(matches.remove_one(id::SOURCE).map(Source::String))
        .unwrap();

    Opts {
        source,
        edition: matches.remove_one(id::EDITION),
        emit: matches.remove_one(id::EMIT),
        lex_only: matches.remove_one(id::LEX_ONLY).unwrap_or_default(),
        skip_marker: matches.remove_one(id::SKIP_MARKER).unwrap_or_default(),
        strip_frontmatter: !matches.remove_one(id::NO_STRIP_FRONTMATTER).unwrap_or(false),
        strip_shebang: !matches.remove_one(id::NO_STRIP_SHEBANG).unwrap_or(false),
        gatekeep: matches.remove_one(id::GATEKEEP).unwrap_or_default(),
        short: matches.remove_one(id::SHORT).unwrap_or_default(),
        color: matches.remove_one(id::COLOR).unwrap(),
    }
}

pub(crate) struct Opts {
    pub(crate) source: Source,
    pub(crate) edition: Option<Edition>,
    pub(crate) emit: Option<ArtifactType>,
    pub(crate) lex_only: bool,
    pub(crate) skip_marker: rasur::fmter::SkipMarker,
    pub(crate) strip_frontmatter: bool,
    pub(crate) strip_shebang: bool,
    pub(crate) gatekeep: bool,
    pub(crate) short: bool,
    pub(crate) color: clap::ColorChoice,
}

pub(crate) enum Source {
    String(String),
    Path(PathBuf),
    Stdin,
}

#[derive(Clone, Copy)]
pub(crate) enum ArtifactType {
    Tokens,
    Ast,
    Features,
    Fmt,
}

macro_rules! parse {
    ($( $key:literal => $value:expr ),+ $(,)?)  => { |source| Ok(match source {
        $( $key => $value, )+
        _ => return Err(format!("possible values: {}", [$(concat!("`", $key, "`")),+].join(", "))),
    })}
}

fn parse_edition(source: &str) -> Result<Edition, String> {
    source.parse().map_err(|()| {
        use std::fmt::Write as _;

        let mut msg = String::from("possible values: ");
        let mut editions = Edition::ALL;
        if let Some(edition) = editions.next() {
            _ = write!(msg, "`{edition}`");
        }
        editions.for_each(|edition| _ = write!(msg, ", `{edition}`"));
        msg
    })
}

fn parse_artifact_type(source: &str) -> Result<ArtifactType, String> {
    parse!(
        "tokens" => ArtifactType::Tokens,
        "ast" => ArtifactType::Ast,
        "features" => ArtifactType::Features,
        "fmt" => ArtifactType::Fmt,
    )(source)
}

fn parse_skip_marker(source: &str) -> Result<rasur::fmter::SkipMarker, String> {
    use rasur::fmter::SkipMarker::*;

    parse!(
        "none" => None,
        "all" => All,
        "rustfmt" => Rustfmt,
        "rasur" => Rasur,
    )(source)
}

macro_rules! ids {
    ($($name:ident),+ $(,)?) => {
        mod id {
            $( pub(super) const $name: &str = stringify!($name); )+
        }
    };
}

#[rustfmt::skip]
ids! {
    COLOR, EDITION, EMIT, GATEKEEP, LEX_ONLY,
    NO_STRIP_FRONTMATTER, NO_STRIP_SHEBANG,
    PATH, SHORT, SKIP_MARKER, SOURCE,
}
