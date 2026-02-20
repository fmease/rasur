use clap::{Arg, ArgAction::SetTrue, Command, builder::EnumValueParser};
use std::path::PathBuf;

// FIXME: Ideally, we would be using something more lightweight than `clap`.

pub(crate) fn opts() -> Opts {
    let mut matches = Command::new(env!("CARGO_PKG_NAME"))
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
                .help("Set the edition of the source file"),
        )
        .arg(
            Arg::new(id::FMT)
                .long("fmt")
                .action(SetTrue)
                .help("Render the source code as derived from the AST"),
        )
        .arg(Arg::new(id::AST).long("ast").action(SetTrue).help("Emit the abstract syntax tree"))
        .arg(Arg::new(id::TOKENS).long("tokens").action(SetTrue).help("Emit the tokens"))
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
            Arg::new(id::SKIP_MARKER)
                .long("skip-marker")
                .requires(id::FMT)
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
        .remove_one(id::SOURCE)
        .map(Source::String)
        .xor(matches.remove_one(id::PATH).map(Source::Path))
        .unwrap();

    Opts {
        source,
        edition: matches.remove_one(id::EDITION),
        emit_ast: matches.remove_one(id::AST).unwrap_or_default(),
        emit_tokens: matches.remove_one(id::TOKENS).unwrap_or_default(),
        fmt: matches.remove_one(id::FMT).unwrap_or_default(),
        lex_only: matches.remove_one(id::LEX_ONLY).unwrap_or_default(),
        skip_marker: rasur::fmter::SkipMarker::None, // FIXME
        strip_frontmatter: !matches.remove_one(id::NO_STRIP_FRONTMATTER).unwrap_or(false),
        strip_shebang: !matches.remove_one(id::NO_STRIP_SHEBANG).unwrap_or(false),
        short: matches.remove_one(id::SHORT).unwrap_or_default(),
        color: matches.remove_one(id::COLOR).unwrap(),
    }
}

pub(crate) struct Opts {
    pub(crate) source: Source,
    pub(crate) edition: Option<rasur::Edition>,
    pub(crate) emit_ast: bool,
    pub(crate) emit_tokens: bool,
    pub(crate) fmt: bool,
    pub(crate) lex_only: bool,
    pub(crate) skip_marker: rasur::fmter::SkipMarker,
    pub(crate) strip_frontmatter: bool,
    pub(crate) strip_shebang: bool,
    pub(crate) short: bool,
    pub(crate) color: clap::ColorChoice,
}

pub(crate) enum Source {
    String(String),
    Path(PathBuf),
}

macro_rules! parse {
    ($( $key:literal => $value:expr ),+ $(,)?)  => { |source| Ok(match source {
        $( $key => $value, )+
        _ => return Err(format!("possible values: {}", [$(concat!("`", $key, "`")),+].join(", "))),
    })}
}

fn parse_edition(source: &str) -> Result<rasur::Edition, String> {
    use rasur::Edition::*;

    parse!(
        "2015" => Rust2015,
        "2018" => Rust2018,
        "2021" => Rust2021,
        "2024" => Rust2024,
        "future" => Future,
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
    AST, COLOR, EDITION, FMT, LEX_ONLY,
    NO_STRIP_FRONTMATTER, NO_STRIP_SHEBANG,
    PATH, SHORT, SKIP_MARKER, SOURCE, TOKENS,
}
