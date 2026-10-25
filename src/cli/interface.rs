use clap::{Arg, ArgAction::SetTrue, Command, builder::EnumValueParser};
use painter::AnsiColor;
use rasur::edition::Edition;
use std::{
    io::{self, Write as _},
    path::PathBuf,
};

// FIXME: Ideally, we would be using something more lightweight than `clap`.

pub(crate) fn opts() -> Opts {
    let mut matches = Command::new("rasur")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(
            Arg::new(id::PATH)
                .value_parser(clap::builder::ValueParser::path_buf())
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
            Arg::new(id::KIND)
                .short('K')
                .long("kind")
                .value_parser(parse_fragment_kind)
                .default_value(FragmentKind::File.to_str())
                .help("Set the syntax fragment kind of the source file"),
        )
        .arg(
            Arg::new(id::NO_STRIP_BOM)
                .long("no-strip-bom")
                .action(SetTrue)
                .help("Don't strip BOMs (byte order marks)"),
        )
        .arg(
            Arg::new(id::NO_SUBSTITUTE_CRLF)
                .long("no-substitute-crlf")
                .action(SetTrue)
                .help("Don't substitute CRLFs with LFs"),
        )
        .arg(
            Arg::new(id::NO_STRIP_SHEBANG)
                .long("no-strip-shebang")
                .action(SetTrue)
                .help("Don't strip shebang"),
        )
        .arg(
            Arg::new(id::NO_STRIP_FRONTMATTER)
                .long("no-strip-frontmatter")
                .action(SetTrue)
                .help("Don't strip frontmatter"),
        )
        .arg(
            Arg::new(id::GATEKEEP)
                .short('G')
                .long("gatekeep")
                .action(SetTrue)
                .help("Forbid the use of unstable features that aren't explicitly enabled"),
        )
        .arg(
            Arg::new(id::UNLOCK_SUPER_INTERNAL_FEATURES)
                .long(UNLOCK_SUPER_INTERNAL_FEATURES_OPT)
                .action(SetTrue)
                .help("Allow certain internal language features to be enabled"),
        )
        .arg(
            Arg::new(id::LEX_ONLY)
                .long("lex-only")
                .action(SetTrue)
                .help("Halt after lexing the source file"),
        )
        .arg(
            Arg::new(id::EMIT)
                .long("emit")
                .value_name("TYPE")
                .value_parser(parse_artifact_type)
                .help("Emit the given artifact type"),
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

    match matches.remove_one(id::COLOR).unwrap() {
        clap::ColorChoice::Always => painter::ColorChoice::Always.write_global(),
        clap::ColorChoice::Never => painter::ColorChoice::Never.write_global(),
        clap::ColorChoice::Auto => {}
    }

    let source = matches
        .remove_one(id::PATH)
        .map(|path| if &path == "-" { Source::Stdin } else { Source::Path(path) })
        .xor(matches.remove_one(id::SOURCE).map(Source::String));
    let emit = matches.remove_one(id::EMIT);

    // FIXME: Style diverges from `clap`. Just get rid of `clap`.
    if source.is_none() && emit != Some(ArtifactType::UnicodeVersion) {
        let mut p = painter::Painter::new(io::stderr(), io::BufWriter::new);
        p.with(AnsiColor::BrightRed.on_default().bold(), |p| write!(p, "error:")).unwrap();
        write!(p, " missing argument '").unwrap();
        p.with(AnsiColor::Yellow, |p| write!(p, "[{}]", id::PATH)).unwrap();
        write!(p, "' or '").unwrap();
        p.with(AnsiColor::Yellow, |p| write!(p, "--source <{}>", id::SOURCE)).unwrap();
        writeln!(p, "'").unwrap();
        p.flush().unwrap();

        std::process::exit(2);
    };

    Opts {
        source,
        edition: matches.remove_one(id::EDITION),
        kind: matches.remove_one(id::KIND).unwrap(),
        emit,
        lex_only: matches.remove_one(id::LEX_ONLY).unwrap_or_default(),
        strip_bom: !matches.remove_one(id::NO_STRIP_BOM).unwrap_or(false),
        substitute_crlf: !matches.remove_one(id::NO_SUBSTITUTE_CRLF).unwrap_or(false),
        strip_shebang: !matches.remove_one(id::NO_STRIP_SHEBANG).unwrap_or(false),
        strip_frontmatter: !matches.remove_one(id::NO_STRIP_FRONTMATTER).unwrap_or(false),
        gatekeep: matches.remove_one(id::GATEKEEP).unwrap_or_default(),
        unlock_super_internal_features: matches
            .remove_one(id::UNLOCK_SUPER_INTERNAL_FEATURES)
            .unwrap_or_default(),
        short: matches.remove_one(id::SHORT).unwrap_or_default(),
    }
}

pub(crate) struct Opts {
    pub(crate) source: Option<Source>,
    pub(crate) edition: Option<Edition>,
    pub(crate) kind: FragmentKind,
    pub(crate) emit: Option<ArtifactType>,
    pub(crate) lex_only: bool,
    pub(crate) strip_bom: bool,
    pub(crate) substitute_crlf: bool,
    pub(crate) strip_shebang: bool,
    pub(crate) strip_frontmatter: bool,
    pub(crate) gatekeep: bool,
    pub(crate) unlock_super_internal_features: bool,
    pub(crate) short: bool,
}

pub(crate) enum Source {
    String(String),
    Path(PathBuf),
    Stdin,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArtifactType {
    UnicodeVersion,
    Tokens,
    Ast,
    Features,
    Fmt,
}

pub(crate) const UNLOCK_SUPER_INTERNAL_FEATURES_OPT: &str = "unlock-super-internal-features";

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
        "Unicode-version" => ArtifactType::UnicodeVersion,
        "tokens" => ArtifactType::Tokens,
        "ast" => ArtifactType::Ast,
        "features" => ArtifactType::Features,
        "fmt" => ArtifactType::Fmt,
    )(source)
}

#[derive(Clone, Copy)]
pub(crate) enum FragmentKind {
    Expr,
    File,
    Item,
    Pat,
    Stmt,
    Ty,
}

impl FragmentKind {
    pub fn to_str(self) -> &'static str {
        match self {
            Self::Expr => "expr",
            Self::File => "file",
            Self::Item => "item",
            Self::Pat => "pat",
            Self::Stmt => "stmt",
            Self::Ty => "ty",
        }
    }
}

fn parse_fragment_kind(source: &str) -> Result<FragmentKind, String> {
    parse!(
        "expr" => FragmentKind::Expr,
        "file" => FragmentKind::File,
        "item" => FragmentKind::Item,
        "pat" => FragmentKind::Pat,
        "stmt" => FragmentKind::Stmt,
        "ty" => FragmentKind::Ty,
    )(source)
}

macro_rules! ids {
    ($($name:ident),+ $(,)?) => {
        mod id {
            $( pub(super) const $name: &str = stringify!($name); )+
        }
    };
}

ids! {
    COLOR,
    EDITION,
    EMIT,
    GATEKEEP,
    KIND,
    LEX_ONLY,
    NO_STRIP_BOM,
    NO_STRIP_FRONTMATTER,
    NO_STRIP_SHEBANG,
    NO_SUBSTITUTE_CRLF,
    PATH,
    SHORT,
    SOURCE,
    UNLOCK_SUPER_INTERNAL_FEATURES,
}
