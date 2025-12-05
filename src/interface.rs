use std::{ffi::OsString, path::PathBuf};

pub(crate) fn opts() -> Result<Opts, Error> {
    let mut source = None;
    let mut edition = None;
    let mut emit_tokens = false;
    let mut emit_ast = false;
    let mut lex_only = false;
    let mut fmt = false;
    let mut skip_marker = None;
    let mut short = false;

    let mut args = std::env::args_os().skip(1);

    while let Some(arg) = args.next() {
        if let Some(opt) = arg.as_encoded_bytes().strip_prefix(b"-") {
            match opt {
                b"-tok" => emit_tokens = true,
                b"-ast" => emit_ast = true,
                b"-lex-only" => lex_only = true,
                b"e" | b"-edition" => {
                    if edition.is_some() {
                        return Err(Error::DuplicateOpt("--edition"));
                    }
                    let edition_ = args.next().ok_or(Error::MissingArgToOpt("--edition"))?;
                    edition = Some(
                        parse_edition(edition_.as_encoded_bytes())
                            .map_err(|()| Error::InvalidArg("EDITION"))?,
                    );
                }
                b"-fmt" => fmt = true,
                b"-skip-marker" => {
                    if skip_marker.is_some() {
                        return Err(Error::DuplicateOpt("--skip-marker"));
                    }
                    let skip_marker_ =
                        args.next().ok_or(Error::MissingArgToOpt("--skip-marker"))?;
                    skip_marker = Some(
                        parse_skip_marker(skip_marker_.as_encoded_bytes())
                            .map_err(|()| Error::InvalidArg("MARKER"))?,
                    );
                }
                b":" | b"-source" => {
                    match source {
                        Some(Source::String(_)) => {
                            return Err(Error::DuplicateOpt("--source"));
                        }
                        Some(Source::Path(_)) => {
                            return Err(Error::IncompatibleArgs("--source", "PATH"));
                        }
                        None => {}
                    }
                    let source_ = args.next().ok_or(Error::MissingArgToOpt("--source"))?;
                    source = Some(Source::String(
                        source_.into_string().map_err(|_| Error::InvalidArg("SOURCE"))?,
                    ))
                }
                b"-short" => short = true,
                _ => return Err(Error::UnknownOpt(arg.to_owned())),
            }
        } else {
            match source {
                Some(Source::Path(_)) => return Err(Error::UnknownArg(arg.to_owned())),
                // FIXME: Incompatible(Opt("--source"), Arg("PATH"))
                Some(Source::String(_)) => return Err(Error::IncompatibleArgs("SOURCE", "PATH")),
                None => source = Some(Source::Path(PathBuf::from(arg))),
            }
        }
    }

    if !fmt && skip_marker.is_some() {
        return Err(Error::MissingOpt { opt: "--fmt", due_to: "--skip-marker" });
    }

    // FIXME: Missing(Arg("PATH"), Opt("--source"))
    let source = source.ok_or(Error::MissingArgs { any: &["PATH", "SOURCE"] })?;

    let skip_marker = skip_marker.unwrap_or_default();

    Ok(Opts { source, edition, emit_tokens, emit_ast, lex_only, fmt, skip_marker, short })
}

pub(crate) struct Opts {
    pub(crate) source: Source,
    pub(crate) edition: Option<rasur::Edition>,
    pub(crate) emit_tokens: bool,
    pub(crate) emit_ast: bool,
    pub(crate) lex_only: bool,
    pub(crate) fmt: bool,
    pub(crate) skip_marker: rasur::fmter::SkipMarker,
    pub(crate) short: bool,
}

pub(crate) enum Source {
    String(String),
    Path(PathBuf),
}

pub(crate) enum Error {
    DuplicateOpt(&'static str),
    // FIXME: Refine it to Incompatible(Opt(…), Arg(…))
    IncompatibleArgs(&'static str, &'static str),
    InvalidArg(&'static str),
    // FIXME: Refine it to Missing([Opt(…), Arg(…), …]),
    MissingArgs { any: &'static [&'static str] }, // invariant: non-empty
    MissingArgToOpt(&'static str),
    MissingOpt { opt: &'static str, due_to: &'static str },
    UnknownArg(OsString),
    UnknownOpt(OsString),
}

// FIXME: These diagnostics could be heavily improved, they're just stand-ins.
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateOpt(opt) => write!(f, "option `{opt}` can't be passed multiple times"),
            Self::IncompatibleArgs(fst, snd) => {
                write!(f, "arguments `{fst}` and `{snd}` are incompatible")
            }
            Self::InvalidArg(arg) => write!(f, "argument `{arg}` is invalid"),
            Self::MissingArgs { any: args } => {
                write!(f, "missing required arguments: ")?;
                let mut args = args.iter().peekable();
                if let Some(arg) = args.next() {
                    write!(f, "`{arg}`")?;
                }
                while let Some(arg) = args.next() {
                    let prefix = if args.peek().is_some() { ", " } else { " or " };
                    write!(f, "{prefix}`{arg}`")?;
                }
                Ok(())
            }
            Self::MissingArgToOpt(opt) => write!(f, "missing argument to option `{opt}`"),
            Self::MissingOpt { opt, due_to } => {
                write!(f, "option `{due_to}` requires option `{opt}` to be set")
            }
            Self::UnknownArg(arg) => write!(f, "unexpected argument `{}`", arg.display()),
            Self::UnknownOpt(opt) => write!(f, "unknown option `{}`", opt.display()),
        }
    }
}

fn parse_edition(source: &[u8]) -> Result<rasur::Edition, ()> {
    use rasur::Edition::*;

    Ok(match source {
        b"2015" => Rust2015,
        b"2018" => Rust2018,
        b"2021" => Rust2021,
        b"2024" => Rust2024,
        b"future" => Future,
        _ => return Err(()),
    })
}

fn parse_skip_marker(source: &[u8]) -> Result<rasur::fmter::SkipMarker, ()> {
    use rasur::fmter::SkipMarker::*;

    Ok(match source {
        b"none" => None,
        b"all" => All,
        b"rustfmt" => Rustfmt,
        b"rasur" => Rasur,
        _ => return Err(()),
    })
}
