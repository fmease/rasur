use std::path::PathBuf;

pub(crate) fn opts() -> Result<Opts, ()> {
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
        if let Some(opt) = arg.as_encoded_bytes().strip_prefix(b"--") {
            match opt {
                b"tok" => emit_tokens = true,
                b"ast" => emit_ast = true,
                b"lex-only" => lex_only = true,
                b"edition" => {
                    if edition.is_some() {
                        eprintln!("error: `--edition` can't be passed multiple times");
                        return Err(());
                    }
                    let edition_ = args.next().ok_or_else(|| {
                        eprintln!("error: missing argument to `--edition`");
                    })?;
                    edition = Some(parse_edition(edition_.as_encoded_bytes()).map_err(|()| {
                        eprintln!("error: invalid edition `{}`", edition_.display());
                    })?);
                }
                b"fmt" => fmt = true,
                b"skip-marker" => {
                    if skip_marker.is_some() {
                        eprintln!("error: `--skip-marker` can't be passed multiple times");
                        return Err(());
                    }
                    let skip_marker_ = args.next().ok_or_else(|| {
                        eprintln!("error: missing argument to `--skip-marker`");
                    })?;
                    skip_marker =
                        Some(parse_skip_marker(skip_marker_.as_encoded_bytes()).map_err(|()| {
                            eprintln!("error: invalid skip marker `{}`", skip_marker_.display());
                        })?);
                }
                b"source" => {
                    match source {
                        Some(Source::String(_)) => {
                            eprintln!("error: `--source` can't be passed multiple times");
                            return Err(());
                        }
                        Some(Source::Path(_)) => {
                            eprintln!(
                                "error: argument `--source SOURCE` is incompatible with argument `PATH`"
                            );
                            return Err(());
                        }
                        None => {}
                    }
                    let source_ = args.next().ok_or_else(|| {
                        eprintln!("error: missing argument to `--skip-marker`");
                    })?;
                    source =
                        Some(Source::String(source_.into_string().map_err(|_| {
                            eprintln!("error: argument `SOURCE` isn't valid UTF-8")
                        })?))
                }
                b"short" => short = true,
                _ => {
                    eprintln!("error: unknown flag `{}`", arg.display());
                    return Err(());
                }
            }
        } else {
            match source {
                Some(Source::Path(_)) => {
                    eprintln!("error: unexpected argument `{}`", arg.display());
                    return Err(());
                }
                Some(Source::String(_)) => {
                    eprintln!(
                        "error: argument `--source SOURCE` is incompatible with argument `PATH`"
                    );
                    return Err(());
                }
                None => source = Some(Source::Path(PathBuf::from(arg))),
            }
        }
    }

    if !fmt && skip_marker.is_some() {
        eprintln!("`--skip-marker` requires `--fmt` to be set");
        return Err(());
    }

    let source = source
        .ok_or_else(|| eprintln!("error: missing required argument `PATH` or `--source SOURCE`"))?;

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
