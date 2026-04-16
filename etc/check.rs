#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
clap = "4.5.58"
painter = { path = "lib/painter" }
walkdir = "2.5.0"
---
#![feature(exit_status_error)]

#[cfg(not(unix))]
compile_error!("non-Unix platforms not supported");

use painter::{AnsiColor, Effects, Painter};
use std::{
    collections::BTreeSet,
    env, fs,
    io::{self, BufRead as _, Write as _},
    os::unix::process::ExitStatusExt as _,
    path::{Path, PathBuf},
    process::{Command, ExitCode, ExitStatus, Stdio},
    sync::{Arc, Mutex},
    time::Instant,
};

const TMP_DIR_PATH: &str = "/tmp/rasurck";

// FIXME: If more than one edition is passed and we should save the failures, consider writing
//        the relevant results to `FAILURES.$EDITION.txt` per EDITION (loading would need to
//        account for that, of course).

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<(), ()> {
    let mut opts = interface::opts();

    let mut p = Painter::new(io::stdout(), io::BufWriter::new);

    if let Some(path) = &opts.save {
        write!(p, "Do you really want to save test failures as `{}`? [y/N]: ", path.display())
            .unwrap();
        p.flush().unwrap();

        let mut answer = String::new();
        io::stdin().read_line(&mut answer).unwrap();
        let answer = answer.trim();

        if !answer.eq_ignore_ascii_case("y") && !answer.eq_ignore_ascii_case("yes") {
            eprintln!("Operation denied; aborting.");
            return Err(());
        } else {
            writeln!(p).unwrap();
        }
    }

    if let Some(path) = &opts.load {
        opts.paths.extend(
            io::BufReader::new(fs::File::open(path).unwrap())
                .lines()
                .map(|line| PathBuf::from(line.unwrap())),
        );
    }

    let entries = opts.paths.iter().flat_map(walkdir::WalkDir::new);
    let entries = Arc::new(Mutex::new(entries));

    let rasur_path = &rasur_path(&opts);
    let rustc_path = &rustc_path(&opts);
    let time = Instant::now();

    let mut stats = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..opts.jobs.get())
            .map(|_| {
                let entries = entries.clone();
                let test_opts = &opts.test;

                scope.spawn(move || {
                    let chunk_size = opts.chunk_size.get();

                    let mut local_entries = Vec::with_capacity(chunk_size);
                    let mut stats = Stats::default();

                    loop {
                        {
                            let mut entries = entries.lock().unwrap();

                            for _ in 0..chunk_size {
                                local_entries.push(match entries.next() {
                                    Some(entry) => entry,
                                    None => break,
                                });
                            }
                        }

                        if local_entries.is_empty() {
                            break;
                        }

                        for entry in local_entries.drain(..) {
                            check(entry, rasur_path, rustc_path, test_opts, &mut stats);
                        }
                    }

                    stats
                })
            })
            .collect();

        handles
            .into_iter()
            .fold(Stats::default(), |stats, handle| stats.adjoin(handle.join().unwrap()))
    });

    let duration = time.elapsed();

    stats.failures.sort_unstable_by(|(path0, ..), (path1, ..)| path0.cmp(path1));

    stats.render(duration, &opts, &mut p).unwrap();

    if let Some(path) = opts.save
        && !stats.failures.is_empty()
    {
        let mut file = io::BufWriter::new(fs::File::create(path).unwrap());

        for (path, ..) in &stats.failures {
            writeln!(file, "{path}").unwrap();
        }
    }

    if let Measure::CfgFalse = opts.test.measure
        && stats.total > 0
    {
        _ = fs::remove_dir_all(TMP_DIR_PATH);
    }

    if stats.failures.is_empty() { Ok(()) } else { Err(()) }
}

fn rasur_path(opts: &interface::Opts) -> PathBuf {
    let profile = match opts.debug {
        true => "debug",
        false => "release",
    };
    [env!("CARGO_MANIFEST_DIR"), "..", "target", profile, "rasur-cli"].into_iter().collect()
}

fn rustc_path(opts: &interface::Opts) -> PathBuf {
    let mut output = Command::new("rustup")
        .arg(format!("+{}", opts.toolchain))
        .args(["which", "rustc"])
        .output()
        .unwrap();
    output.status.exit_ok().unwrap();
    output.stdout.pop(); // \n
    PathBuf::from(String::from_utf8(output.stdout).unwrap())
}

#[derive(Default)]
struct Stats {
    failures: Vec<(String, Vec<(Edition, Mismatch)>)>,
    total: usize,
}

impl Stats {
    fn adjoin(mut self, mut other: Self) -> Self {
        self.failures.append(&mut other.failures);
        self.total += other.total;
        self
    }

    fn render(
        &self,
        duration: std::time::Duration,
        opts: &interface::Opts,
        p: &mut Painter<impl io::Write>,
    ) -> io::Result<()> {
        let Self { ref failures, total } = *self;

        if !failures.is_empty() {
            let width = failures.iter().map(|(path, ..)| path.len()).max().unwrap_or_default();

            writeln!(p, "{:─^width$}", " FAILURES ")?;
            for (path, subfailures) in failures {
                write!(p, "{path:<width$}")?;

                const SPACER: &str = "    ";

                if subfailures.len() < opts.test.editions.len() {
                    let mut condensed = Vec::<(Range, _)>::new();
                    for &(edition, mismatch) in subfailures {
                        if let Some((last_editions, last_mismatch)) = condensed.last_mut()
                            && mismatch == *last_mismatch
                            && last_editions.end as usize + 1 == edition as usize
                        {
                            last_editions.end = edition;
                        } else {
                            let range = Range { start: edition, end: edition };
                            condensed.push((range, mismatch));
                        }
                    }

                    for (editions, mismatch) in condensed {
                        write!(p, "{SPACER}{}", editions.start.to_str())?;
                        if editions.end != editions.start {
                            write!(p, "–{}", editions.end.to_str())?;
                        }
                        write!(p, "  ")?;
                        mismatch.render(p)?;
                    }

                    struct Range {
                        start: Edition,
                        end: Edition,
                    }
                } else {
                    let (_, mismatch) = subfailures.first().unwrap();
                    write!(p, "{SPACER}")?;
                    mismatch.render(p)?;
                }
                writeln!(p)?;
            }
            writeln!(p, "{:─^width$}", "")?;
            writeln!(p)?;
        }

        const PADDING: usize = 4;

        let (color, tag) = match (failures.as_slice(), total) {
            (_, 0) => (AnsiColor::Yellow, "NO TESTS WERE RUN!"),
            ([], _) => (AnsiColor::Green, "ALL TESTS PASSED!"),
            ([_, ..], _) => (AnsiColor::Red, "SOME TESTS FAILED!"),
        };
        p.with(color.on_default().effects(Effects::BOLD), |p| {
            write!(p, "{tag}{}", " ".repeat(PADDING))
        })?;
        let indent = " ".repeat(tag.len() + PADDING);

        let failed = failures.len();
        let passed = total - failed;
        let percentage = if total == 0 { 1. } else { (passed as f32 / total as f32) * 1. } * 100.;

        let column = |p: &mut Painter<_>, count, tag, color| {
            if count > 0 {
                p.set(color)?;
                p.set(Effects::BOLD)?;
            }
            write!(p, "{count}")?;
            if count > 0 {
                p.unset()?;
            }
            write!(p, " {tag}")?;
            if count > 0 {
                p.unset()?;
            }
            io::Result::Ok(())
        };

        const SEP: &str = " │ ";

        column(p, passed, "passed", AnsiColor::Green)?;
        write!(p, " ({percentage:.2}%)")?;
        write!(p, "{SEP}")?;
        column(p, failed, "failed", AnsiColor::Red)?;
        write!(p, "{SEP}")?;
        write!(p, "{total} in total")?;
        writeln!(p)?;

        write!(p, "{indent}")?;
        p.with(AnsiColor::BrightBlack, |p| write!(p, "{duration:?}"))?;

        writeln!(p)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Mismatch {
    rasur: ExitStatus,
    rustc: ExitStatus,
}

impl Mismatch {
    fn render(self, p: &mut Painter<impl io::Write>) -> io::Result<()> {
        let status = |p: &mut Painter<_>, status: ExitStatus| match () {
            () if let Some(code) = status.code() => write!(p, "{code}"),
            () if let Some(signal) = status.signal() => write!(p, "{signal}s"),
            () => write!(p, "{}w", status.into_raw()),
        };

        status(p, self.rasur)?;
        write!(p, "·")?;
        status(p, self.rustc)
    }
}

fn check(
    entry: Result<walkdir::DirEntry, walkdir::Error>,
    rasur_path: &Path,
    rustc_path: &Path,
    opts: &TestOpts,
    stats: &mut Stats,
) {
    // FIXME: Mark file as invalid instead!
    let entry = entry.expect("failed to read dir entry");

    if !entry.file_type().is_file() {
        return;
    }

    let path = entry.path();
    if path.extension().is_none_or(|ext| ext != "rs") {
        return;
    }

    stats.total += 1;

    let mut subfailures = Vec::new();

    for &edition in &opts.editions {
        let rasur = run_rasur(path, edition, opts.measure, rasur_path);
        let rustc = run_rustc(path, edition, opts.measure, rustc_path);

        if (rasur == rustc) == opts.invert {
            subfailures.push((edition, Mismatch { rasur, rustc }));
        }
    }

    if !subfailures.is_empty() {
        stats.failures.push((path.display().to_string(), subfailures));
    }
}

fn run_rustc(path: &Path, edition: Edition, measure: Measure, rustc_path: &Path) -> ExitStatus {
    let mut cmd = Command::new(rustc_path);
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::null());
    cmd.arg(path);
    cmd.args(["--edition", edition.to_str(), "-Zunstable-options"]);

    match measure {
        Measure::ParseOnly => cmd.arg("-Zparse-crate-root-only"),
        Measure::CfgFalse => cmd.args([
            "-Zcrate-attr=cfg(false)",
            "--crate-type=lib",
            "--emit=metadata",
            "--out-dir",
            TMP_DIR_PATH,
        ]),
    };

    cmd.status().expect("failed to execute `rustc`")
}

fn run_rasur(path: &Path, edition: Edition, measure: Measure, rasur_path: &Path) -> ExitStatus {
    let mut cmd = Command::new(rasur_path);
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::null());
    cmd.arg(path);
    cmd.args(["--edition", edition.to_str()]);

    match measure {
        Measure::ParseOnly => {}
        Measure::CfgFalse => {
            cmd.arg("--gatekeep");
        }
    }

    cmd.status().expect("failed to execute `rasur`")
}

struct TestOpts {
    // FIXME: move out of test opts?
    editions: BTreeSet<Edition>,
    measure: Measure,
    invert: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Edition {
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024,
    Future,
}

impl Edition {
    fn to_str(self) -> &'static str {
        match self {
            Self::Rust2015 => "2015",
            Self::Rust2018 => "2018",
            Self::Rust2021 => "2021",
            Self::Rust2024 => "2024",
            Self::Future => "future",
        }
    }
}

#[derive(Clone, Copy)]
enum Measure {
    ParseOnly,
    CfgFalse,
}

mod interface {
    use super::{Edition, Measure, TestOpts};
    use clap::{Arg, ArgAction, Command, value_parser as P};
    use std::{num::NonZeroUsize, path::PathBuf};

    const DEFAULT_CHUNK_SIZE: &str = "10";
    const DEFAULT_MEASURE: &str = "parse-only";
    const DEFAULT_STORAGE_PATH: &str = "FAILURES.txt";

    pub(super) fn opts() -> Opts {
        let jobs = Arg::new(id::JOBS)
            .short('j')
            .long("jobs")
            .value_parser(P!(NonZeroUsize))
            .help("Set the number of threads used for running tests in parallel");
        let jobs = match std::thread::available_parallelism() {
            // FIXME: unfortunately, clap requires the &str to live for the static lifetime.
            Ok(value) => jobs.default_value(&*value.to_string().leak()),
            Err(_) => jobs.required(true),
        };

        let mut matches = Command::new(env!("CARGO_PKG_NAME"))
            // FIXME: is specifying this value parser necessary? ig ig
            .arg(
                Arg::new(id::PATHS)
                    .action(ArgAction::Append)
                    .value_parser(P!(PathBuf))
                    .help("Paths to the source files"),
            )
            .arg(
                Arg::new(id::EDITIONS)
                    .short('e')
                    .long("edition")
                    .value_parser(parse_editions)
                    .action(ArgAction::Append)
                    .default_value("2015")
                    .help("Set the editions used for the source files"),
            )
            .arg(
                Arg::new(id::TOOLCHAIN)
                    .short('T')
                    .long("toolchain")
                    .default_value("nightly")
                    .help("Override the rustup toolchain"),
            )
            .arg(
                Arg::new(id::MEASURE)
                    .short('m')
                    .long("measure")
                    .value_parser(parse_measure)
                    .default_value(DEFAULT_MEASURE),
            )
            .arg(Arg::new(id::INVERT).short('I').long("invert").action(ArgAction::SetTrue).help("Invert the test expectation (so unequal exit statuses means success)"))
            .arg(jobs)
            .arg(
                Arg::new(id::CHUNK_SIZE)
                    .short('c')
                    .long("chunk")
                    .value_parser(P!(NonZeroUsize))
                    .default_value(DEFAULT_CHUNK_SIZE).help("Set the amount of tests a single thread should run sequentially each 'step'"),
            )
            .arg(
                Arg::new(id::SAVE)
                    .long("save")
                    .value_name("PATH")
                    .require_equals(true)
                    .num_args(..=1)
                    .default_missing_value(DEFAULT_STORAGE_PATH)
                    .value_parser(P!(PathBuf))
                    .help(format!(
                        "Save the paths to failing tests in a file [fallback: {DEFAULT_STORAGE_PATH}]"
                    )),
            )
            .arg(
                Arg::new(id::LOAD)
                    .long("load")
                    .value_name("PATH")
                    .require_equals(true)
                    .num_args(..=1)
                    .default_missing_value(DEFAULT_STORAGE_PATH)
                    .value_parser(P!(PathBuf))
                    .conflicts_with(id::PATHS)
                    .help(format!("Load the paths from a file [fallback: {DEFAULT_STORAGE_PATH}]")),
            )
            .arg(Arg::new(id::DEBUG).short('D').long("debug").action(ArgAction::SetTrue).help("Use the debug build of rasur instead of the release one"))
            .get_matches();

        Opts {
            toolchain: matches.remove_one(id::TOOLCHAIN).unwrap(),
            paths: matches.remove_many(id::PATHS).map(Iterator::collect).unwrap_or_default(),
            jobs: matches.remove_one(id::JOBS).unwrap(),
            chunk_size: matches.remove_one(id::CHUNK_SIZE).unwrap(),
            test: TestOpts {
                editions: matches
                    .remove_many::<Editions>(id::EDITIONS)
                    .unwrap()
                    .flat_map(Editions::expand)
                    .copied()
                    .collect(),
                measure: matches.remove_one(id::MEASURE).unwrap(),
                invert: matches.remove_one(id::INVERT).unwrap_or_default(),
            },
            save: matches.remove_one(id::SAVE),
            load: matches.remove_one(id::LOAD),
            debug: matches.remove_one(id::DEBUG).unwrap_or_default(),
        }
    }

    pub(super) struct Opts {
        pub(super) toolchain: String,
        pub(super) paths: Vec<PathBuf>,
        pub(super) jobs: NonZeroUsize,
        pub(super) chunk_size: NonZeroUsize,
        pub(super) test: TestOpts,
        pub(super) save: Option<PathBuf>,
        pub(super) load: Option<PathBuf>,
        pub(super) debug: bool,
    }

    macro_rules! parse {
        ($( $key:literal => $value:expr ),+ $(,)?)  => {
            |source| Ok(match source {
                $( $key => $value, )+
                _ => return Err(format!("possible values: {}", [$(concat!("`", $key, "`")),+].join(", "))),
            })
        }
    }

    #[derive(Clone, Copy)]
    enum Editions {
        Rust2015,
        Rust2018,
        Rust2021,
        Rust2024,
        Future,
        Most,
        All,
    }

    impl Editions {
        fn expand(self) -> &'static [Edition] {
            use Edition::*;

            match self {
                Self::Rust2015 => &[Rust2015],
                Self::Rust2018 => &[Rust2018],
                Self::Rust2021 => &[Rust2021],
                Self::Rust2024 => &[Rust2024],
                Self::Future => &[Future],
                Self::Most => &[Rust2015, Rust2018, Rust2021, Rust2024],
                Self::All => &[Rust2015, Rust2018, Rust2021, Rust2024, Future],
            }
        }
    }

    fn parse_editions(source: &str) -> Result<Editions, String> {
        parse!(
            "2015" => Editions::Rust2015,
            "2018" => Editions::Rust2018,
            "2021" => Editions::Rust2021,
            "2024" => Editions::Rust2024,
            "future" => Editions::Future,
            "most" => Editions::Most,
            "all" => Editions::All,
        )(source)
    }

    fn parse_measure(source: &str) -> Result<Measure, String> {
        parse!(
            "parse-only" => Measure::ParseOnly,
            "cfg-false" => Measure::CfgFalse,
        )(source)
    }

    macro_rules! ids {
        ($( $id:ident ),+ $(,)?) => {
            mod id {
                $( pub(super) const $id: &str = stringify!($id); )+
            }
        }
    }

    ids! {
        CHUNK_SIZE,
        DEBUG,
        EDITIONS,
        INVERT,
        JOBS,
        LOAD,
        MEASURE,
        PATHS,
        SAVE,
        TOOLCHAIN,
    }
}
