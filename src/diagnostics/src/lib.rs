#![deny(unused_must_use, rust_2018_idioms)]

use painter::{AnsiColor, Effects};
use std::{
    borrow::Cow,
    io::{self, Write as _},
    path::Path,
    range::Range,
};
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr as _;

pub struct Diag {
    severity: Severity,
    title: Option<Cow<'static, str>>,
    highlight: Option<(Range<usize>, Option<Cow<'static, str>>)>,
    subs: Vec<(SubSeverity, Cow<'static, str>)>,
}

impl Diag {
    pub fn new(severity: Severity) -> Self {
        Self { severity, title: None, highlight: None, subs: Vec::new() }
    }

    pub fn error(title: impl Into<Cow<'static, str>>) -> Self {
        Self::new(Severity::Error).title(title)
    }

    pub fn title(mut self, title: impl Into<Cow<'static, str>>) -> Self {
        self.title = Some(title.into());
        self
    }

    pub fn highlight(mut self, range: Range<usize>) -> Self {
        self.highlight = Some((range, None));
        self
    }

    pub fn label(mut self, label: impl Into<Cow<'static, str>>) -> Self {
        if let Some((_, label_)) = &mut self.highlight {
            *label_ = Some(label.into());
            self
        } else {
            self.note(label)
        }
    }

    pub fn note(mut self, note: impl Into<Cow<'static, str>>) -> Self {
        self.subs.push((SubSeverity::Note, note.into()));
        self
    }

    pub fn help(mut self, message: impl Into<Cow<'static, str>>) -> Self {
        self.subs.push((SubSeverity::Help, message.into()));
        self
    }

    pub fn emit(self, cx: &Context<'_>) {
        let mut p = Painter::new(std::io::stderr().lock(), io::BufWriter::new);
        self.render(cx, &mut p).unwrap();
    }

    fn render(self, cx: &Context<'_>, p: &mut Painter) -> io::Result<()> {
        let Self { severity, title, highlight, subs } = self;

        p.set(Effects::BOLD)?;
        p.with(severity.color(), |p| write!(p, "{}", severity.name()))?;
        if let Some(title) = title {
            if !severity.is_serious() {
                p.unset()?;
            }
            write!(p, ": {title}")?;
            if severity.is_serious() {
                p.unset()?;
            }
        }
        if let Some(file) = &cx.file
            && let Some((range, label)) = highlight
        {
            let (first_line, last_line) = lines(file.source, range);

            let column_number =
                first_line.content[..first_line.highlight.start as usize].graphemes(true).count()
                    + 1;

            write!(p, "    ")?;
            p.with(Effects::ITALIC, |p| {
                match file.path {
                    SourcePath::Anon => {}
                    SourcePath::Real(path) => write!(p, "{}", path.display())?,
                }
                write!(p, ":{}:{column_number}", first_line.number)
            })?;

            if !cx.short {
                first_line.render(severity, label, last_line.is_some(), p)?;

                if let Some(last_line) = last_line {
                    last_line.render(severity, None, false, p)?;
                }
            }
        }
        for (severity, message) in subs {
            const INDENT: &str = "  ";
            let severity = severity.name();
            writeln!(p)?;
            p.with(SubSeverity::COLOR.on_default().bold(), |p| write!(p, "{INDENT}{severity}"))?;
            let mut lines = message.split('\n');
            write!(p, ": {}", lines.next().unwrap())?;
            let padding = " ".repeat(severity.len());
            for line in lines {
                writeln!(p)?;
                write!(p, "{INDENT}{padding}  {line}")?;
            }
        }
        writeln!(p)?;
        p.flush()?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub enum Severity {
    Bug,
    Error,
    Warning,
    Debug,
}

impl Severity {
    fn name(self) -> &'static str {
        match self {
            Self::Bug => "internal error",
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Debug => "debug",
        }
    }

    fn color(self) -> AnsiColor {
        match self {
            Self::Bug | Self::Error => AnsiColor::BrightRed,
            Self::Warning => AnsiColor::BrightYellow,
            Self::Debug => AnsiColor::BrightMagenta,
        }
    }

    fn is_serious(self) -> bool {
        matches!(self, Self::Error | Self::Warning)
    }
}

pub enum SubSeverity {
    Note,
    Help,
}

impl SubSeverity {
    const COLOR: AnsiColor = AnsiColor::BrightWhite;

    fn name(self) -> &'static str {
        match self {
            Self::Note => "note",
            Self::Help => "help",
        }
    }
}

fn lines(source: &str, range: Range<usize>) -> (Line<'_>, Option<Line<'_>>) {
    let mut needles = source[range].split('\n');
    let mut needle = needles.next().unwrap();

    let mut first = None;
    let mut last = None;

    for (index, line) in source.split('\n').enumerate() {
        if let Some(highlight) = line.substr_range(needle) {
            let line = Line { number: index + 1, content: line, highlight };
            *if first.is_none() { &mut first } else { &mut last } = Some(line);
            needle = match needles.next() {
                Some(needle) => needle,
                None => break,
            }
        }
    }

    (first.unwrap(), last)
}

struct Line<'a> {
    number: usize,
    content: &'a str,
    highlight: Range<usize>,
}

impl Line<'_> {
    fn render(
        self,
        severity: Severity,
        label: Option<Cow<'static, str>>,
        incomplete: bool,
        p: &mut Painter,
    ) -> io::Result<()> {
        let Self { number: _, content, highlight } = self;

        struct Underline {
            string: String,
            offset: usize,
        }

        let underline =
            match (content[..highlight.start as usize].width(), content[highlight].width()) {
                (0, 0) => Underline { string: "\\".into(), offset: 0 },
                (offset, 0) => Underline { string: "/\\".into(), offset: offset - 1 },
                (offset, width) => {
                    let mut string = "^".repeat(width);
                    if incomplete {
                        string += "...";
                    }
                    Underline { string, offset }
                }
            };

        writeln!(p)?;
        writeln!(p, "{}", self.content)?;

        write!(p, "{}", " ".repeat(underline.offset))?;
        p.with(severity.color().on_default().bold(), |p| {
            write!(p, "{}", underline.string)?;
            if let Some(label) = label {
                let mut lines = label.split('\n');
                write!(p, " {}", lines.next().unwrap())?;
                let padding = " ".repeat(underline.offset + underline.string.len());
                for line in lines {
                    writeln!(p)?;
                    write!(p, "{padding} {line}")?;
                }
            }
            Ok(())
        })?;

        io::Result::Ok(())
    }
}

type Painter = painter::Painter<io::BufWriter<io::StderrLock<'static>>>;

pub struct Context<'a> {
    pub short: bool,
    pub file: Option<SourceFile<'a>>,
}

pub struct SourceFile<'a> {
    pub path: SourcePath<'a>,
    pub source: &'a str,
}

#[derive(Clone, Copy)]
pub enum SourcePath<'a> {
    Real(&'a Path),
    Anon,
}
