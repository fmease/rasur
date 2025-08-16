use crate::token;
use crate::{ast, span::Span};
use std::fmt::Write as _;

mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

// FIXME: Reproduce comments.
// FIXME: The indentation setup is busted.

pub(crate) struct Cfg {
    pub(crate) indent: usize,
    pub(crate) skip_marker: SkipMarker,
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4, skip_marker: SkipMarker::default() }
    }
}

#[derive(Default)]
pub(crate) enum SkipMarker {
    None,
    All,
    Rustfmt,
    #[default]
    Rasur,
}

macro fmt($cx:ident, $($arg:tt)*) {
    _ = $cx.output.write_fmt(format_args!($($arg)*))
}

pub(crate) fn fmt(file: ast::File<'_>, source: &str, cfg: Cfg) -> String {
    let mut cx = Cx { cfg, source, indent: 0, output: String::with_capacity(source.len()) };
    file.fmt(&mut cx);
    cx.output
}

struct Cx<'src> {
    cfg: Cfg,
    source: &'src str,
    indent: usize,
    output: String,
}

impl<'src> Cx<'src> {
    fn source(&self, span: Span) -> &'src str {
        &self.source[span.range()]
    }

    fn indent(&mut self) {
        self.indent += self.cfg.indent;
    }

    fn dedent(&mut self) {
        self.indent -= self.cfg.indent;
    }

    fn line_break(&mut self) {
        self.output.push('\n');
        _ = self.output.write_fmt(format_args!("{0:1$}", "", self.indent));
    }

    fn skip(&self, attrs: &[ast::Attr<'_>]) -> bool {
        if let SkipMarker::None = self.cfg.skip_marker {
            return false;
        }

        // FIXME: Look into cfg_attrs, too
        // FIXME: Support rustfmt_skip or whatever that legacy attr is called
        attrs.iter().any(|attr| {
            let ast::AttrKind::Unit = attr.kind else { return false };

            let &[ast::PathSeg { ident: tool, args: () }, ast::PathSeg { ident: "skip", args: () }] =
                attr.path.segs.as_slice()
            else {
                return false;
            };

            match self.cfg.skip_marker {
                SkipMarker::None => unreachable!(),
                SkipMarker::All => matches!(tool, "rustfmt" | "rasur"),
                SkipMarker::Rustfmt => tool == "rustfmt",
                SkipMarker::Rasur => tool == "rasur",
            }
        })
    }
}

impl Fmt for ast::File<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, items, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }

        if !attrs.is_empty() {
            for attr in attrs {
                attr.fmt(cx);
                cx.line_break();
            }
            cx.line_break();
        }

        for item in items {
            item.fmt(cx);
            cx.line_break();
        }
    }
}

impl Fmt for ast::Attr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { style, path, kind } = self;

        fmt!(cx, "#");
        if let ast::AttrStyle::Inner = style {
            fmt!(cx, "!");
        }
        fmt!(cx, "[");
        path.fmt(cx);
        match kind {
            ast::AttrKind::Unit => {}
            ast::AttrKind::Call(bracket, stream) => {
                (bracket, ast::Orientation::Open).fmt(cx);
                stream.fmt(cx);
                (bracket, ast::Orientation::Close).fmt(cx);
            }
            ast::AttrKind::Assign(expr) => {
                fmt!(cx, " = ");
                expr.fmt(cx);
            }
        }
        fmt!(cx, "]");
    }
}

impl Fmt for ast::Lit<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Bool(lit) => fmt!(cx, "{lit}"),
            Self::Char(lit) => fmt!(cx, "{lit}"),
            Self::Num(lit) | Self::Str(lit) => fmt!(cx, "{lit}"),
        }
    }
}

impl Fmt for ast::TokenStream {
    // FIXME: Actually just print as is for now
    fn fmt(self, cx: &mut Cx<'_>) {
        Punctuated::new(self, " ").fmt(cx);
    }
}

impl Fmt for token::Token {
    fn fmt(self, cx: &mut Cx<'_>) {
        let str = match self.kind.repr() {
            token::Repr::Src(src) => src,
            token::Repr::Tag(_) => cx.source(self.span),
        };
        fmt!(cx, "{str}");
    }
}

impl<M: path::GenericArgsMode> Fmt for ast::MacroCall<'_, M> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { path, bracket, stream } = self;

        path.fmt(cx);
        fmt!(cx, "!");
        if let ast::Bracket::Curly = bracket {
            fmt!(cx, " ");
        }
        (bracket, ast::Orientation::Open).fmt(cx);
        stream.fmt(cx);
        (bracket, ast::Orientation::Close).fmt(cx);
    }
}

impl Fmt for (ast::Bracket, ast::Orientation) {
    fn fmt(self, cx: &mut Cx<'_>) {
        #![expect(clippy::enum_glob_use)]
        use ast::Bracket::*;
        use ast::Orientation::*;
        let fmt = match self {
            (Round, Open) => "(",
            (Round, Close) => ")",
            (Square, Open) => "[",
            (Square, Close) => "]",
            (Curly, Open) => "{",
            (Curly, Close) => "}",
        };
        fmt!(cx, "{fmt}");
    }
}

impl Fmt for TrailingSpace<ast::Mutability> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(mut_) = self;
        match mut_ {
            ast::Mutability::Mut => fmt!(cx, "mut "),
            ast::Mutability::Not => {}
        }
    }
}

struct TrailingSpace<T>(T);

trait TrailingSpaceExt: Sized {
    fn trailing_space(self) -> TrailingSpace<Self> {
        TrailingSpace(self)
    }
}

impl<T> TrailingSpaceExt for T {}

struct Punctuated<T> {
    nodes: Vec<T>,
    sep: &'static str,
}

impl<T> Punctuated<T> {
    fn new(nodes: Vec<T>, sep: &'static str) -> Self {
        Self { nodes, sep }
    }
}

impl<T: Fmt> Fmt for Punctuated<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { nodes, sep } = self;
        let mut nodes = nodes.into_iter();
        if let Some(node) = nodes.next() {
            node.fmt(cx);
        }
        for node in nodes {
            fmt!(cx, "{sep}");
            node.fmt(cx);
        }
    }
}

struct Tup<T>(Vec<T>);

impl<T: Fmt> Fmt for Tup<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(nodes) = self;
        fmt!(cx, "(");
        // FIXME: Simplify!
        if !nodes.is_empty() {
            let mut nodes = nodes.into_iter();
            if let Some(node) = nodes.next() {
                node.fmt(cx);
            }
            match nodes.next() {
                Some(node) => {
                    fmt!(cx, ", ");
                    node.fmt(cx);
                }
                None => fmt!(cx, ","),
            }
            for node in nodes {
                fmt!(cx, ", ");
                node.fmt(cx);
            }
        }
        fmt!(cx, ")");
    }
}

impl<T: Fmt> Fmt for Box<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        (*self).fmt(cx);
    }
}

impl<T: Fmt> Fmt for Option<T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if let Some(this) = self {
            this.fmt(cx);
        }
    }
}

trait Fmt {
    fn fmt(self, cx: &mut Cx<'_>);
}
