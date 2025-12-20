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

pub struct Cfg {
    pub indent: usize,
    pub skip_marker: SkipMarker,
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4, skip_marker: SkipMarker::default() }
    }
}

#[derive(Default)]
pub enum SkipMarker {
    None,
    All,
    Rustfmt,
    #[default]
    Rasur,
}

macro fmt($cx:ident, $($arg:tt)*) {
    _ = $cx.output.write_fmt(format_args!($($arg)*))
}

pub fn fmt(file: ast::File<'_>, source: &str, cfg: Cfg) -> String {
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

    fn skip<M: ast::AttrMode>(&self, attrs: &[ast::Attr<'_, M>]) -> bool {
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

        let non_empty_attrs = !attrs.is_empty();
        attrs.interleave(LineBreak).fmt(cx);
        if non_empty_attrs && !items.is_empty() {
            LineBreak.fmt(cx);
        }
        items.interleave(LineBreak).fmt(cx);
    }
}

impl Fmt for ast::Attr<'_, ast::attr::Outer> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.upcast().fmt(cx);
    }
}

impl Fmt for ast::Attr<'_, ast::attr::Inner> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.upcast().fmt(cx);
    }
}

impl Fmt for ast::Attr<'_, ast::attr::Any> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { style, safety, path, kind } = self;

        fmt!(cx, "#");
        match style {
            ast::AttrStyle::Inner => fmt!(cx, "!"),
            ast::AttrStyle::Outer => {}
        }
        fmt!(cx, "[");

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe => fmt!(cx, "unsafe("),
        }

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

        match safety {
            ast::Safety::Inherited => {}
            ast::Safety::Unsafe => fmt!(cx, ")"),
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
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: Actually just print the source temporarily.
        self.interleave(" ").fmt(cx);
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

impl<X> Fmt for TrailingSpace<(ast::BorrowKind<X>, ast::Mutability)> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self((kind, mut_)) = self;

        match kind {
            ast::BorrowKind::Pin => fmt!(cx, "pin "),
            ast::BorrowKind::Raw(_) => fmt!(cx, "raw "),
            ast::BorrowKind::Ref => {}
        }
        mut_.trailing_space().fmt(cx);
        if let (ast::BorrowKind::Pin | ast::BorrowKind::Raw(_), ast::Mutability::Not) = (kind, mut_)
        {
            fmt!(cx, "const ");
        }
    }
}

struct Cluster<'src, T> {
    attrs: Vec<ast::Attr<'src, ast::attr::Inner>>,
    nodes: Vec<T>,
}

impl<T: Fmt> Fmt for Cluster<'_, T> {
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: Honor fmt skips here or check if all callers do!

        let Self { attrs, nodes } = self;

        let non_empty = !attrs.is_empty() || !nodes.is_empty();

        fmt!(cx, "{{");
        if non_empty {
            cx.indent();
            LineBreak.fmt(cx);
        }

        let non_empty_attrs = !attrs.is_empty();
        attrs.interleave(LineBreak).fmt(cx);
        if non_empty_attrs && !nodes.is_empty() {
            LineBreak.fmt(cx);
        }
        nodes.interleave(LineBreak).fmt(cx);

        if non_empty {
            cx.dedent();
            LineBreak.fmt(cx);
        }
        fmt!(cx, "}}");
    }
}

#[derive(Clone, Copy)]
struct LineBreak;

impl Fmt for LineBreak {
    fn fmt(self, cx: &mut Cx<'_>) {
        cx.output.push('\n');
        _ = cx.output.write_fmt(format_args!("{0:1$}", "", cx.indent));
    }
}

impl Fmt for &'static str {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "{self}");
    }
}

struct TrailingSpace<T>(T);

trait TrailingSpaceExt: Sized {
    fn trailing_space(self) -> TrailingSpace<Self> {
        TrailingSpace(self)
    }
}

impl<T> TrailingSpaceExt for T {}

struct Interleave<Nodes, Sep> {
    nodes: Nodes,
    sep: Sep,
}

impl<Node, Sep> Fmt for Interleave<Node, Sep>
where
    Node: IntoIterator<Item: Fmt>,
    Sep: Fmt + Copy,
{
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { nodes, sep } = self;

        let mut nodes = nodes.into_iter();
        if let Some(node) = nodes.next() {
            node.fmt(cx);
        }
        for node in nodes {
            sep.fmt(cx);
            node.fmt(cx);
        }
    }
}

trait InterleaveExt: Sized {
    fn interleave<Sep>(self, sep: Sep) -> Interleave<Self, Sep> {
        Interleave { nodes: self, sep }
    }
}

impl<T> InterleaveExt for T {}

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
