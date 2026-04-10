mod attr;
mod expr;
mod item;
mod pat;
mod path;
mod stmt;
mod ty;

use crate::{
    ast,
    edition::Edition,
    lexer::Frontmatter,
    span::{At as _, Span},
};
use std::fmt::Write as _;

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

#[derive(Default, Clone, Copy)]
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

#[must_use]
pub fn fmt(
    file: ast::File<'_>,
    source: &str,
    shebang: Option<Span>,
    frontmatter: Option<Frontmatter>,
    edition: Edition,
    cfg: Cfg,
) -> String {
    let output = String::with_capacity(source.len()); // FIXME: better heuristic
    let mut cx = Cx { cfg, source, shebang, frontmatter, edition, indent: 0, output };
    file.fmt(&mut cx);
    cx.output
}

struct Cx<'src> {
    cfg: Cfg,
    source: &'src str,
    shebang: Option<Span>,
    frontmatter: Option<Frontmatter>,
    edition: Edition,
    indent: usize,
    output: String,
}

impl<'src> Cx<'src> {
    fn source(&self, span: Span) -> &'src str {
        self.source.at(span)
    }

    fn indent(&mut self) {
        self.indent += self.cfg.indent;
    }

    fn dedent(&mut self) {
        self.indent -= self.cfg.indent;
    }

    // FIXME: rename to `should_skip`
    fn skip<M: ast::AttrMode>(&self, attrs: &[ast::Attr<'_, M>]) -> bool {
        if let SkipMarker::None = self.cfg.skip_marker {
            return false;
        }

        // FIXME: Look into cfg_attrs, too
        // FIXME: Support rustfmt_skip or whatever that legacy attr is called
        attrs.iter().any(|attr| {
            let ast::AttrKind::Regular(attr) = &attr.kind else { return false };
            let ast::MetaArgs::Unit = attr.args else { return false };

            let &[
                ast::PathSeg { ident: ast::Ident!(tool), args: () },
                ast::PathSeg { ident: ast::Ident!("skip"), args: () },
            ] = attr.path.segs.as_slice()
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
        let Self { attrs, items } = self;

        if cx.skip(&attrs) {
            cx.source.fmt(cx);
            return;
        }

        if let Some(shebang) = cx.shebang {
            fmt!(cx, "{}", cx.source(shebang));
            LineBreak.fmt(cx);
        }

        if let Some(Frontmatter { fence, infostring, content, span: _ }) = cx.frontmatter {
            let fence = usize::from(fence.into_inner());

            fmt!(cx, "{:->fence$}", "");
            if !infostring.is_empty() {
                fmt!(cx, " {}", cx.source(infostring));
            }
            LineBreak.fmt(cx);
            fmt!(cx, "{}", cx.source(content));
            fmt!(cx, "{:->fence$}", "");
            LineBreak.fmt(cx);
        }

        let non_empty_attrs = !attrs.is_empty();
        attrs.interleave(LineBreak).fmt(cx);
        if non_empty_attrs && !items.is_empty() {
            LineBreak.fmt(cx);
        }
        items.interleave(LineBreak).fmt(cx);
    }
}

impl Fmt for ast::Lit<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { kind: _, value, suffix } = self;
        value.fmt(cx);
        suffix.fmt(cx);
    }
}

impl Fmt for ast::TokenStream {
    fn fmt(self, cx: &mut Cx<'_>) {
        // FIXME: That's really naive (and wrong in the case of LitSuffix).
        self.interleave(" ").fmt(cx);
    }
}

impl Fmt for ast::Token {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { kind: _, span } = self;

        cx.source(span).fmt(cx);
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
    attrs: Vec<ast::Attr<'src, ast::InnerAttrStyle>>,
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

impl Fmt for &str {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "{self}");
    }
}

struct TrailingSpace<T>(T);

impl<T: Fmt> Fmt for TrailingSpace<Option<T>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(Some(node)) = self else { return };
        node.fmt(cx);
        fmt!(cx, " ");
    }
}

trait TrailingSpaceExt: Sized {
    #[must_use]
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
        nodes.next().fmt(cx);
        for node in nodes {
            sep.fmt(cx);
            node.fmt(cx);
        }
    }
}

trait InterleaveExt: Sized {
    #[must_use]
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
            nodes.next().fmt(cx);
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

struct BuiltinSyntax<F: FnOnce(&mut Cx<'_>)>(&'static str, F);

impl<F: FnOnce(&mut Cx<'_>)> Fmt for BuiltinSyntax<F> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(name, content) = self;

        fmt!(cx, "builtin");
        if cx.edition >= Edition::Rust2021 {
            fmt!(cx, " ");
        }
        fmt!(cx, "#");
        if cx.edition >= Edition::Rust2021 {
            fmt!(cx, " ");
        }
        name.fmt(cx);
        fmt!(cx, "(");
        content(cx);
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

struct Inline<F: FnOnce(&mut Cx<'_>)>(F);

impl<F: FnOnce(&mut Cx<'_>)> Fmt for Inline<F> {
    fn fmt(self, cx: &mut Cx<'_>) {
        self.0(cx)
    }
}

trait Fmt {
    fn fmt(self, cx: &mut Cx<'_>);
}
