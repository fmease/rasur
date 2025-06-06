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
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4 }
    }
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

    fn skip(attrs: &[ast::Attr<'_>]) -> bool {
        // FIXME: Look into cfg_attrs, too
        // FIXME: Make tool mod config'able: "rasur"|"rustfmt"|both
        // FIXME: Support rustfmt_skip or whatever that legacy attr is called
        attrs.iter().any(|attr| {
            matches!(
                &*attr.path.segs,
                [
                    ast::PathSeg { ident: "rustfmt", args: () },
                    ast::PathSeg { ident: "skip", args: () }
                ]
            ) && matches!(attr.kind, ast::AttrKind::Unit)
        })
    }
}

impl Fmt for ast::File<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, items, span } = self;

        if Cx::skip(&attrs) {
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

impl Fmt for ast::TokenStream {
    // FIXME: Actually just print as is for now
    fn fmt(self, cx: &mut Cx<'_>) {
        Punctuated::new(self, " ").fmt(cx);
    }
}

impl Fmt for ast::Token {
    fn fmt(self, cx: &mut Cx<'_>) {
        let str = match self.kind {
            ast::TokenKind::Ampersand => "&",
            ast::TokenKind::Apostrophe => "'",
            ast::TokenKind::Asterisk => "*",
            ast::TokenKind::At => "@",
            ast::TokenKind::Bang => "!",
            ast::TokenKind::BangEquals => "!=",
            ast::TokenKind::Caret => "^",
            ast::TokenKind::CloseCurlyBracket => "}",
            ast::TokenKind::CloseRoundBracket => ")",
            ast::TokenKind::CloseSquareBracket => "]",
            ast::TokenKind::Colon => ":",
            ast::TokenKind::Comma => ",",
            ast::TokenKind::Dot => ".",
            ast::TokenKind::DoubleAmpersand => "&&",
            ast::TokenKind::DoubleColon => "::",
            ast::TokenKind::DoubleDot => "..",
            ast::TokenKind::DoubleEquals => "==",
            ast::TokenKind::DoublePipe => "||",
            ast::TokenKind::EndOfInput => "",
            ast::TokenKind::Equals => "=",
            ast::TokenKind::GreaterThan => ">",
            ast::TokenKind::GreaterThanEquals => ">=",
            ast::TokenKind::Hash => "#",
            ast::TokenKind::Hyphen => "-",
            ast::TokenKind::LessThan => "<",
            ast::TokenKind::LessThanEquals => "<=",
            ast::TokenKind::OpenCurlyBracket => "{",
            ast::TokenKind::OpenRoundBracket => "(",
            ast::TokenKind::OpenSquareBracket => "[",
            ast::TokenKind::Percent => "%",
            ast::TokenKind::Pipe => "|",
            ast::TokenKind::Plus => "+",
            ast::TokenKind::QuestionMark => "?",
            ast::TokenKind::Semicolon => ";",
            ast::TokenKind::Slash => "/",
            ast::TokenKind::ThinArrow => "->",
            ast::TokenKind::TripleDot => "...",
            ast::TokenKind::WideArrow => "=>",
            ast::TokenKind::Ident
            | ast::TokenKind::NumLit
            | ast::TokenKind::StrLit
            | ast::TokenKind::Error => cx.source(self.span),
        };
        fmt!(cx, "{str}");
    }
}

impl<A: path::FmtGenericArgs> Fmt for ast::MacroCall<'_, A> {
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
