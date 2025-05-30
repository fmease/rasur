use crate::{ast, span::Span};
use std::fmt::Write as _;

// FIXME: Reproduce comments.
// FIXME: The indentation setup is busted.
//        The context should automatically push self.indent after `\n` on fmt (smh)

pub(crate) struct Cfg {
    pub(crate) indent: usize,
}

impl Default for Cfg {
    fn default() -> Self {
        Self { indent: 4 }
    }
}

macro_rules! fmt {
    ($cx:ident, indent) => {
        _ = $cx.buf.write_str(&" ".repeat($cx.indent)) // FIXME: Don't allocate extra string!
    };
    ($cx:ident, $($arg:tt)*) => {
        _ = $cx.buf.write_fmt(format_args!($($arg)*))
    };
}

pub(crate) fn fmt(file: ast::File<'_>, source: &str, cfg: Cfg) -> String {
    let mut cx = Cx { cfg, source, indent: 0, buf: String::with_capacity(source.len()) };
    file.fmt(&mut cx);
    cx.buf
}

struct Cx<'src> {
    cfg: Cfg,
    source: &'src str,
    indent: usize,
    buf: String,
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

    fn skip(&self, attrs: &[ast::Attr<'_>]) -> bool {
        // FIXME: Look into cfg_attrs, too
        // FIXME: Make tool mod config'able: "rasur"|"rustfmt"|both
        attrs.iter().any(|attr| {
            matches!(attr.path.hook, ast::PathHook::Local)
                && matches!(
                    &*attr.path.segs,
                    [
                        ast::PathSeg { ident: "rustfmt", args: () },
                        ast::PathSeg { ident: "skip", args: () }
                    ]
                )
                && matches!(attr.kind, ast::AttrKind::Unit)
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
                fmt!(cx, "\n");
            }
            fmt!(cx, "\n");
        }

        Punctuated::new(items, "\n\n").fmt(cx);
    }
}

impl Fmt for ast::Item<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, "\n");
        }

        // FIXME: Not all items support visibility.
        vis.fmt(cx);

        match kind {
            ast::ItemKind::Const(item) => item.fmt(cx),
            ast::ItemKind::Enum(item) => item.fmt(cx),
            ast::ItemKind::ExternBlock(item) => item.fmt(cx),
            ast::ItemKind::Fn(item) => item.fmt(cx),
            ast::ItemKind::Impl(item) => item.fmt(cx),
            ast::ItemKind::Mod(item) => item.fmt(cx),
            ast::ItemKind::Static(item) => item.fmt(cx),
            ast::ItemKind::Struct(item) => item.fmt(cx),
            ast::ItemKind::Trait(item) => item.fmt(cx),
            ast::ItemKind::Ty(item) => item.fmt(cx),
            ast::ItemKind::Union(item) => item.fmt(cx),
            ast::ItemKind::MacroDef(item) => item.fmt(cx),
            ast::ItemKind::MacroCall(call) => {
                let needs_semi = call.bracket != ast::Bracket::Curly;
                call.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
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

impl<'src, A: FmtGenericArgs> Fmt for ast::Path<'src, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { hook, segs } = self;

        if let ast::PathHook::Global = hook {
            fmt!(cx, "::");
        }
        Punctuated::new(segs, "::").fmt(cx);
    }
}

impl<'src, A: FmtGenericArgs> Fmt for ast::PathSeg<'src, A> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { ident, args } = self;

        fmt!(cx, "{ident}");
        A::fmt(args, cx);
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
            ast::TokenKind::Apostrophe => "'",
            ast::TokenKind::Bang => "!",
            ast::TokenKind::QuestionMark => "?",
            ast::TokenKind::CloseAngleBracket => ">",
            ast::TokenKind::CloseCurlyBracket => "}",
            ast::TokenKind::CloseRoundBracket => ")",
            ast::TokenKind::CloseSquareBracket => "]",
            ast::TokenKind::Colon => ":",
            ast::TokenKind::Comma => ",",
            ast::TokenKind::Dot => ".",
            ast::TokenKind::EndOfInput => "",
            ast::TokenKind::Equals => "=",
            ast::TokenKind::Hash => "#",
            ast::TokenKind::Hyphen => "-",
            ast::TokenKind::OpenAngleBracket => "<",
            ast::TokenKind::OpenCurlyBracket => "{",
            ast::TokenKind::OpenRoundBracket => "(",
            ast::TokenKind::OpenSquareBracket => "[",
            ast::TokenKind::Plus => "+",
            ast::TokenKind::Semicolon => ";",
            ast::TokenKind::Slash => "/",
            ast::TokenKind::Asterisk => "*",
            ast::TokenKind::ThinArrow => "->",
            ast::TokenKind::WideArrow => "=>",
            ast::TokenKind::Ampersand => "&",
            ast::TokenKind::Pipe => "|",
            ast::TokenKind::Ident
            | ast::TokenKind::NumLit
            | ast::TokenKind::StrLit
            | ast::TokenKind::Error => cx.source(self.span),
        };
        fmt!(cx, "{str}")
    }
}

impl Fmt for ast::Generics<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { params, preds } = self;

        params.fmt(cx);
        preds.fmt(cx);
    }
}

impl Fmt for Vec<ast::GenericParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if !self.is_empty() {
            fmt!(cx, "<");
            Punctuated::new(self, ", ").fmt(cx);
            fmt!(cx, ">");
        }
    }
}

impl Fmt for ast::GenericParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self.kind {
            ast::GenericParamKind::Ty(bounds) => {
                fmt!(cx, "{}", self.binder);
                if !bounds.is_empty() {
                    fmt!(cx, ": ");
                    bounds.fmt(cx);
                }
            }
            ast::GenericParamKind::Const(ty) => {
                fmt!(cx, "const {}: ", self.binder);
                ty.fmt(cx);
            }
            ast::GenericParamKind::Lifetime(bounds) => {
                ast::Lifetime(self.binder).fmt(cx);
                if !bounds.is_empty() {
                    fmt!(cx, ": ");
                    Punctuated::new(bounds, " + ").fmt(cx);
                }
            }
        }
    }
}

trait FmtGenericArgs: ast::GenericArgs::Kind {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>);
}

impl FmtGenericArgs for ast::GenericArgs::Disallowed {
    fn fmt((): Self::Args<'_>, _: &mut Cx<'_>) {}
}

impl FmtGenericArgs for ast::GenericArgs::Allowed {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        args.fmt(cx);
    }
}

impl FmtGenericArgs for ast::GenericArgs::DisambiguatedOnly {
    fn fmt(args: Self::Args<'_>, cx: &mut Cx<'_>) {
        if args.as_ref().is_some_and(|args| !args.is_empty()) {
            fmt!(cx, "::");
        }
        args.fmt(cx);
    }
}

impl Fmt for Vec<ast::GenericArg<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if !self.is_empty() {
            fmt!(cx, "<");
            Punctuated::new(self, ", ").fmt(cx);
            fmt!(cx, ">");
        }
    }
}

impl Fmt for ast::GenericArg<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            ast::GenericArg::Ty(ty) => ty.fmt(cx),
            ast::GenericArg::Const => todo!(), // FIXME
            ast::GenericArg::Lifetime(lt) => lt.fmt(cx),
        }
    }
}

impl Fmt for Vec<ast::Predicate<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        if self.is_empty() {
            return;
        }
        fmt!(cx, " where ");
        Punctuated::new(self, ", ").fmt(cx);
    }
}

impl Fmt for ast::Predicate<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(pred) => {
                pred.ty.fmt(cx);
                fmt!(cx, ":");
                if !pred.bounds.is_empty() {
                    fmt!(cx, " ");
                }
                pred.bounds.fmt(cx);
            }
            Self::Outlives(pred) => {
                pred.lt.fmt(cx);
                fmt!(cx, ":");
                if !pred.bounds.is_empty() {
                    fmt!(cx, " ");
                }
                Punctuated::new(pred.bounds, " + ").fmt(cx);
            }
        }
    }
}

impl Fmt for Vec<ast::Bound<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        Punctuated::new(self, " + ").fmt(cx);
    }
}

impl Fmt for ast::Bound<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Trait(mods, path) => {
                mods.fmt(cx);
                path.fmt(cx)
            }
            Self::Outlives(lt) => lt.fmt(cx),
        }
    }
}

impl Fmt for ast::TraitBoundModifiers {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { polarity } = self;

        match polarity {
            ast::BoundPolarity::Positive => {}
            ast::BoundPolarity::Negative => fmt!(cx, "!"),
            ast::BoundPolarity::Maybe => fmt!(cx, "?"),
        }
    }
}

impl Fmt for ast::ConstItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, ty, body } = self;

        fmt!(cx, "const {binder}");
        generics.params.fmt(cx);
        fmt!(cx, ": ");
        ty.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";")
    }
}

impl Fmt for ast::EnumItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics } = self;

        fmt!(cx, "enum {binder}");
        generics.fmt(cx);
        fmt!(cx, " {{}}");
    }
}

impl Fmt for ast::ExternBlockItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { abi, body } = self;

        fmt!(cx, "extern {:?}", abi.unwrap_or("C"));
        body.fmt(cx);
    }
}

impl Fmt for Vec<ast::ExternItem<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, " {{");
        if self.is_empty() {
            fmt!(cx, "}}")
        } else {
            fmt!(cx, "\n");
            cx.indent();
            Punctuated::new(self, "\n\n").fmt(cx); // FIXME: doesn't indent elems
            cx.dedent();
            fmt!(cx, "\n}}");
        }
    }
}

impl Fmt for ast::ExternItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, "\n");
        }

        // FIXME: Not all assoc items support visibility.
        vis.fmt(cx);

        match kind {
            ast::ExternItemKind::Fn(item) => item.fmt(cx),
            ast::ExternItemKind::Static(item) => item.fmt(cx),
            ast::ExternItemKind::Ty(item) => item.fmt(cx),
            ast::ExternItemKind::MacroCall(call) => {
                let needs_semi = call.bracket != ast::Bracket::Curly;
                call.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
        }
    }
}

impl Fmt for ast::FnItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { constness, binder, generics, params, ret_ty, body } = self;

        match constness {
            ast::Constness::Const => fmt!(cx, "const "),
            ast::Constness::Not => {}
        }
        fmt!(cx, "fn {binder}");
        generics.params.fmt(cx);
        params.fmt(cx);
        if let Some(ty) = ret_ty {
            fmt!(cx, " -> ");
            ty.fmt(cx);
        }
        generics.preds.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " ");
            body.fmt(cx);
        } else {
            fmt!(cx, ";");
        }
    }
}

impl Fmt for Vec<ast::FnParam<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, "(");
        Punctuated::new(self, ", ").fmt(cx);
        fmt!(cx, ")");
    }
}

impl Fmt for ast::FnParam<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, ty } = self;

        pat.fmt(cx);
        fmt!(cx, ": ");
        ty.fmt(cx);
    }
}

impl Fmt for ast::ImplItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { generics, constness, polarity, trait_ref, self_ty, body } = self;

        fmt!(cx, "impl");
        generics.params.fmt(cx);
        fmt!(cx, " ");
        if let ast::Constness::Const = constness {
            fmt!(cx, "const ");
        }
        if let ast::ImplPolarity::Negative = polarity {
            fmt!(cx, "!");
        }
        if let Some(trait_ref) = trait_ref {
            trait_ref.fmt(cx);
            fmt!(cx, " for ");
        }
        self_ty.fmt(cx);
        generics.preds.fmt(cx);
        body.fmt(cx);
    }
}

impl Fmt for ast::ModItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, body } = self;

        fmt!(cx, "mod {binder}");
        match body {
            Some(items) => {
                fmt!(cx, " {{\n");
                cx.indent();
                for item in items {
                    fmt!(cx, indent);
                    item.fmt(cx);
                }
                cx.dedent();
                fmt!(cx, indent);
                fmt!(cx, "}}");
            }
            None => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::StaticItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { mut_, binder, ty, body } = self;

        fmt!(cx, "static ");
        match mut_ {
            ast::Mutability::Mut => fmt!(cx, "mut "),
            ast::Mutability::Imm => {}
        }
        fmt!(cx, "{binder}: ");
        ty.fmt(cx);
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";")
    }
}

impl Fmt for ast::StructItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, body } = self;

        fmt!(cx, "struct {binder}");
        generics.fmt(cx);
        match body {
            ast::StructBody::Normal { fields } => {
                fmt!(cx, " {{\n");
                cx.indent();
                for field in fields {
                    fmt!(cx, indent);
                    field.fmt(cx);
                    fmt!(cx, ",\n");
                }
                cx.dedent();
                fmt!(cx, indent);
                fmt!(cx, "}}");
            }
            ast::StructBody::Unit => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::StructField<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { vis, binder, ty } = self;

        vis.fmt(cx);
        fmt!(cx, "{binder}: ");
        ty.fmt(cx);
    }
}

impl Fmt for ast::TraitItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, bounds, body } = self;

        fmt!(cx, "trait {binder}");
        generics.params.fmt(cx);
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        generics.preds.fmt(cx);
        body.fmt(cx);
    }
}

impl Fmt for ast::TyItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics, bounds, body } = self;

        fmt!(cx, "type {binder}");
        generics.params.fmt(cx);
        if !bounds.is_empty() {
            fmt!(cx, ": ");
            bounds.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        generics.preds.fmt(cx);
        fmt!(cx, ";")
    }
}

impl Fmt for ast::UnionItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, generics } = self;

        fmt!(cx, "union {binder}");
        generics.fmt(cx);
        fmt!(cx, " {{}}")
    }
}

impl Fmt for ast::MacroDef<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { binder, params, body, style } = self;

        let prefix = match style {
            ast::MacroDefStyle::Old => "macro_rules!",
            ast::MacroDefStyle::New => "macro",
        };

        fmt!(cx, "{prefix} {binder}");
        if let Some(params) = params {
            fmt!(cx, "(");
            params.fmt(cx);
            fmt!(cx, ")");
        }
        fmt!(cx, " {{ ");
        body.fmt(cx);
        fmt!(cx, " }}");
    }
}

impl Fmt for Vec<ast::AssocItem<'_>> {
    fn fmt(self, cx: &mut Cx<'_>) {
        fmt!(cx, " {{");
        if self.is_empty() {
            fmt!(cx, "}}")
        } else {
            fmt!(cx, "\n");
            cx.indent();
            Punctuated::new(self, "\n\n").fmt(cx); // FIXME: doesn't indent elems
            cx.dedent();
            fmt!(cx, "\n}}");
        }
    }
}

impl Fmt for ast::AssocItem<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, vis, kind, span } = self;

        if cx.skip(&attrs) {
            fmt!(cx, "{}", cx.source(span));
            return;
        }
        for attr in attrs {
            attr.fmt(cx);
            fmt!(cx, "\n");
        }

        // FIXME: Not all assoc items support visibility.
        vis.fmt(cx);

        match kind {
            ast::AssocItemKind::Const(item) => item.fmt(cx),
            ast::AssocItemKind::Fn(item) => item.fmt(cx),
            ast::AssocItemKind::Ty(item) => item.fmt(cx),
            ast::AssocItemKind::MacroCall(call) => {
                let needs_semi = call.bracket != ast::Bracket::Curly;
                call.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
        }
    }
}

impl Fmt for ast::Ty<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Inferred => fmt!(cx, "_"),
            Self::FnPtr((), ret_ty) => {
                fmt!(cx, "fn()");
                if let Some(ret_ty) = ret_ty {
                    fmt!(cx, " -> ");
                    ret_ty.fmt(cx);
                }
            }
            Self::Ref(lt, mut_, ty) => {
                fmt!(cx, "&");
                if let Some(lt) = lt {
                    lt.fmt(cx);
                    fmt!(cx, " ");
                }
                match mut_ {
                    ast::Mutability::Mut => fmt!(cx, "mut "),
                    ast::Mutability::Imm => {}
                }
                ty.fmt(cx);
            }
            Self::Ptr(mut_, ty) => {
                fmt!(cx, "*");
                match mut_ {
                    ast::Mutability::Mut => fmt!(cx, "mut "),
                    ast::Mutability::Imm => fmt!(cx, "const "),
                }
                ty.fmt(cx);
            }
            Self::Never => fmt!(cx, "!"),
            // FIXME: In Rust 2015 if `bounds.is_empty()`, you need to render it as `r#dyn`.
            Self::DynTrait(bounds) => {
                fmt!(cx, "dyn");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::ImplTrait(bounds) => {
                fmt!(cx, "impl");
                if !bounds.is_empty() {
                    fmt!(cx, " ");
                }
                bounds.fmt(cx);
            }
            Self::Array(ty, expr) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "; ");
                expr.fmt(cx);
                fmt!(cx, "]")
            }
            Self::Slice(ty) => {
                fmt!(cx, "[");
                ty.fmt(cx);
                fmt!(cx, "]")
            }
            Self::Tup(tys) => Tup(tys).fmt(cx),
            Self::Error => fmt!(cx, "/*error*/"),
        }
    }
}

impl Fmt for ast::Lifetime<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self(lt) = self;
        fmt!(cx, "'{lt}");
    }
}

impl Fmt for ast::Expr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Wildcard => fmt!(cx, "_"),
            Self::If(expr) => {
                fmt!(cx, "if ");
                expr.condition.fmt(cx);
                fmt!(cx, " ");
                expr.consequent.fmt(cx);
                if let Some(alternate) = expr.alternate {
                    fmt!(cx, " else ");
                    alternate.fmt(cx);
                }
            }
            Self::Match { scrutinee, arms } => {
                let is_non_empty = !arms.is_empty();

                fmt!(cx, "match ");
                scrutinee.fmt(cx);
                fmt!(cx, " {{");
                cx.indent();
                if is_non_empty {
                    fmt!(cx, "\n");
                }
                for arm in arms {
                    let needs_comma = !arm.body.has_trailing_block(ast::TrailingBlockMode::Match);

                    fmt!(cx, indent);
                    arm.fmt(cx);
                    if needs_comma {
                        fmt!(cx, ",");
                    }
                    fmt!(cx, "\n");
                }
                cx.dedent();
                if is_non_empty {
                    // Because we added a trailing line break above.
                    fmt!(cx, indent);
                }
                fmt!(cx, "}}");
            }
            Self::BoolLit(lit) => fmt!(cx, "{lit}"),
            Self::NumLit(lit) => fmt!(cx, "{lit}"),
            Self::StrLit(lit) => fmt!(cx, "{lit}"),
            Self::Borrow(mut_, expr) => {
                fmt!(cx, "&");
                match mut_ {
                    ast::Mutability::Mut => fmt!(cx, "mut "),
                    ast::Mutability::Imm => {}
                }
                expr.fmt(cx);
            }
            Self::Block(expr) => expr.fmt(cx),
            Self::Tup(exprs) => Tup(exprs).fmt(cx),
            Self::MacroCall(call) => call.fmt(cx),
        }
    }
}

impl Fmt for ast::MatchArm<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, body } = self;

        pat.fmt(cx);
        fmt!(cx, " => ");
        body.fmt(cx);
    }
}

impl Fmt for ast::Pat<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Path(path) => path.fmt(cx),
            Self::Wildcard => fmt!(cx, "_"),
            Self::NumLit(lit) => fmt!(cx, "{lit}"),
            Self::StrLit(lit) => fmt!(cx, "{lit:?}"),
            Self::Borrow(mut_, pat) => {
                fmt!(cx, "&");
                match mut_ {
                    ast::Mutability::Mut => fmt!(cx, "mut "),
                    ast::Mutability::Imm => {}
                }
                pat.fmt(cx);
            }
            Self::Tup(pats) => Tup(pats).fmt(cx),
            Self::MacroCall(call) => call.fmt(cx),
        }
    }
}

impl Fmt for ast::BlockExpr<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { attrs, stmts } = self;
        let is_non_empty = !attrs.is_empty() || !stmts.is_empty();

        fmt!(cx, "{{");
        if is_non_empty {
            fmt!(cx, "\n");
        }
        cx.indent();
        for attr in attrs {
            fmt!(cx, indent);
            attr.fmt(cx);
            fmt!(cx, "\n");
        }
        for stmt in stmts {
            if let ast::Stmt::Empty = stmt {
                continue;
            }
            fmt!(cx, indent);
            stmt.fmt(cx);
            fmt!(cx, "\n");
        }
        cx.dedent();

        if is_non_empty {
            // Because we added a trailing line break above.
            fmt!(cx, indent);
        }

        fmt!(cx, "}}");
    }
}

impl Fmt for ast::Stmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            Self::Item(item) => item.fmt(cx),
            Self::Let(stmt) => stmt.fmt(cx),
            Self::Expr(expr, semi) => {
                let needs_semi = matches!(semi, ast::Semicolon::Yes if !expr.has_trailing_block(ast::TrailingBlockMode::Normal));
                expr.fmt(cx);
                if needs_semi {
                    fmt!(cx, ";");
                }
            }
            Self::Empty => fmt!(cx, ";"),
        }
    }
}

impl Fmt for ast::LetStmt<'_> {
    fn fmt(self, cx: &mut Cx<'_>) {
        let Self { pat, ty, body } = self;

        fmt!(cx, "let ");
        pat.fmt(cx);
        if let Some(ty) = ty {
            fmt!(cx, ": ");
            ty.fmt(cx);
        }
        if let Some(body) = body {
            fmt!(cx, " = ");
            body.fmt(cx);
        }
        fmt!(cx, ";");
    }
}

impl<'src, A: FmtGenericArgs> Fmt for ast::MacroCall<'src, A> {
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
        fmt!(cx, "{fmt}")
    }
}

impl Fmt for ast::Visibility {
    fn fmt(self, cx: &mut Cx<'_>) {
        match self {
            ast::Visibility::Inherited => {}
            ast::Visibility::Public => fmt!(cx, "pub "),
        }
    }
}

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
