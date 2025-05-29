pub(crate) use crate::lexer::TokenKind;
use crate::span::Span;

// FIXME: Create newtype for idents!
pub(crate) type Ident<'src> = &'src str;

#[derive(Debug)]
pub(crate) struct File<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) items: Vec<Item<'src>>,
    pub(crate) span: Span,
}

// FIXME: Maybe represent as Item<Free>?
#[derive(Debug)]
pub(crate) struct Item<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility,
    pub(crate) kind: ItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum Visibility {
    Inherited,
    Public,
}

#[derive(Debug)]
pub(crate) enum ItemKind<'src> {
    Const(ConstItem<'src>),
    Enum(EnumItem<'src>),
    ExternBlock(ExternBlockItem<'src>),
    Fn(FnItem<'src>),
    Impl(ImplItem<'src>),
    MacroCall(MacroCall<'src, ()>),
    MacroDef(MacroDef<'src>),
    Mod(ModItem<'src>),
    Static(StaticItem<'src>),
    Struct(StructItem<'src>),
    Trait(TraitItem<'src>),
    Ty(TyItem<'src>),
    Union(UnionItem<'src>),
}

#[derive(Debug)]
pub(crate) struct ConstItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct EnumItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
}

#[derive(Debug)]
pub(crate) struct ExternBlockItem<'src> {
    pub(crate) abi: Option<&'src str>,
    pub(crate) body: Vec<ExternItem<'src>>,
}

// FIXME: Maybe represent as Item<Extern>?
#[derive(Debug)]
pub(crate) struct ExternItem<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility,
    pub(crate) kind: ExternItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum ExternItemKind<'src> {
    Fn(FnItem<'src>),
    MacroCall(MacroCall<'src, ()>),
    Static(StaticItem<'src>),
    Ty(TyItem<'src>),
}

#[derive(Debug)]
pub(crate) struct FnItem<'src> {
    pub(crate) constness: Constness,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) params: Vec<Param<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Constness {
    Const,
    Not,
}

#[derive(Debug)]
pub(crate) struct ImplItem<'src> {
    pub(crate) generics: Generics<'src>,
    pub(crate) constness: Constness,
    pub(crate) polarity: ImplPolarity,
    pub(crate) trait_ref: Option<Path<'src, Vec<GenericArg<'src>>>>,
    pub(crate) self_ty: Ty<'src>,
    pub(crate) body: Vec<AssocItem<'src>>,
}

#[derive(Debug)]
pub(crate) enum ImplPolarity {
    Positive,
    Negative,
}

#[derive(Debug)]
pub(crate) struct ModItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) body: Option<Vec<Item<'src>>>,
}

#[derive(Debug)]
pub(crate) struct StaticItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) body: StructBody<'src>,
}

#[derive(Debug)]
pub(crate) struct TraitItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
    pub(crate) body: Vec<AssocItem<'src>>,
}

// FIXME: Maybe represent as Item<Assoc>?
#[derive(Debug)]
pub(crate) struct AssocItem<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility,
    pub(crate) kind: AssocItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum AssocItemKind<'src> {
    Const(ConstItem<'src>),
    Fn(FnItem<'src>),
    MacroCall(MacroCall<'src, ()>),
    Ty(TyItem<'src>),
}

#[derive(Debug)]
pub(crate) struct TyItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
    pub(crate) body: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) struct UnionItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
}

#[derive(Debug)]
pub(crate) struct MacroDef<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) params: Option<TokenStream>,
    pub(crate) body: TokenStream,
    pub(crate) style: MacroDefStyle,
}

#[derive(Debug)]
pub(crate) enum MacroDefStyle {
    /// Macro 1.2 aka. `macro_rules!`.
    Old,
    /// Macro 2.0.
    New,
}

#[derive(Debug)]
pub(crate) struct MacroCall<'src, A> {
    pub(crate) path: Path<'src, A>,
    pub(crate) bracket: Bracket,
    pub(crate) stream: TokenStream,
}

#[derive(Debug)]
pub(crate) enum StructBody<'src> {
    // FIXME: Better name for this
    Normal { fields: Vec<StructField<'src>> },
    Unit,
}

#[derive(Debug)]
pub(crate) struct StructField<'src> {
    pub(crate) vis: Visibility,
    pub(crate) binder: Ident<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct Generics<'src> {
    pub(crate) params: Vec<GenericParam<'src>>,
    pub(crate) preds: Vec<Predicate<'src>>,
}

#[derive(Debug)]
pub(crate) struct GenericParam<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) kind: GenericParamKind<'src>,
}

#[derive(Debug)]
pub(crate) enum GenericParamKind<'src> {
    Ty(Vec<Bound<'src>>),
    Const(Ty<'src>),
    Lifetime,
}

#[derive(Debug)]
#[expect(dead_code)] // FIXME
pub(crate) enum GenericArg<'src> {
    Ty(Ty<'src>),
    Const,
    Lifetime,
}

#[derive(Debug)]
pub(crate) enum Predicate<'src> {
    Trait(TraitPredicate<'src>),
}

#[derive(Debug)]
pub(crate) struct TraitPredicate<'src> {
    pub(crate) ty: Ty<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
}

#[derive(Debug)]
pub(crate) enum Bound<'src> {
    Trait(Path<'src, Vec<GenericArg<'src>>>),
}

#[derive(Debug)]
pub(crate) struct Param<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    Path(Path<'src, Vec<GenericArg<'src>>>),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Block(Box<BlockExpr<'src>>),
    Tup(Vec<Expr<'src>>),
    Underscore,
    MacroCall(MacroCall<'src, Vec<GenericArg<'src>>>),
}

impl Expr<'_> {
    pub(crate) fn has_trailing_block(&self) -> bool {
        match self {
            Self::Block(..) | Self::MacroCall(MacroCall { bracket: Bracket::Curly, .. }) => true,
            Self::Path(_)
            | Self::NumLit(_)
            | Self::StrLit(_)
            | Self::Tup(_)
            | Self::Underscore
            | Self::MacroCall(_) => false,
        }
    }
}

#[derive(Debug)]
pub(crate) struct BlockExpr<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) stmts: Vec<Stmt<'src>>,
}

#[derive(Debug)]
pub(crate) enum Stmt<'src> {
    Item(Item<'src>),
    Let(LetStmt<'src>),
    Expr(Expr<'src>, Semicolon),
    Empty,
}

#[derive(Debug)]
pub(crate) enum Semicolon {
    Yes,
    No,
}

#[derive(Debug)]
pub(crate) struct LetStmt<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Option<Ty<'src>>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Pat<'src> {
    Path(Path<'src, Vec<GenericArg<'src>>>),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Wildcard,
    Tup(Vec<Pat<'src>>),
    MacroCall(MacroCall<'src, Vec<GenericArg<'src>>>),
}

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Never,
    Inferred,
    Path(Path<'src, Vec<GenericArg<'src>>>),
    FnPtr((), Option<Box<Ty<'src>>>),
    Array(Box<Ty<'src>>, Expr<'src>),
    Slice(Box<Ty<'src>>),
    Tup(Vec<Ty<'src>>),
    Error,
}

#[derive(Debug)]
pub(crate) struct Attr<'src> {
    pub(crate) style: AttrStyle,
    pub(crate) path: Path<'src, ()>,
    pub(crate) kind: AttrKind<'src>,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) enum AttrStyle {
    Inner,
    Outer,
}

#[derive(Debug)]
pub(crate) enum AttrKind<'src> {
    Unit,
    Call(Bracket, TokenStream),
    Assign(Expr<'src>),
}

pub(crate) type TokenStream = Vec<TokenKind>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(Clone, Copy)]
pub(crate) enum Orientation {
    Open,
    Close,
}

#[derive(Debug)]
pub(crate) struct Path<'src, A> {
    pub(crate) hook: PathHook,
    pub(crate) segs: Vec<PathSeg<'src, A>>,
}

#[derive(Debug)]
pub(crate) enum PathHook {
    Global,
    Local,
}

#[derive(Debug)]
pub(crate) struct PathSeg<'src, A> {
    pub(crate) ident: Ident<'src>,
    pub(crate) args: A,
}
