#![expect(dead_code)] // FIXME

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
    Fn(FnItem<'src>),
    Impl(ImplItem<'src>),
    Mod(ModItem<'src>),
    Static(StaticItem<'src>),
    Struct(StructItem<'src>),
    Trait(TraitItem<'src>),
    Ty(TyItem<'src>),
    Union(UnionItem<'src>),
    MacroCall(MacroCall<'src>),
}

#[derive(Debug)]
pub(crate) struct ConstItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct EnumItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) generics: Generics<'src>,
}

#[derive(Debug)]
pub(crate) struct FnItem<'src> {
    pub(crate) constness: Constness,
    pub(crate) name: Ident<'src>,
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
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct ModItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) items: Option<Vec<Item<'src>>>,
}

#[derive(Debug)]
pub(crate) struct StaticItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) body: StructBody<'src>,
}

#[derive(Debug)]
pub(crate) struct TraitItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) generics: Generics<'src>,
}

#[derive(Debug)]
pub(crate) struct TyItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) body: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) struct UnionItem<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) generics: Generics<'src>,
}

#[derive(Debug)]
pub(crate) struct MacroCall<'src> {
    pub(crate) path: Path<'src>,
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
    pub(crate) name: Ident<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct Generics<'src> {
    pub(crate) params: Vec<GenParam<'src>>,
}

#[derive(Debug)]
pub(crate) struct GenParam<'src> {
    pub(crate) name: Ident<'src>,
}

#[derive(Debug)]
pub(crate) struct Param<'src> {
    pub(crate) name: Ident<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    Ident(Ident<'src>),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Block(Box<BlockExpr<'src>>),
}

#[derive(Debug)]
pub(crate) struct BlockExpr<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) expr: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Array(Box<Ty<'src>>, Expr<'src>),
    Ident(Ident<'src>),
    Inferred,
    Never,
    Slice(Box<Ty<'src>>),
    Tup(Vec<Ty<'src>>),
}

#[derive(Debug)]
pub(crate) struct Attr<'src> {
    pub(crate) style: AttrStyle,
    pub(crate) path: Path<'src>,
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
pub(crate) struct Path<'src> {
    pub(crate) locality: PathLocality,
    pub(crate) segs: Vec<Ident<'src>>,
}

#[derive(Debug)]
pub(crate) enum PathLocality {
    Global,
    Local,
}
