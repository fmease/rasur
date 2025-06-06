use super::{
    Attr, BlockExpr, Bound, Expr, GenericArgsPolicy, Generics, Ident, MacroCall, Mutability, Pat,
    Path, PathTree, Span, TokenStream, Ty,
};

// FIXME: Maybe represent as Item<Free>?
#[derive(Debug)]
pub(crate) struct Item<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) kind: ItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum ItemKind<'src> {
    Const(Box<ConstItem<'src>>),
    Enum(Box<EnumItem<'src>>),
    ExternBlock(Box<ExternBlockItem<'src>>),
    ExternCrate(Box<ExternCrateItem<'src>>),
    Fn(Box<FnItem<'src>>),
    Impl(Box<ImplItem<'src>>),
    MacroCall(Box<MacroCall<'src, GenericArgsPolicy::Forbidden>>),
    MacroDef(Box<MacroDef<'src>>),
    Mod(Box<ModItem<'src>>),
    Static(Box<StaticItem<'src>>),
    Struct(Box<StructItem<'src>>),
    Trait(Box<TraitItem<'src>>),
    Ty(Box<TyAliasItem<'src>>),
    Union(Box<UnionItem<'src>>),
    Use(Box<UseItem<'src>>),
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
    pub(crate) variants: Vec<Variant<'src>>,
}

#[derive(Debug)]
pub(crate) struct Variant<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) binder: Ident<'src>,
    pub(crate) kind: VariantKind<'src>,
    pub(crate) discr: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum VariantKind<'src> {
    Unit,
    Tuple(Vec<TupleField<'src>>),
    Struct(Vec<StructField<'src>>),
}

#[derive(Debug)]
pub(crate) struct TupleField<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct StructField<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) binder: Ident<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct ExternBlockItem<'src> {
    pub(crate) abi: Option<&'src str>,
    pub(crate) body: Vec<ExternItem<'src>>,
}

#[derive(Debug)]
pub(crate) struct ExternCrateItem<'src> {
    pub(crate) target: Ident<'src>,
    pub(crate) binder: Option<Ident<'src>>,
}

// FIXME: Maybe represent as Item<Extern>?
#[derive(Debug)]
pub(crate) struct ExternItem<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) kind: ExternItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum ExternItemKind<'src> {
    Fn(Box<FnItem<'src>>),
    MacroCall(Box<MacroCall<'src, GenericArgsPolicy::Forbidden>>),
    Static(Box<StaticItem<'src>>),
    Ty(Box<TyAliasItem<'src>>),
}

#[derive(Debug)]
pub(crate) struct FnItem<'src> {
    pub(crate) constness: Constness,
    pub(crate) safety: Safety,
    pub(crate) externness: Externness<'src>,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) params: Vec<FnParam<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) body: Option<BlockExpr<'src>>,
}

#[derive(Debug)]
pub(crate) enum Constness {
    Const,
    Not,
}

#[derive(Debug)]
pub(crate) enum Safety {
    Inherited,
    Safe,
    Unsafe,
}

#[derive(Debug)]
pub(crate) enum Externness<'src> {
    Extern(Option<&'src str>),
    Not,
}

#[derive(Debug)]
pub(crate) struct FnParam<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct ImplItem<'src> {
    pub(crate) safety: Safety,
    pub(crate) generics: Generics<'src>,
    pub(crate) constness: Constness,
    pub(crate) polarity: ImplPolarity,
    pub(crate) trait_ref: Option<Path<'src, GenericArgsPolicy::Allowed>>,
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
    pub(crate) mut_: Mutability,
    pub(crate) binder: Ident<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) kind: VariantKind<'src>,
}

#[derive(Debug)]
pub(crate) struct TraitItem<'src> {
    pub(crate) safety: Safety,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
    pub(crate) body: Vec<AssocItem<'src>>,
}

// FIXME: Maybe represent as Item<Assoc>?
#[derive(Debug)]
pub(crate) struct AssocItem<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) kind: AssocItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum AssocItemKind<'src> {
    Const(Box<ConstItem<'src>>),
    Fn(Box<FnItem<'src>>),
    MacroCall(Box<MacroCall<'src, GenericArgsPolicy::Forbidden>>),
    Ty(Box<TyAliasItem<'src>>),
}

#[derive(Debug)]
pub(crate) struct TyAliasItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
    pub(crate) body: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) struct UnionItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) fields: Vec<StructField<'src>>,
}

#[derive(Debug)]
pub(crate) struct UseItem<'src> {
    pub(crate) tree: PathTree<'src>,
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
pub(crate) enum Visibility<'src> {
    Inherited,
    Restricted(Path<'src, GenericArgsPolicy::Forbidden>),
    Public,
}
