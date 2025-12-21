use super::{
    Attr, BlockExpr, Bound, Expr, Externness, Generics, Ident, MacroCall, Mutability,
    NoGenericArgs, Pat, Path, PathExt, PathTree, Safety, Span, TokenStream, Ty,
    UnambiguousGenericArgs,
};
use Default::default;

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
    Delegation(Box<DelegationItem<'src>>),
    Enum(Box<EnumItem<'src>>),
    ExternBlock(Box<ExternBlockItem<'src>>),
    ExternCrate(Box<ExternCrateItem<'src>>),
    Fn(Box<FnItem<'src>>),
    Impl(Box<ImplItem<'src>>),
    MacroCall(Box<MacroCall<'src, NoGenericArgs>>),
    MacroDef(Box<MacroDef<'src>>),
    Mod(Box<ModItem<'src>>),
    Static(Box<StaticItem<'src>>),
    Struct(Box<StructItem<'src>>),
    Trait(Box<TraitItem<'src>>),
    TraitAlias(Box<TraitAliasItem<'src>>),
    TyAlias(Box<TyAliasItem<'src>>),
    Union(Box<UnionItem<'src>>),
    Use(Box<UseItem<'src>>),
}

#[derive(Debug)]
pub(crate) struct ConstItem<'src> {
    pub(crate) defaultness: Defaultness,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct DelegationItem<'src> {
    pub(crate) ext: Option<PathExt<'src>>,
    pub(crate) path: PathTree<'src>,
    pub(crate) body: Option<BlockExpr<'src>>,
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
    pub(crate) vis: Visibility<'src>,
    pub(crate) binder: Ident<'src>,
    pub(crate) kind: VariantKind<'src>,
    pub(crate) discr: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) enum VariantKind<'src> {
    Unit,
    Tuple(Vec<TupleFieldDef<'src>>),
    Struct(Vec<StructFieldDef<'src>>),
}

impl VariantKind<'_> {
    pub(crate) fn needs_semicolon(&self) -> bool {
        match self {
            Self::Unit | Self::Tuple(_) => true,
            Self::Struct(_) => false,
        }
    }
}

#[derive(Debug)]
pub(crate) struct TupleFieldDef<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) default: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct StructFieldDef<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) vis: Visibility<'src>,
    pub(crate) safety: Safety,
    pub(crate) binder: Ident<'src>,
    pub(crate) ty: Ty<'src>,
    pub(crate) default: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct ExternBlockItem<'src> {
    pub(crate) safety: Safety,
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
    MacroCall(Box<MacroCall<'src, NoGenericArgs>>),
    Static(Box<StaticItem<'src>>),
    Ty(Box<TyAliasItem<'src>>),
}

#[derive(Debug)]
pub(crate) struct FnItem<'src> {
    pub(crate) modifiers: FnItemModifiers<'src>,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) params: Vec<FnParam<'src>>,
    pub(crate) ret_ty: Option<Ty<'src>>,
    pub(crate) contract: Contract<'src>,
    pub(crate) body: Option<BlockExpr<'src>>,
}

#[derive(Debug)]
pub(crate) struct FnItemModifiers<'src> {
    pub(crate) defaultness: Defaultness,
    pub(crate) constness: Constness = default(),
    pub(crate) asyncness: Asyncness = default(),
    pub(crate) genness: Genness = default(),
    pub(crate) safety: Safety<()> = default(),
    pub(crate) externness: Externness<'src> = default(),
}

#[derive_const(Default)]
#[derive(Debug)]
pub(crate) enum Constness {
    Const,
    #[default]
    Not,
}

#[derive_const(Default)]
#[derive(Debug)]
pub(crate) enum Asyncness {
    Async,
    #[default]
    Not,
}

// FIXME: Awful name, rethink whole naming scheme here
#[derive_const(Default)]
#[derive(Debug)]
pub(crate) enum Genness {
    Gen,
    #[default]
    Not,
}

#[derive(Debug)]
pub(crate) struct FnParam<'src> {
    pub(crate) attrs: Vec<Attr<'src>>,
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) struct Contract<'src> {
    pub(crate) requires: Option<Box<BlockExpr<'src>>>,
    pub(crate) ensures: Option<Box<Expr<'src>>>,
}

#[derive(Debug)]
pub(crate) struct ImplItem<'src> {
    pub(crate) generics: Generics<'src>,
    pub(crate) constness: Constness,
    pub(crate) trait_ref: Option<ImplTraitRef<'src>>,
    pub(crate) self_ty: Ty<'src>,
    pub(crate) body: Vec<AssocItem<'src>>,
}

#[derive(Debug)]
pub(crate) struct ImplTraitRef<'src> {
    pub(crate) defaultness: Defaultness,
    pub(crate) safety: Safety,
    pub(crate) polarity: ImplPolarity,
    pub(crate) path: Path<'src, UnambiguousGenericArgs>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ImplPolarity {
    Positive,
    Negative,
}

#[derive(Debug)]
pub(crate) struct ModItem<'src> {
    // <https://github.com/rust-lang/rust/pull/75857>
    pub(crate) safety: Safety,
    pub(crate) binder: Ident<'src>,
    pub(crate) body: Option<Vec<Item<'src>>>,
}

#[derive(Debug)]
pub(crate) struct StaticItem<'src> {
    pub(crate) safety: Safety<()>,
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
    pub(crate) modifiers: TraitItemModifiers,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
    pub(crate) body: Vec<AssocItem<'src>>,
}

#[derive(Debug)]
pub(crate) struct TraitAliasItem<'src> {
    pub(crate) constness: Constness,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
}

#[derive(Default, Debug)]
pub(crate) struct TraitItemModifiers {
    pub(crate) constness: Constness,
    pub(crate) safety: Safety,
    pub(crate) autoness: Autoness,
}

#[derive(Default, Debug)]
pub(crate) enum Autoness {
    Auto,
    #[default]
    Not,
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
    Delegation(Box<DelegationItem<'src>>),
    Fn(Box<FnItem<'src>>),
    MacroCall(Box<MacroCall<'src, NoGenericArgs>>),
    Ty(Box<TyAliasItem<'src>>),
}

#[derive(Debug)]
pub(crate) struct TyAliasItem<'src> {
    pub(crate) defaultness: Defaultness,
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
    pub(crate) body: Option<Ty<'src>>,
}

#[derive(Debug)]
pub(crate) struct UnionItem<'src> {
    pub(crate) binder: Ident<'src>,
    pub(crate) generics: Generics<'src>,
    pub(crate) fields: Vec<StructFieldDef<'src>>,
}

#[derive(Debug)]
pub(crate) struct UseItem<'src> {
    pub(crate) path: PathTree<'src>,
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
    Restricted(Path<'src, NoGenericArgs>),
    Public,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Defaultness {
    Default,
    Final,
}
