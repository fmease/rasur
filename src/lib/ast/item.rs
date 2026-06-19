use super::{
    Attr, BlockExpr, Bound, Expr, Extern, Generics, Ident, MacroCall, Mut, NoGenericArgs,
    ObligatorilyDisambiguatedGenericArgs, Pat, Path, PathExt, Safety, Span, TokenStream, Ty,
    UnambiguousGenericArgs,
};
use Default::default;

#[derive(Debug)]
pub struct File<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub items: Vec<Item<'src>>,
}

// FIXME: Maybe represent as Item<Free>?
#[derive(Debug)]
pub struct Item<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub vis: Visibility<'src>,
    pub kind: ItemKind<'src>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ItemKind<'src> {
    Const(Box<ConstItem<'src>>),
    ConstBlock(Box<ConstBlockItem<'src>>),
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
pub struct ConstItem<'src> {
    pub override_policy: OverridePolicy,
    pub type_level: TypeLevel,
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub ty: Ty<'src>,
    pub body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub enum TypeLevel {
    Yes,
    No,
}

#[derive(Debug)]
pub struct ConstBlockItem<'src> {
    pub body: BlockExpr<'src>,
}

#[derive(Debug)]
pub struct DelegationItem<'src> {
    pub ext: Option<PathExt<'src>>,
    pub path: DelegationPathTree<'src>,
    pub body: Option<BlockExpr<'src>>,
}

#[derive(Debug)]
pub struct DelegationPathTree<'src> {
    pub path: Path<'src, ObligatorilyDisambiguatedGenericArgs>,
    pub kind: DelegationPathTreeKind<'src>,
}

#[derive(Debug)]
pub enum DelegationPathTreeKind<'src> {
    Global,
    Stump(Option<Ident<'src>>),
    Branch(Vec<(Ident<'src>, Option<Ident<'src>>)>),
}

#[derive(Debug)]
pub struct EnumItem<'src> {
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub variants: Vec<Variant<'src>>,
}

#[derive(Debug)]
pub struct Variant<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub vis: Visibility<'src>,
    pub binder: Ident<'src>,
    pub kind: VariantKind<'src>,
    pub discr: Option<Expr<'src>>,
}

#[derive(Debug)]
pub enum VariantKind<'src> {
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
pub struct TupleFieldDef<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub vis: Visibility<'src>,
    pub mut_restriction: Option<Path<'src, NoGenericArgs>>,
    pub ty: Ty<'src>,
    pub default: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct StructFieldDef<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub vis: Visibility<'src>,
    pub mut_restriction: Option<Path<'src, NoGenericArgs>>,
    pub safety: Safety,
    pub binder: Ident<'src>,
    pub ty: Ty<'src>,
    pub default: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct ExternBlockItem<'src> {
    pub safety: Safety,
    pub abi: Option<&'src str>,
    pub body: Vec<ExternItem<'src>>,
}

#[derive(Debug)]
pub struct ExternCrateItem<'src> {
    pub target: Ident<'src>,
    pub binder: Option<Ident<'src>>,
}

// FIXME: Maybe represent as Item<Extern>?
#[derive(Debug)]
pub struct ExternItem<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub vis: Visibility<'src>,
    pub kind: ExternItemKind<'src>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExternItemKind<'src> {
    Fn(Box<FnItem<'src>>),
    MacroCall(Box<MacroCall<'src, NoGenericArgs>>),
    Static(Box<StaticItem<'src>>),
    Ty(Box<TyAliasItem<'src>>),
}

#[derive(Debug)]
pub struct FnItem<'src> {
    pub modifiers: FnItemModifiers<'src>,
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub params: Vec<FnParam<'src>>,
    pub ret_ty: Option<Ty<'src>>,
    pub contract: Contract<'src>,
    pub body: Option<BlockExpr<'src>>,
}

#[derive(Debug)]
pub struct FnItemModifiers<'src> {
    pub override_policy: OverridePolicy,
    pub const_: Const = default(),
    pub async_: Async = default(),
    pub gen_: Gen = default(),
    pub safety: Safety<()> = default(),
    pub extern_: Extern<'src> = default(),
}

#[derive_const(Default)]
#[derive(Debug)]
pub enum Const {
    Yes,
    #[default]
    No,
}

#[derive_const(Default)]
#[derive(Debug)]
pub enum Async {
    Yes,
    #[default]
    No,
}

#[derive_const(Default)]
#[derive(Debug)]
pub enum Gen {
    Yes,
    #[default]
    No,
}

#[derive(Debug)]
pub struct FnParam<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub pat: Pat<'src>,
    pub ty: Ty<'src>,
}

#[derive(Debug)]
pub struct Contract<'src> {
    pub requires: Option<Box<BlockExpr<'src>>>,
    pub ensures: Option<Box<Expr<'src>>>,
}

#[derive(Debug)]
pub struct ImplItem<'src> {
    pub generics: Generics<'src>,
    pub const_: Const,
    pub trait_ref: Option<ImplTraitRef<'src>>,
    pub self_ty: Ty<'src>,
    pub body: ImplBody<'src>,
}

#[derive(Debug)]
pub struct ImplTraitRef<'src> {
    pub override_policy: OverridePolicy,
    pub safety: Safety,
    pub polarity: ImplPolarity,
    pub path: Path<'src, UnambiguousGenericArgs>,
}

#[derive(Clone, Copy, Debug)]
pub enum ImplPolarity {
    Positive,
    Negative(Span),
}

#[derive(Debug)]
pub enum ImplBody<'src> {
    Normal(Vec<AssocItem<'src>>),
    Delegated(Option<BlockExpr<'src>>),
}

#[derive(Debug)]
pub struct ModItem<'src> {
    // <https://github.com/rust-lang/rust/pull/75857>
    pub safety: Safety,
    pub binder: Ident<'src>,
    pub body: Option<Vec<Item<'src>>>,
}

#[derive(Debug)]
pub struct StaticItem<'src> {
    pub safety: Safety<()>,
    pub mut_: Mut,
    pub binder: Ident<'src>,
    pub ty: Ty<'src>,
    pub body: Option<Expr<'src>>,
}

#[derive(Debug)]
pub struct StructItem<'src> {
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub kind: VariantKind<'src>,
}

#[derive(Debug)]
pub struct TraitItem<'src> {
    pub modifiers: TraitItemModifiers<'src>,
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub bounds: Vec<Bound<'src>>,
    pub body: Vec<AssocItem<'src>>,
}

#[derive(Debug)]
pub struct TraitAliasItem<'src> {
    pub const_: Const,
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub bounds: Vec<Bound<'src>>,
}

#[derive(Default, Debug)]
pub struct TraitItemModifiers<'src> {
    pub impl_restriction: Option<(Span, Path<'src, NoGenericArgs>)>,
    pub const_: Const,
    pub safety: Safety,
    pub auto: Auto,
}

#[derive(Default, Debug)]
pub enum Auto {
    Yes(Span),
    #[default]
    No,
}

// FIXME: Maybe represent as Item<Assoc>?
#[derive(Debug)]
pub struct AssocItem<'src> {
    pub attrs: Vec<Attr<'src>>,
    pub vis: Visibility<'src>,
    pub kind: AssocItemKind<'src>,
    pub span: Span,
}

#[derive(Debug)]
pub enum AssocItemKind<'src> {
    Const(Box<ConstItem<'src>>),
    Delegation(Box<DelegationItem<'src>>),
    Fn(Box<FnItem<'src>>),
    MacroCall(Box<MacroCall<'src, NoGenericArgs>>),
    Ty(Box<TyAliasItem<'src>>),
}

#[derive(Debug)]
pub struct TyAliasItem<'src> {
    pub override_policy: OverridePolicy,
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub bounds: Vec<Bound<'src>>,
    pub body: Option<Ty<'src>>,
}

#[derive(Debug)]
pub struct UnionItem<'src> {
    pub binder: Ident<'src>,
    pub generics: Generics<'src>,
    pub fields: Vec<StructFieldDef<'src>>,
}

#[derive(Debug)]
pub struct UseItem<'src> {
    pub path: UsePathTree<'src>,
}

#[derive(Debug)]
pub struct UsePathTree<'src> {
    pub path: Path<'src, NoGenericArgs>,
    pub kind: UsePathTreeKind<'src>,
}

#[derive(Debug)]
pub enum UsePathTreeKind<'src> {
    Global,
    Stump(Option<Ident<'src>>),
    Branch(Vec<UsePathTree<'src>>),
}

#[derive(Debug)]
pub struct MacroDef<'src> {
    pub binder: Ident<'src>,
    pub params: Option<TokenStream>,
    pub body: TokenStream,
    pub style: MacroDefStyle,
}

#[derive(Debug)]
pub enum MacroDefStyle {
    /// Macro 1.2 aka. `macro_rules!`.
    Old,
    /// Macro 2.0.
    New,
}

#[derive(Debug)]
pub enum Visibility<'src> {
    Inherited,
    Restricted(Path<'src, NoGenericArgs>),
    Public,
}

#[derive(Clone, Copy, Debug)]
pub enum OverridePolicy {
    Allowed,
    Forbidden,
    Implicit,
}
