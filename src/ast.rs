pub(crate) use crate::lexer::{Token, TokenKind};
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
    pub(crate) vis: Visibility<'src>,
    pub(crate) kind: ItemKind<'src>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub(crate) enum Visibility<'src> {
    Inherited,
    Restricted(Path<'src, GenericArgsPolicy::Forbidden>),
    Public,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutable {
    Yes,
    No,
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
    Ty(Box<TyItem<'src>>),
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
    Ty(Box<TyItem<'src>>),
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
    pub(crate) mut_: Mutable,
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
    Ty(Box<TyItem<'src>>),
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
pub(crate) struct UseItem<'src> {
    pub(crate) tree: PathTree<'src>,
}

#[derive(Debug)]
pub(crate) struct PathTree<'src> {
    pub(crate) path: Path<'src, GenericArgsPolicy::Forbidden>,
    pub(crate) kind: PathTreeKind<'src>,
}

#[derive(Debug)]
pub(crate) enum PathTreeKind<'src> {
    Global,
    Stump(Option<Ident<'src>>),
    Branch(Vec<PathTree<'src>>),
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
pub(crate) struct MacroCall<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) path: Path<'src, A>,
    pub(crate) bracket: Bracket,
    pub(crate) stream: TokenStream,
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
    Lifetime(Vec<Lifetime<'src>>),
}

#[derive(Debug)]
pub(crate) enum GenericArgs<'src> {
    Angle(Vec<AngleGenericArg<'src>>),
    Paren { inputs: Vec<Ty<'src>>, output: Option<Ty<'src>> },
    ParenElided,
}

#[derive(Debug)]
pub(crate) enum AngleGenericArg<'src> {
    Argument(GenericArg<'src>),
    Constraint(AssocItemConstraint<'src>),
}

#[derive(Debug)]
pub(crate) enum GenericArg<'src> {
    Ty(Ty<'src>),
    Const(Expr<'src>),
    Lifetime(Lifetime<'src>),
}

#[derive(Debug)]
pub(crate) struct AssocItemConstraint<'src> {
    pub(crate) ident: Ident<'src>,
    pub(crate) args: Option<GenericArgs<'src>>,
    pub(crate) kind: AssocItemConstraintKind<'src>,
}

#[derive(Debug)]
pub(crate) enum AssocItemConstraintKind<'src> {
    Equality(Term<'src>),
    Bound(Vec<Bound<'src>>),
}

#[derive(Debug)]
pub(crate) enum Term<'src> {
    Ty(Ty<'src>),
    Const(Expr<'src>),
}

#[derive(Debug)]
pub(crate) enum Predicate<'src> {
    Trait(TraitPredicate<'src>),
    Outlives(OutlivesPredicate<'src>),
}

#[derive(Debug)]
pub(crate) struct TraitPredicate<'src> {
    pub(crate) ty: Ty<'src>,
    pub(crate) bounds: Vec<Bound<'src>>,
}

#[derive(Debug)]
pub(crate) struct OutlivesPredicate<'src> {
    pub(crate) lt: Lifetime<'src>,
    pub(crate) bounds: Vec<Lifetime<'src>>,
}

#[derive(Debug)]
pub(crate) enum Bound<'src> {
    Trait(TraitBoundModifiers, Path<'src, GenericArgsPolicy::Allowed>),
    Outlives(Lifetime<'src>),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TraitBoundModifiers {
    pub(crate) polarity: BoundPolarity,
    // constness
    // asyncness
}

impl TraitBoundModifiers {
    pub(crate) const NONE: Self = Self { polarity: BoundPolarity::Positive };
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum BoundPolarity {
    Positive,
    Negative,
    Maybe,
}

#[derive(Debug)]
pub(crate) struct FnParam<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) ty: Ty<'src>,
}

#[derive(Debug)]
pub(crate) enum Expr<'src> {
    UnOp(UnOp, Box<Expr<'src>>),
    BinOp(BinOp, Box<Expr<'src>>, Box<Expr<'src>>),
    Cast(Box<Expr<'src>>, Box<Ty<'src>>),
    Path(Path<'src, GenericArgsPolicy::DisambiguatedOnly>),
    Wildcard,
    Continue,
    Break(Option<&'src str>, Option<Box<Expr<'src>>>),
    Return(Option<Box<Expr<'src>>>),
    If(Box<IfExpr<'src>>),
    Loop(Box<BlockExpr<'src>>),
    Match(Box<MatchExpr<'src>>),
    While(Box<WhileExpr<'src>>),
    BoolLit(bool),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Borrow(Mutable, Box<Expr<'src>>),
    Field(Box<Expr<'src>>, Ident<'src>),
    Call(Box<Expr<'src>>, Vec<Expr<'src>>),
    Index(Box<Expr<'src>>, Box<Expr<'src>>),
    Block(Box<BlockExpr<'src>>),
    ConstBlock(Box<BlockExpr<'src>>),
    UnsafeBlock(Box<BlockExpr<'src>>),
    Tup(Vec<Expr<'src>>),
    Grouped(Box<Expr<'src>>),
    MacroCall(Box<MacroCall<'src, GenericArgsPolicy::DisambiguatedOnly>>),
}

impl Expr<'_> {
    // FIXME: Bad name (e.g. `break {}` is `false` despite "ha[ving] [æ] trailing block")
    pub(crate) fn has_trailing_block(&self, mode: TrailingBlockMode) -> bool {
        match self {
            Self::If(_)
            | Self::Loop(_)
            | Self::Match(_)
            | Self::While(_)
            | Self::Block(_)
            | Self::ConstBlock(_)
            | Self::UnsafeBlock(_) => true,
            Self::MacroCall(MacroCall { bracket: Bracket::Curly, .. }) => match mode {
                TrailingBlockMode::Normal => true,
                TrailingBlockMode::Match => false,
            },
            Self::UnOp(..)
            | Self::BinOp(..)
            | Self::Cast(..)
            | Self::Path(_)
            | Self::Wildcard
            | Self::Continue
            | Self::Break(..)
            | Self::Return(_)
            | Self::BoolLit(_)
            | Self::NumLit(_)
            | Self::StrLit(_)
            | Self::Borrow(..)
            | Self::Field(..)
            | Self::Call(..)
            | Self::Index(..)
            | Self::Tup(_)
            | Self::Grouped(_)
            | Self::MacroCall(_) => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum UnOp {
    Deref,
    Neg,
    Not,
    // FIXME: Remove from this list
    Try,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BinOp {
    Add,
    And,
    BitAnd,
    BitOr,
    BitXor,
    Div,
    Mul,
    Or,
    Rem,
    Sub,
    Assign,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl BinOp {
    pub(crate) fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::And => "&&",
            Self::Assign => "=",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::Div => "/",
            Self::Eq => "==",
            Self::Ge => ">=",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Lt => "<",
            Self::Mul => "*",
            Self::Ne => "!=",
            Self::Or => "||",
            Self::Rem => "%",
            Self::Sub => "-",
        }
    }
}

#[derive(Debug)]
pub(crate) enum TrailingBlockMode {
    Normal,
    Match,
}

#[derive(Debug)]
pub(crate) struct IfExpr<'src> {
    pub(crate) condition: Expr<'src>,
    pub(crate) consequent: BlockExpr<'src>,
    pub(crate) alternate: Option<Expr<'src>>,
}

#[derive(Debug)]
pub(crate) struct MatchExpr<'src> {
    pub(crate) scrutinee: Expr<'src>,
    pub(crate) arms: Vec<MatchArm<'src>>,
}

#[derive(Debug)]
pub(crate) struct MatchArm<'src> {
    pub(crate) pat: Pat<'src>,
    pub(crate) body: Expr<'src>,
}

#[derive(Debug)]
pub(crate) struct WhileExpr<'src> {
    pub(crate) condition: Expr<'src>,
    pub(crate) body: BlockExpr<'src>,
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
    Path(Path<'src, GenericArgsPolicy::DisambiguatedOnly>),
    NumLit(Ident<'src>),
    StrLit(Ident<'src>),
    Wildcard,
    Tup(Vec<Pat<'src>>),
    Borrow(Mutable, Box<Pat<'src>>),
    Grouped(Box<Pat<'src>>),
    MacroCall(MacroCall<'src, GenericArgsPolicy::DisambiguatedOnly>),
}

#[derive(Debug)]
pub(crate) enum Ty<'src> {
    Never,
    Inferred,
    DynTrait(Vec<Bound<'src>>),
    FnPtr((), Option<Box<Ty<'src>>>),
    ImplTrait(Vec<Bound<'src>>),
    Path(Path<'src, GenericArgsPolicy::Allowed>),
    Ref(Option<Lifetime<'src>>, Mutable, Box<Ty<'src>>),
    Ptr(Mutable, Box<Ty<'src>>),
    Array(Box<Ty<'src>>, Expr<'src>),
    Slice(Box<Ty<'src>>),
    Tup(Vec<Ty<'src>>),
    Grouped(Box<Ty<'src>>),
    Error,
}

#[derive(Debug)]
pub(crate) struct Lifetime<'src>(pub(crate) Ident<'src>);

#[derive(Debug)]
pub(crate) struct Attr<'src> {
    pub(crate) style: AttrStyle,
    pub(crate) path: Path<'src, GenericArgsPolicy::Forbidden>,
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

pub(crate) type TokenStream = Vec<Token>;

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
pub(crate) struct Path<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) segs: Vec<PathSeg<'src, A>>,
}

impl<'src, A: GenericArgsPolicy::Kind> Path<'src, A> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { segs: vec![PathSeg::ident(ident)] }
    }
}

#[derive(Debug)]
pub(crate) struct PathSeg<'src, A: GenericArgsPolicy::Kind> {
    pub(crate) ident: Ident<'src>,
    pub(crate) args: A::Args<'src>,
}

impl<'src, A: GenericArgsPolicy::Kind> PathSeg<'src, A> {
    pub(crate) fn ident(ident: Ident<'src>) -> Self {
        Self { ident, args: Default::default() }
    }
}

#[expect(non_snake_case)]
pub(crate) mod GenericArgsPolicy {
    #[derive(Debug)]
    pub(crate) enum Forbidden {}
    #[derive(Debug)]
    pub(crate) enum Allowed {}
    #[derive(Debug)]
    pub(crate) enum DisambiguatedOnly {}

    pub(crate) trait Kind {
        type Args<'src>: Default + std::fmt::Debug;
    }

    impl Kind for Allowed {
        type Args<'src> = Option<super::GenericArgs<'src>>;
    }

    impl Kind for DisambiguatedOnly {
        type Args<'src> = <Allowed as Kind>::Args<'src>;
    }

    impl Kind for Forbidden {
        type Args<'src> = ();
    }
}
