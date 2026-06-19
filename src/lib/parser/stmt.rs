use super::{
    Fragment, Result, TokenKind,
    expr::{AttrPolicy, LetPolicy, OpPolicy, StructPolicy},
    frags,
    item::ItemCx,
    pat::OrPolicy,
};
use crate::{ast, feature::Feature};

impl<'src> super::Parser<'_, '_, 'src> {
    /// Parse a statement.
    pub(super) fn parse_stmt(&mut self, delimiter: TokenKind) -> Result<ast::Stmt<'src>> {
        let mut attrs = self.parse_attrs(ast::AttrStyle::Outer)?;

        // We only consider "restricted" items to prevent ambiguities.
        //
        // 1. We don't want to recognize `default` as an item modifier.
        //    * This mimics rustc which doesn't accept code like `fn f() { default fn f() {} }`
        //    * It precludes us from rejecting expr `default as $ty`
        // 2. We exclude macro call items because macro call exprs should take precedence.
        //    * The latter permit generic args in their path.
        //    * We check for items before exprs since it's easier to detect expr prefixes when
        //      checking item prefixes than the other way around (in cases where they share a
        //      prefix).
        // 3. We exclude const block items since const block exprs should take precedence.
        if self.begins_restricted_item() {
            let mut item = self.parse_item(ItemCx::Boring)?;
            attrs.append(&mut item.attrs);
            item.attrs = attrs;

            return Ok(ast::Stmt::Item(item));
        }

        let super_ = if self.token.kind == TokenKind::Super && self.peek(1).kind == TokenKind::Let {
            self.feature(Feature::super_let, self.token.span);
            self.advance();
            ast::Super::Yes
        } else {
            ast::Super::No
        };
        if self.consume(TokenKind::Let) {
            let pat = self.parse_pat(OrPolicy::Yield)?;
            let ty = self.consume(TokenKind::SingleColon).then(|| self.parse_ty()).transpose()?;
            let body = if self.consume(TokenKind::SingleEquals) {
                let consequent = self.parse_expr()?;
                let alternate = if let token = self.token
                    && self.consume(TokenKind::Else)
                {
                    if !consequent.kind.else_may_follow() {
                        // FIXME: Improve diagnostic.
                        self.unexpected(token, frags![TokenKind::Semicolon]);
                    }
                    Some(self.parse_block_expr(AttrPolicy::Reject)?)
                } else {
                    None
                };
                Some(ast::LetStmtBody { consequent, alternate })
            } else {
                None
            };
            // FIXME: Should mention `else`, too, where applicable.
            self.parse(TokenKind::Semicolon)?;
            return Ok(ast::Stmt::Let(Box::new(ast::LetStmt { attrs, super_, pat, ty, body })));
        }

        if self.begins_expr() {
            let rule = ast::CurlyBracketedMacroCallIsBoundary::Yes;

            let mut expr = self.parse_expr_where(
                StructPolicy::Parse,
                LetPolicy::YieldOrReject,
                OpPolicy::YieldOnBoundary(rule),
            )?;
            attrs.append(&mut expr.attrs);
            expr.attrs = attrs;

            let is_boundary = expr.kind.is_boundary(rule);

            let semi = self.consume_or_parse(
                TokenKind::Semicolon,
                self.token.kind == delimiter || is_boundary,
            )?;

            if (semi || is_boundary)
                && let ast::ExprKind::MacroCall(call) = expr.kind
            {
                return Ok(ast::Stmt::MacroCall(call));
            }

            let semi = if semi { ast::Semicolon::Yes } else { ast::Semicolon::No };
            return Ok(ast::Stmt::Expr(expr, semi));
        }

        if let TokenKind::Semicolon = self.token.kind
            && attrs.is_empty()
        {
            self.advance();
            Ok(ast::Stmt::Empty)
        } else {
            self.unexpected(self.token, frags![Fragment::Stmt]);
            Err(())
        }
    }
}

impl ast::ExprKind<'_> {
    fn else_may_follow(&self) -> bool {
        match self {
            | Self::Array(_)
            | Self::Ascription(..)
            | Self::Await(_)
            | Self::Call(..)
            | Self::Continue(_)
            | Self::Field(..)
            | Self::GenBlock(..)
            | Self::Grouped(_)
            | Self::Index(..)
            | Self::Lit(_)
            | Self::MethodCall(_)
            | Self::Move(_)
            | Self::OffsetOf(..)
            | Self::Path(_)
            | Self::Repeat(..)
            | Self::Try(_)
            | Self::Tuple(_)
            | Self::UnsafeBinderCast(..)
            | Self::Use(_)
            | Self::Wildcard
            | Self::Yield(ast::YieldExpr::Postfix(_))
            | Self::Error(_) => true,
            | Self::BinOp(ast::BinOp::And | ast::BinOp::Or, ..)
            | Self::Block(..)
            | Self::ForLoop(_)
            | Self::If(_)
            | Self::Loop(..)
            | Self::Match(_)
            | Self::SpecialBlock(..)
            | Self::Struct(_)
            | Self::WhileLoop(_) => false,
            | Self::MacroCall(expr) => expr.bracket.else_may_follow(),
            | Self::Become(expr)
            | Self::BinOp(.., expr)
            | Self::Borrow(.., expr)
            | Self::UnOp(_, expr) => expr.kind.else_may_follow(),
            | Self::Closure(expr) => expr.body.kind.else_may_follow(),
            | Self::Let(expr) => expr.body.kind.else_may_follow(),
            | Self::Break(_, expr)
            | Self::Range(_, expr, _)
            | Self::Return(expr)
            | Self::Yeet(expr)
            | Self::Yield(ast::YieldExpr::Prefix(expr)) => {
                expr.as_ref().is_none_or(|expr| expr.kind.else_may_follow())
            }
            | Self::Cast(_, ty) => ty.else_may_follow(),
        }
    }
}

impl ast::Ty<'_> {
    fn else_may_follow(&self) -> bool {
        match self {
            | Self::All
            | Self::Array(..)
            | Self::CVariadics
            | Self::DynTrait(..)
            | Self::Error(_)
            | Self::FieldOf(..)
            | Self::Grouped(_)
            | Self::ImplTrait(_)
            | Self::ImplicitSelf
            | Self::Inferred
            | Self::Never
            | Self::Path(_)
            | Self::Slice(_)
            | Self::Tuple(_) => true,
            Self::FnPtr(ty) => ty.output.as_ref().is_none_or(ast::Ty::else_may_follow),
            Self::MacroCall(ty) => ty.bracket.else_may_follow(),
            Self::Ptr(_, ty) | Self::UnsafeBinder(_, ty) => ty.else_may_follow(),
            // BLOCKED(upstream): FIXME: consider view (presence forces false)
            Self::Ref(ty) => ty.pointee.else_may_follow(),
        }
    }
}

impl ast::Bracket {
    fn else_may_follow(self) -> bool {
        match self {
            Self::Round | Self::Square => true,
            Self::Curly => false,
        }
    }
}
