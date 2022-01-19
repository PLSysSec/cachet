// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;

use std::ops::{Deref, DerefMut};

use lazy_static::lazy_static;
use typed_index_collections::{TiSlice, TiVec};

use crate::type_checker;

use crate::ast::{BuiltInType, BuiltInVar, Ident, Span, Spanned};

pub use crate::normalizer::ast::*;

lazy_static! {
    static ref TMP_VAR_IDENT: Ident = "tmp".into();
}

pub fn normalize(env: type_checker::Env) -> Env {
    let global_var_items = &env.global_var_items;

    let fn_items: TiVec<FnIndex, _> = env
        .fn_items
        .into_iter()
        .map(|fn_item| normalize_callable_item(global_var_items, fn_item))
        .collect();

    let op_items: TiVec<OpIndex, _> = env
        .op_items
        .into_iter()
        .map(|op_item| normalize_callable_item(global_var_items, op_item))
        .collect();

    Env {
        enum_items: env.enum_items,
        struct_items: env.struct_items,
        ir_items: env.ir_items,
        global_var_items: env.global_var_items,
        fn_items,
        op_items,
        decl_order: env.decl_order,
    }
}

fn normalize_callable_item(
    global_var_items: &TiSlice<GlobalVarIndex, GlobalVarItem>,
    callable_item: type_checker::CallableItem,
) -> CallableItem {
    let body = callable_item.body.map(|mut body| {
        let mut normalizer = Normalizer::new(
            global_var_items,
            &callable_item.params.var_params,
            &mut body.locals.local_vars,
        );
        let stmts = normalizer.normalize_body_block(body.block);
        Body {
            locals: body.locals,
            stmts,
        }
    });

    CallableItem {
        path: callable_item.path,
        parent: callable_item.parent,
        is_unsafe: callable_item.is_unsafe,
        params: callable_item.params,
        param_order: callable_item.param_order,
        ret: callable_item.ret,
        body,
    }
}

struct Normalizer<'a> {
    global_var_items: &'a TiSlice<GlobalVarIndex, GlobalVarItem>,
    var_params: &'a TiSlice<VarParamIndex, VarParam>,
    local_vars: &'a mut TiVec<LocalVarIndex, LocalVar>,
}

impl<'a> Normalizer<'a> {
    fn new(
        global_var_items: &'a TiSlice<GlobalVarIndex, GlobalVarItem>,
        var_params: &'a TiSlice<VarParamIndex, VarParam>,
        local_vars: &'a mut TiVec<LocalVarIndex, LocalVar>,
    ) -> Self {
        Normalizer {
            global_var_items,
            var_params,
            local_vars,
        }
    }

    fn normalize_body_block(&mut self, block: type_checker::Block) -> Vec<Stmt> {
        if block.type_() == BuiltInType::Unit.into() {
            return self.normalize_unused_block(block);
        }

        let mut stmts = self.normalize_block_stmts(block.stmts);
        let mut scoped_normalizer = ScopedNormalizer::new(self, &mut stmts);
        let value = scoped_normalizer.normalize_used_expr(block.value);
        stmts.push(RetStmt { value }.into());
        stmts
    }

    fn normalize_unused_block(&mut self, block: type_checker::Block) -> Vec<Stmt> {
        let mut stmts = self.normalize_block_stmts(block.stmts);
        let mut scoped_normalizer = ScopedNormalizer::new(self, &mut stmts);
        scoped_normalizer.normalize_unused_expr(block.value);
        stmts
    }

    fn normalize_block_stmts(&mut self, stmts: Vec<type_checker::Stmt>) -> Vec<Stmt> {
        let mut normalized_stmts = Vec::with_capacity(stmts.len());
        let mut scoped_normalizer = ScopedNormalizer::new(self, &mut normalized_stmts);

        for stmt in stmts {
            scoped_normalizer.normalize_stmt(stmt);
        }

        normalized_stmts
    }

    fn is_mut_var(&self, var_index: VarIndex) -> bool {
        match var_index {
            VarIndex::BuiltIn(_) | VarIndex::EnumVariant(_) | VarIndex::OutParam(_) => false,
            VarIndex::Global(global_var_index) => self.global_var_items[global_var_index].is_mut,
            VarIndex::Param(var_param_index) => self.var_params[var_param_index].is_mut,
            VarIndex::Local(local_var_index) => self.local_vars[local_var_index].is_mut,
        }
    }
}

struct ScopedNormalizer<'a, 'b> {
    normalizer: &'b mut Normalizer<'a>,
    stmts: &'b mut Vec<Stmt>,
}

impl<'a> Deref for ScopedNormalizer<'a, '_> {
    type Target = Normalizer<'a>;

    fn deref(&self) -> &Self::Target {
        self.normalizer
    }
}

impl<'a> DerefMut for ScopedNormalizer<'a, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.normalizer
    }
}

impl<'a, 'b> ScopedNormalizer<'a, 'b> {
    fn new(normalizer: &'b mut Normalizer<'a>, stmts: &'b mut Vec<Stmt>) -> Self {
        ScopedNormalizer { normalizer, stmts }
    }

    fn recurse<'c>(&'c mut self, stmts: &'c mut Vec<Stmt>) -> ScopedNormalizer<'a, 'c> {
        ScopedNormalizer {
            normalizer: self.normalizer,
            stmts,
        }
    }

    fn normalize_arg(&mut self, arg: type_checker::Arg) -> Arg {
        match arg {
            type_checker::Arg::Expr(expr) => self.normalize_atom_expr(expr).into(),
            type_checker::Arg::OutVar(out_var_arg) => out_var_arg.into(),
            type_checker::Arg::Label(label_index) => label_index.into(),
        }
    }

    fn normalize_call(&mut self, call: type_checker::Call) -> (Vec<Stmt>, Call) {
        let mut stmts = Vec::new();
        let mut scoped_normalizer = self.recurse(&mut stmts);

        let args = call
            .args
            .into_iter()
            .map(|arg| scoped_normalizer.normalize_arg(arg))
            .collect();

        (
            stmts,
            Call {
                target: call.target,
                is_unsafe: call.is_unsafe,
                args,
                ret: call.ret,
            },
        )
    }

    fn normalize_used_call(&mut self, call: type_checker::Call) -> Expr {
        let (stmts, call) = self.normalize_call(call);
        let value = call.into();

        if stmts.is_empty() {
            value
        } else {
            BlockExpr {
                kind: None,
                stmts,
                value,
            }
            .into()
        }
    }

    fn normalize_unused_call(&mut self, call: type_checker::Call) {
        let (mut stmts, call) = self.normalize_call(call);
        let stmt = call.into();

        self.stmts.push(if stmts.is_empty() {
            stmt
        } else {
            stmts.push(stmt);
            BlockStmt { kind: None, stmts }.into()
        });
    }

    fn normalize_stmt(&mut self, stmt: type_checker::Stmt) {
        match stmt {
            type_checker::Stmt::Let(let_stmt) => self.normalize_let_stmt(let_stmt),
            type_checker::Stmt::If(if_stmt) => self.normalize_if_stmt(if_stmt),
            type_checker::Stmt::Check(check_stmt) => self.normalize_check_stmt(check_stmt),
            type_checker::Stmt::Goto(goto_stmt) => self.stmts.push(goto_stmt.into()),
            type_checker::Stmt::Emit(call) => self.normalize_unused_call(call),
            type_checker::Stmt::Expr(expr) => self.normalize_unused_expr(expr),
        }
    }

    fn normalize_let_stmt(&mut self, let_stmt: type_checker::LetStmt) {
        let rhs = self.normalize_expr(let_stmt.rhs);

        self.stmts.push(
            LetStmt {
                lhs: let_stmt.lhs,
                rhs,
            }
            .into(),
        );
    }

    fn normalize_if_stmt(&mut self, if_stmt: type_checker::IfStmt) {
        let cond = self.normalize_expr(if_stmt.cond);
        let then = self.normalize_unused_block(if_stmt.then);
        let else_ = if_stmt
            .else_
            .map(|else_| self.normalize_unused_block(else_));

        self.stmts.push(IfStmt { cond, then, else_ }.into());
    }

    fn normalize_check_stmt(&mut self, check_stmt: type_checker::CheckStmt) {
        let cond = self.normalize_expr(check_stmt.cond);

        self.stmts.push(
            CheckStmt {
                kind: check_stmt.kind,
                cond,
            }
            .into(),
        );
    }

    fn normalize_expr(&mut self, expr: type_checker::Expr) -> Expr {
        if expr.type_() == BuiltInType::Unit.into() {
            self.normalize_unused_expr(expr);
            BuiltInVar::Unit.into()
        } else {
            self.normalize_used_expr(expr)
        }
    }

    fn normalize_atom_expr(&mut self, expr: type_checker::Expr) -> AtomExpr {
        match self.normalize_expr(expr).try_into() {
            Ok(atom_expr) => match atom_expr {
                AtomExpr::Var(var_expr) if self.is_mut_var(var_expr.var) => {
                    self.push_tmp_expr(var_expr.into()).into()
                }
                atom_expr @ _ => atom_expr,
            },
            Err(expr) => {
                debug_assert_ne!(expr.type_(), BuiltInType::Unit.into());
                self.push_tmp_expr(expr).into()
            }
        }
    }

    fn push_tmp_expr(&mut self, expr: Expr) -> VarExpr {
        let tmp_local_var_type_index = expr.type_();

        let tmp_local_var = LocalVar {
            ident: Spanned::new(Span::initial(), *TMP_VAR_IDENT),
            is_mut: false,
            type_: tmp_local_var_type_index,
        };

        let tmp_local_var_index = self.local_vars.push_and_get_key(tmp_local_var);

        self.stmts.push(
            LetStmt {
                lhs: tmp_local_var_index,
                rhs: expr,
            }
            .into(),
        );

        VarExpr {
            var: tmp_local_var_index.into(),
            type_: tmp_local_var_type_index,
        }
    }

    fn normalize_used_expr(&mut self, expr: type_checker::Expr) -> Expr {
        debug_assert_ne!(expr.type_(), BuiltInType::Unit.into());

        match expr {
            type_checker::Expr::Block(block_expr) => self.normalize_used_block_expr(*block_expr),
            type_checker::Expr::Var(var_expr) => var_expr.into(),
            type_checker::Expr::Call(call) => self.normalize_used_call(call),
            type_checker::Expr::Negate(negate_expr) => {
                self.normalize_used_negate_expr(*negate_expr).into()
            }
            type_checker::Expr::Cast(cast_expr) => {
                self.normalize_used_cast_expr(*cast_expr).into()
            }
            type_checker::Expr::Compare(compare_expr) => {
                self.normalize_used_compare_expr(*compare_expr)
            }
            type_checker::Expr::Assign(_) => {
                unreachable!("assignment expressions should be `Unit`-typed")
            }
        }
    }

    fn normalize_unused_expr(&mut self, expr: type_checker::Expr) {
        match expr {
            type_checker::Expr::Block(block_expr) => self.normalize_unused_block_expr(*block_expr),
            type_checker::Expr::Var(_) => (),
            type_checker::Expr::Call(call) => self.normalize_unused_call(call),
            type_checker::Expr::Negate(negate_expr) => {
                self.normalize_unused_negate_expr(*negate_expr)
            }
            type_checker::Expr::Cast(cast_expr) => self.normalize_unused_cast_expr(*cast_expr),
            type_checker::Expr::Compare(compare_expr) => {
                self.normalize_unused_compare_expr(*compare_expr)
            }
            type_checker::Expr::Assign(assign_expr) => {
                self.normalize_unused_assign_expr(*assign_expr)
            }
        }
    }

    fn normalize_used_block_expr(&mut self, block_expr: type_checker::BlockExpr) -> Expr {
        let mut stmts = self.normalize_block_stmts(block_expr.block.stmts);

        let mut scoped_normalizer = self.recurse(&mut stmts);
        let value = scoped_normalizer.normalize_expr(block_expr.block.value);

        if stmts.is_empty() {
            value
        } else {
            BlockExpr {
                kind: block_expr.kind,
                stmts,
                value,
            }
            .into()
        }
    }

    fn normalize_unused_block_expr(&mut self, block_expr: type_checker::BlockExpr) {
        let mut stmts = self.normalize_block_stmts(block_expr.block.stmts);

        let mut scoped_normalizer = self.recurse(&mut stmts);
        scoped_normalizer.normalize_unused_expr(block_expr.block.value);

        if !stmts.is_empty() {
            self.stmts.push(
                BlockStmt {
                    kind: block_expr.kind,
                    stmts,
                }
                .into(),
            );
        }
    }

    fn normalize_used_negate_expr(&mut self, negate_expr: type_checker::NegateExpr) -> NegateExpr {
        let expr = self.normalize_used_expr(negate_expr.expr);

        NegateExpr {
            kind: negate_expr.kind,
            expr,
        }
    }

    fn normalize_unused_negate_expr(&mut self, negate_expr: type_checker::NegateExpr) {
        self.normalize_unused_expr(negate_expr.expr);
    }

    fn normalize_used_cast_expr(&mut self, cast_expr: type_checker::CastExpr) -> CastExpr {
        let expr = self.normalize_used_expr(cast_expr.expr);

        CastExpr {
            kind: cast_expr.kind,
            expr,
            type_: cast_expr.type_,
        }
    }

    fn normalize_unused_cast_expr(&mut self, cast_expr: type_checker::CastExpr) {
        self.normalize_unused_expr(cast_expr.expr);
    }

    fn normalize_used_compare_expr(&mut self, compare_expr: type_checker::CompareExpr) -> Expr {
        let mut stmts = Vec::new();
        let mut scoped_normalizer = self.recurse(&mut stmts);

        let lhs = scoped_normalizer.normalize_atom_expr(compare_expr.lhs);
        let rhs = scoped_normalizer.normalize_atom_expr(compare_expr.rhs);

        let value = CompareExpr {
            kind: compare_expr.kind,
            lhs,
            rhs,
        }
        .into();

        if stmts.is_empty() {
            value
        } else {
            BlockExpr {
                kind: None,
                stmts,
                value,
            }
            .into()
        }
    }

    fn normalize_unused_compare_expr(&mut self, compare_expr: type_checker::CompareExpr) {
        self.normalize_unused_expr(compare_expr.lhs);
        self.normalize_unused_expr(compare_expr.rhs);
    }

    fn normalize_unused_assign_expr(&mut self, assign_expr: type_checker::AssignExpr) {
        let rhs = self.normalize_expr(assign_expr.rhs);

        self.stmts.push(
            AssignStmt {
                lhs: assign_expr.lhs.value,
                rhs,
            }
            .into(),
        );
    }
}