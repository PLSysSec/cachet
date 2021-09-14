// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;
mod graphs;

use std::iter;
use std::ops::{Deref, DerefMut};

use codespan::Span;
use iterate::iterate;

use crate::parser::Path;
use crate::resolver;
use crate::FrontendError;

use crate::type_checker::graphs::{FnGraph, TypeGraph};

pub use crate::type_checker::ast::*;
pub use crate::type_checker::error::*;

pub fn type_check(mut env: resolver::Env) -> Result<Env, TypeCheckErrors> {
    let fn_bodies: Vec<_> = env
        .fn_defs
        .iter_mut()
        .map(|fn_def| fn_def.body.take())
        .collect();

    let mut type_checker = TypeChecker::new(
        &env.enum_defs,
        &env.struct_defs,
        &env.const_defs,
        &env.fn_defs,
    );

    let type_def_order = type_checker.flatten_type_graph();

    let fn_bodies: Vec<_> = env
        .fn_defs
        .iter()
        .zip(fn_bodies.into_iter())
        .enumerate()
        .map(|(fn_index, (fn_def, body))| {
            body.map(|body| {
                type_checker
                    .type_check_body(Context::for_fn(fn_index, &fn_def, &type_checker), body)
            })
        })
        .collect();

    let op_defs: Vec<_> = env
        .op_defs
        .into_iter()
        .map(|op_def| {
            let body = type_checker.type_check_body(Context::for_op(&op_def.sig), op_def.body);
            OpDef {
                sig: op_def.sig,
                body,
            }
        })
        .collect();

    let fn_def_order = type_checker.flatten_fn_graph();

    if !type_checker.errors.is_empty() {
        type_checker.errors.sort_by_key(|error| error.span());
        return Err(TypeCheckErrors(type_checker.errors));
    }

    let fn_defs = env
        .fn_defs
        .into_iter()
        .zip(fn_bodies.into_iter())
        .map(|(fn_def, body)| FnDef {
            sig: fn_def.sig,
            parent_type: fn_def.parent_type,
            is_unsafe: fn_def.is_unsafe,
            body,
        })
        .collect();

    Ok(Env {
        enum_defs: env.enum_defs,
        struct_defs: env.struct_defs,
        type_def_order,
        const_defs: env.const_defs,
        fn_defs,
        fn_def_order,
        op_defs,
    })
}

#[derive(Clone)]
struct Context<'a> {
    fn_index: Option<FnIndex>,
    path: Path<&'a str>,
    is_fallible: bool,
    is_unsafe: bool,
    param_vars: &'a [ParamVar],
    ret: TypeIndex,
}

impl<'a> Context<'a> {
    fn for_fn<'b: 'a>(
        fn_index: FnIndex,
        fn_def: &'a resolver::FnDef,
        type_checker: &TypeChecker<'b>,
    ) -> Self {
        Context {
            fn_index: Some(fn_index),
            path: type_checker.get_fn_path(fn_def),
            is_fallible: fn_def.sig.is_fallible,
            is_unsafe: fn_def.is_unsafe,
            param_vars: &fn_def.sig.param_vars,
            ret: fn_def.sig.ret,
        }
    }

    fn for_op(sig: &'a Sig) -> Self {
        Context {
            fn_index: None,
            path: Path::from_ident(&sig.ident.value),
            is_fallible: sig.is_fallible,
            is_unsafe: false,
            param_vars: &sig.param_vars,
            ret: sig.ret,
        }
    }

    fn inside_block_expr_with_kind(&self, kind: BlockExprKind) -> Self {
        let mut context = self.clone();
        *(match kind {
            BlockExprKind::Fallible => &mut context.is_fallible,
            BlockExprKind::Unsafe => &mut context.is_unsafe,
        }) = true;
        context
    }
}

struct TypeChecker<'a> {
    enum_defs: &'a [EnumDef],
    struct_defs: &'a [StructDef],
    const_defs: &'a [ConstDef],
    fn_defs: &'a [resolver::FnDef],
    fn_graph: FnGraph,
    errors: Vec<TypeCheckError>,
}

impl<'a> TypeChecker<'a> {
    fn new(
        enum_defs: &'a [EnumDef],
        struct_defs: &'a [StructDef],
        const_defs: &'a [ConstDef],
        fn_defs: &'a [resolver::FnDef],
    ) -> Self {
        TypeChecker {
            enum_defs,
            struct_defs,
            const_defs,
            fn_defs,
            fn_graph: FnGraph::new(fn_defs.len()),
            errors: Vec::new(),
        }
    }

    fn type_check_body<'b>(&mut self, context: Context<'b>, body: resolver::Body) -> Body {
        let ret = context.ret;

        let mut param_var_writes = vec![None; context.param_vars.len()];
        let mut local_vars = body.local_vars;
        let mut scoped_type_checker = ScopedTypeChecker {
            type_checker: self,
            context,
            param_var_writes: &mut param_var_writes,
            local_vars: &mut local_vars,
        };

        let block_span = body.block.span;
        let block = scoped_type_checker.type_check_block_expecting_type(body.block, ret);
        scoped_type_checker.ensure_all_out_params_written(block_span);

        let local_vars = local_vars
            .into_iter()
            .map(|local_var| LocalVar {
                ident: local_var.ident,
                type_: local_var.type_.unwrap(),
            })
            .collect();

        Body { local_vars, block }
    }

    fn try_match_types(
        &self,
        parent_type: TypeIndex,
        child_type: TypeIndex,
    ) -> Option<Vec<TypeIndex>> {
        if parent_type == child_type {
            return Some(Vec::new());
        }

        let mut upcast_route = Vec::new();
        let mut curr_type = child_type;
        loop {
            curr_type = match curr_type {
                TypeIndex::BuiltIn(_) => return None,
                TypeIndex::Enum(_) => return None,
                TypeIndex::Struct(struct_index) => self.struct_defs[struct_index].subtype_of?,
            };

            if curr_type == child_type {
                // We went all the way 'round a cycle. Though type cycles are
                // forbidden in Cachet, they can still exist during
                // type-checking. Break out here to prevent infinite loops.
                return None;
            }

            upcast_route.push(curr_type);

            if curr_type == parent_type {
                return Some(upcast_route);
            }
        }
    }

    fn try_build_upcast_chain(&self, expr: Expr, type_: TypeIndex) -> (bool, Expr) {
        let upcast_route = match self.try_match_types(type_, expr.type_()) {
            Some(upcast_route) => upcast_route,
            None => return (false, expr),
        };
        (
            true,
            build_cast_chain(CastExprKind::Upcast, expr, upcast_route.into_iter()),
        )
    }

    fn expect_expr_type(&mut self, expr: Spanned<Expr>, type_: TypeIndex) -> Expr {
        let expr_span = expr.span;
        let (success, expr) = self.try_build_upcast_chain(expr.value, type_);
        if !success {
            self.errors.push(TypeCheckError::ExprTypeMismatch {
                expected_type: self.get_type_ident(type_).to_owned(),
                found_type: self.get_type_ident(expr.type_()).to_owned(),
                expr_span,
            });
        }
        expr
    }

    fn flatten_type_graph(&mut self) -> Vec<TypeIndex> {
        let type_graph = TypeGraph::new(self.enum_defs, self.struct_defs);
        let type_sccs = type_graph.sccs();

        for cycle_type_indices in type_sccs.iter_cycles() {
            let mut cycle_types: Vec<_> = cycle_type_indices
                .map(|cycle_type_index| match cycle_type_index {
                    TypeIndex::BuiltIn(_) => {
                        unreachable!("built-in types can't participate in cycles")
                    }
                    TypeIndex::Enum(enum_index) => self.enum_defs[enum_index].ident.clone(),
                    TypeIndex::Struct(struct_index) => {
                        self.struct_defs[struct_index].ident.clone()
                    }
                })
                .collect();

            let first_cycle_type = cycle_types.remove(0);
            self.errors.push(TypeCheckError::SubtypeCycle {
                first_cycle_type,
                other_cycle_types: cycle_types,
            });
        }

        type_sccs.iter_post_order().collect()
    }

    fn flatten_fn_graph(&mut self) -> Vec<FnIndex> {
        let fn_sccs = self.fn_graph.sccs();

        for cycle_fn_indices in fn_sccs.iter_cycles() {
            let mut cycle_fns: Vec<_> = cycle_fn_indices
                .map(|cycle_fn_index| {
                    cycle_fn_index.map(|cycle_fn_index| {
                        self.get_fn_path(&self.fn_defs[cycle_fn_index]).to_owned()
                    })
                })
                .collect();

            // Rotate cycle_fns so that the first element corresponds to the call
            // that appears first in the source code.
            let first_cycle_fn_index = cycle_fns
                .iter()
                .enumerate()
                .min_by_key(|(_, cycle_fn)| cycle_fn.span)
                .map(|(cycle_fn_index, _)| cycle_fn_index)
                .unwrap();
            cycle_fns.rotate_left(first_cycle_fn_index);

            let first_cycle_fn = cycle_fns.remove(0);
            self.errors.push(TypeCheckError::FnCallCycle {
                first_cycle_fn,
                other_cycle_fns: cycle_fns,
            });
        }

        fn_sccs.iter_post_order().collect()
    }

    fn get_type_ident(&self, index: TypeIndex) -> &'a str {
        match index {
            TypeIndex::BuiltIn(built_in_type) => built_in_type.ident(),
            TypeIndex::Enum(enum_index) => &self.enum_defs[enum_index].ident.value,
            TypeIndex::Struct(struct_index) => &self.struct_defs[struct_index].ident.value,
        }
    }

    fn get_fn_path<'b>(&self, fn_def: &'b resolver::FnDef) -> Path<&'b str>
    where
        'a: 'b,
    {
        Path::new(
            fn_def
                .parent_type
                .map(|parent_type| self.get_type_ident(parent_type)),
            &fn_def.sig.ident.value,
        )
    }
}

fn build_cast_chain(
    kind: CastExprKind,
    expr: Expr,
    cast_route: impl Iterator<Item = TypeIndex>,
) -> Expr {
    cast_route.fold(expr, |expr, type_| CastExpr { kind, expr, type_ }.into())
}

struct ScopedTypeChecker<'a, 'b> {
    type_checker: &'a mut TypeChecker<'b>,
    context: Context<'a>,
    param_var_writes: &'a mut [Option<Span>],
    local_vars: &'a mut [resolver::LocalVar],
}

impl<'a> Deref for ScopedTypeChecker<'_, 'a> {
    type Target = TypeChecker<'a>;

    fn deref(&self) -> &Self::Target {
        self.type_checker
    }
}

impl<'a> DerefMut for ScopedTypeChecker<'_, 'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.type_checker
    }
}

impl<'a, 'b> ScopedTypeChecker<'a, 'b> {
    fn type_check_block(&mut self, block: resolver::Block) -> Block {
        let stmts = block
            .stmts
            .into_iter()
            .map(|stmt| self.type_check_stmt(stmt))
            .collect();

        let value = match block.value {
            None => BuiltInVar::Unit.into(),
            Some(value) => self.type_check_expr(value),
        };

        Block { stmts, value }
    }

    fn type_check_block_expecting_type(
        &mut self,
        block: Spanned<resolver::Block>,
        expected_type: TypeIndex,
    ) -> Block {
        let block_span = block.span;
        let mut block = self.type_check_block(block.value);
        block.value = self.expect_expr_type(Spanned::new(block_span, block.value), expected_type);
        block
    }

    fn type_check_stmt(&mut self, stmt: resolver::Stmt) -> Stmt {
        match stmt {
            resolver::Stmt::Let(let_stmt) => self.type_check_let_stmt(let_stmt).into(),
            resolver::Stmt::Check(check_stmt) => self.type_check_check_stmt(check_stmt).into(),
            resolver::Stmt::Expr(expr) => self.type_check_expr(expr).into(),
        }
    }

    fn type_check_let_stmt(&mut self, let_stmt: resolver::LetStmt) -> LetStmt {
        let rhs = let_stmt.rhs.map(|rhs| self.type_check_expr(rhs));

        let lhs = &mut self.local_vars[let_stmt.lhs];

        let rhs = match lhs.type_ {
            Some(type_) => self.expect_expr_type(rhs, type_),
            None => {
                lhs.type_ = Some(rhs.value.type_());
                rhs.value
            }
        };

        LetStmt {
            lhs: let_stmt.lhs,
            rhs,
        }
    }

    fn type_check_check_stmt(&mut self, check_stmt: resolver::CheckStmt) -> CheckStmt {
        let cond = self.type_check_expr_expecting_type(check_stmt.cond, BuiltInType::Bool.into());

        CheckStmt {
            kind: check_stmt.kind,
            cond,
        }
    }

    fn type_check_expr(&mut self, expr: resolver::Expr) -> Expr {
        match expr {
            resolver::Expr::Block(block_expr) => self.type_check_block_expr(*block_expr).into(),
            resolver::Expr::Var(index) => self.type_check_var_expr(index).into(),
            resolver::Expr::Call(call_expr) => self.type_check_call_expr(call_expr).into(),
            resolver::Expr::Cast(cast_expr) => self.type_check_cast_expr(*cast_expr),
            resolver::Expr::Compare(compare_expr) => {
                self.type_check_compare_expr(*compare_expr).into()
            }
            resolver::Expr::Assign(assign_expr) => {
                self.type_check_assign_expr(*assign_expr).into()
            }
        }
    }

    fn type_check_expr_expecting_type(
        &mut self,
        expr: Spanned<resolver::Expr>,
        expected_type: TypeIndex,
    ) -> Expr {
        let expr = expr.map(|expr| self.type_check_expr(expr));
        self.expect_expr_type(expr, expected_type)
    }

    fn type_check_block_expr(&mut self, block_expr: resolver::BlockExpr) -> BlockExpr {
        let block = match block_expr.kind {
            None => self.type_check_block(block_expr.block),
            Some(kind) => ScopedTypeChecker {
                type_checker: self.type_checker,
                context: self.context.inside_block_expr_with_kind(kind),
                param_var_writes: self.param_var_writes,
                local_vars: self.local_vars,
            }
            .type_check_block(block_expr.block),
        };

        BlockExpr {
            kind: block_expr.kind,
            block,
        }
    }

    fn type_check_var_expr(&mut self, index: Spanned<VarIndex>) -> VarExpr {
        let type_ = match index.value {
            VarIndex::BuiltIn(built_in_var) => built_in_var.type_(),
            VarIndex::EnumVariant(enum_variant_index) => enum_variant_index.type_(),
            VarIndex::Const(const_index) => self.const_defs[const_index].type_,
            VarIndex::ScopedVar(ScopedVarIndex::ParamVar(param_var_index)) => {
                let param_var = &self.context.param_vars[param_var_index];
                if param_var.is_out {
                    self.errors.push(TypeCheckError::ReadFromOutParamVar {
                        ident: Spanned::new(index.span, param_var.ident.value.clone()),
                        defined_at: param_var.ident.span,
                    });
                }
                param_var.type_
            }
            VarIndex::ScopedVar(ScopedVarIndex::LocalVar(local_var_index)) => self.local_vars
                [local_var_index]
                .type_
                .expect("local variable used before its definition"),
        };

        VarExpr {
            index: index.value,
            type_,
        }
    }

    fn type_check_call_expr(&mut self, call_expr: resolver::CallExpr) -> CallExpr {
        if let Some(source_fn_index) = self.context.fn_index {
            self.fn_graph.record_call(source_fn_index, call_expr.target);
        }

        let fn_def = &self.fn_defs[call_expr.target.value];

        if fn_def.sig.is_fallible && !self.context.is_fallible {
            self.type_checker
                .errors
                .push(TypeCheckError::FallibleCallInInfallibleContext {
                    target: Spanned::new(
                        call_expr.target.span,
                        self.get_fn_path(fn_def).to_owned(),
                    ),
                    target_defined_at: fn_def.sig.ident.span,
                });
        }
        if fn_def.is_unsafe && !self.context.is_unsafe {
            self.type_checker
                .errors
                .push(TypeCheckError::UnsafeCallInSafeContext {
                    target: Spanned::new(
                        call_expr.target.span,
                        self.get_fn_path(fn_def).to_owned(),
                    ),
                    target_defined_at: fn_def.sig.ident.span,
                });
        }

        let expected_arg_count = fn_def.sig.param_vars.len();
        let found_arg_count = call_expr.args.value.len();
        if found_arg_count != expected_arg_count {
            self.type_checker
                .errors
                .push(TypeCheckError::ArgCountMismatch {
                    expected_arg_count,
                    found_arg_count,
                    target: Spanned::new(
                        call_expr.target.span,
                        self.get_fn_path(fn_def).to_owned(),
                    ),
                    target_defined_at: fn_def.sig.ident.span,
                    args_span: call_expr.args.span,
                });
        }

        let param_vars = fn_def
            .sig
            .param_vars
            .iter()
            .map(Some)
            .chain(iter::repeat(None));
        let mut deferred_param_var_writes = Vec::new();
        let args: Vec<_> = param_vars
            .zip(call_expr.args.value.into_iter())
            .map(|(param_var, arg)| {
                self.type_check_call_expr_arg(
                    fn_def,
                    param_var,
                    arg,
                    &mut deferred_param_var_writes,
                )
            })
            .collect();
        for index in deferred_param_var_writes {
            self.record_param_var_write(index);
        }

        CallExpr {
            target: call_expr.target.value,
            is_fallible: fn_def.sig.is_fallible,
            is_unsafe: fn_def.is_unsafe,
            args,
            ret: fn_def.sig.ret,
        }
    }

    fn type_check_call_expr_arg(
        &mut self,
        fn_def: &resolver::FnDef,
        param_var: Option<&ParamVar>,
        arg: resolver::CallExprArg,
        deferred_param_var_writes: &mut Vec<Spanned<ParamVarIndex>>,
    ) -> CallExprArg {
        let (arg, arg_span, found_kind): (CallExprArg, _, _) = match arg {
            resolver::CallExprArg::Expr(expr) => (
                self.type_check_expr(expr.value).into(),
                expr.span,
                ArgKind::Expr,
            ),
            resolver::CallExprArg::OutRef(out_ref) => (
                self.type_check_out_ref(param_var, out_ref, deferred_param_var_writes)
                    .into(),
                out_ref.span,
                ArgKind::OutRef,
            ),
        };

        if let Some(param_var) = param_var {
            let expected_kind = if param_var.is_out {
                ArgKind::OutRef
            } else {
                ArgKind::Expr
            };
            if expected_kind != found_kind {
                self.type_checker
                    .errors
                    .push(TypeCheckError::ArgKindMismatch {
                        expected_kind,
                        found_kind,
                        arg_span,
                        target: self.get_fn_path(fn_def).to_owned(),
                        param_var: param_var.ident.clone(),
                    });
            } else {
                let (success, arg) = match arg {
                    CallExprArg::Expr(expr) => {
                        let (success, expr) = self.try_build_upcast_chain(expr, param_var.type_);
                        (success, CallExprArg::Expr(expr))
                    }
                    CallExprArg::OutRef(mut out_ref) => {
                        let success = if let Some(upcast_route) =
                            self.try_match_types(out_ref.type_, param_var.type_)
                        {
                            // Upcasting may be needed to convert from the type
                            // the out-parameter expects to the type of the
                            // parameter or out-variable we're passing into it.
                            out_ref.type_ = param_var.type_;
                            out_ref.upcast_route = upcast_route;
                            true
                        } else {
                            false
                        };
                        (success, CallExprArg::OutRef(out_ref))
                    }
                };

                if !success {
                    self.type_checker
                        .errors
                        .push(TypeCheckError::ArgTypeMismatch {
                            expected_type: self.get_type_ident(param_var.type_).to_owned(),
                            found_type: self.get_type_ident(arg.type_()).to_owned(),
                            arg_span,
                            target: self.get_fn_path(fn_def).to_owned(),
                            param_var: param_var.ident.clone(),
                        });
                }

                return arg;
            }
        }

        arg
    }

    fn type_check_out_ref(
        &mut self,
        param_var: Option<&ParamVar>,
        index: Spanned<ScopedVarIndex>,
        deferred_param_var_writes: &mut Vec<Spanned<ParamVarIndex>>,
    ) -> OutRef {
        let type_ = match index.value {
            ScopedVarIndex::ParamVar(param_var_index) => {
                let param_var = &self.context.param_vars[param_var_index];
                if !param_var.is_out {
                    self.errors.push(TypeCheckError::WriteToReadOnlyVar {
                        ident: Spanned::new(index.span, param_var.ident.value.clone()),
                        defined_at: param_var.ident.span,
                    });
                }
                deferred_param_var_writes.push(Spanned::new(index.span, param_var_index));
                param_var.type_
            }
            ScopedVarIndex::LocalVar(local_var_index) => {
                let local_var = &mut self.local_vars[local_var_index];
                match local_var.type_ {
                    Some(type_) => type_,
                    None => {
                        match param_var {
                            Some(param_var) => {
                                local_var.type_ = Some(param_var.type_);
                                param_var.type_
                            }
                            // Can't infer a type and no type ascription.
                            None => BuiltInType::Unit.into(),
                        }
                    }
                }
            }
        };

        OutRef {
            index: index.value,
            type_,
            upcast_route: Vec::new(),
        }
    }

    fn type_check_cast_expr(&mut self, cast_expr: resolver::CastExpr) -> Expr {
        let expr = self.type_check_expr(cast_expr.expr.value);

        let target_type = cast_expr.type_;
        let (upcast_success, expr) = self.try_build_upcast_chain(expr, target_type.value);
        if upcast_success {
            return expr;
        }

        let source_type = expr.type_();
        if let Some(downcast_route) = self.try_match_types(source_type, target_type.value) {
            if !self.context.is_unsafe {
                self.type_checker
                    .errors
                    .push(TypeCheckError::UnsafeCastInSafeContext {
                        source_type: self.get_type_ident(source_type).to_owned(),
                        target_type: target_type
                            .map(|target_type| self.get_type_ident(target_type).to_owned()),
                    });
            }

            if downcast_route.is_empty() {
                return expr;
            }

            return build_cast_chain(
                CastExprKind::Downcast,
                expr,
                iterate![
                    ..downcast_route.into_iter().rev().skip(1),
                    target_type.value
                ],
            );
        }

        self.type_checker.errors.push(TypeCheckError::InvalidCast {
            source_type: self.get_type_ident(source_type).to_owned(),
            target_type: self.get_type_ident(target_type.value).to_owned(),
            expr_span: cast_expr.expr.span,
        });

        // Emit an incorrectly-typed cast expression so that anything depending
        // on its type for inference lines up correctly.
        CastExpr {
            kind: CastExprKind::Upcast,
            expr,
            type_: target_type.value,
        }
        .into()
    }

    fn type_check_compare_expr(&mut self, compare_expr: resolver::CompareExpr) -> CompareExpr {
        let lhs = self.type_check_expr(compare_expr.lhs.value);
        let rhs = self.type_check_expr(compare_expr.rhs.value);

        let lhs_type = lhs.type_();
        let rhs_type = rhs.type_();

        let (lhs, rhs) = if let Some(rhs_upcast_route) = self.try_match_types(lhs_type, rhs_type) {
            (
                lhs,
                build_cast_chain(CastExprKind::Upcast, rhs, rhs_upcast_route.into_iter()),
            )
        } else if let Some(lhs_upcast_route) = self.try_match_types(rhs_type, lhs_type) {
            (
                build_cast_chain(CastExprKind::Upcast, lhs, lhs_upcast_route.into_iter()),
                rhs,
            )
        } else {
            self.type_checker
                .errors
                .push(TypeCheckError::BinaryOperatorTypeMismatch {
                    operator_span: compare_expr.kind.span,
                    lhs_span: compare_expr.lhs.span,
                    lhs_type: self.get_type_ident(lhs_type).to_owned(),
                    rhs_span: compare_expr.rhs.span,
                    rhs_type: self.get_type_ident(rhs_type).to_owned(),
                });
            (lhs, rhs)
        };

        if compare_expr.kind.value.is_numeric() {
            if !lhs_type.is_numeric() {
                self.type_checker
                    .errors
                    .push(TypeCheckError::NumericOperatorTypeMismatch {
                        operand_span: compare_expr.lhs.span,
                        operand_type: self.get_type_ident(lhs_type).to_owned(),
                        operator_span: compare_expr.kind.span,
                    });
            }

            if !rhs_type.is_numeric() {
                self.type_checker
                    .errors
                    .push(TypeCheckError::NumericOperatorTypeMismatch {
                        operand_span: compare_expr.rhs.span,
                        operand_type: self.get_type_ident(rhs_type).to_owned(),
                        operator_span: compare_expr.kind.span,
                    });
            }
        }

        CompareExpr {
            kind: compare_expr.kind.value,
            lhs,
            rhs,
        }
    }

    fn type_check_assign_expr(&mut self, assign_expr: resolver::AssignExpr) -> AssignExpr {
        let lhs = &self.context.param_vars[assign_expr.lhs.value];
        self.record_param_var_write(assign_expr.lhs);

        let rhs = self.type_check_expr(assign_expr.rhs.value);
        let rhs = if !lhs.is_out {
            self.errors.push(TypeCheckError::WriteToReadOnlyVar {
                ident: Spanned::new(assign_expr.lhs.span, lhs.ident.value.clone()),
                defined_at: lhs.ident.span,
            });
            rhs
        } else {
            let (success, rhs) = self.try_build_upcast_chain(rhs, lhs.type_);
            if !success {
                self.type_checker
                    .errors
                    .push(TypeCheckError::AssignTypeMismatch {
                        expected_type: self.get_type_ident(lhs.type_).to_owned(),
                        found_type: self.get_type_ident(rhs.type_()).to_owned(),
                        rhs_span: assign_expr.rhs.span,
                        param_var: lhs.ident.clone(),
                    });
            }
            rhs
        };

        AssignExpr {
            lhs: assign_expr.lhs.value,
            rhs,
        }
    }

    fn record_param_var_write(&mut self, index: Spanned<ParamVarIndex>) {
        let first_write_span = &mut self.param_var_writes[index.value];
        match first_write_span {
            Some(first_write_span) => {
                self.type_checker
                    .errors
                    .push(TypeCheckError::ExtraOutParamVarWrite {
                        extra_write_span: index.span,
                        first_write_span: *first_write_span,
                        context: self.context.path.to_owned(),
                        param_var: self.context.param_vars[index.value].ident.clone(),
                    });
            }
            None => {
                *first_write_span = Some(index.span);
            }
        }
    }

    fn ensure_all_out_params_written(&mut self, ret_span: Span) {
        for (param_var, first_write_span) in self
            .context
            .param_vars
            .iter()
            .zip(self.param_var_writes.iter())
        {
            if param_var.is_out && first_write_span.is_none() {
                self.type_checker
                    .errors
                    .push(TypeCheckError::UnwrittenOutParamVar {
                        ret_span: ret_span,
                        context: self.context.path.to_owned(),
                        param_var: param_var.ident.clone(),
                    });
            }
        }
    }
}
