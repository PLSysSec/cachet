// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;
mod graphs;

use std::iter;
use std::ops::{Deref, DerefMut};

use iterate::iterate;
use lazy_static::lazy_static;
use typed_index_collections::{TiSlice, TiVec};

use crate::ast::{
    BlockKind, BuiltInType, BuiltInVar, CastKind, Ident, MaybeSpanned, NegateKind, Path, Span,
    Spanned,
};
use crate::resolver;
use crate::FrontendError;

pub use crate::type_checker::ast::*;
pub use crate::type_checker::error::*;
use crate::type_checker::graphs::{CallGraph, TypeGraph};

pub fn type_check(mut env: resolver::Env) -> Result<Env, TypeCheckErrors> {
    // Set up an internal placeholder type used to "turn off" type-checking for
    // variables whose types can't be inferred. This type shouldn't escape the
    // type-checking phase.
    let unknown_type_index = env
        .struct_items
        .push_and_get_key(StructItem {
            ident: Spanned::new(Span::initial(), *UNKNOWN_TYPE_IDENT),
            supertype: None,
        })
        .into();

    let mut type_checker = TypeChecker::new(&env, unknown_type_index);

    let fn_items = type_checker.type_check_fn_items();
    let op_items = type_checker.type_check_op_items();

    let decl_order = type_checker.finish()?;

    // Strip out the internal "unknown" type, so it doesn't escape the
    // type-checking phase.
    let popped_type_index = env
        .struct_items
        .pop_key_value()
        .map(|(struct_index, _)| struct_index.into());
    debug_assert_eq!(popped_type_index, Some(unknown_type_index));

    Ok(Env {
        enum_items: env.enum_items,
        struct_items: env.struct_items,
        ir_items: env.ir_items,
        global_var_items: env.global_var_items,
        fn_items,
        op_items,
        decl_order,
    })
}

lazy_static! {
    static ref UNKNOWN_TYPE_IDENT: Ident = Ident::from("<Unknown>");
}

fn build_cast_chain(
    kind: CastKind,
    expr: Expr,
    cast_route: impl Iterator<Item = TypeIndex>,
) -> Expr {
    cast_route.fold(expr, |expr, type_index| {
        CastExpr {
            kind,
            expr,
            type_: type_index,
        }
        .into()
    })
}

struct TypeChecker<'a> {
    errors: Vec<TypeCheckError>,
    env: &'a resolver::Env,
    unknown_type: TypeIndex,
    call_graph: CallGraph,
}

impl<'a> TypeChecker<'a> {
    fn new(env: &'a resolver::Env, unknown_type_index: TypeIndex) -> Self {
        TypeChecker {
            errors: Vec::new(),
            env,
            unknown_type: unknown_type_index,
            call_graph: CallGraph::new(env.fn_items.len(), env.op_items.len()),
        }
    }

    fn finish(mut self) -> Result<Vec<DeclIndex>, TypeCheckErrors> {
        if !self.errors.is_empty() {
            self.errors.sort_by_key(|error| error.span());
            return Err(TypeCheckErrors(self.errors));
        }

        let unknown_type_index = self.unknown_type;
        let decl_order = self
            .flatten_type_graph()
            .into_iter()
            .filter_map(|type_index| {
                if type_index == unknown_type_index {
                    None
                } else {
                    DeclIndex::try_from(type_index).ok()
                }
            })
            .chain(self.env.global_var_items.keys().map(DeclIndex::from))
            .chain(self.flatten_call_graph().into_iter().map(DeclIndex::from))
            .collect();

        Ok(decl_order)
    }

    fn flatten_type_graph(&mut self) -> Vec<TypeIndex> {
        let type_graph = TypeGraph::new(&self.env.enum_items, &self.env.struct_items);
        let type_sccs = type_graph.sccs();

        for cycle_type_indexes in type_sccs.iter_cycles() {
            let mut cycle_types: Vec<_> = cycle_type_indexes
                .map(|cycle_type_index| match cycle_type_index {
                    TypeIndex::BuiltIn(_) => {
                        unreachable!("built-in types can't participate in cycles")
                    }
                    TypeIndex::Enum(enum_index) => self.env[enum_index].ident,
                    TypeIndex::Struct(struct_index) => self.env[struct_index].ident,
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

    fn flatten_call_graph(&mut self) -> Vec<CallableIndex> {
        let callable_sccs = self.call_graph.sccs();

        for cycle_callable_indexes in callable_sccs.iter_cycles() {
            let mut cycle_callables: Vec<_> = cycle_callable_indexes
                .map(|cycle_callable_index| {
                    cycle_callable_index
                        .map(|cycle_callable_index| self.env[cycle_callable_index].path.value)
                })
                .collect();

            // Rotate cycle_callables so that the first element corresponds to
            // the call that appears first in the source code.
            let first_cycle_callable_index = cycle_callables
                .iter()
                .enumerate()
                .min_by_key(|(_, cycle_callable)| cycle_callable.span)
                .map(|(cycle_callable_index, _)| cycle_callable_index)
                .unwrap();
            cycle_callables.rotate_left(first_cycle_callable_index);

            let first_cycle_callable = cycle_callables.remove(0);
            self.errors.push(TypeCheckError::CallCycle {
                first_cycle_callable,
                other_cycle_callables: cycle_callables,
            });
        }

        callable_sccs.iter_post_order().collect()
    }

    fn type_check_fn_items(&mut self) -> TiVec<FnIndex, CallableItem> {
        self.type_check_callable_items(self.env.fn_items.keys())
    }

    fn type_check_op_items(&mut self) -> TiVec<OpIndex, CallableItem> {
        self.type_check_callable_items(self.env.op_items.keys())
    }

    fn type_check_callable_items<I: Into<CallableIndex>>(
        &mut self,
        callable_indexes: impl Iterator<Item = I>,
    ) -> TiVec<I, CallableItem> {
        callable_indexes
            .map(|callable_index| self.type_check_callable_item(callable_index.into()))
            .collect()
    }

    fn type_check_callable_item(&mut self, callable_index: CallableIndex) -> CallableItem {
        let callable_item = &self.env[callable_index];

        let ret = callable_item.type_();

        let body = callable_item.body.value.as_ref().map(|body| {
            let mut local_vars = body.locals.local_vars.clone();

            let mut scoped_type_checker =
                ScopedTypeChecker::new(self, callable_index, &mut local_vars);
            let block = scoped_type_checker.type_check_block_expecting_type(
                Spanned::new(callable_item.body.span, &body.block),
                ret,
            );

            let locals = Locals {
                local_vars: local_vars
                    .into_iter()
                    .map(|local_var| LocalVar {
                        ident: local_var.ident,
                        is_mut: local_var.is_mut,
                        type_: local_var.type_.expect(
                            "the types of all local variables should be inferred by this point",
                        ),
                    })
                    .collect(),
                local_labels: body.locals.local_labels.clone(),
            };

            Body { locals, block }
        });

        CallableItem {
            path: callable_item.path,
            parent: callable_item.parent,
            is_unsafe: callable_item.is_unsafe,
            params: callable_item.params.clone(),
            param_order: callable_item.param_order.clone(),
            ret,
            body,
        }
    }

    fn expect_expr_type(&mut self, expr: Spanned<Expr>, type_index: TypeIndex) -> Expr {
        let expr_span = expr.span;
        let (success, expr) = self.try_build_upcast_chain(expr.value, type_index);
        if !success {
            self.errors.push(TypeCheckError::ExprTypeMismatch {
                expected_type: self.get_type_ident(type_index),
                found_type: self.get_type_ident(expr.type_()),
                expr_span,
            });
        }
        expr
    }

    fn try_build_upcast_chain(&self, expr: Expr, type_: TypeIndex) -> (bool, Expr) {
        let upcast_route = match self.try_match_types(type_, expr.type_()) {
            Some(upcast_route) => upcast_route,
            None => return (false, expr),
        };
        (
            true,
            build_cast_chain(CastKind::Upcast, expr, upcast_route.into_iter()),
        )
    }

    fn try_match_types(
        &self,
        supertype_index: TypeIndex,
        subtype_index: TypeIndex,
    ) -> Option<Vec<TypeIndex>> {
        // The internal "unknown" type unifies with everything. It's No values
        // of this type will ever actually be produced.
        if supertype_index == subtype_index
            || supertype_index == self.unknown_type
            || subtype_index == self.unknown_type
        {
            return Some(Vec::new());
        }

        let mut upcast_route = Vec::new();
        let mut curr_type_index = subtype_index;
        loop {
            curr_type_index = match curr_type_index {
                TypeIndex::BuiltIn(_) => return None,
                TypeIndex::Enum(_) => return None,
                TypeIndex::Struct(struct_index) => self.env.struct_items[struct_index].supertype?,
            };

            if curr_type_index == subtype_index {
                // We went all the way 'round a cycle. Though type cycles are
                // forbidden in Cachet, they can still exist during
                // type-checking. Break out here to prevent infinite loops.
                return None;
            }

            upcast_route.push(curr_type_index);

            if curr_type_index == supertype_index {
                return Some(upcast_route);
            }
        }
    }

    fn get_type_ident(&self, type_index: TypeIndex) -> Ident {
        match type_index {
            TypeIndex::BuiltIn(built_in_type) => built_in_type.ident(),
            TypeIndex::Enum(enum_index) => self.env.enum_items[enum_index].ident.value,
            TypeIndex::Struct(struct_index) => self.env.struct_items[struct_index].ident.value,
        }
    }

    fn is_numeric_type(&self, type_index: TypeIndex) -> bool {
        // Consider the internal "unknown" type to be numeric for the purposes
        // of the type-checking phase.
        type_index.is_numeric() || type_index == self.unknown_type
    }
}

struct ParamSummary {
    ident: Spanned<Ident>,
    type_: Option<TypeIndex>,
    expected_arg_kind: ArgKind,
}

struct ScopedTypeChecker<'a, 'b> {
    type_checker: &'b mut TypeChecker<'a>,
    callable_index: CallableIndex,
    is_unsafe: bool,
    local_vars: &'b mut TiSlice<LocalVarIndex, resolver::LocalVar>,
}

impl<'a, 'b> Deref for ScopedTypeChecker<'a, 'b> {
    type Target = TypeChecker<'a>;

    fn deref(&self) -> &Self::Target {
        self.type_checker
    }
}

impl<'a, 'b> DerefMut for ScopedTypeChecker<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.type_checker
    }
}

impl<'a, 'b> ScopedTypeChecker<'a, 'b> {
    fn new(
        type_checker: &'b mut TypeChecker<'a>,
        callable_index: CallableIndex,
        local_vars: &'b mut TiSlice<LocalVarIndex, resolver::LocalVar>,
    ) -> Self {
        let is_unsafe = type_checker.env[callable_index].is_unsafe;
        ScopedTypeChecker {
            type_checker,
            callable_index,
            is_unsafe,
            local_vars,
        }
    }

    fn recurse<'c>(&'c mut self, is_unsafe: bool) -> ScopedTypeChecker<'a, 'c> {
        ScopedTypeChecker {
            type_checker: self.type_checker,
            callable_index: self.callable_index,
            is_unsafe: self.is_unsafe || is_unsafe,
            local_vars: self.local_vars,
        }
    }

    fn type_check_arg(
        &mut self,
        target: CallableIndex,
        param_index: Option<resolver::ParamIndex>,
        arg: Spanned<&resolver::Arg>,
    ) -> Arg {
        let callable_item = &self.env[target];

        let param_summary = param_index.map(|param_index| match param_index {
            resolver::ParamIndex::Var(var_param_index) => {
                let var_param = &callable_item.params[var_param_index];
                ParamSummary {
                    ident: var_param.ident,
                    type_: Some(var_param.type_),
                    expected_arg_kind: ArgKind::Expr,
                }
            }
            resolver::ParamIndex::OutVar(out_var_param_index) => {
                let out_var_param = &callable_item.params[out_var_param_index];
                ParamSummary {
                    ident: out_var_param.ident,
                    type_: Some(out_var_param.type_),
                    expected_arg_kind: ArgKind::OutVar,
                }
            }
            resolver::ParamIndex::Label(label_param_index) => ParamSummary {
                ident: callable_item.params[label_param_index],
                type_: None,
                expected_arg_kind: ArgKind::Label,
            },
        });

        let arg_span = arg.span;
        let (found_arg_kind, arg) = match &arg.value {
            resolver::Arg::Expr(expr) => (
                ArgKind::Expr,
                self.type_check_expr_arg(
                    callable_item.path.value,
                    param_summary.as_ref(),
                    Spanned::new(arg_span, expr),
                )
                .into(),
            ),
            resolver::Arg::OutVar(out_var) => (
                ArgKind::OutVar,
                self.type_check_out_var_arg(
                    callable_item.path.value,
                    param_summary.as_ref(),
                    Spanned::new(arg_span, *out_var),
                )
                .into(),
            ),
            resolver::Arg::Label(label_index) => (ArgKind::Label, label_index.into()),
        };

        if let Some(ParamSummary {
            ident: param_ident,
            expected_arg_kind,
            ..
        }) = param_summary
        {
            if expected_arg_kind != found_arg_kind {
                self.errors.push(TypeCheckError::ArgKindMismatch {
                    expected_arg_kind,
                    found_arg_kind,
                    arg_span,
                    target: callable_item.path.value,
                    param: param_ident,
                });
            }
        }

        arg
    }

    fn type_check_expr_arg(
        &mut self,
        target: Path,
        param_summary: Option<&ParamSummary>,
        arg: Spanned<&resolver::Expr>,
    ) -> Expr {
        let expr = self.type_check_expr(arg.value);

        let expr = match param_summary {
            Some(&ParamSummary {
                ident: param_ident,
                type_: Some(param_type_index),
                ..
            }) => {
                let (success, expr) = self.try_build_upcast_chain(expr, param_type_index);
                if !success {
                    self.type_checker
                        .errors
                        .push(TypeCheckError::ArgTypeMismatch {
                            expected_type: self.get_type_ident(param_type_index),
                            found_type: self.get_type_ident(expr.type_()),
                            arg_span: arg.span,
                            target,
                            param: param_ident,
                        });
                }
                expr
            }
            _ => expr,
        };

        expr
    }

    fn type_check_out_var_arg(
        &mut self,
        target: Path,
        param_summary: Option<&ParamSummary>,
        arg: Spanned<OutVar>,
    ) -> OutVarArg {
        // Assign the internal "unknown" type if we don't have a parameter type
        // to cross-reference against. It will unify with whatever the argument
        // has for its type. An error should be reported elsewhere if this
        // happens (e.g., too many arguments or wrong argument kind), preventing
        // the internal "unknown" type from escaping the type-checking phase.
        let param_type_index = param_summary
            .and_then(|param_summary| param_summary.type_)
            .unwrap_or(self.unknown_type);

        let out_var_type_index = match arg.value {
            OutVar::Out(out_var_index) => self.get_var_type(out_var_index),
            // If this is an `out let foo`-style argument with no type
            // ascription, infer the type from the parameter.
            OutVar::OutLet(out_let_var_index) => *self.local_vars[out_let_var_index]
                .type_
                .get_or_insert(param_type_index),
        };

        // Upcasting may be needed to convert from the type the out-parameter
        // yields to the type of the variable being written.
        let upcast_route = match self.try_match_types(out_var_type_index, param_type_index) {
            Some(upcast_route) => upcast_route,
            None => {
                self.type_checker
                    .errors
                    .push(TypeCheckError::ArgTypeMismatch {
                        expected_type: self.get_type_ident(param_type_index),
                        found_type: self.get_type_ident(out_var_type_index),
                        arg_span: arg.span,
                        target,
                        param: param_summary.unwrap().ident,
                    });
                Vec::new()
            }
        };

        OutVarArg {
            out_var: arg.value,
            type_: param_type_index,
            upcast_route,
        }
    }

    fn type_check_call(&mut self, call: &resolver::Call) -> Call {
        self.type_checker
            .call_graph
            .record_call(self.callable_index, call.target);

        let callable_item = &self.env[call.target.value];

        if callable_item.is_unsafe && !self.is_unsafe {
            self.type_checker
                .errors
                .push(TypeCheckError::UnsafeCallInSafeContext {
                    target: Spanned::new(call.target.span, callable_item.path.value),
                    target_defined_at: callable_item.path.span,
                });
        }

        let expected_arg_count = callable_item.param_order.len();
        let found_arg_count = call.args.value.len();
        if found_arg_count != expected_arg_count {
            self.type_checker
                .errors
                .push(TypeCheckError::ArgCountMismatch {
                    expected_arg_count,
                    found_arg_count,
                    target: Spanned::new(call.target.span, callable_item.path.value),
                    target_defined_at: callable_item.path.span,
                    args_span: call.args.span,
                });
        }

        // Extend the parameter order past the end so that excess arguments
        // still get type-checked.
        let param_order = callable_item
            .param_order
            .iter()
            .copied()
            .map(Some)
            .chain(iter::repeat(None));

        let args: Vec<_> = param_order
            .zip(call.args.value.iter())
            .map(|(param_index, arg)| {
                self.type_check_arg(call.target.value, param_index, arg.as_ref())
            })
            .collect();

        Call {
            target: call.target.value,
            is_unsafe: callable_item.is_unsafe,
            args,
            ret: callable_item.type_(),
        }
    }

    fn type_check_block_expecting_type(
        &mut self,
        block: Spanned<&resolver::Block>,
        expected_type_index: TypeIndex,
    ) -> Block {
        let block_span = block.span;
        let mut block = self.type_check_block(block.value);
        block.value =
            self.expect_expr_type(Spanned::new(block_span, block.value), expected_type_index);
        block
    }

    fn type_check_block(&mut self, block: &resolver::Block) -> Block {
        let stmts = block
            .stmts
            .iter()
            .map(|stmt| self.type_check_stmt(stmt))
            .collect();

        let value = match &block.value {
            None => BuiltInVar::Unit.into(),
            Some(value) => self.type_check_expr(value),
        };

        Block { stmts, value }
    }

    fn type_check_stmt(&mut self, stmt: &resolver::Stmt) -> Stmt {
        match stmt {
            resolver::Stmt::Let(let_stmt) => self.type_check_let_stmt(let_stmt).into(),
            resolver::Stmt::If(if_stmt) => self.type_check_if_stmt(if_stmt).into(),
            resolver::Stmt::Check(check_stmt) => self.type_check_check_stmt(check_stmt).into(),
            resolver::Stmt::Goto(goto_stmt) => GotoStmt {
                label: goto_stmt.label,
            }
            .into(),
            resolver::Stmt::Emit(call) => self.type_check_call(call).into(),
            resolver::Stmt::Expr(expr) => self.type_check_expr(expr).into(),
        }
    }

    fn type_check_let_stmt(&mut self, let_stmt: &resolver::LetStmt) -> LetStmt {
        let rhs = let_stmt.rhs.as_ref().map(|rhs| self.type_check_expr(rhs));

        let lhs = &mut self.local_vars[let_stmt.lhs];

        let rhs = match lhs.type_ {
            Some(lhs_type_index) => self.expect_expr_type(rhs, lhs_type_index),
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

    fn type_check_if_stmt(&mut self, if_stmt: &resolver::IfStmt) -> IfStmt {
        let cond =
            self.type_check_expr_expecting_type(if_stmt.cond.as_ref(), BuiltInType::Bool.into());

        let then = self.type_check_block(&if_stmt.then);

        let else_ = if_stmt
            .else_
            .as_ref()
            .map(|else_| self.type_check_block(else_));

        IfStmt { cond, then, else_ }
    }

    fn type_check_check_stmt(&mut self, check_stmt: &resolver::CheckStmt) -> CheckStmt {
        let cond = self
            .type_check_expr_expecting_type(check_stmt.cond.as_ref(), BuiltInType::Bool.into());

        CheckStmt {
            kind: check_stmt.kind,
            cond,
        }
    }

    fn type_check_expr_expecting_type(
        &mut self,
        expr: Spanned<&resolver::Expr>,
        expected_type_index: TypeIndex,
    ) -> Expr {
        let expr = expr.map(|expr| self.type_check_expr(expr));
        self.expect_expr_type(expr, expected_type_index)
    }

    fn type_check_expr(&mut self, expr: &resolver::Expr) -> Expr {
        match expr {
            resolver::Expr::Block(block_expr) => self.type_check_block_expr(block_expr).into(),
            resolver::Expr::Var(index) => self.type_check_var_expr(*index).into(),
            resolver::Expr::Call(call) => self.type_check_call(call).into(),
            resolver::Expr::Negate(negate_expr) => self.type_check_negate_expr(negate_expr).into(),
            resolver::Expr::Cast(cast_expr) => self.type_check_cast_expr(cast_expr),
            resolver::Expr::Compare(compare_expr) => {
                self.type_check_compare_expr(compare_expr).into()
            }
            resolver::Expr::Assign(assign_expr) => self.type_check_assign_expr(assign_expr).into(),
        }
    }

    fn type_check_block_expr(&mut self, block_expr: &resolver::BlockExpr) -> BlockExpr {
        let block = match block_expr.kind {
            None => self.type_check_block(&block_expr.block),
            Some(kind) => self
                .recurse(kind == BlockKind::Unsafe)
                .type_check_block(&block_expr.block),
        };

        BlockExpr {
            kind: block_expr.kind,
            block,
        }
    }

    fn type_check_var_expr(&mut self, var_index: Spanned<VarIndex>) -> VarExpr {
        let var_type_index = self.get_var_type(var_index);

        VarExpr {
            var: var_index.value,
            type_: var_type_index,
        }
    }

    fn type_check_negate_expr(&mut self, negate_expr: &resolver::NegateExpr) -> NegateExpr {
        let expr = negate_expr
            .expr
            .as_ref()
            .map(|expr| self.type_check_expr(expr));
        let expr_type_index = expr.value.type_();

        let expr = match negate_expr.kind.value {
            NegateKind::Arithmetic => {
                if !self.is_numeric_type(expr_type_index) {
                    self.type_checker
                        .errors
                        .push(TypeCheckError::NumericOperatorTypeMismatch {
                            operand_span: expr.span,
                            operand_type: self.get_type_ident(expr_type_index),
                            operator_span: negate_expr.kind.span,
                        });
                }

                match expr_type_index {
                    TypeIndex::BuiltIn(BuiltInType::Bool) => CastExpr {
                        kind: CastKind::Upcast,
                        expr: expr.value,
                        type_: BuiltInType::Int32.into(),
                    }
                    .into(),
                    _ => expr.value,
                }
            }
            NegateKind::Logical => self.expect_expr_type(expr, BuiltInType::Bool.into()),
        };

        NegateExpr {
            kind: negate_expr.kind.value,
            expr,
        }
    }

    fn type_check_cast_expr(&mut self, cast_expr: &resolver::CastExpr) -> Expr {
        let expr = self.type_check_expr(&cast_expr.expr.value);

        let target_type_index = cast_expr.type_;
        let (upcast_success, expr) = self.try_build_upcast_chain(expr, target_type_index.value);
        if upcast_success {
            return expr;
        }

        let source_type_index = expr.type_();
        if let Some(downcast_route) =
            self.try_match_types(source_type_index, target_type_index.value)
        {
            if !self.is_unsafe {
                self.type_checker
                    .errors
                    .push(TypeCheckError::UnsafeCastInSafeContext {
                        source_type: self.get_type_ident(source_type_index),
                        target_type: target_type_index
                            .map(|target_type_index| self.get_type_ident(target_type_index)),
                    });
            }

            if downcast_route.is_empty() {
                return expr;
            }

            return build_cast_chain(
                CastKind::Downcast,
                expr,
                iterate![
                    ..downcast_route.into_iter().rev().skip(1),
                    target_type_index.value
                ],
            );
        }

        self.type_checker.errors.push(TypeCheckError::InvalidCast {
            source_type: self.get_type_ident(source_type_index),
            target_type: self.get_type_ident(target_type_index.value),
            expr_span: cast_expr.expr.span,
        });

        // Emit an incorrectly-typed cast expression so that anything depending
        // on its type for inference lines up correctly.
        CastExpr {
            kind: CastKind::Upcast,
            expr,
            type_: target_type_index.value,
        }
        .into()
    }

    fn type_check_compare_expr(&mut self, compare_expr: &resolver::CompareExpr) -> CompareExpr {
        let lhs = self.type_check_expr(&compare_expr.lhs.value);
        let rhs = self.type_check_expr(&compare_expr.rhs.value);

        let lhs_type_index = lhs.type_();
        let rhs_type_index = rhs.type_();

        let (lhs, rhs) = if let Some(rhs_upcast_route) =
            self.try_match_types(lhs_type_index, rhs_type_index)
        {
            (
                lhs,
                build_cast_chain(CastKind::Upcast, rhs, rhs_upcast_route.into_iter()),
            )
        } else if let Some(lhs_upcast_route) = self.try_match_types(rhs_type_index, lhs_type_index)
        {
            (
                build_cast_chain(CastKind::Upcast, lhs, lhs_upcast_route.into_iter()),
                rhs,
            )
        } else {
            self.type_checker
                .errors
                .push(TypeCheckError::BinaryOperatorTypeMismatch {
                    operator_span: compare_expr.kind.span,
                    lhs_span: compare_expr.lhs.span,
                    lhs_type: self.get_type_ident(lhs_type_index),
                    rhs_span: compare_expr.rhs.span,
                    rhs_type: self.get_type_ident(rhs_type_index),
                });
            (lhs, rhs)
        };

        if compare_expr.kind.value.is_numeric() {
            if !self.is_numeric_type(lhs_type_index) {
                self.type_checker
                    .errors
                    .push(TypeCheckError::NumericOperatorTypeMismatch {
                        operand_span: compare_expr.lhs.span,
                        operand_type: self.get_type_ident(lhs_type_index),
                        operator_span: compare_expr.kind.span,
                    });
            }

            if !self.is_numeric_type(rhs_type_index) {
                self.type_checker
                    .errors
                    .push(TypeCheckError::NumericOperatorTypeMismatch {
                        operand_span: compare_expr.rhs.span,
                        operand_type: self.get_type_ident(rhs_type_index),
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

    fn type_check_assign_expr(&mut self, assign_expr: &resolver::AssignExpr) -> AssignExpr {
        let lhs_type_index = self.get_var_type(assign_expr.lhs);

        let rhs = self.type_check_expr(&assign_expr.rhs.value);
        let (success, rhs) = self.try_build_upcast_chain(rhs, lhs_type_index);
        if !success {
            let lhs_path = self.get_var_path(assign_expr.lhs.value);
            self.type_checker
                .errors
                .push(TypeCheckError::AssignTypeMismatch {
                    expected_type: self.get_type_ident(lhs_type_index),
                    found_type: self.get_type_ident(rhs.type_()),
                    lhs: lhs_path.value,
                    lhs_defined_at: lhs_path.span,
                    rhs_span: assign_expr.rhs.span,
                });
        }

        AssignExpr {
            lhs: assign_expr.lhs,
            rhs,
        }
    }

    fn get_var_path(&self, var_index: VarIndex) -> MaybeSpanned<Path> {
        match var_index {
            VarIndex::BuiltIn(built_in_var) => Path::from(built_in_var.ident()).into(),
            VarIndex::EnumVariant(enum_variant_index) => self.env[enum_variant_index].into(),
            VarIndex::Global(global_var_index) => {
                self.env.global_var_items[global_var_index].path.into()
            }
            VarIndex::Param(var_param_index) => self.env[self.callable_index].params
                [var_param_index]
                .ident
                .map(Path::from)
                .into(),
            VarIndex::OutParam(out_var_param_index) => self.env[self.callable_index].params
                [out_var_param_index]
                .ident
                .map(Path::from)
                .into(),
            VarIndex::Local(local_var_index) => self.local_vars[local_var_index]
                .ident
                .map(Path::from)
                .into(),
        }
    }

    fn get_var_type(&mut self, var_index: Spanned<VarIndex>) -> TypeIndex {
        match var_index.value {
            VarIndex::BuiltIn(built_in_var) => built_in_var.type_(),
            VarIndex::EnumVariant(enum_variant_index) => enum_variant_index.type_(),
            VarIndex::Global(global_var_index) => {
                self.env.global_var_items[global_var_index].type_
            }
            VarIndex::Param(var_param_index) => {
                self.env[self.callable_index].params[var_param_index].type_
            }
            VarIndex::OutParam(out_var_param_index) => {
                self.env[self.callable_index].params[out_var_param_index].type_
            }
            VarIndex::Local(local_var_index) => {
                let local_var = &self.local_vars[local_var_index];
                match local_var.type_ {
                    Some(local_var_type) => local_var_type,
                    None => {
                        self.type_checker
                            .errors
                            .push(TypeCheckError::LocalVarUsedBeforeDef {
                                var: Spanned::new(var_index.span, local_var.ident.value),
                                defined_at: local_var.ident.span,
                            });
                        // Raising the error above will prevent the internal
                        // "unknown" type from escaping the type-checking phase.
                        self.unknown_type
                    }
                }
            }
        }
    }
}