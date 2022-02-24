// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::{HashMap, HashSet};
use std::iter;
use std::ops::{Deref, DerefMut};

use derive_more::{Display, From};
use enum_map::EnumMap;
use fix_hidden_lifetime_bug::Captures;
use iterate::iterate;
use lazy_static::lazy_static;
use typed_index_collections::{TiSlice, TiVec};
use void::unreachable;

use cachet_lang::ast::{BuiltInVar, CastKind, CheckKind, CompareKind, Ident, NegateKind, Path};
use cachet_lang::flattener::{self, Typed};
use cachet_util::{typed_field_index, MaybeOwned};

use crate::bpl::ast::*;

mod ast;

#[derive(Clone, Debug, Display)]
pub struct BplCode(Code);

pub fn compile(env: &flattener::Env) -> BplCode {
    let mut compiler = Compiler::new(env);

    for enum_item in &env.enum_items {
        compiler.compile_enum_item(enum_item);
    }

    for struct_item in &env.struct_items {
        compiler.compile_struct_item(struct_item);
    }

    for ir_item in &env.ir_items {
        compiler.compile_ir_item(ir_item);
    }

    for global_var_item in &env.global_var_items {
        compiler.compile_global_var_item(global_var_item);
    }

    for fn_index in env.fn_items.keys() {
        compiler.compile_callable_item(fn_index.into());
    }

    for op_index in env.op_items.keys() {
        compiler.compile_callable_item(op_index.into());
    }

    if let Some(top_op_item) = env.op_items.last() {
        if let Some(flattener::ParentIndex::Ir(top_ir_index)) = top_op_item.parent {
            let mut bottom_ir_index = top_ir_index;
            while let Some(next_ir_index) = env[bottom_ir_index].emits {
                bottom_ir_index = next_ir_index.value;
            }

            let flow_graph = trace_entry_point(&env, top_op_item, bottom_ir_index);
            compiler.compile_entry_point(top_op_item, top_ir_index, bottom_ir_index, &flow_graph);
        }
    }

    BplCode(compiler.items.into())
}

struct Compiler<'a> {
    env: &'a flattener::Env,
    items: Vec<Item>,
}

impl<'a> Compiler<'a> {
    fn new(env: &'a flattener::Env) -> Self {
        Compiler {
            env,
            items: Vec::new(),
        }
    }

    fn compile_enum_item(&mut self, enum_item: &flattener::EnumItem) {
        let type_ident = UserTypeIdent::from(enum_item.ident.value);

        let type_item = TypeItem {
            ident: type_ident.into(),
            attr: Some(TypeAttr::DataType),
            type_: None,
        }
        .into();

        let variant_ctor_fn_items = enum_item.variants.iter().map(|variant_path| {
            FnItem {
                ident: TypeMemberFnIdent {
                    type_ident,
                    selector: VariantCtorTypeMemberFnSelector::from(variant_path.value.ident())
                        .into(),
                }
                .into(),
                attr: Some(FnAttr::Ctor),
                param_vars: TypedVars::default(),
                ret: type_ident.into(),
                value: None,
            }
            .into()
        });

        self.items
            .extend(iterate![type_item, ..variant_ctor_fn_items]);
    }

    fn compile_struct_item(&mut self, struct_item: &flattener::StructItem) {
        if BLOCKED_PATHS.contains(&struct_item.ident.value.into()) {
            return;
        }

        let type_ident = UserTypeIdent::from(struct_item.ident.value);

        self.items.push(
            TypeItem {
                ident: type_ident.into(),
                attr: None,
                type_: None,
            }
            .into(),
        );

        if let Some(supertype_index) = struct_item.supertype {
            let supertype_ident = self.get_type_ident(supertype_index);

            self.items.extend([
                generate_cast_fn_item(CastKind::Downcast, supertype_ident, type_ident).into(),
                generate_cast_fn_item(CastKind::Upcast, supertype_ident, type_ident).into(),
                generate_cast_axiom_item(CastKind::Downcast, supertype_ident, type_ident).into(),
                generate_cast_axiom_item(CastKind::Upcast, supertype_ident, type_ident).into(),
            ]);
        }
    }

    fn compile_ir_item(&mut self, ir_item: &flattener::IrItem) {
        if ir_item.emits.is_some() {
            return;
        }

        let ir_ident = IrIdent::from(ir_item.ident.value);

        let op_type_item = TypeItem {
            ident: IrMemberTypeIdent {
                ir_ident,
                selector: IrMemberTypeSelector::Op,
            }
            .into(),
            attr: Some(TypeAttr::DataType),
            type_: None,
        };

        let external_op_ctor_fn_item = FnItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: OpCtorIrMemberFnSelector::from(OpSelector::External).into(),
            }
            .into(),
            attr: Some(FnAttr::Ctor),
            param_vars: TypedVars::default(),
            ret: op_type_item.ident.into(),
            value: None,
        };

        let pc_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::Pc,
            }
            .into(),
            type_: PreludeTypeIdent::Pc.into(),
        });

        let ops_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::Ops,
            }
            .into(),
            type_: MapType {
                key_types: vec![PreludeTypeIdent::Pc.into()],
                value_type: op_type_item.ident.into(),
            }
            .into(),
        });

        let incr_pc_stmt: Stmt = AssignStmt {
            lhs: pc_global_var_item.var.ident.into(),
            rhs: ArithExpr {
                kind: ArithKind::Add,
                lhs: pc_global_var_item.var.ident.into(),
                rhs: Literal::Int(1).into(),
            }
            .into(),
        }
        .into();

        let step_proc_item = ProcItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::Step,
            }
            .into(),
            attr: Some(InlineProcAttr { depth: 1 }.into()),
            param_vars: TypedVars::default(),
            ret_vars: TypedVars::default(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![incr_pc_stmt.clone()],
            }),
        };

        let pc_emit_paths_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::PcEmitPaths,
            }
            .into(),
            type_: MapType {
                key_types: vec![PreludeTypeIdent::Pc.into()],
                value_type: PreludeTypeIdent::EmitPath.into(),
            }
            .into(),
        });

        let emit_proc_item = ProcItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::Emit,
            }
            .into(),
            attr: None,
            param_vars: vec![
                TypedVar {
                    ident: ParamVarIdent::Op.into(),
                    type_: op_type_item.ident.into(),
                },
                TypedVar {
                    ident: ParamVarIdent::EmitPath.into(),
                    type_: PreludeTypeIdent::EmitPath.into(),
                },
            ]
            .into(),
            ret_vars: TypedVars::default(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![
                    AssignStmt {
                        lhs: IndexExpr {
                            base: pc_emit_paths_global_var_item.var.ident.into(),
                            key: pc_global_var_item.var.ident.into(),
                            value: None,
                        }
                        .into(),
                        rhs: ParamVarIdent::EmitPath.into(),
                    }
                    .into(),
                    AssignStmt {
                        lhs: IndexExpr {
                            base: ops_global_var_item.var.ident.into(),
                            key: pc_global_var_item.var.ident.into(),
                            value: None,
                        }
                        .into(),
                        rhs: ParamVarIdent::Op.into(),
                    }
                    .into(),
                    incr_pc_stmt,
                ],
            }),
        };

        let label_type_item = TypeItem {
            ident: IrMemberTypeIdent {
                ir_ident,
                selector: IrMemberTypeSelector::Label,
            }
            .into(),
            attr: None,
            type_: Some(NativeTypeIdent::Int.into()),
        };

        let next_label_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::NextLabel,
            }
            .into(),
            type_: label_type_item.ident.into(),
        });

        let label_pcs_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::LabelPcs,
            }
            .into(),
            type_: MapType {
                key_types: vec![label_type_item.ident.into()],
                value_type: PreludeTypeIdent::Pc.into(),
            }
            .into(),
        });

        let label_proc_item = ProcItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::Label,
            }
            .into(),
            attr: None,
            param_vars: TypedVars::default(),
            ret_vars: vec![TypedVar {
                ident: VarIdent::Ret.into(),
                type_: label_type_item.ident.into(),
            }]
            .into(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![
                    AssignStmt {
                        lhs: VarIdent::Ret.into(),
                        rhs: next_label_global_var_item.var.ident.into(),
                    }
                    .into(),
                    AssignStmt {
                        lhs: next_label_global_var_item.var.ident.into(),
                        rhs: ArithExpr {
                            kind: ArithKind::Add,
                            lhs: next_label_global_var_item.var.ident.into(),
                            rhs: Literal::Int(1).into(),
                        }
                        .into(),
                    }
                    .into(),
                ],
            }),
        };

        let bind_proc_item = ProcItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::Bind,
            }
            .into(),
            attr: Some(InlineProcAttr { depth: 1 }.into()),
            param_vars: vec![TypedVar {
                ident: ParamVarIdent::Label.into(),
                type_: label_type_item.ident.into(),
            }]
            .into(),
            ret_vars: TypedVars::default(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![
                    AssignStmt {
                        lhs: IndexExpr {
                            base: label_pcs_global_var_item.var.ident.into(),
                            key: ParamVarIdent::Label.into(),
                            value: None,
                        }
                        .into(),
                        rhs: pc_global_var_item.var.ident.into(),
                    }
                    .into(),
                ],
            }),
        };

        let goto_proc_item = ProcItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::Goto,
            }
            .into(),
            attr: Some(InlineProcAttr { depth: 1 }.into()),
            param_vars: vec![TypedVar {
                ident: ParamVarIdent::Label.into(),
                type_: label_type_item.ident.into(),
            }]
            .into(),
            ret_vars: TypedVars::default(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![
                    AssignStmt {
                        lhs: pc_global_var_item.var.ident.into(),
                        rhs: IndexExpr {
                            base: label_pcs_global_var_item.var.ident.into(),
                            key: ParamVarIdent::Label.into(),
                            value: None,
                        }
                        .into(),
                    }
                    .into(),
                ],
            }),
        };

        self.items.extend([
            op_type_item.into(),
            external_op_ctor_fn_item.into(),
            pc_global_var_item.into(),
            ops_global_var_item.into(),
            step_proc_item.into(),
            pc_emit_paths_global_var_item.into(),
            emit_proc_item.into(),
            label_type_item.into(),
            next_label_global_var_item.into(),
            label_pcs_global_var_item.into(),
            label_proc_item.into(),
            bind_proc_item.into(),
            goto_proc_item.into(),
        ]);
    }

    fn compile_global_var_item(&mut self, global_var_item: &flattener::GlobalVarItem) {
        let parent_ident = global_var_item
            .path
            .value
            .parent()
            .map(|parent_path| parent_path.ident());
        let var_ident = global_var_item.path.value.ident();
        let ident = UserGlobalVarIdent {
            parent_ident,
            var_ident,
        }
        .into();

        let type_ = self.get_type_ident(global_var_item.type_).into();

        let var = TypedVar { ident, type_ };
        self.items.push(if global_var_item.is_mut {
            GlobalVarItem::from(var).into()
        } else {
            ConstItem::from(var).into()
        });
    }

    fn compile_callable_item(&mut self, callable_index: flattener::CallableIndex) {
        let callable_item = &self.env[callable_index];
        if BLOCKED_PATHS.contains(&callable_item.path.value) {
            return;
        }

        let callable_parent_ident = callable_item
            .path
            .value
            .parent()
            .map(|parent_path| parent_path.ident());
        let callable_ident = callable_item.path.value.ident();
        let fn_ident = UserFnIdent {
            parent_ident: callable_parent_ident,
            fn_ident: callable_ident,
        };

        let (mut param_vars, mut ret_vars) =
            self.compile_params(&callable_item.params, &callable_item.param_order);

        if let flattener::CallableIndex::Op(_) = callable_index {
            // Define datatype constructors for interpreter ops.
            if let Some(ir_index) = callable_item.interprets {
                let ir_ident = self.env[ir_index].ident.value.into();
                let ident = IrMemberFnIdent {
                    ir_ident,
                    selector: OpCtorIrMemberFnSelector {
                        op_selector: UserOpSelector::from(callable_ident).into(),
                    }
                    .into(),
                }
                .into();

                let ret = IrMemberTypeIdent {
                    ir_ident,
                    selector: IrMemberTypeSelector::Op,
                }
                .into();

                self.items.push(
                    FnItem {
                        ident,
                        attr: Some(FnAttr::Ctor),
                        param_vars: param_vars.clone(),
                        ret,
                        value: None,
                    }
                    .into(),
                );
            }

            // Add an extea `emit_path` parameter to compiler ops.
            if let Some(_) = callable_item.emits {
                param_vars.push(TypedVar {
                    ident: ParamVarIdent::EmitPath.into(),
                    type_: PreludeTypeIdent::EmitPath.into(),
                });
            }
        }

        let body = callable_item
            .body
            .as_ref()
            .map(|body| self.compile_body(callable_index, &callable_item, body));

        match CallableRepr::for_callable(callable_item) {
            CallableRepr::Fn => {
                self.items.push(
                    FnItem {
                        ident: fn_ident.into(),
                        attr: None,
                        param_vars,
                        ret: self.get_type_ident(callable_item.type_()).into(),
                        value: None,
                    }
                    .into(),
                );
            }
            CallableRepr::Proc => {
                if let Some(ret) = callable_item.ret {
                    ret_vars.push(TypedVar {
                        ident: VarIdent::Ret,
                        type_: self.get_type_ident(ret).into(),
                    });
                }

                let (attr, body) = match body {
                    Some(body) => (None, body),
                    None => {
                        let body = if ret_vars.is_empty() {
                            Body::default()
                        } else {
                            // For external functions represented as procedures with
                            // return varlabies (for, e.g., out-parameters), set up a
                            // procedure body where each return variable is assigned to
                            // the result of delegating to a unique uninterpreted
                            // function.

                            let arg_exprs: Vec<_> =
                                param_vars.iter().map(|param| param.ident.into()).collect();

                            ret_vars
                                .iter()
                                .map(|ret_var| {
                                    let ident = ExternalUserFnHelperFnIdent {
                                        fn_ident,
                                        ret_var_ident: ret_var.ident,
                                    }
                                    .into();

                                    self.items.push(
                                        FnItem {
                                            ident,
                                            attr: None,
                                            param_vars: param_vars.clone(),
                                            ret: ret_var.type_.clone(),
                                            value: None,
                                        }
                                        .into(),
                                    );

                                    AssignStmt {
                                        lhs: ret_var.ident.into(),
                                        rhs: CallExpr {
                                            target: ident,
                                            arg_exprs: arg_exprs.clone(),
                                        }
                                        .into(),
                                    }
                                    .into()
                                })
                                .collect()
                        };

                        (Some(InlineProcAttr { depth: 1 }.into()), body)
                    }
                };

                self.items.push(
                    ProcItem {
                        ident: fn_ident.into(),
                        attr,
                        param_vars,
                        ret_vars,
                        body: Some(body),
                    }
                    .into(),
                );
            }
        };
    }

    fn compile_entry_point(
        &mut self,
        top_op_item: &flattener::CallableItem,
        top_ir_index: flattener::IrIndex,
        bottom_ir_index: flattener::IrIndex,
        flow_graph: &FlowGraph,
    ) {
        let top_op_ident = top_op_item.path.value.ident();

        let top_ir_ident = self.env[top_ir_index].ident.value.into();
        let bottom_ir_ident = self.env[bottom_ir_index].ident.value.into();

        let ident = EntryPointFnIdent {
            ir_ident: top_ir_ident,
            user_op_selector: top_op_ident.into(),
        }
        .into();

        // The parameters of the entry point are the variable parameters of the
        // top-level op. These will be considered by the solver to have
        // arbitrary initial values.
        let param_vars: TypedVars = top_op_item
            .param_order
            .iter()
            .filter_map(|param_index| match param_index {
                flattener::ParamIndex::Var(var_param_index) => {
                    Some(self.compile_var_param(&top_op_item.params[var_param_index]))
                }
                _ => None,
            })
            .collect();

        // Label parameters to the top-level op are reflected as local
        // variables.
        let label_params: Vec<&flattener::Label> = top_op_item
            .param_order
            .iter()
            .filter_map(|param_index| match param_index {
                flattener::ParamIndex::Label(label_param_index) => {
                    Some(&top_op_item.params[label_param_index])
                }
                _ => None,
            })
            .collect();
        let label_param_local_vars: Vec<LocalVar> = label_params
            .iter()
            .map(|label_param| self.compile_label_param(label_param).into())
            .collect();

        // Start by filling in all of the label parameter local variables with
        // fresh labels.
        let mut stmts: Vec<Stmt> = label_params
            .iter()
            .zip(label_param_local_vars.iter())
            .map(|(label_param, label_param_local_var)| {
                CallStmt {
                    call: CallExpr {
                        target: IrMemberFnIdent {
                            ir_ident: self.env[label_param.ir].ident.value.into(),
                            selector: IrMemberFnSelector::Label,
                        }
                        .into(),
                        arg_exprs: Vec::new(),
                    },
                    ret_var_idents: vec![label_param_local_var.var.ident],
                }
                .into()
            })
            .collect();

        let pc_emit_paths_var_ident: VarIdent = IrMemberGlobalVarIdent {
            ir_ident: bottom_ir_ident,
            selector: IrMemberGlobalVarSelector::PcEmitPaths,
        }
        .into();

        let pc_var_ident: VarIdent = IrMemberGlobalVarIdent {
            ir_ident: bottom_ir_ident,
            selector: IrMemberGlobalVarSelector::Pc,
        }
        .into();

        let nil_emit_path_expr: Expr = generate_emit_path_expr(&EmitLabelIdent::default()).into();

        let assume_pc_emit_paths_uninit_stmt = CheckStmt {
            kind: CheckKind::Assume,
            attr: None,
            cond: ForAllExpr {
                vars: vec![TypedVar {
                    ident: VarIdent::Pc,
                    type_: PreludeTypeIdent::Pc.into(),
                }]
                .into(),
                expr: CompareExpr {
                    kind: CompareKind::Eq,
                    lhs: IndexExpr {
                        base: pc_emit_paths_var_ident.into(),
                        key: VarIdent::Pc.into(),
                        value: None,
                    }
                    .into(),
                    rhs: nil_emit_path_expr.clone(),
                }
                .into(),
            }
            .into(),
        }
        .into();

        let init_pc_stmt: Stmt = AssignStmt {
            lhs: pc_var_ident.into(),
            rhs: Literal::Int(0).into(),
        }
        .into();

        // Call function corresponding to the top-level op, passing along the
        // root emit path, variable parameters, and labels from the local
        // variables declared for its labels parameters.
        let top_op_var_arg_exprs = param_vars.iter().map(|param_var| param_var.ident.into());
        let top_op_label_arg_exprs = label_param_local_vars
            .iter()
            .map(|label_param_local_var| label_param_local_var.var.ident.into());
        let top_op_arg_exprs = top_op_var_arg_exprs
            .chain(top_op_label_arg_exprs)
            .chain(iter::once(nil_emit_path_expr))
            .collect();
        let call_top_op_fn_stmt = CallExpr {
            target: UserFnIdent {
                parent_ident: Some(top_ir_ident.ident),
                fn_ident: top_op_ident,
            }
            .into(),
            arg_exprs: top_op_arg_exprs,
        }
        .into();

        stmts.extend([
            assume_pc_emit_paths_uninit_stmt,
            init_pc_stmt.clone(),
            call_top_op_fn_stmt,
        ]);

        // Bind all external labels to the trailing external emit. This covers
        // the top-level label parameters of the bottom IR.
        stmts.extend(
            label_params
                .iter()
                .zip(label_param_local_vars.iter())
                .filter(|(label_param, _)| label_param.ir == bottom_ir_index)
                .map(|(_, label_param_local_var)| {
                    CallExpr {
                        target: IrMemberFnIdent {
                            ir_ident: bottom_ir_ident,
                            selector: IrMemberFnSelector::Bind,
                        }
                        .into(),
                        arg_exprs: vec![label_param_local_var.var.ident.into()],
                    }
                    .into()
                }),
        );

        // Emit a trailing "external" op after everything else, to represent
        // control flow being transferred to some point outside the program.
        // This represents both normal termination and jumping to an
        // externally-provided label.
        let external_emit_node = &flow_graph.emit_nodes.last().unwrap();
        let external_emit_stmt = CallExpr {
            target: IrMemberFnIdent {
                ir_ident: bottom_ir_ident,
                selector: IrMemberFnSelector::Emit,
            }
            .into(),
            arg_exprs: vec![
                CallExpr {
                    target: IrMemberFnIdent {
                        ir_ident: bottom_ir_ident,
                        selector: OpCtorIrMemberFnSelector::from(OpSelector::External).into(),
                    }
                    .into(),
                    arg_exprs: Vec::new(),
                }
                .into(),
                generate_emit_path_expr(&external_emit_node.label_ident).into(),
            ],
        }
        .into();
        stmts.extend([external_emit_stmt, init_pc_stmt]);

        for emit_node in &flow_graph.emit_nodes {
            let emit_label_stmt = LabelStmt {
                label_ident: emit_node.label_ident.clone().into(),
            }
            .into();

            let assume_emit_path_stmt = CheckStmt {
                kind: CheckKind::Assume,
                attr: Some(CheckAttr::Partition),
                cond: CompareExpr {
                    kind: CompareKind::Eq,
                    lhs: IndexExpr {
                        base: pc_emit_paths_var_ident.into(),
                        key: pc_var_ident.into(),
                        value: None,
                    }
                    .into(),
                    rhs: generate_emit_path_expr(&emit_node.label_ident).into(),
                }
                .into(),
            }
            .into();

            stmts.extend([emit_label_stmt, assume_emit_path_stmt]);

            if let Some(target) = emit_node.target {
                let op_item = &self.env[target];
                let op_ident = op_item.path.value.ident();

                let assign_op_stmt = AssignStmt {
                    lhs: VarIdent::Op.into(),
                    rhs: IndexExpr {
                        base: IrMemberGlobalVarIdent {
                            ir_ident: bottom_ir_ident,
                            selector: IrMemberGlobalVarSelector::Ops,
                        }
                        .into(),
                        key: pc_var_ident.into(),
                        value: None,
                    }
                    .into(),
                }
                .into();

                let call_op_fn_stmt = CallExpr {
                    target: UserFnIdent {
                        parent_ident: Some(bottom_ir_ident.ident),
                        fn_ident: op_ident,
                    }
                    .into(),
                    arg_exprs: op_item
                        .param_order
                        .iter()
                        .filter_map(|param_index| match param_index {
                            flattener::ParamIndex::Var(var_param_index) => {
                                Some(op_item.params[var_param_index].ident.value)
                            }
                            flattener::ParamIndex::OutVar(_) => None,
                            flattener::ParamIndex::Label(label_param_index) => {
                                Some(op_item.params[label_param_index].ident.value)
                            }
                        })
                        .map(|param_ident| {
                            CallExpr {
                                target: OpCtorFieldFnIdent {
                                    param_var_ident: UserParamVarIdent::from(param_ident).into(),
                                    op_ctor_ident: IrMemberFnIdent {
                                        ir_ident: bottom_ir_ident,
                                        selector: OpCtorIrMemberFnSelector {
                                            op_selector: UserOpSelector::from(op_ident).into(),
                                        }
                                        .into(),
                                    },
                                }
                                .into(),
                                arg_exprs: vec![VarIdent::Op.into()],
                            }
                            .into()
                        })
                        .collect(),
                }
                .into();

                let mut succ_emit_node_indexes = HashSet::new();
                for succ in &emit_node.succs {
                    match succ {
                        EmitSucc::Emit(emit_node_index) => {
                            succ_emit_node_indexes.insert(*emit_node_index);
                        }
                        EmitSucc::Label(label_node_index) => {
                            succ_emit_node_indexes
                                .extend(flow_graph[label_node_index].bound_to.iter().copied());
                        }
                    }
                }
                debug_assert!(!succ_emit_node_indexes.is_empty());

                let goto_succs_stmt = GotoStmt {
                    labels: succ_emit_node_indexes
                        .iter()
                        .map(|succ_emit_node_index| {
                            flow_graph[succ_emit_node_index].label_ident.clone().into()
                        })
                        .collect(),
                }
                .into();

                stmts.extend([assign_op_stmt, call_op_fn_stmt, goto_succs_stmt]);
            }
        }

        // Tack on an additional local variable to hold the op being
        // interpreted.
        let mut local_vars = label_param_local_vars;
        local_vars.push(
            TypedVar {
                ident: VarIdent::Op,
                type_: IrMemberTypeIdent {
                    ir_ident: bottom_ir_ident,
                    selector: IrMemberTypeSelector::Op,
                }
                .into(),
            }
            .into(),
        );

        self.items.push(
            ProcItem {
                ident,
                attr: Some(ProcAttr::EntryPoint),
                param_vars,
                ret_vars: TypedVars::default(),
                body: Some(Body { local_vars, stmts }),
            }
            .into(),
        );
    }

    fn compile_params(
        &self,
        params: &flattener::Params,
        param_order: &[flattener::ParamIndex],
    ) -> (TypedVars, TypedVars) {
        let mut param_vars = TypedVars::default();
        let mut out_param_ret_vars = TypedVars::default();

        for param_index in param_order {
            match param_index {
                flattener::ParamIndex::Var(var_param_index) => {
                    param_vars.push(self.compile_var_param(&params[var_param_index]));
                }
                flattener::ParamIndex::OutVar(out_var_param_index) => out_param_ret_vars
                    .push(self.compile_out_var_param(&params[out_var_param_index])),
                flattener::ParamIndex::Label(label_param_index) => {
                    param_vars.push(self.compile_label_param(&params[label_param_index]));
                }
            }
        }

        (param_vars, out_param_ret_vars)
    }

    fn compile_var_param(&self, var_param: &flattener::VarParam) -> TypedVar {
        TypedVar {
            ident: UserParamVarIdent::from(var_param.ident.value).into(),
            type_: self.get_type_ident(var_param.type_).into(),
        }
    }

    fn compile_out_var_param(&self, out_var_param: &flattener::OutVarParam) -> TypedVar {
        self.compile_var_param(&flattener::VarParam {
            ident: out_var_param.ident,
            is_mut: true,
            type_: out_var_param.type_,
        })
    }

    fn compile_label_param(&self, label_param: &flattener::Label) -> TypedVar {
        TypedVar {
            ident: UserParamVarIdent::from(label_param.ident.value).into(),
            type_: IrMemberTypeIdent {
                ir_ident: self.env[label_param.ir].ident.value.into(),
                selector: IrMemberTypeSelector::Label,
            }
            .into(),
        }
    }

    fn compile_body(
        &mut self,
        callable_index: flattener::CallableIndex,
        callable_item: &flattener::CallableItem,
        body: &flattener::Body,
    ) -> Body {
        let mut local_vars = self.compile_locals(&body.locals).collect();

        let mut scoped_compiler = ScopedCompiler::new(
            self,
            callable_index,
            callable_item,
            &body.locals,
            &mut local_vars,
        );
        scoped_compiler.compile_stmts(&body.stmts);
        let stmts = scoped_compiler.stmts;

        Body { local_vars, stmts }
    }

    fn compile_locals<'b>(
        &'b self,
        locals: &'b flattener::Locals,
    ) -> impl 'b + Iterator<Item = LocalVar> + Captures<'a> {
        iterate![
            ..self.compile_local_vars(&locals.local_vars),
            ..self.compile_local_labels(&locals.local_labels),
        ]
    }

    fn compile_local_vars<'b>(
        &'b self,
        local_vars: &'b TiSlice<LocalVarIndex, flattener::LocalVar>,
    ) -> impl 'b + Iterator<Item = LocalVar> + Captures<'a> {
        local_vars
            .iter_enumerated()
            .map(|(local_var_index, local_var)| self.compile_local_var(local_var_index, local_var))
    }

    fn compile_local_var(
        &self,
        local_var_index: LocalVarIndex,
        local_var: &flattener::LocalVar,
    ) -> LocalVar {
        TypedVar {
            ident: LocalVarIdent {
                ident: local_var.ident.value,
                index: local_var_index,
            }
            .into(),
            type_: self.get_type_ident(local_var.type_).into(),
        }
        .into()
    }

    fn compile_local_labels<'b>(
        &'b self,
        local_labels: &'b TiSlice<LocalLabelIndex, flattener::Label>,
    ) -> impl 'b + Iterator<Item = LocalVar> + Captures<'a> {
        local_labels
            .iter_enumerated()
            .map(|(local_label_index, local_label)| {
                self.compile_local_label(local_label_index, local_label)
            })
    }

    fn compile_local_label(
        &self,
        local_label_index: LocalLabelIndex,
        local_label: &flattener::Label,
    ) -> LocalVar {
        TypedVar {
            ident: LocalLabelVarIdent {
                ident: local_label.ident.value,
                index: local_label_index,
            }
            .into(),
            type_: IrMemberTypeIdent {
                ir_ident: self.env[local_label.ir].ident.value.into(),
                selector: IrMemberTypeSelector::Label,
            }
            .into(),
        }
        .into()
    }

    fn get_type_ident(&self, type_index: flattener::TypeIndex) -> UserTypeIdent {
        let ident = match type_index {
            flattener::TypeIndex::BuiltIn(built_in_type) => built_in_type.ident(),
            flattener::TypeIndex::Enum(enum_index) => self.env[enum_index].ident.value,
            flattener::TypeIndex::Struct(struct_index) => self.env[struct_index].ident.value,
        };

        UserTypeIdent::from(ident)
    }
}

fn generate_cast_fn_item(
    kind: CastKind,
    supertype_ident: UserTypeIdent,
    subtype_ident: UserTypeIdent,
) -> FnItem {
    let (source_type_ident, target_type_ident) = match kind {
        CastKind::Downcast => (supertype_ident, subtype_ident),
        CastKind::Upcast => (subtype_ident, supertype_ident),
    };

    FnItem {
        ident: TypeMemberFnIdent {
            type_ident: subtype_ident,
            selector: CastTypeMemberFnSelector {
                kind,
                supertype_ident,
            }
            .into(),
        }
        .into(),
        attr: None,
        param_vars: vec![TypedVar {
            ident: ParamVarIdent::In.into(),
            type_: source_type_ident.into(),
        }]
        .into(),
        ret: target_type_ident.into(),
        value: None,
    }
}

fn generate_cast_axiom_item(
    kind: CastKind,
    supertype_ident: UserTypeIdent,
    subtype_ident: UserTypeIdent,
) -> AxiomItem {
    let in_var = TypedVar {
        ident: ParamVarIdent::In.into(),
        type_: match kind {
            CastKind::Downcast => subtype_ident,
            CastKind::Upcast => supertype_ident,
        }
        .into(),
    };

    let inner_call_expr = CallExpr {
        target: TypeMemberFnIdent {
            type_ident: subtype_ident,
            selector: CastTypeMemberFnSelector {
                kind: kind.reverse(),
                supertype_ident,
            }
            .into(),
        }
        .into(),
        arg_exprs: vec![in_var.ident.into()],
    }
    .into();

    let lhs = CallExpr {
        target: TypeMemberFnIdent {
            type_ident: subtype_ident,
            selector: CastTypeMemberFnSelector {
                kind,
                supertype_ident,
            }
            .into(),
        }
        .into(),
        arg_exprs: vec![inner_call_expr],
    }
    .into();

    let rhs = in_var.ident.into();

    let cond = ForAllExpr {
        vars: vec![in_var].into(),
        expr: CompareExpr {
            kind: CompareKind::Eq,
            lhs,
            rhs,
        }
        .into(),
    }
    .into();

    AxiomItem { cond }
}

fn generate_emit_path_expr(emit_label_ident: &EmitLabelIdent) -> CallExpr {
    let nil_emit_path_ctor_expr = CallExpr {
        target: PreludeFnIdent::NilEmitPathCtor.into(),
        arg_exprs: Vec::new(),
    };

    emit_label_ident
        .segments
        .iter()
        .fold(nil_emit_path_ctor_expr, |accum, emit_label_segment| {
            CallExpr {
                target: PreludeFnIdent::ConsEmitPathCtor.into(),
                arg_exprs: vec![
                    accum.into(),
                    Literal::Int(emit_label_segment.local_emit_index.into()).into(),
                ],
            }
        })
}

enum CallableRepr {
    Fn,
    Proc,
}

impl CallableRepr {
    fn for_callable(callable_item: &flattener::CallableItem) -> Self {
        if BLOCKED_PATHS.contains(&callable_item.path.value) {
            return Self::Proc;
        }

        let has_out_params =
            callable_item
                .param_order
                .iter()
                .any(|param_index| match param_index {
                    flattener::ParamIndex::OutVar(_) => true,
                    _ => false,
                });

        match (has_out_params, callable_item.ret, &callable_item.body) {
            (false, Some(_), None) => Self::Fn,
            _ => Self::Proc,
        }
    }
}

struct ScopedCompiler<'a, 'b> {
    compiler: &'b mut Compiler<'a>,
    callable_index: flattener::CallableIndex,
    callable_item: &'b flattener::CallableItem,
    callable_locals: &'b flattener::Locals,
    local_vars: &'b mut Vec<LocalVar>,
    next_local_emit_index: MaybeOwned<'b, LocalEmitIndex>,
    next_synthetic_var_indexes: MaybeOwned<'b, EnumMap<SyntheticVarKind, usize>>,
    stmts: Vec<Stmt>,
}

impl<'a> Deref for ScopedCompiler<'a, '_> {
    type Target = Compiler<'a>;

    fn deref(&self) -> &Self::Target {
        self.compiler
    }
}

impl<'a> DerefMut for ScopedCompiler<'a, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.compiler
    }
}

impl<'a, 'b> ScopedCompiler<'a, 'b> {
    fn new(
        compiler: &'b mut Compiler<'a>,
        callable_index: flattener::CallableIndex,
        callable_item: &'b flattener::CallableItem,
        callable_locals: &'b flattener::Locals,
        local_vars: &'b mut Vec<LocalVar>,
    ) -> Self {
        ScopedCompiler {
            compiler,
            callable_index,
            callable_item,
            callable_locals,
            local_vars,
            next_local_emit_index: MaybeOwned::Owned(LocalEmitIndex::from(0)),
            next_synthetic_var_indexes: MaybeOwned::Owned(EnumMap::default()),
            stmts: Vec::new(),
        }
    }

    fn recurse<'c>(&'c mut self) -> ScopedCompiler<'a, 'c> {
        ScopedCompiler {
            compiler: self.compiler,
            callable_index: self.callable_index,
            callable_item: self.callable_item,
            callable_locals: self.callable_locals,
            local_vars: self.local_vars,
            next_local_emit_index: MaybeOwned::Borrowed(&mut self.next_local_emit_index),
            next_synthetic_var_indexes: MaybeOwned::Borrowed(&mut self.next_synthetic_var_indexes),
            stmts: Vec::new(),
        }
    }

    fn compile_args(&mut self, args: &[flattener::Arg]) -> (Vec<Expr>, Vec<VarIdent>) {
        let mut arg_exprs = Vec::new();
        let mut ret_var_idents = Vec::new();

        for arg in args {
            match arg {
                flattener::Arg::Expr(atom_expr) => {
                    arg_exprs.push(self.compile_expr_arg(atom_expr))
                }
                flattener::Arg::OutVar(out_var_arg) => {
                    ret_var_idents.push(self.compile_out_var_arg(out_var_arg))
                }
                flattener::Arg::Label(label_index) => {
                    arg_exprs.push(self.compile_label_arg(*label_index))
                }
            }
        }

        (arg_exprs, ret_var_idents)
    }

    fn compile_expr_arg(&mut self, atom_expr: &flattener::AtomExpr) -> Expr {
        self.compile_atom_expr(atom_expr)
    }

    fn compile_out_var_arg(&mut self, out_var_arg: &flattener::OutVarArg) -> VarIdent {
        // TODO(spinda): Handle out-parameter upcasting.

        match out_var_arg.out_var {
            flattener::OutVar::Out(var_index) => self
                .get_var_ident(var_index.value)
                .expect("invalid out-variable argument"),
            // TODO(spinda): Eliminate these in the normalizer.
            flattener::OutVar::OutLet(_) => unimplemented!(),
        }
    }

    fn compile_label_arg(&mut self, label_index: flattener::LabelIndex) -> Expr {
        match label_index {
            flattener::LabelIndex::Param(label_param_index) => {
                UserParamVarIdent::from(self.callable_item.params[label_param_index].ident.value)
                    .into()
            }
            flattener::LabelIndex::Local(local_label_index) => LocalLabelVarIdent {
                ident: self.callable_locals[local_label_index].ident.value,
                index: local_label_index,
            }
            .into(),
        }
    }

    fn compile_invocation(
        &mut self,
        invoke_expr: &flattener::InvokeExpr,
        generate_proc_ret_var_ident: impl FnOnce(&mut Self, flattener::TypeIndex) -> VarIdent,
    ) -> CompiledInvocation {
        let callable_item = &self.env[invoke_expr.call.target];

        let callable_parent_ident = callable_item.path.value.parent().map(Path::ident);
        let callable_ident = callable_item.path.value.ident();
        let target = UserFnIdent {
            parent_ident: callable_parent_ident,
            fn_ident: callable_ident,
        }
        .into();

        let (arg_exprs, mut ret_var_idents) = self.compile_args(&invoke_expr.call.args);
        let call_expr = CallExpr { target, arg_exprs };

        match CallableRepr::for_callable(callable_item) {
            CallableRepr::Fn => CompiledInvocation::FnCall(call_expr),
            CallableRepr::Proc => {
                let ret_var_ident = match callable_item.ret {
                    Some(ret) => {
                        let ret_var_ident = generate_proc_ret_var_ident(self, ret);
                        ret_var_idents.push(ret_var_ident);
                        ret_var_ident
                    }
                    None => built_in_var_ident(BuiltInVar::Unit).into(),
                };

                self.stmts.push(
                    CallStmt {
                        call: call_expr,
                        ret_var_idents,
                    }
                    .into(),
                );

                CompiledInvocation::ProcCall(ret_var_ident)
            }
        }
    }

    fn compile_block(&mut self, stmts: &[flattener::Stmt]) -> Block {
        let mut scoped_compiler = self.recurse();
        scoped_compiler.compile_stmts(stmts);
        scoped_compiler.stmts.into()
    }

    fn compile_stmts(&mut self, stmts: &[flattener::Stmt]) {
        self.stmts.reserve(stmts.len());
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
    }

    fn compile_stmt(&mut self, stmt: &flattener::Stmt) {
        match stmt {
            flattener::Stmt::Let(let_stmt) => self.compile_let_stmt(let_stmt),
            flattener::Stmt::Label(label_stmt) => self.compile_label_stmt(label_stmt),
            flattener::Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt),
            flattener::Stmt::Check(check_stmt) => self.compile_check_stmt(check_stmt),
            flattener::Stmt::Goto(goto_stmt) => self.compile_goto_stmt(goto_stmt),
            flattener::Stmt::Bind(bind_stmt) => self.compile_bind_stmt(bind_stmt),
            flattener::Stmt::Emit(call) => self.compile_emit_stmt(call),
            flattener::Stmt::Block(void, _) => unreachable(*void),
            flattener::Stmt::Invoke(invoke_stmt) => self.compile_invoke_stmt(invoke_stmt),
            flattener::Stmt::Assign(assign_stmt) => self.compile_assign_stmt(assign_stmt),
            flattener::Stmt::Ret(ret_stmt) => self.compile_ret_stmt(ret_stmt),
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: &flattener::LetStmt) {
        let lhs = LocalVarIdent {
            ident: self.callable_locals[let_stmt.lhs].ident.value,
            index: let_stmt.lhs,
        }
        .into();

        let rhs = self.compile_expr(&let_stmt.rhs);

        self.stmts.push(AssignStmt { lhs, rhs }.into());
    }

    fn compile_label_stmt(&mut self, label_stmt: &flattener::LabelStmt) {
        let local_label = &self.callable_locals[label_stmt.label];
        let ir_ident = self.env[local_label.ir].ident.value.into();

        let call = CallExpr {
            target: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::Label,
            }
            .into(),
            arg_exprs: vec![].into(),
        };

        let ret_var_idents = vec![
            LocalLabelVarIdent {
                ident: local_label.ident.value,
                index: label_stmt.label,
            }
            .into(),
        ];

        self.stmts.push(
            CallStmt {
                call,
                ret_var_idents,
            }
            .into(),
        );
    }

    fn compile_if_stmt(&mut self, if_stmt: &flattener::IfStmt) {
        let cond = self.compile_expr(&if_stmt.cond);

        let then = self.compile_block(&if_stmt.then);

        let else_ = if_stmt
            .else_
            .as_ref()
            .map(|else_| self.compile_block(else_));

        self.stmts.push(IfStmt { cond, then, else_ }.into());
    }

    fn compile_check_stmt(&mut self, check_stmt: &flattener::CheckStmt) {
        let cond = self.compile_expr(&check_stmt.cond);

        self.stmts.push(
            CheckStmt {
                kind: check_stmt.kind,
                attr: None,
                cond,
            }
            .into(),
        );
    }

    fn compile_goto_stmt(&mut self, goto_stmt: &flattener::GotoStmt) {
        let ir_ident = self.env[goto_stmt.ir].ident.value.into();
        let target = IrMemberFnIdent {
            ir_ident,
            selector: IrMemberFnSelector::Goto,
        }
        .into();

        let arg_exprs = vec![self.compile_label_arg(goto_stmt.label)];

        self.stmts
            .extend([CallExpr { target, arg_exprs }.into(), Stmt::Ret]);
    }

    fn compile_bind_stmt(&mut self, bind_stmt: &flattener::BindStmt) {
        let ir_ident = self.env[bind_stmt.ir].ident.value.into();
        let target = IrMemberFnIdent {
            ir_ident,
            selector: IrMemberFnSelector::Bind,
        }
        .into();

        let arg_exprs = vec![self.compile_label_arg(bind_stmt.label)];

        self.stmts.extend([CallExpr { target, arg_exprs }.into()]);
    }

    fn compile_emit_stmt(&mut self, emit_stmt: &flattener::EmitStmt) {
        let ir_item = &self.env[emit_stmt.ir];

        let ir_ident = IrIdent::from(ir_item.ident.value);
        let op_ident = self.env[emit_stmt.call.target].path.value.ident();

        let (mut op_arg_exprs, _) = self.compile_args(&emit_stmt.call.args);

        let local_emit_index = *self.next_local_emit_index;
        *self.next_local_emit_index = LocalEmitIndex::from(usize::from(local_emit_index) + 1);

        let emit_path_expr = CallExpr {
            target: PreludeFnIdent::ConsEmitPathCtor.into(),
            arg_exprs: vec![
                ParamVarIdent::EmitPath.into(),
                Literal::Int(local_emit_index.into()).into(),
            ],
        }
        .into();

        match ir_item.emits {
            Some(_) => {
                // If the IR of the emitted op describes a compiler, call
                // directly into the other op.

                let target = UserFnIdent {
                    parent_ident: Some(ir_ident.ident),
                    fn_ident: op_ident,
                }
                .into();

                op_arg_exprs.push(emit_path_expr);

                self.stmts.push(
                    CallExpr {
                        target,
                        arg_exprs: op_arg_exprs,
                    }
                    .into(),
                );
            }
            None => {
                // Otherwise, build the op via the corresponding datatype
                // constructor and pass it to the emit helper function.

                let op_ctor_target = IrMemberFnIdent {
                    ir_ident,
                    selector: OpCtorIrMemberFnSelector {
                        op_selector: UserOpSelector::from(op_ident).into(),
                    }
                    .into(),
                }
                .into();
                let op_ctor_call_expr = CallExpr {
                    target: op_ctor_target,
                    arg_exprs: op_arg_exprs,
                }
                .into();

                let emit_target = IrMemberFnIdent {
                    ir_ident,
                    selector: IrMemberFnSelector::Emit,
                }
                .into();

                let emit_args = vec![op_ctor_call_expr, emit_path_expr];

                self.stmts.push(
                    CallExpr {
                        target: emit_target,
                        arg_exprs: emit_args,
                    }
                    .into(),
                );
            }
        }
    }

    fn compile_invoke_stmt(&mut self, invoke_stmt: &flattener::InvokeStmt) {
        let generate_synthetic_ret_var = |scoped_compiler: &mut Self, ret| {
            scoped_compiler
                .generate_synthetic_var(
                    SyntheticVarKind::Out,
                    scoped_compiler.get_type_ident(ret).into(),
                )
                .into()
        };

        match self.compile_invocation(invoke_stmt, generate_synthetic_ret_var) {
            CompiledInvocation::FnCall(call_expr) => {
                // The invocation compiles down to a Boogie function call. We can't
                // directly insert this as a statement, but we can wrap it up in an
                // assign statement and insert that.

                let callable_item = &self.env[invoke_stmt.call.target];

                let lhs = generate_synthetic_ret_var(self, callable_item.type_()).into();

                self.stmts.push(
                    AssignStmt {
                        lhs,
                        rhs: call_expr.into(),
                    }
                    .into(),
                );
            }
            CompiledInvocation::ProcCall(_) => (),
        }
    }

    fn compile_assign_stmt(&mut self, assign_stmt: &flattener::AssignStmt) {
        let lhs = self.compile_var_access(assign_stmt.lhs);

        let rhs = self.compile_expr(&assign_stmt.rhs);

        self.stmts.push(AssignStmt { lhs, rhs }.into());
    }

    fn compile_ret_stmt(&mut self, ret_stmt: &flattener::RetStmt) {
        let assign_stmt = ret_stmt.value.as_ref().and_then(|value| {
            let rhs = match value {
                flattener::Expr::Invoke(invoke_expr) => {
                    // If the return value is an invoke expression, we can have
                    // it assign directly to the containing function's return
                    // variable when the invoke expression compiles down to a
                    // Boogie procedure call.
                    match self.compile_invocation(invoke_expr, |_, _| VarIdent::Ret) {
                        // If the result is a Boogie function call, we still
                        // need to handle the assignment ourselves.
                        CompiledInvocation::FnCall(call_expr) => call_expr.into(),
                        CompiledInvocation::ProcCall(_) => return None,
                    }
                }
                value => self.compile_expr(value),
            };

            Some(
                AssignStmt {
                    lhs: VarIdent::Ret.into(),
                    rhs,
                }
                .into(),
            )
        });

        let step_call = match (self.callable_index, self.callable_item.interprets) {
            (flattener::CallableIndex::Op(_), Some(ir_index)) => {
                let ir_ident = self.env[ir_index].ident.value.into();

                Some(
                    CallExpr {
                        target: IrMemberFnIdent {
                            ir_ident,
                            selector: IrMemberFnSelector::Step,
                        }
                        .into(),
                        arg_exprs: Vec::new(),
                    }
                    .into(),
                )
            }
            _ => None,
        };

        self.stmts
            .extend(iterate![..assign_stmt, ..step_call, Stmt::Ret]);
    }

    fn compile_expr(&mut self, expr: &flattener::Expr) -> Expr {
        match expr {
            flattener::Expr::Block(void, _) => unreachable(*void),
            flattener::Expr::Literal(literal) => compile_literal(*literal).into(),
            flattener::Expr::Var(var_expr) => self.compile_var_expr(var_expr),
            flattener::Expr::Invoke(invoke_expr) => self.compile_invoke_expr(invoke_expr),
            flattener::Expr::Negate(negate_expr) => self.compile_negate_expr(&negate_expr),
            flattener::Expr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr).into(),
            flattener::Expr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).into()
            }
        }
    }

    fn compile_atom_expr(&mut self, atom_expr: &flattener::AtomExpr) -> Expr {
        match atom_expr {
            flattener::AtomExpr::Literal(literal) => compile_literal(*literal).into(),
            flattener::AtomExpr::Var(var_expr) => self.compile_var_expr(var_expr),
            flattener::AtomExpr::Negate(negate_expr) => self.compile_negate_expr(&negate_expr),
            flattener::AtomExpr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr).into(),
            flattener::AtomExpr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).into()
            }
        }
    }

    fn compile_var_expr(&self, var_expr: &flattener::VarExpr) -> Expr {
        self.compile_var_access(var_expr.var)
    }

    fn compile_var_access(&self, var_index: flattener::VarIndex) -> Expr {
        match var_index {
            flattener::VarIndex::EnumVariant(enum_variant_index) => {
                let enum_item = &self.env[enum_variant_index.enum_index];
                let variant_path = enum_item[enum_variant_index.variant_index];

                CallExpr {
                    target: TypeMemberFnIdent {
                        type_ident: enum_item.ident.value.into(),
                        selector: VariantCtorTypeMemberFnSelector::from(
                            variant_path.value.ident(),
                        )
                        .into(),
                    }
                    .into(),
                    arg_exprs: vec![],
                }
                .into()
            }
            var_index => self.get_var_ident(var_index).unwrap().into(),
        }
    }

    fn compile_invoke_expr(&mut self, invoke_expr: &flattener::InvokeExpr) -> Expr {
        let compiled_invocation = self.compile_invocation(invoke_expr, |scoped_compiler, ret| {
            scoped_compiler
                .generate_synthetic_var(
                    SyntheticVarKind::Out,
                    scoped_compiler.get_type_ident(ret).into(),
                )
                .into()
        });

        match compiled_invocation {
            CompiledInvocation::FnCall(call_expr) => call_expr.into(),
            CompiledInvocation::ProcCall(ret_var_ident) => ret_var_ident.into(),
        }
    }

    fn compile_negate_expr<E: CompileExpr + Typed>(
        &mut self,
        negate_expr: &flattener::NegateExpr<E>,
    ) -> Expr {
        let expr = negate_expr.expr.compile(self);

        match negate_expr.kind {
            NegateKind::Arithmetic => {
                let type_ident = self.get_type_ident(negate_expr.type_()).into();

                CallExpr {
                    target: TypeMemberFnIdent {
                        type_ident,
                        selector: TypeMemberFnSelector::Negate,
                    }
                    .into(),
                    arg_exprs: vec![expr],
                }
                .into()
            }
            kind => NegateExpr { kind, expr }.into(),
        }
    }

    fn compile_cast_expr<E: CompileExpr + Typed>(
        &mut self,
        cast_expr: &flattener::CastExpr<E>,
    ) -> CallExpr {
        let expr = cast_expr.expr.compile(self);

        let source_type_index = cast_expr.expr.type_();
        let target_type_index = cast_expr.type_;

        let (supertype_index, subtype_index) = match cast_expr.kind {
            CastKind::Downcast => (source_type_index, target_type_index),
            CastKind::Upcast => (target_type_index, source_type_index),
        };

        let supertype_ident = self.get_type_ident(supertype_index);
        let subtype_ident = self.get_type_ident(subtype_index);

        CallExpr {
            target: TypeMemberFnIdent {
                type_ident: subtype_ident,
                selector: CastTypeMemberFnSelector {
                    kind: cast_expr.kind,
                    supertype_ident,
                }
                .into(),
            }
            .into(),
            arg_exprs: vec![expr],
        }
    }

    fn compile_compare_expr(&mut self, compare_expr: &flattener::CompareExpr) -> CompareExpr {
        let lhs = self.compile_atom_expr(&compare_expr.lhs);
        let rhs = self.compile_atom_expr(&compare_expr.rhs);

        CompareExpr {
            kind: compare_expr.kind,
            lhs,
            rhs,
        }
    }

    fn generate_synthetic_var(
        &mut self,
        kind: SyntheticVarKind,
        type_: Type,
    ) -> SyntheticVarIdent {
        let index = &mut self.next_synthetic_var_indexes[kind];
        let ident = SyntheticVarIdent {
            kind,
            index: *index,
        };
        *index += 1;

        self.local_vars.push(
            TypedVar {
                ident: ident.into(),
                type_,
            }
            .into(),
        );

        ident
    }

    fn get_var_ident(&self, var_index: flattener::VarIndex) -> Option<VarIdent> {
        match var_index {
            flattener::VarIndex::BuiltIn(built_in_var) => {
                Some(built_in_var_ident(built_in_var).into())
            }
            flattener::VarIndex::EnumVariant(_) => None,
            flattener::VarIndex::Global(global_var_index) => {
                let global_var_item = &self.env[global_var_index];
                Some(
                    UserGlobalVarIdent {
                        parent_ident: global_var_item.path.value.parent().map(Path::ident),
                        var_ident: global_var_item.path.value.ident().into(),
                    }
                    .into(),
                )
            }
            flattener::VarIndex::Param(var_param_index) => {
                let var_param = &self.callable_item.params[var_param_index];
                Some(UserParamVarIdent::from(var_param.ident.value).into())
            }
            flattener::VarIndex::OutParam(out_var_param_index) => {
                let out_var_param = &self.callable_item.params[out_var_param_index];
                Some(UserParamVarIdent::from(out_var_param.ident.value).into())
            }
            flattener::VarIndex::Local(local_var_index) => {
                let local_var = &self.callable_locals[local_var_index];
                Some(
                    LocalVarIdent {
                        ident: local_var.ident.value,
                        index: local_var_index,
                    }
                    .into(),
                )
            }
        }
    }
}

fn compile_literal(literal: flattener::Literal) -> Literal {
    match literal {
        flattener::Literal::Int32(n) => Literal::Bv32(n as u32),
    }
}

fn built_in_var_ident(built_in_var: BuiltInVar) -> UserGlobalVarIdent {
    UserGlobalVarIdent {
        parent_ident: None,
        var_ident: built_in_var.ident(),
    }
}

trait CompileExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> Expr;
}

impl CompileExpr for flattener::Expr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> Expr {
        scoped_compiler.compile_expr(self)
    }
}

impl CompileExpr for flattener::AtomExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> Expr {
        scoped_compiler.compile_atom_expr(self)
    }
}

enum CompiledInvocation {
    FnCall(CallExpr),
    ProcCall(VarIdent),
}

#[derive(Clone, Debug, Default)]
struct FlowGraph {
    emit_nodes: TiVec<EmitNodeIndex, EmitNode>,
    label_nodes: TiVec<LabelNodeIndex, LabelNode>,
}

typed_field_index!(FlowGraph:emit_nodes[EmitNodeIndex] => EmitNode);
typed_field_index!(FlowGraph:label_nodes[LabelNodeIndex] => LabelNode);

#[derive(Clone, Debug)]
struct EmitNode {
    label_ident: EmitLabelIdent,
    succs: HashSet<EmitSucc>,
    target: Option<flattener::CallableIndex>,
}

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
enum EmitSucc {
    Emit(EmitNodeIndex),
    Label(LabelNodeIndex),
}

type LabelScope = HashMap<flattener::LabelIndex, LabelNodeIndex>;

#[derive(Clone, Debug, Default)]
struct LabelNode {
    bound_to: HashSet<EmitNodeIndex>,
}

fn trace_entry_point(
    env: &flattener::Env,
    top_op_item: &flattener::CallableItem,
    bottom_ir_index: flattener::IrIndex,
) -> FlowGraph {
    let mut flow_tracer = FlowTracer::new(env, bottom_ir_index);
    flow_tracer.init_top_label_params(&top_op_item.params, &top_op_item.param_order);
    flow_tracer.trace_op_item(top_op_item);
    flow_tracer.bind_external_labels();
    flow_tracer.graph.into_owned()
}

struct FlowTracer<'a> {
    env: &'a flattener::Env,
    bottom_ir_index: flattener::IrIndex,
    graph: MaybeOwned<'a, FlowGraph>,
    curr_emit_label_ident: &'a EmitLabelIdent,
    next_local_emit_index: MaybeOwned<'a, LocalEmitIndex>,
    pred_emits: MaybeOwned<'a, HashSet<EmitNodeIndex>>,
    label_scope: MaybeOwned<'a, LabelScope>,
    bound_labels: MaybeOwned<'a, HashSet<LabelNodeIndex>>,
}

impl<'a> FlowTracer<'a> {
    fn new(env: &'a flattener::Env, bottom_ir_index: flattener::IrIndex) -> Self {
        static ROOT_EMIT_LABEL_IDENT: EmitLabelIdent = EmitLabelIdent {
            segments: Vec::new(),
        };

        FlowTracer {
            env,
            bottom_ir_index,
            graph: MaybeOwned::Owned(FlowGraph::default()),
            curr_emit_label_ident: &ROOT_EMIT_LABEL_IDENT,
            next_local_emit_index: MaybeOwned::Owned(LocalEmitIndex::from(0)),
            pred_emits: MaybeOwned::Owned(HashSet::new()),
            label_scope: MaybeOwned::Owned(LabelScope::new()),
            bound_labels: MaybeOwned::Owned(HashSet::new()),
        }
    }

    fn init_top_label_params(
        &mut self,
        params: &flattener::Params,
        param_order: &[flattener::ParamIndex],
    ) {
        for param_index in param_order {
            if let flattener::ParamIndex::Label(label_param_index) = param_index {
                let label_param = &params[label_param_index];
                if label_param.ir == self.bottom_ir_index {
                    self.create_label_node(label_param_index.into());
                }
            }
        }
    }

    fn init_label_params_with_args(
        &mut self,
        params: &flattener::Params,
        param_order: &[flattener::ParamIndex],
        args: &[flattener::Arg],
        caller_label_scope: &LabelScope,
    ) {
        for item in param_order.iter().zip(args) {
            if let (
                flattener::ParamIndex::Label(label_param_index),
                flattener::Arg::Label(arg_label_index),
            ) = item
            {
                let label_param = &params[label_param_index];
                if label_param.ir == self.bottom_ir_index {
                    let arg_label_node_index = caller_label_scope[arg_label_index];
                    self.label_scope
                        .insert(label_param_index.into(), arg_label_node_index);
                }
            }
        }
    }

    fn bind_external_labels(&mut self) {
        self.bound_labels.extend(
            self.graph
                .label_nodes
                .iter_enumerated()
                .filter(|(_, label_node)| label_node.bound_to.is_empty())
                .map(|(label_node_index, _)| label_node_index),
        );

        let emit_label_segment = EmitLabelSegment {
            local_emit_index: self.take_local_emit_index(),
            ir_ident: self.env[self.bottom_ir_index].ident.value.into(),
            op_selector: OpSelector::External,
        };
        let emit_label_ident = vec![emit_label_segment].into();
        self.create_emit_node(emit_label_ident, None);
    }

    fn trace_op_item(&mut self, op_item: &flattener::CallableItem) {
        if let Some(body) = &op_item.body {
            self.trace_body(body);
        }
    }

    fn trace_body(&mut self, body: &flattener::Body) {
        self.trace_local_labels(&body.locals.local_labels);
        self.trace_stmts(&body.stmts);
    }

    fn trace_local_labels(&mut self, local_labels: &TiSlice<LocalLabelIndex, flattener::Label>) {
        for (local_label_index, local_label) in local_labels.iter_enumerated() {
            if local_label.ir == self.bottom_ir_index {
                self.create_label_node(local_label_index.into());
            }
        }
    }

    fn trace_stmts(&mut self, stmts: &[flattener::Stmt]) {
        for stmt in stmts {
            self.trace_stmt(stmt);
        }
    }

    fn trace_branches<'b>(&mut self, branches: impl Iterator<Item = &'b [flattener::Stmt]>) {
        // First, back up the predecessor emits and bound labels going into the
        // branches.
        let init_pred_emits = self.pred_emits.clone();
        let init_bound_labels = self.bound_labels.clone();
        self.pred_emits.clear();
        self.bound_labels.clear();

        for branch in branches {
            // Trace each branch from the starting predecessor emits and bound
            // labels.
            let mut flow_tracer = FlowTracer {
                env: self.env,
                bottom_ir_index: self.bottom_ir_index,
                graph: MaybeOwned::Borrowed(&mut self.graph),
                curr_emit_label_ident: self.curr_emit_label_ident,
                next_local_emit_index: MaybeOwned::Borrowed(&mut self.next_local_emit_index),
                pred_emits: MaybeOwned::Owned(init_pred_emits.clone()),
                label_scope: MaybeOwned::Borrowed(&mut self.label_scope),
                bound_labels: MaybeOwned::Owned(init_bound_labels.clone()),
            };
            flow_tracer.trace_stmts(branch);

            // Accumulate the predecessor emits and bound labels at the end of
            // each branch.
            self.pred_emits.extend(flow_tracer.pred_emits.into_owned());
            self.bound_labels
                .extend(flow_tracer.bound_labels.into_owned());
        }
    }

    fn trace_stmt(&mut self, stmt: &flattener::Stmt) {
        match stmt {
            flattener::Stmt::Let(_)
            | flattener::Stmt::Label(_)
            | flattener::Stmt::Check(_)
            | flattener::Stmt::Invoke(_)
            | flattener::Stmt::Assign(_)
            | flattener::Stmt::Ret(_) => (),
            flattener::Stmt::If(if_stmt) => self.trace_if_stmt(if_stmt),
            flattener::Stmt::Goto(goto_stmt) => self.trace_goto_stmt(goto_stmt),
            flattener::Stmt::Bind(bind_stmt) => self.trace_bind_stmt(bind_stmt),
            flattener::Stmt::Emit(emit_stmt) => self.trace_emit_stmt(emit_stmt),
            flattener::Stmt::Block(void, _) => unreachable(*void),
        }
    }

    fn trace_if_stmt(&mut self, if_stmt: &flattener::IfStmt) {
        static EMPTY_BRANCH: Vec<flattener::Stmt> = Vec::new();
        self.trace_branches(
            [
                if_stmt.then.as_slice(),
                match &if_stmt.else_ {
                    Some(else_) => &else_,
                    None => &EMPTY_BRANCH,
                },
            ]
            .into_iter(),
        );
    }

    fn trace_goto_stmt(&mut self, goto_stmt: &flattener::GotoStmt) {
        let label_node_index = self.label_scope[&goto_stmt.label];
        self.link_pred_emits_to_succ(label_node_index.into());
    }

    fn trace_bind_stmt(&mut self, bind_stmt: &flattener::BindStmt) {
        let label_node_index = self.label_scope[&bind_stmt.label];
        self.bound_labels.insert(label_node_index);
    }

    fn trace_emit_stmt(&mut self, emit_stmt: &flattener::EmitStmt) {
        let ir_item = &self.env[emit_stmt.ir];
        let op_item = &self.env[emit_stmt.call.target];

        let new_emit_label_segment = EmitLabelSegment {
            local_emit_index: self.take_local_emit_index(),
            ir_ident: ir_item.ident.value.into(),
            op_selector: UserOpSelector::from(op_item.path.value.ident()).into(),
        };

        // Extend the emit label identifier with the emit we're currently
        // looking at.
        let new_emit_label_ident: EmitLabelIdent = self
            .curr_emit_label_ident
            .segments
            .iter()
            .copied()
            .chain(iter::once(new_emit_label_segment))
            .collect();

        // If the op we're emitting is at the interpreter level, create a
        // corresponding emit node and set it as the new predecessor on-deck.
        if ir_item.emits.is_none() {
            self.create_emit_node(new_emit_label_ident.clone(), Some(emit_stmt.call.target));
        }

        // Trace inside the emitted op. It's important that we do this *after*
        // potentially creating a new emit node, so that, e.g., any goto
        // statements inside the op link their labels as successors of the new
        // emit node.

        let mut flow_tracer = FlowTracer {
            env: self.env,
            bottom_ir_index: self.bottom_ir_index,
            graph: MaybeOwned::Borrowed(&mut self.graph),
            curr_emit_label_ident: &new_emit_label_ident,
            next_local_emit_index: MaybeOwned::Owned(LocalEmitIndex::from(0)),
            pred_emits: MaybeOwned::Borrowed(&mut self.pred_emits),
            label_scope: MaybeOwned::Owned(LabelScope::new()),
            bound_labels: MaybeOwned::Borrowed(&mut self.bound_labels),
        };
        flow_tracer.init_label_params_with_args(
            &op_item.params,
            &op_item.param_order,
            &emit_stmt.call.args,
            &self.label_scope,
        );
        flow_tracer.trace_op_item(op_item);
    }

    fn create_emit_node(
        &mut self,
        label_ident: EmitLabelIdent,
        target: Option<flattener::CallableIndex>,
    ) {
        let emit_node_index = self.graph.emit_nodes.push_and_get_key(EmitNode {
            label_ident,
            succs: HashSet::new(),
            target,
        });

        self.link_pred_emits_to_succ(emit_node_index.into());
        self.pred_emits.insert(emit_node_index);

        self.link_bound_labels_to_emit(emit_node_index);
    }

    fn link_pred_emits_to_succ(&mut self, succ: EmitSucc) {
        for pred_emit in self.pred_emits.drain() {
            self.graph[pred_emit].succs.insert(succ);
        }
    }

    fn link_bound_labels_to_emit(&mut self, emit_node_index: EmitNodeIndex) {
        for bound_label_index in self.bound_labels.drain() {
            self.graph[bound_label_index]
                .bound_to
                .insert(emit_node_index);
        }
    }

    fn take_local_emit_index(&mut self) -> LocalEmitIndex {
        let local_emit_index = *self.next_local_emit_index;
        *self.next_local_emit_index = LocalEmitIndex::from(usize::from(local_emit_index) + 1);
        local_emit_index
    }

    fn create_label_node(&mut self, label_index: flattener::LabelIndex) {
        let label_node_index = self
            .graph
            .label_nodes
            .push_and_get_key(LabelNode::default());

        self.label_scope.insert(label_index, label_node_index);
    }
}

lazy_static! {
    // Temporary hack to implement a few functions in Boogie, until we implement
    // support for conditional compilation and polymorphism in Cachet.
    pub static ref BLOCKED_PATHS: HashSet<Path> = HashSet::from([
        Ident::from("ValueReg").into(),
        Ident::from("MASM").nest("getValue".into()),
        Ident::from("MASM").nest("setValue".into()),
        Ident::from("MASM").nest("getInt32".into()),
        Ident::from("MASM").nest("setInt32".into()),
        Ident::from("MASM").nest("getObject".into()),
        Ident::from("MASM").nest("setObject".into()),
        Ident::from("CacheIR").nest("allocateValueReg".into()),
        Ident::from("CacheIR").nest("releaseValueReg".into()),
        Ident::from("CacheIR").nest("allocateReg".into()),
        Ident::from("CacheIR").nest("releaseReg".into()),
    ]);
}
