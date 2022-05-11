// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::{BTreeSet, HashMap};
use std::fmt::{self, Display};
use std::iter;
use std::ops::{Deref, DerefMut};

use enum_map::EnumMap;
use fix_hidden_lifetime_bug::Captures;
use iterate::iterate;
use lazy_static::lazy_static;
use typed_index_collections::TiSlice;
use void::unreachable;

use cachet_lang::ast::{
    ArithKind, CastKind, CheckKind, CompareKind, Ident, NegateKind, Path, VarParamKind,
};
use cachet_lang::built_in::BuiltInVar;
use cachet_lang::flattener::{self, Typed};
use cachet_util::MaybeOwned;

use crate::bpl::ast::*;
use crate::bpl::flow::{trace_entry_point, EmitSucc, FlowGraph};

mod ast;
mod flow;

#[derive(Clone, Debug)]
pub struct BplCode(Code);

impl Display for BplCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}\n\n{}", include_str!("bpl/prelude.bpl"), self.0)
    }
}

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
        if BLOCKED_PATHS.get(&struct_item.ident.value.into()).is_some() {
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

        for struct_field in struct_item.fields.values() {
            self.items.push(
                FnItem {
                    ident: self.get_field_fn_ident(struct_field.into()),
                    param_vars: vec![
                        TypedVar {
                            ident: ParamVarIdent::Instance.into(),
                            type_: type_ident.into(),
                        }
                        .into(),
                    ]
                    .into(),
                    attr: None,
                    ret: self.get_type_ident(struct_field.type_).into(),
                    value: None,
                }
                .into(),
            )
        }

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
        // TODO(spinda): Always generate label infrastructure for IRs.

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

        let exit_op_ctor_fn_item = FnItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: OpCtorIrMemberFnSelector::from(OpSelector::Exit).into(),
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

        let bind_exit_proc_item = ProcItem {
            ident: IrMemberFnIdent {
                ir_ident,
                selector: IrMemberFnSelector::BindExit,
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
                        // PC 0 is reserved for an exit point.
                        rhs: Literal::Int(0).into(),
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
            exit_op_ctor_fn_item.into(),
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
            bind_exit_proc_item.into(),
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

        if BLOCKED_PATHS.get(&callable_item.path.value).is_some() || callable_item.is_prelude() {
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
                        // For external functions represented as procedures with
                        // return varlabies (for, e.g., out-parameters), set up a
                        // procedure body where each return variable is assigned to
                        // the result of delegating to a unique uninterpreted
                        // function.

                        let arg_exprs: Vec<_> =
                            param_vars.iter().map(|param| param.ident.into()).collect();

                        let ret_var_assign_stmts = ret_vars.iter().map(|ret_var| {
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
                        });

                        // Also bind all label out-parameters to an exit
                        // point.

                        let out_label_param_bind_stmts = callable_item
                            .param_order
                            .iter()
                            .filter_map(|param_index| match param_index {
                                flattener::ParamIndex::Label(label_param_index) => {
                                    Some(label_param_index)
                                }
                                _ => None,
                            })
                            .map(|label_param_index| &callable_item.params[label_param_index])
                            .filter(|label_param| label_param.is_out)
                            .map(|label_param| {
                                let ir_index = label_param.label.ir;
                                let ir_ident = self.env[ir_index].ident.value.into();

                                CallExpr {
                                    target: IrMemberFnIdent {
                                        ir_ident,
                                        selector: IrMemberFnSelector::BindExit,
                                    }
                                    .into(),
                                    arg_exprs: vec![
                                        UserParamVarIdent::from(label_param.label.ident.value)
                                            .into(),
                                    ],
                                }
                                .into()
                            });

                        let body = iterate![..ret_var_assign_stmts, ..out_label_param_bind_stmts]
                            .collect();

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
                    let var_param = &top_op_item.params[var_param_index];
                    match var_param.kind {
                        VarParamKind::In | VarParamKind::Mut => {
                            Some(self.compile_var_param(var_param))
                        }
                        VarParamKind::Out => None,
                    }
                }
                _ => None,
            })
            .collect();

        // Label parameters to the top-level op are reflected as local
        // variables.
        let label_params: Vec<&flattener::LabelParam> = top_op_item
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
                            ir_ident: self.env[label_param.label.ir].ident.value.into(),
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

        let init_emit_pc_stmt: Stmt = AssignStmt {
            lhs: pc_var_ident.into(),
            rhs: Literal::Int(0).into(),
        }
        .into();

        stmts.extend([assume_pc_emit_paths_uninit_stmt, init_emit_pc_stmt]);

        // Bind all top-level label parameters to the exit point we're about to
        // emit at PC 0. Filter down to just the labels of the bottom-level IR,
        // i.e., the ones we could actually jump to.
        stmts.extend(
            label_params
                .iter()
                .zip(label_param_local_vars.iter())
                .filter(|(label_param, _)| label_param.label.ir == bottom_ir_index)
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

        // Emit a leading "exit" op at the reserved PC 0, to represent control
        // flow being transferred to some point outside the program from a jump
        // to an externally-bound label (e.g., top-level label parameters).
        let exit_op_ctor_call_expr: Expr = CallExpr {
            target: IrMemberFnIdent {
                ir_ident: bottom_ir_ident,
                selector: OpCtorIrMemberFnSelector::from(OpSelector::Exit).into(),
            }
            .into(),
            arg_exprs: Vec::new(),
        }
        .into();
        let exit_emit_label_ident = &flow_graph[flow_graph.exit_emit_node_index()].label_ident;
        let exit_emit_path_expr: Expr = generate_emit_path_expr(&exit_emit_label_ident).into();
        let leading_exit_emit_stmt = CallExpr {
            target: IrMemberFnIdent {
                ir_ident: bottom_ir_ident,
                selector: IrMemberFnSelector::Emit,
            }
            .into(),
            arg_exprs: vec![exit_op_ctor_call_expr.clone(), exit_emit_path_expr.clone()],
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

        // Emit a trailing "exit" op after everything else, to represent
        // control flow being transferred to some point outside the program
        // following normal termination (i.e., control flow reaching the end of
        // the generated program).
        let trailing_exit_emit_stmt = CallExpr {
            target: IrMemberFnIdent {
                ir_ident: bottom_ir_ident,
                selector: IrMemberFnSelector::Emit,
            }
            .into(),
            arg_exprs: vec![exit_op_ctor_call_expr, exit_emit_path_expr],
        }
        .into();

        // Note that PC 0 is reserved for a leading exit-point which breaks from
        // control-flow within the program out to external code. We generate
        // this up front so we have a known PC for binding things like top-level
        // incoming label parameters and label out-parameters of external
        // functions. The first real op of the generated program is emitted at
        // PC 1, thus this is where the counter starts for the interpreter
        // phase.
        let init_interpret_pc_stmt: Stmt = AssignStmt {
            lhs: pc_var_ident.into(),
            rhs: Literal::Int(1).into(),
        }
        .into();

        stmts.extend([
            leading_exit_emit_stmt,
            call_top_op_fn_stmt,
            trailing_exit_emit_stmt,
            init_interpret_pc_stmt,
        ]);

        // For each emit node in the control-flow graph, emit a labeled section
        // which interprets the corresponding op and jumps to labels
        // corresponding to the possible subsequent emits. We do this for all
        // the regular emit nodes first, and then for the artificially-inserted
        // "exit" emit node at the end.
        let emit_nodes = iterate![
            ..flow_graph
                .emit_nodes
                .iter()
                .filter(|emit_node| !emit_node.is_exit()),
            &flow_graph[flow_graph.exit_emit_node_index()],
        ];
        for emit_node in emit_nodes {
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
                                let var_param = &op_item.params[var_param_index];
                                match var_param.kind {
                                    VarParamKind::In | VarParamKind::Mut => {
                                        Some(var_param.ident.value)
                                    }
                                    VarParamKind::Out => None,
                                }
                            }
                            flattener::ParamIndex::Label(label_param_index) => {
                                Some(op_item.params[label_param_index].label.ident.value)
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

                // Use a `BTreeSet` (as opposed to a `HashSet`) so the order of
                // the indexes is consistent for the same input every time the
                // compiler is run. The order of the indexes determines the
                // order of the labels in the goto statement generated below.
                let mut succ_emit_node_indexes = BTreeSet::new();
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

                // TODO(spinda): Elide goto on fallthrough.
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
                    let var_param = &params[var_param_index];
                    // Regular variable parameters become regular parameters;
                    // variable out-parameters become return variables.
                    match var_param.kind {
                        VarParamKind::In | VarParamKind::Mut => &mut param_vars,
                        VarParamKind::Out => &mut out_param_ret_vars,
                    }
                    .push(self.compile_var_param(var_param));
                }
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

    fn compile_label_param(&self, label_param: &flattener::LabelParam) -> TypedVar {
        // Both label parameters and out-parameters should be compiled to
        // regular Boogie parameters. Unlike with variable out-parameters, which
        // get translated to return variables, we don't actually *assign*
        // through label out-parameters, we just bind the label IDs passed in.
        TypedVar {
            ident: UserParamVarIdent::from(label_param.label.ident.value).into(),
            type_: IrMemberTypeIdent {
                ir_ident: self.env[label_param.label.ir].ident.value.into(),
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

    fn get_field_fn_ident(&self, field_index: flattener::FieldIndex) -> FnIdent {
        let struct_ident = self.env[field_index.struct_].ident.value;
        TypeMemberFnIdent {
            type_ident: UserTypeIdent::from(struct_ident),
            selector: TypeMemberFnSelector::Field(field_index.ident.into()),
        }
        .into()
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
        match BLOCKED_PATHS.get(&callable_item.path.value) {
            Some(BlockedType::Proc) => return Self::Proc,
            _ => (),
        };

        let has_out_params =
            callable_item
                .param_order
                .iter()
                .any(|param_index| match param_index {
                    flattener::ParamIndex::Var(var_param_index) => {
                        callable_item.params[var_param_index].kind == VarParamKind::Out
                    }
                    flattener::ParamIndex::Label(label_param_index) => {
                        callable_item.params[label_param_index].is_out
                    }
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
                flattener::Arg::Expr(pure_expr) => {
                    arg_exprs.push(self.compile_expr_arg(pure_expr))
                }
                flattener::Arg::OutVar(out_var_arg) => {
                    ret_var_idents.push(self.compile_out_var_arg(out_var_arg))
                }
                flattener::Arg::Label(label_arg) => {
                    arg_exprs.push(self.compile_label_arg(label_arg))
                }
            }
        }

        (arg_exprs, ret_var_idents)
    }

    fn compile_expr_arg(&mut self, pure_expr: &flattener::PureExpr) -> Expr {
        self.compile_pure_expr(pure_expr)
    }

    fn compile_out_var_arg(&mut self, out_var_arg: &flattener::OutVarArg) -> VarIdent {
        // TODO(spinda): Handle out-parameter upcasting.

        match out_var_arg.out_var {
            flattener::OutVar::Free(var_index) => self
                .get_var_ident(var_index.value)
                .expect("invalid out-variable argument"),
            // TODO(spinda): Eliminate these in the normalizer.
            flattener::OutVar::Fresh(_) => unimplemented!(),
        }
    }

    fn compile_label_arg(&mut self, label_arg: &flattener::LabelArg) -> Expr {
        self.compile_internal_label_arg(label_arg.label)
    }

    fn compile_internal_label_arg(&mut self, label_index: flattener::LabelIndex) -> Expr {
        // As with compiling label parameters and out-parameters, we compile
        // label arguments and label out-arguments the same way when generating
        // Boogie.
        match label_index {
            flattener::LabelIndex::Param(label_param_index) => UserParamVarIdent::from(
                self.callable_item.params[label_param_index]
                    .label
                    .ident
                    .value,
            )
            .into(),
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

    fn compile_if_stmt_recurse(&mut self, if_stmt: &flattener::IfStmt) -> IfStmt {
        let cond = self.compile_expr(&if_stmt.cond);

        let then = self.compile_block(&if_stmt.then);

        let else_ = if_stmt.else_.as_ref().map(|else_| match else_ {
            flattener::ElseClause::ElseIf(else_if) => {
                ast::ElseClause::ElseIf(Box::new(self.compile_if_stmt_recurse(&*else_if)))
            }
            flattener::ElseClause::Else(else_block) => {
                ast::ElseClause::Else(self.compile_block(else_block))
            }
        });

        IfStmt { cond, then, else_ }
    }

    fn compile_if_stmt(&mut self, if_stmt: &flattener::IfStmt) {
        let if_ = self.compile_if_stmt_recurse(if_stmt);
        self.stmts.push(if_.into());
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

        let arg_exprs = vec![self.compile_internal_label_arg(goto_stmt.label)];

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

        let arg_exprs = vec![self.compile_internal_label_arg(bind_stmt.label)];

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
            flattener::Expr::FieldAccess(field_access_expr) => {
                self.compile_field_access_expr(&field_access_expr).into()
            }
            flattener::Expr::Negate(negate_expr) => self.compile_negate_expr(&negate_expr),
            flattener::Expr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr).into(),
            flattener::Expr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).into()
            }
            flattener::Expr::Arith(arith_expr) => self.compile_arith_expr(&arith_expr).into(),
            flattener::Expr::Bitwise(bitwise_expr) => {
                self.compile_bitwise_expr(&bitwise_expr).into()
            }
        }
    }

    fn compile_pure_expr(&mut self, pure_expr: &flattener::PureExpr) -> Expr {
        match pure_expr {
            flattener::PureExpr::Literal(literal) => compile_literal(*literal).into(),
            flattener::PureExpr::Var(var_expr) => self.compile_var_expr(var_expr),
            flattener::PureExpr::FieldAccess(field_access_expr) => {
                self.compile_field_access_expr(&field_access_expr).into()
            }
            flattener::PureExpr::Negate(negate_expr) => self.compile_negate_expr(&negate_expr),
            flattener::PureExpr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr).into(),
            flattener::PureExpr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).into()
            }
            flattener::PureExpr::Arith(arith_expr) => self.compile_arith_expr(&arith_expr).into(),
            flattener::PureExpr::Bitwise(bitwise_expr) => {
                self.compile_bitwise_expr(&bitwise_expr).into()
            }
        }
    }

    fn compile_var_expr(&self, var_expr: &flattener::VarExpr) -> Expr {
        self.compile_var_access(var_expr.var)
    }

    fn compile_var_access(&self, var_index: flattener::VarIndex) -> Expr {
        match var_index {
            flattener::VarIndex::BuiltIn(BuiltInVar::True) => true.into(),
            flattener::VarIndex::BuiltIn(BuiltInVar::False) => false.into(),
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

    fn compile_field_access_expr<E: CompileExpr + Typed>(
        &mut self,
        field_access_expr: &flattener::FieldAccessExpr<E>,
    ) -> CallExpr {
        let field_fn_ident = self.get_field_fn_ident(field_access_expr.field);
        let parent_expr = field_access_expr.parent.compile(self);

        CallExpr {
            target: field_fn_ident,
            arg_exprs: vec![parent_expr],
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

    fn compile_compare_expr(&mut self, compare_expr: &flattener::CompareExpr) -> Expr {
        let lhs = self.compile_pure_expr(&compare_expr.lhs);
        let rhs = self.compile_pure_expr(&compare_expr.rhs);
        let type_ident = self.get_type_ident(compare_expr.lhs.type_());

        let selector = match compare_expr.kind {
            CompareKind::Eq | CompareKind::Neq => {
                return CompareExpr {
                    kind: compare_expr.kind,
                    lhs,
                    rhs,
                }
                .into();
            }
            CompareKind::Numeric(kind) => kind.into(),
        };

        CallExpr {
            target: TypeMemberFnIdent {
                type_ident,
                selector,
            }
            .into(),
            arg_exprs: vec![lhs, rhs],
        }
        .into()
    }

    fn compile_arith_expr(&mut self, arith_expr: &flattener::ArithExpr) -> CallExpr {
        let lhs = self.compile_pure_expr(&arith_expr.lhs);
        let rhs = self.compile_pure_expr(&arith_expr.rhs);

        let type_ident = self.get_type_ident(arith_expr.type_()).into();
        let selector = arith_expr.kind.into();

        CallExpr {
            target: TypeMemberFnIdent {
                type_ident,
                selector,
            }
            .into(),
            arg_exprs: vec![lhs, rhs],
        }
        .into()
    }

    fn compile_bitwise_expr(&mut self, bitwise_expr: &flattener::BitwiseExpr) -> CallExpr {
        let lhs = self.compile_pure_expr(&bitwise_expr.lhs);
        let rhs = self.compile_pure_expr(&bitwise_expr.rhs);

        let type_ident = self.get_type_ident(bitwise_expr.type_()).into();
        let selector = bitwise_expr.kind.into();

        CallExpr {
            target: TypeMemberFnIdent {
                type_ident,
                selector,
            }
            .into(),
            arg_exprs: vec![lhs, rhs],
        }
        .into()
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
        flattener::Literal::Int64(n) => Literal::Bv64(n as u64),
        flattener::Literal::UInt16(n) => Literal::Bv16(n),
        flattener::Literal::Double(n) => Literal::Float64(n),
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

impl CompileExpr for flattener::PureExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> Expr {
        scoped_compiler.compile_pure_expr(self)
    }
}

enum CompiledInvocation {
    FnCall(CallExpr),
    ProcCall(VarIdent),
}

pub enum BlockedType {
    Proc,
    Var,
}

lazy_static! {
    // Temporary hack to implement a few functions in Boogie, until we implement
    // support for conditional compilation and polymorphism in Cachet.
    pub static ref BLOCKED_PATHS: HashMap<Path, BlockedType> = {
        use BlockedType::*;
        HashMap::from([
            (Ident::from("ValueReg").into(), Var),
            (Ident::from("MASM").nest("getValue".into()), Proc),
            (Ident::from("MASM").nest("setValue".into()), Proc),
            (Ident::from("MASM").nest("getInt32".into()), Proc),
            (Ident::from("MASM").nest("setInt32".into()), Proc),
            (Ident::from("MASM").nest("getBool".into()), Proc),
            (Ident::from("MASM").nest("setBool".into()), Proc),
            (Ident::from("MASM").nest("getObject".into()), Proc),
            (Ident::from("MASM").nest("setObject".into()), Proc),
            (Ident::from("CacheIR").nest("allocateValueReg".into()), Proc),
            (Ident::from("CacheIR").nest("releaseValueReg".into()), Proc),
            (Ident::from("CacheIR").nest("allocateReg".into()), Proc),
            (Ident::from("CacheIR").nest("releaseReg".into()), Proc),
        ])
    };

}
