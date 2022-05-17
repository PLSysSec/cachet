// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::{self, Display};
use std::iter;
use std::mem;
use std::ops::{Deref, DerefMut};

use enum_map::EnumMap;
use fix_hidden_lifetime_bug::Captures;
use iterate::iterate;
use lazy_static::lazy_static;
use typed_index_collections::TiSlice;
use void::unreachable;

use cachet_lang::ast::{
    ArithBinOper, BinOper, CastKind, CheckKind, CompareBinOper, Ident, NegateKind, Path,
    VarParamKind, VarRefKind,
};
use cachet_lang::built_in::{BuiltInType, BuiltInVar};
use cachet_lang::flattener::{self, HasAttrs, Typed};
use cachet_util::MaybeOwned;

use crate::bpl::ast::*;
use crate::bpl::flow::{trace_entry_point, EmitSucc, FlowGraph};

mod ast;
mod flow;

// * Boogie Compilation Entry Point

#[derive(Clone, Debug)]
pub struct BplCode(Code);

impl Display for BplCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}\n{}", include_str!("bpl/prelude.bpl"), self.0)
    }
}

pub fn compile(env: &flattener::Env) -> BplCode {
    let mut compiler = Compiler::new(env);

    compiler.inject_generated_prelude_items();

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

    compiler.inject_init_proc_item();

    BplCode(compiler.items.into())
}

// * Top-Level Boogie Compiler

struct Compiler<'a> {
    env: &'a flattener::Env,
    items: Vec<Item>,
    init_stmts: Vec<Stmt>,
}

impl<'a> Compiler<'a> {
    fn new(env: &'a flattener::Env) -> Self {
        Compiler {
            env,
            items: Vec::new(),
            init_stmts: Vec::new(),
        }
    }

    fn inject_generated_prelude_items(&mut self) {
        self.items.extend(iterate![
            CommentItem::from("* Reference Tables").into(),
            ..iterate![
                UserTypeIdent::Struct,
                ..BuiltInType::ALL.map(|built_in_type| built_in_type.ident().into())
            ]
            .flat_map(generate_type_ref_items),
            CommentItem::from("... end prelude ...").into()
        ]);
    }

    fn inject_init_proc_item(&mut self) {
        self.items.push(
            ProcItem {
                ident: FnIdent::Init,
                attr: Some(InlineProcAttr { depth: 1 }.into()),
                param_vars: TypedVars::default(),
                ret_vars: TypedVars::default(),
                body: Some(Body {
                    local_vars: Vec::new(),
                    stmts: mem::replace(&mut self.init_stmts, Vec::new()),
                }),
            }
            .into(),
        );
    }

    fn compile_entry_point(
        &mut self,
        top_op_item: &flattener::CallableItem,
        top_ir_index: flattener::IrIndex,
        bottom_ir_index: flattener::IrIndex,
        flow_graph: &FlowGraph,
    ) {
        let top_ir_ident = UserIdent::from(self.env[top_ir_index].ident.value);
        let bottom_ir_ident = UserIdent::from(self.env[bottom_ir_index].ident.value);

        // The parameters of the entry point are the variable parameters of the
        // top-level op. These will be considered by the solver to have
        // arbitrary initial values.
        let param_vars: TypedVars = top_op_item
            .param_order
            .iter()
            .filter_map(|param_index| match param_index {
                flattener::ParamIndex::Var(var_param_index) => Some(var_param_index),
                _ => None,
            })
            .map(|var_param_index| {
                let var_param = &top_op_item.params[var_param_index];
                self.compile_var_param(var_param)
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

        // Start with a call to the `init` procedure.
        let mut stmts = vec![
            CallExpr {
                target: FnIdent::Init,
                arg_exprs: Vec::new(),
            }
            .into(),
        ];

        // Fill in all of the label parameter local variables with fresh labels.
        stmts.extend(label_params.iter().zip(label_param_local_vars.iter()).map(
            |(label_param, label_param_local_var)| {
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
            },
        ));

        // Set up program-counter/control-flow-tracking infrastructure.

        let pc_emit_call_paths_var_ident: VarIdent = IrMemberVarIdent {
            ir_ident: bottom_ir_ident,
            selector: IrMemberVarSelector::PcEmitCallPaths,
        }
        .into();

        let pc_var_ident: VarIdent = IrMemberVarIdent {
            ir_ident: bottom_ir_ident,
            selector: IrMemberVarSelector::Pc,
        }
        .into();

        let nil_call_path_expr = generate_call_path_expr(&CallPath::default());

        let assume_pc_emit_call_paths_uninit_stmt = CheckStmt {
            kind: CheckKind::Assume,
            attr: None,
            cond: ForAllExpr {
                vars: vec![TypedVar {
                    ident: VarIdent::Pc,
                    type_: PreludeTypeIdent::Pc.into(),
                }]
                .into(),
                expr: BinOperExpr {
                    oper: CompareBinOper::Eq.into(),
                    lhs: IndexExpr {
                        base: pc_emit_call_paths_var_ident.into(),
                        key: VarIdent::Pc.into(),
                        value: None,
                    }
                    .into(),
                    rhs: nil_call_path_expr.clone(),
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

        stmts.extend([assume_pc_emit_call_paths_uninit_stmt, init_emit_pc_stmt]);

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
        let exit_emit_call_path = &flow_graph[flow_graph.exit_emit_node_index()].call_path;
        let exit_emit_call_path_expr = generate_call_path_expr(exit_emit_call_path);
        let leading_exit_emit_stmt = CallExpr {
            target: IrMemberFnIdent {
                ir_ident: bottom_ir_ident,
                selector: IrMemberFnSelector::Emit,
            }
            .into(),
            arg_exprs: vec![
                exit_op_ctor_call_expr.clone(),
                exit_emit_call_path_expr.clone(),
            ],
        }
        .into();

        // Call function corresponding to the top-level op, passing along the
        // root call path, variable parameters, and labels from the local
        // variables declared for its label parameters.
        let top_op_var_arg_exprs = param_vars.iter().map(|param_var| param_var.ident.into());
        let top_op_label_arg_exprs = label_param_local_vars
            .iter()
            .map(|label_param_local_var| label_param_local_var.var.ident.into());
        let top_op_arg_exprs = top_op_var_arg_exprs
            .chain(top_op_label_arg_exprs)
            .chain([nil_call_path_expr, Literal::Int(0).into()])
            .collect();
        let call_top_op_fn_stmt = CallExpr {
            target: UserFnIdent::from(top_op_item.path.value).into(),
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
            arg_exprs: vec![exit_op_ctor_call_expr, exit_emit_call_path_expr],
        }
        .into();

        // Note that PC 0 is reserved for a leading exit-point which breaks from
        // control-flow within the program out to external code. We generate
        // this up front so we have a known PC for binding things like top-level
        // incoming label parameters and label out-reference parameters of
        // external functions. The first real op of the generated program is
        // emitted at PC 1, thus this is where the counter starts for the
        // interpreter phase.
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
                label_ident: EmitLabelIdent::from(emit_node.call_path.clone()).into(),
            }
            .into();

            let emit_call_path_expr = generate_call_path_expr(&emit_node.call_path);

            let assume_call_path_stmt = CheckStmt {
                kind: CheckKind::Assume,
                attr: Some(CheckAttr::Partition),
                cond: BinOperExpr {
                    oper: CompareBinOper::Eq.into(),
                    lhs: IndexExpr {
                        base: pc_emit_call_paths_var_ident.into(),
                        key: pc_var_ident.into(),
                        value: None,
                    }
                    .into(),
                    rhs: emit_call_path_expr,
                }
                .into(),
            }
            .into();

            stmts.extend([emit_label_stmt, assume_call_path_stmt]);

            if let Some(target) = emit_node.target {
                let op_item = &self.env[target];
                let op_ident = op_item.path.value.ident();
                debug_assert_eq!(op_item.path.value, bottom_ir_ident.0.nest(op_ident));

                let assign_op_stmt = AssignStmt {
                    lhs: VarIdent::Op.into(),
                    rhs: IndexExpr {
                        base: IrMemberVarIdent {
                            ir_ident: bottom_ir_ident,
                            selector: IrMemberVarSelector::Ops,
                        }
                        .into(),
                        key: pc_var_ident.into(),
                        value: None,
                    }
                    .into(),
                }
                .into();

                let call_op_fn_stmt = CallExpr {
                    target: UserFnIdent::from(op_item.path.value).into(),
                    arg_exprs: op_item
                        .param_order
                        .iter()
                        .map(|param_index| match param_index {
                            flattener::ParamIndex::Var(var_param_index) => {
                                op_item.params[var_param_index].ident.value
                            }
                            flattener::ParamIndex::Label(label_param_index) => {
                                op_item.params[label_param_index].label.ident.value
                            }
                        })
                        .map(|param_ident| {
                            CallExpr {
                                target: OpCtorFieldFnIdent {
                                    param_var_ident: UserParamVarIdent::from(param_ident).into(),
                                    ir_ident: bottom_ir_ident,
                                    op_ctor_selector: op_ident.into(),
                                }
                                .into(),
                                arg_exprs: vec![VarIdent::Op.into()],
                            }
                            .into()
                        })
                        .chain(iter::once(Literal::Int(0).into()))
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
                            EmitLabelIdent::from(
                                flow_graph[succ_emit_node_index].call_path.clone(),
                            )
                            .into()
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

        let ident = IrMemberFnIdent {
            ir_ident: top_ir_ident,
            selector: EntryPointIrMemberFnSelector::from(top_op_item.path.value.ident()).into(),
        }
        .into();

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

    fn compile_enum_item(&mut self, enum_item: &flattener::EnumItem) {
        let type_ident = UserTypeIdent::from(enum_item.ident.value);

        if BLOCKED_PATHS.contains(&enum_item.ident.value.into()) {
            return;
        }

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

        let ref_items = generate_type_ref_items(type_ident);

        self.items
            .extend(iterate![type_item, ..variant_ctor_fn_items, ..ref_items]);
    }

    fn compile_struct_item(&mut self, struct_item: &flattener::StructItem) {
        if BLOCKED_PATHS.contains(&struct_item.ident.value.into()) {
            return;
        }

        let type_ident = UserTypeIdent::from(struct_item.ident.value);

        self.items.extend([
            TypeItem {
                ident: type_ident.into(),
                attr: None,
                type_: Some(UserTypeIdent::Struct.into()),
            }
            .into(),
            // Alias `Struct^Ref` and its local-reference constructor.
            {
                let mut ref_type_item = generate_type_ref_type_item(type_ident);
                ref_type_item.attr = None;
                ref_type_item.type_ = Some(generate_ref_type_ident(UserTypeIdent::Struct).into());
                ref_type_item.into()
            },
            {
                let mut ref_ctor_item = generate_type_local_ref_ctor_item(type_ident);
                ref_ctor_item.attr = Some(FnAttr::Inline);
                ref_ctor_item.value = Some(
                    generate_local_ref_ctor_expr(
                        UserTypeIdent::Struct,
                        ParamVarIdent::LocalCallIndex.into(),
                    )
                    .into(),
                );
                ref_ctor_item.into()
            },
        ]);

        for field in &struct_item.fields {
            let field_fn_ident = TypeMemberFnIdent {
                type_ident,
                selector: TypeMemberFnSelector::Field(field.ident.value.into()),
            }
            .into();

            self.items.push(
                FnItem {
                    ident: field_fn_ident,
                    param_vars: vec![
                        TypedVar {
                            ident: ParamVarIdent::Instance.into(),
                            type_: type_ident.into(),
                        }
                        .into(),
                    ]
                    .into(),
                    attr: None,
                    ret: self.get_type_ident(field.type_).into(),
                    value: None,
                }
                .into(),
            )
        }
    }

    fn compile_ir_item(&mut self, ir_item: &flattener::IrItem) {
        // TODO(spinda): Always generate label infrastructure for IRs.

        if ir_item.emits.is_some() || BLOCKED_PATHS.contains(&ir_item.ident.value.into()) {
            return;
        }

        let ir_ident = UserIdent::from(ir_item.ident.value);

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

        let pc_global_var_item = VarItem::from(TypedVar {
            ident: IrMemberVarIdent {
                ir_ident,
                selector: IrMemberVarSelector::Pc,
            }
            .into(),
            type_: PreludeTypeIdent::Pc.into(),
        });

        let ops_global_var_item = VarItem::from(TypedVar {
            ident: IrMemberVarIdent {
                ir_ident,
                selector: IrMemberVarSelector::Ops,
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
            rhs: BinOperExpr {
                oper: ArithBinOper::Add.into(),
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

        let pc_emit_call_paths_global_var_item = VarItem::from(TypedVar {
            ident: IrMemberVarIdent {
                ir_ident,
                selector: IrMemberVarSelector::PcEmitCallPaths,
            }
            .into(),
            type_: MapType {
                key_types: vec![PreludeTypeIdent::Pc.into()],
                value_type: PreludeTypeIdent::CallPath.into(),
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
                    ident: ParamVarIdent::CallPath.into(),
                    type_: PreludeTypeIdent::CallPath.into(),
                },
            ]
            .into(),
            ret_vars: TypedVars::default(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![
                    AssignStmt {
                        lhs: IndexExpr {
                            base: pc_emit_call_paths_global_var_item.var.ident.into(),
                            key: pc_global_var_item.var.ident.into(),
                            value: None,
                        }
                        .into(),
                        rhs: ParamVarIdent::CallPath.into(),
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

        let next_label_global_var_item = VarItem::from(TypedVar {
            ident: IrMemberVarIdent {
                ir_ident,
                selector: IrMemberVarSelector::NextLabel,
            }
            .into(),
            type_: label_type_item.ident.into(),
        });

        let label_pcs_global_var_item = VarItem::from(TypedVar {
            ident: IrMemberVarIdent {
                ir_ident,
                selector: IrMemberVarSelector::LabelPcs,
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
                        rhs: BinOperExpr {
                            oper: ArithBinOper::Add.into(),
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
            pc_emit_call_paths_global_var_item.into(),
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
        if global_var_item.is_prelude() {
            return;
        }

        let var_ident = GlobalVarPathIdent::from(global_var_item.path.value);

        // Global variables get a corresponding constructor for the reference
        // datatype corresponding to the global variable's type. For mutable
        // global variables, the current value of the global variable lives in
        // the reference table.
        self.items.push(
            self.generate_ref_ctor_item(global_var_item.type_, var_ident.into())
                .into(),
        );

        if !global_var_item.is_mut {
            // Immutable global variables directly become Boogie constants.
            let type_ident = self.get_type_ident(global_var_item.type_);
            self.items.push(
                ConstItem::from(TypedVar {
                    ident: var_ident.into(),
                    type_: type_ident.into(),
                })
                .into(),
            );

            // Tack onto the init statements an assumption that the value in the
            // reference table is equal to the constant.
            self.init_stmts.push(
                CheckStmt {
                    kind: CheckKind::Assume,
                    attr: None,
                    cond: BinOperExpr {
                        oper: CompareBinOper::Eq.into(),
                        lhs: self.generate_deref_expr(
                            global_var_item.type_,
                            generate_ref_ctor_expr(type_ident, var_ident.into()).into(),
                        ),
                        rhs: var_ident.into(),
                    }
                    .into(),
                }
                .into(),
            );
        }
    }

    fn compile_callable_item(&mut self, callable_index: flattener::CallableIndex) {
        let callable_item = &self.env[callable_index];
        if callable_item.is_prelude() || BLOCKED_PATHS.contains(&callable_item.path.value) {
            return;
        }

        let fn_ident = UserFnIdent::from(callable_item.path.value);

        let mut param_vars: TypedVars = self
            .compile_params(&callable_item.params, &callable_item.param_order)
            .collect();

        if let flattener::CallableIndex::Op(_) = callable_index {
            // Define datatype constructors for interpreter ops.
            if let Some(ir_index) = callable_item.interprets {
                let ir_ident = UserIdent::from(self.env[ir_index].ident.value);
                let callable_ident = callable_item.path.value.ident();
                debug_assert_eq!(callable_item.path.value, ir_ident.0.nest(callable_ident));

                let ident = IrMemberFnIdent {
                    ir_ident,
                    selector: OpCtorIrMemberFnSelector::from(callable_ident).into(),
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
        }

        let compiled_item = match CallableRepr::for_callable(callable_item) {
            CallableRepr::Fn => FnItem {
                ident: fn_ident.into(),
                attr: None,
                param_vars,
                ret: self.get_type_ident(callable_item.type_()).into(),
                value: None,
            }
            .into(),
            CallableRepr::Proc {
                with_call_path,
                with_frame,
            } => {
                if with_call_path {
                    param_vars.push(TypedVar {
                        ident: ParamVarIdent::CallPath.into(),
                        type_: PreludeTypeIdent::CallPath.into(),
                    });
                }

                if with_frame {
                    param_vars.push(TypedVar {
                        ident: ParamVarIdent::Frame.into(),
                        type_: NativeTypeIdent::Int.into(),
                    });
                }

                let ret_vars: TypedVars = callable_item
                    .ret
                    .iter()
                    .map(|ret| TypedVar {
                        ident: VarIdent::Ret,
                        type_: self.get_type_ident(*ret).into(),
                    })
                    .collect();

                let (attr, body) = match &callable_item.body {
                    None => (
                        Some(InlineProcAttr { depth: 1 }.into()),
                        self.compile_external_fn_proc_body(
                            &callable_item,
                            fn_ident,
                            &param_vars,
                            &ret_vars,
                        ),
                    ),
                    Some(body) => (
                        None,
                        self.compile_body(callable_index, &callable_item, body),
                    ),
                };

                ProcItem {
                    ident: fn_ident.into(),
                    attr,
                    param_vars,
                    ret_vars,
                    body: Some(body),
                }
                .into()
            }
        };

        self.items.push(compiled_item);
    }

    fn compile_params<'b>(
        &'b self,
        params: &'b flattener::Params,
        param_order: &'b [flattener::ParamIndex],
    ) -> impl 'b + Iterator<Item = TypedVar> {
        param_order.iter().map(|param_index| match param_index {
            flattener::ParamIndex::Var(var_param_index) => {
                self.compile_var_param(&params[var_param_index])
            }
            flattener::ParamIndex::Label(label_param_index) => {
                self.compile_label_param(&params[label_param_index])
            }
        })
    }

    fn compile_var_param(&self, var_param: &flattener::VarParam) -> TypedVar {
        let ident = UserParamVarIdent::from(var_param.ident.value).into();

        let type_ident = self.get_type_ident(var_param.type_);
        let type_ = match var_param.kind {
            VarParamKind::Value { .. } => type_ident.into(),
            VarParamKind::Ref(_) => generate_ref_type_ident(type_ident).into(),
        };

        TypedVar { ident, type_ }
    }

    fn compile_label_param(&self, label_param: &flattener::LabelParam) -> TypedVar {
        TypedVar {
            ident: UserParamVarIdent::from(label_param.label.ident.value).into(),
            type_: IrMemberTypeIdent {
                ir_ident: self.env[label_param.label.ir].ident.value.into(),
                selector: IrMemberTypeSelector::Label,
            }
            .into(),
        }
    }

    fn compile_external_fn_proc_body(
        &mut self,
        callable_item: &flattener::CallableItem,
        fn_ident: UserFnIdent,
        param_vars: &TypedVars,
        ret_vars: &TypedVars,
    ) -> Body {
        // For external functions represented as procedures (e.g., those that
        // don't return a value, or those with out-reference parameters), set up
        // a procedure body where the outputs from the function (return value
        // and/or variable out-reference parameters) are assigned to the result
        // of delegating to unique unintepreted functions.

        // The value of each output depends on the observable (readable) inputs
        // to the function, i.e., the variable value and in-reference
        // parameters.
        let (helper_param_vars, helper_arg_exprs): (TypedVars, Vec<_>) = callable_item
            .param_order
            .iter()
            .zip(param_vars.iter())
            .filter_map(|(param_index, param_var)| match param_index {
                flattener::ParamIndex::Var(var_param_index) => {
                    Some((&callable_item.params[var_param_index], param_var))
                }
                flattener::ParamIndex::Label(_) => None,
            })
            .filter(|(var_param, _)| match var_param.kind {
                VarParamKind::Value { .. } => true,
                VarParamKind::Ref(var_ref_kind) => match var_ref_kind {
                    VarRefKind::In => true,
                    VarRefKind::Out => false,
                },
            })
            .map(|(var_param, param_var)| {
                let mut param_var = param_var.clone();
                let mut arg_expr = param_var.ident.into();

                // Parameters passed by-reference into the function are passed
                // by-value into the helper.
                if let VarParamKind::Ref(_) = var_param.kind {
                    param_var.type_ = self.get_type_ident(var_param.type_).into();
                    arg_expr = self.generate_deref_expr(var_param.type_, arg_expr);
                }

                (param_var, arg_expr)
            })
            .unzip();

        let ret_var_helpers: Vec<_> = ret_vars
            .iter()
            .map(|ret_var| (ret_var, ret_var.ident.into()))
            .collect();
        let var_out_ref_param_helpers: Vec<_> = callable_item
            .param_order
            .iter()
            .zip(param_vars.iter())
            .filter_map(|(param_index, param_var)| match param_index {
                flattener::ParamIndex::Var(var_param_index) => {
                    let var_param = &callable_item.params[var_param_index];
                    match var_param.kind {
                        VarParamKind::Ref(VarRefKind::Out) => Some((
                            param_var,
                            self.generate_deref_expr(var_param.type_, param_var.ident.into()),
                        )),
                        _ => None,
                    }
                }
                flattener::ParamIndex::Label(_) => None,
            })
            .collect();

        let helper_assign_stmts =
            iterate![..ret_var_helpers, ..var_out_ref_param_helpers].map(|(lhs_var, lhs_expr)| {
                let helper_fn_ident = HelperFnIdent {
                    fn_ident,
                    var_ident: lhs_var.ident,
                }
                .into();

                self.items.push(
                    FnItem {
                        ident: helper_fn_ident,
                        attr: None,
                        param_vars: helper_param_vars.clone(),
                        ret: lhs_var.type_.clone(),
                        value: None,
                    }
                    .into(),
                );

                AssignStmt {
                    lhs: lhs_expr,
                    rhs: CallExpr {
                        target: helper_fn_ident,
                        arg_exprs: helper_arg_exprs.clone(),
                    }
                    .into(),
                }
                .into()
            });

        // Also bind all label out-reference parameters to an exit point.

        let label_out_ref_param_bind_stmts = callable_item
            .param_order
            .iter()
            .filter_map(|param_index| match param_index {
                flattener::ParamIndex::Label(label_param_index) => Some(label_param_index),
                _ => None,
            })
            .map(|label_param_index| &callable_item.params[label_param_index])
            .filter(|label_param| label_param.is_out_ref)
            .map(|label_param| {
                let ir_index = label_param.label.ir;
                let ir_ident = self.env[ir_index].ident.value.into();

                CallExpr {
                    target: IrMemberFnIdent {
                        ir_ident,
                        selector: IrMemberFnSelector::BindExit,
                    }
                    .into(),
                    arg_exprs: vec![UserParamVarIdent::from(label_param.label.ident.value).into()],
                }
                .into()
            });

        iterate![..helper_assign_stmts, ..label_out_ref_param_bind_stmts].collect()
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
                ident: local_var.ident.value.into(),
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
                ident: local_label.ident.value.into(),
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

    fn generate_ref_ctor_item(
        &self,
        type_index: flattener::TypeIndex,
        ref_ctor_selector: RefCtorSelector,
    ) -> FnItem {
        let type_ident = self.get_type_ident(type_index);

        let ret = generate_ref_type_ident(match type_index {
            // Structs share a common `Struct` type in Boogie. The return type
            // for their datatype construtors needs to point directly to
            // `Struct^Ref`, because Boogie doesn't look through type aliases
            // here.
            flattener::TypeIndex::Struct(_) => UserTypeIdent::Struct,
            type_index => self.get_type_ident(type_index),
        })
        .into();

        FnItem {
            ident: TypeMemberFnIdent {
                type_ident,
                selector: ref_ctor_selector.into(),
            }
            .into(),
            attr: Some(FnAttr::Ctor),
            param_vars: TypedVars::default(),
            ret,
            value: None,
        }
    }

    fn generate_deref_expr(&self, type_index: flattener::TypeIndex, ref_expr: Expr) -> Expr {
        let type_ident = match type_index {
            // All structs share a common `Struct` Boogie type and a common
            // reference table.
            flattener::TypeIndex::Struct(_) => UserTypeIdent::Struct,
            type_index => self.get_type_ident(type_index),
        };

        IndexExpr {
            base: TypeMemberVarIdent {
                type_ident,
                selector: TypeMemberVarSelector::Refs,
            }
            .into(),
            key: ref_expr,
            value: None,
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

fn generate_type_ref_items(type_ident: UserTypeIdent) -> impl IntoIterator<Item = Item> {
    [
        generate_type_ref_type_item(type_ident).into(),
        generate_type_local_ref_ctor_item(type_ident).into(),
        VarItem::from(TypedVar {
            ident: TypeMemberVarIdent {
                type_ident,
                selector: TypeMemberVarSelector::Refs,
            }
            .into(),
            type_: MapType {
                key_types: vec![generate_ref_type_ident(type_ident).into()],
                value_type: type_ident.into(),
            }
            .into(),
        })
        .into(),
    ]
}

fn generate_type_ref_type_item(type_ident: UserTypeIdent) -> TypeItem {
    TypeItem {
        ident: generate_ref_type_ident(type_ident),
        attr: Some(TypeAttr::DataType),
        type_: None,
    }
}

fn generate_type_local_ref_ctor_item(type_ident: UserTypeIdent) -> FnItem {
    FnItem {
        ident: TypeMemberFnIdent {
            type_ident,
            selector: RefCtorSelector::Local.into(),
        }
        .into(),
        attr: Some(FnAttr::Ctor),
        param_vars: vec![
            TypedVar {
                ident: ParamVarIdent::Frame.into(),
                type_: NativeTypeIdent::Int.into(),
            },
            TypedVar {
                ident: ParamVarIdent::LocalCallIndex.into(),
                type_: NativeTypeIdent::Int.into(),
            },
        ]
        .into(),
        ret: generate_ref_type_ident(type_ident).into(),
        value: None,
    }
}

fn generate_call_path_expr(call_path: &CallPath) -> Expr {
    let nil_call_path_ctor_expr = CallExpr {
        target: PreludeFnIdent::NilCallPathCtor.into(),
        arg_exprs: Vec::new(),
    }
    .into();

    call_path
        .iter()
        .fold(nil_call_path_ctor_expr, |accum, call_segment| {
            CallExpr {
                target: PreludeFnIdent::ConsCallPathCtor.into(),
                arg_exprs: vec![
                    accum,
                    Literal::Int(call_segment.local_call_index.into()).into(),
                ],
            }
            .into()
        })
}

fn generate_call_path_cons_expr(local_call_index: LocalCallIndex) -> Expr {
    CallExpr {
        target: PreludeFnIdent::ConsCallPathCtor.into(),
        arg_exprs: vec![
            ParamVarIdent::CallPath.into(),
            Literal::Int(local_call_index.into()).into(),
        ],
    }
    .into()
}

const NEXT_FRAME_EXPR: BinOperExpr = BinOperExpr {
    oper: BplBinOper::Arith(ArithBinOper::Add),
    lhs: Expr::Var(VarIdent::Param(ParamVarIdent::Frame)),
    rhs: Expr::Literal(Literal::Int(1)),
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CallableRepr {
    Fn,
    Proc {
        with_call_path: bool,
        with_frame: bool,
    },
}

impl CallableRepr {
    fn for_callable(callable_item: &flattener::CallableItem) -> Self {
        if BLOCKED_PATHS.contains(&callable_item.path.value) {
            return Self::Proc {
                with_call_path: false,
                with_frame: true,
            };
        };

        let has_ref_params =
            callable_item
                .param_order
                .iter()
                .any(|param_index| match param_index {
                    flattener::ParamIndex::Var(var_param_index) => {
                        match callable_item.params[var_param_index].kind {
                            VarParamKind::Value { .. } => false,
                            VarParamKind::Ref(_) => true,
                        }
                    }
                    flattener::ParamIndex::Label(label_param_index) => {
                        callable_item.params[label_param_index].is_out_ref
                    }
                });

        match (has_ref_params, callable_item.ret, &callable_item.body) {
            (false, Some(_), None) => Self::Fn,
            _ => Self::Proc {
                with_call_path: callable_item.emits.is_some(),
                with_frame: callable_item.body.is_some(),
            },
        }
    }
}

// * Scoped Boogie Compiler

struct ScopedCompiler<'a, 'b> {
    compiler: &'b mut Compiler<'a>,
    callable_index: flattener::CallableIndex,
    callable_item: &'b flattener::CallableItem,
    callable_locals: &'b flattener::Locals,
    local_vars: &'b mut Vec<LocalVar>,
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
            next_synthetic_var_indexes: MaybeOwned::Borrowed(&mut self.next_synthetic_var_indexes),
            stmts: Vec::new(),
        }
    }

    fn compile_args(&mut self, args: &[flattener::Arg]) -> (Vec<Expr>, Vec<Stmt>) {
        let mut var_tmp_refs = HashMap::new();
        let mut post_call_stmts = Vec::new();

        let arg_exprs = args
            .iter()
            .map(|arg| match arg {
                flattener::Arg::Expr(pure_expr) => self.compile_expr_arg(pure_expr),
                flattener::Arg::VarRef(var_ref_arg) => {
                    self.compile_var_ref_arg(var_ref_arg, &mut var_tmp_refs, &mut post_call_stmts)
                }
                flattener::Arg::Label(label_arg) => self.compile_label_arg(label_arg),
            })
            .collect();

        (arg_exprs, post_call_stmts)
    }

    fn compile_expr_arg(&mut self, pure_expr: &flattener::PureExpr) -> Expr {
        self.compile_pure_expr(pure_expr)
    }

    fn compile_var_ref_arg(
        &mut self,
        var_ref_arg: &flattener::VarRefArg,
        var_tmp_refs: &mut HashMap<flattener::VarIndex, TmpRefInfo>,
        post_call_stmts: &mut Vec<Stmt>,
    ) -> Expr {
        let var_index = match &var_ref_arg.var_ref {
            flattener::VarRef::Free(free_var_ref) => free_var_ref.var.value,
            // TODO(spinda): Eliminate these in the normalizer.
            flattener::VarRef::FreshOut(_) => unimplemented!(),
        };

        let var_ref_kind = var_ref_arg.var_ref.kind();

        match var_index {
            // Built-ins, enum variants, and global variables have corresponding
            // reference datatype constructors which we can use here.
            flattener::VarIndex::BuiltIn(built_in_var) => {
                debug_assert_ne!(var_ref_kind, VarRefKind::Out);
                return generate_ref_ctor_expr(
                    self.get_type_ident(built_in_var.type_()),
                    GlobalVarPathIdent::from(built_in_var.path()).into(),
                )
                .into();
            }
            flattener::VarIndex::EnumVariant(index) => {
                debug_assert_ne!(var_ref_kind, VarRefKind::Out);
                let enum_item = &self.env[index.enum_index];
                let enum_ident = enum_item.ident.value.into();
                let variant_ident = enum_item.variants[index.variant_index].value.ident();
                return generate_ref_ctor_expr(
                    enum_ident,
                    EnumVariantRefCtorSelector {
                        enum_ident,
                        variant_selector: variant_ident.into(),
                    }
                    .into(),
                )
                .into();
            }
            flattener::VarIndex::Global(global_var_index) => {
                let var_item = &self.env[global_var_index];
                let var_ident = GlobalVarPathIdent::from(var_item.path.value);
                return generate_ref_ctor_expr(
                    self.get_type_ident(var_item.type_),
                    var_ident.into(),
                )
                .into();
            }
            // Reference parameters can be forwarded along directly, as
            // their Boogie type is already an index into the reference
            // table.
            flattener::VarIndex::Param(var_param_index) => {
                let var_param = &self.callable_item.params[var_param_index];
                if let VarParamKind::Ref(param_var_ref_kind) = var_param.kind {
                    debug_assert_eq!(var_ref_kind, param_var_ref_kind);
                    return UserParamVarIdent::from(var_param.ident.value).into();
                }
            }
            flattener::VarIndex::Local(_) => (),
        };

        // For value parameters and local variable reference parameters,
        // allocate a temporary spot in the reference table, sharded by the
        // current stack frame.

        // Allocate at most one temporary reference index per unique
        // variable, so changes made through duplicate references to the
        // same variable are reflected when looking through either
        // reference.
        let next_ref_index = var_tmp_refs.len();
        let tmp_ref_info = var_tmp_refs.entry(var_index).or_insert_with(|| TmpRefInfo {
            ref_index: next_ref_index,
            used_for_in_ref: false,
            used_for_out_ref: false,
        });
        let tmp_ref_expr: Expr = generate_local_ref_ctor_expr(
            self.get_type_ident(var_ref_arg.type_),
            Literal::Int(tmp_ref_info.ref_index).into(),
        )
        .into();

        match var_ref_kind {
            VarRefKind::In => {
                if !tmp_ref_info.used_for_in_ref {
                    // Before the call, populate the reference table slot with
                    // the value of the variable referenced by the argument.
                    self.stmts.push(
                        AssignStmt {
                            lhs: self.generate_deref_expr(var_ref_arg.type_, tmp_ref_expr.clone()),
                            rhs: self.compile_var_read(var_index),
                        }
                        .into(),
                    );
                    tmp_ref_info.used_for_in_ref = true;
                }
            }
            VarRefKind::Out => {
                if !tmp_ref_info.used_for_out_ref {
                    // After the call, retrieve the new value from the reference
                    // table and write it to the variable referenced by the
                    // argument.
                    post_call_stmts.push(
                        AssignStmt {
                            lhs: self.compile_var_access(var_index),
                            rhs: self.generate_deref_expr(var_ref_arg.type_, tmp_ref_expr.clone()),
                        }
                        .into(),
                    );
                    tmp_ref_info.used_for_out_ref = true;
                }
            }
        }

        tmp_ref_expr
    }

    fn compile_label_arg(&mut self, label_arg: &flattener::LabelArg) -> Expr {
        self.compile_label_arg_raw(label_arg.label)
    }

    fn compile_label_arg_raw(&mut self, label_index: flattener::LabelIndex) -> Expr {
        // As with compiling label parameters and out-reference parameters, we
        // compile label arguments and label out-reference arguments the same
        // way when generating Boogie.
        match label_index {
            flattener::LabelIndex::Param(label_param_index) => UserParamVarIdent::from(
                self.callable_item.params[label_param_index]
                    .label
                    .ident
                    .value,
            )
            .into(),
            flattener::LabelIndex::Local(local_label_index) => LocalLabelVarIdent {
                ident: self.callable_locals[local_label_index].ident.value.into(),
                index: local_label_index,
            }
            .into(),
        }
    }

    fn compile_invocation(
        &mut self,
        invoke_expr: &flattener::InvokeExpr,
        generate_ret_var_ident: impl FnOnce(&mut Self, flattener::TypeIndex) -> VarIdent,
    ) -> Expr {
        let callable_item = &self.env[invoke_expr.call.target];

        let target = UserFnIdent::from(callable_item.path.value).into();
        let (arg_exprs, post_call_stmts) = self.compile_args(&invoke_expr.call.args);
        let mut call_expr = CallExpr { target, arg_exprs };

        match CallableRepr::for_callable(callable_item) {
            CallableRepr::Fn => {
                debug_assert!(post_call_stmts.is_empty());
                call_expr.into()
            }
            CallableRepr::Proc {
                with_call_path,
                with_frame,
            } => {
                if with_call_path {
                    call_expr.arg_exprs.push(generate_call_path_cons_expr(
                        invoke_expr.call.local_call_index,
                    ));
                }

                if with_frame {
                    call_expr.arg_exprs.push(NEXT_FRAME_EXPR.into());
                }

                let mut ret_var_idents = Vec::new();
                let expr = match callable_item.ret {
                    Some(ret) => {
                        let ret_var_ident = generate_ret_var_ident(self, ret);
                        ret_var_idents.push(ret_var_ident);
                        ret_var_ident.into()
                    }
                    None => self.compile_var_read(BuiltInVar::Unit.into()),
                };

                self.stmts.extend(iterate![
                    CallStmt {
                        call: call_expr,
                        ret_var_idents,
                    }
                    .into(),
                    ..post_call_stmts,
                ]);

                expr
            }
        }
    }

    fn compile_invocation_with_ret_var(
        &mut self,
        invoke_expr: &flattener::InvokeExpr,
        ret_var_ident: VarIdent,
    ) -> Option<Expr> {
        let expr = self.compile_invocation(invoke_expr, |_, _| ret_var_ident);
        match expr {
            Expr::Var(var_ident) if var_ident == ret_var_ident => None,
            expr => Some(expr),
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
            ident: self.callable_locals[let_stmt.lhs].ident.value.into(),
            index: let_stmt.lhs,
        }
        .into();
        self.compile_assign_stmt_impl(lhs, &let_stmt.rhs);
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
                ident: local_label.ident.value.into(),
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

        let arg_exprs = vec![self.compile_label_arg_raw(goto_stmt.label)];

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

        let arg_exprs = vec![self.compile_label_arg_raw(bind_stmt.label)];

        self.stmts.extend([CallExpr { target, arg_exprs }.into()]);
    }

    fn compile_emit_stmt(&mut self, emit_stmt: &flattener::EmitStmt) {
        let ir_item = &self.env[emit_stmt.ir];
        let op_item = &self.env[emit_stmt.call.target];

        let (mut op_arg_exprs, post_call_stmts) = self.compile_args(&emit_stmt.call.args);
        debug_assert!(post_call_stmts.is_empty());

        let emit_call_path_expr = generate_call_path_cons_expr(emit_stmt.call.local_call_index);

        match ir_item.emits {
            Some(_) => {
                // If the IR of the emitted op describes a compiler, call
                // directly into the other op.

                debug_assert_eq!(
                    CallableRepr::for_callable(op_item),
                    CallableRepr::Proc {
                        with_call_path: true,
                        with_frame: true,
                    }
                );
                op_arg_exprs.extend([emit_call_path_expr, NEXT_FRAME_EXPR.into()]);

                self.stmts.push(
                    CallExpr {
                        target: UserFnIdent::from(op_item.path.value).into(),
                        arg_exprs: op_arg_exprs,
                    }
                    .into(),
                );
            }
            None => {
                // Otherwise, build the op via the corresponding datatype
                // constructor and pass it to the emit helper function.

                let ir_ident = UserIdent::from(ir_item.ident.value);
                let op_ident = UserIdent::from(op_item.path.value.ident());
                debug_assert_eq!(op_item.path.value, ir_ident.0.nest(op_ident.0));

                let op_ctor_target = IrMemberFnIdent {
                    ir_ident,
                    selector: OpCtorIrMemberFnSelector::from(op_ident).into(),
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
                let emit_arg_exprs = vec![op_ctor_call_expr, emit_call_path_expr];
                self.stmts.push(
                    CallExpr {
                        target: emit_target,
                        arg_exprs: emit_arg_exprs,
                    }
                    .into(),
                );
            }
        }
    }

    fn compile_invoke_stmt(&mut self, invoke_stmt: &flattener::InvokeStmt) {
        // Compile an invoke statement the same way we would as an invoke
        // expression, but discard the resulting expression. If the invocation
        // target is represented as a procedure, a call statement will be
        // inserted, and the expression will be the unused return variable. If
        // the invocation target is represented as a function, the expression
        // will be a function call, which can be safely dropped as it has no
        // side effects.
        self.compile_invoke_expr(invoke_stmt);
    }

    fn compile_assign_stmt(&mut self, assign_stmt: &flattener::AssignStmt) {
        let lhs = self.compile_var_access(assign_stmt.lhs);
        self.compile_assign_stmt_impl(lhs, &assign_stmt.rhs);
    }

    fn compile_assign_stmt_impl(&mut self, lhs: Expr, rhs: &flattener::Expr) {
        let rhs = match (&lhs, rhs) {
            // If the right-hand side is an invoke expression, and the left-hand
            // side compiles down to a plain variable identifier, then we can
            // try to have the generated call assign directly to the left-hand
            // side.
            (Expr::Var(lhs_var_ident), flattener::Expr::Invoke(rhs_invoke_expr)) => {
                self.compile_invocation_with_ret_var(rhs_invoke_expr, *lhs_var_ident)
            }
            (_, rhs) => Some(self.compile_expr(rhs)),
        };

        if let Some(rhs) = rhs {
            self.stmts.push(AssignStmt { lhs, rhs }.into());
        }
    }

    fn compile_ret_stmt(&mut self, ret_stmt: &flattener::RetStmt) {
        let assign_stmt = ret_stmt.value.as_ref().and_then(|value| {
            debug_assert_ne!(value.type_(), BuiltInType::Unit.into());

            let rhs = match value {
                // If the return value is an invoke expression, then we can try
                // to have the generated call assign directly to the containing
                // function's return variable.
                flattener::Expr::Invoke(invoke_expr) => {
                    self.compile_invocation_with_ret_var(invoke_expr, VarIdent::Ret)?
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

        // If we're inside the implementation of an interpreted op, insert
        // a call to step the interpreter PC before returning.
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
            flattener::Expr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr),
            flattener::Expr::BinOper(bin_oper_expr) => {
                self.compile_bin_oper_expr(&bin_oper_expr).into()
            }
        }
    }

    fn compile_pure_expr(&mut self, pure_expr: &flattener::PureExpr) -> Expr {
        match pure_expr {
            flattener::PureExpr::Block(void, _) => unreachable(*void),
            flattener::PureExpr::Literal(literal) => compile_literal(*literal).into(),
            flattener::PureExpr::Var(var_expr) => self.compile_var_expr(var_expr),
            flattener::PureExpr::FieldAccess(field_access_expr) => {
                self.compile_field_access_expr(&field_access_expr).into()
            }
            flattener::PureExpr::Negate(negate_expr) => self.compile_negate_expr(&negate_expr),
            flattener::PureExpr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr),
            flattener::PureExpr::BinOper(bin_oper_expr) => {
                self.compile_bin_oper_expr(&bin_oper_expr).into()
            }
        }
    }

    fn compile_var_expr(&self, var_expr: &flattener::VarExpr) -> Expr {
        self.compile_var_read(var_expr.var)
    }

    fn compile_var_read(&self, var_index: flattener::VarIndex) -> Expr {
        match var_index {
            // Inline `true` and `false` constants.
            flattener::VarIndex::BuiltIn(
                built_in_var @ (BuiltInVar::True | BuiltInVar::False),
            ) => (built_in_var == BuiltInVar::True).into(),
            var_index => self.compile_var_access(var_index),
        }
    }

    fn compile_var_access(&self, var_index: flattener::VarIndex) -> Expr {
        match var_index {
            flattener::VarIndex::BuiltIn(built_in_var) => {
                ConstIdent::from(GlobalVarPathIdent::from(built_in_var.ident())).into()
            }
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
            flattener::VarIndex::Global(global_var_index) => {
                let var_item = &self.env[global_var_index];
                let var_ident = GlobalVarPathIdent::from(var_item.path.value);

                if var_item.is_mut {
                    // Mutable global variables are accessed through the
                    // reference table for their type.
                    self.generate_deref_expr(
                        var_item.type_,
                        generate_ref_ctor_expr(
                            self.get_type_ident(var_item.type_),
                            var_ident.into(),
                        )
                        .into(),
                    )
                } else {
                    // Immutable global vars are plain constants.
                    var_ident.into()
                }
            }
            flattener::VarIndex::Param(var_param_index) => {
                let var_param = &self.callable_item.params[var_param_index];
                let mut var_access_expr = UserParamVarIdent::from(var_param.ident.value).into();
                if let VarParamKind::Ref(_) = var_param.kind {
                    var_access_expr = self.generate_deref_expr(var_param.type_, var_access_expr);
                }
                var_access_expr
            }
            flattener::VarIndex::Local(local_var_index) => {
                let local_var = &self.callable_locals[local_var_index];
                LocalVarIdent {
                    ident: local_var.ident.value.into(),
                    index: local_var_index,
                }
                .into()
            }
        }
    }

    fn compile_invoke_expr(&mut self, invoke_expr: &flattener::InvokeExpr) -> Expr {
        self.compile_invocation(invoke_expr, |scoped_compiler, ret| {
            scoped_compiler
                .take_synthetic_var_ident(
                    SyntheticVarKind::Ret,
                    scoped_compiler.get_type_ident(ret).into(),
                )
                .into()
        })
    }

    fn compile_field_access_expr<E: CompileExpr + Typed>(
        &mut self,
        field_access_expr: &flattener::FieldAccessExpr<E>,
    ) -> CallExpr {
        let struct_item = &self.env[field_access_expr.field.struct_index];
        let field = &struct_item.fields[field_access_expr.field.field_index];
        let field_fn_ident = TypeMemberFnIdent {
            type_ident: struct_item.ident.value.into(),
            selector: TypeMemberFnSelector::Field(field.ident.value.into()),
        }
        .into();

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
            NegateKind::Arith => CallExpr {
                target: TypeMemberFnIdent {
                    type_ident: self.get_type_ident(negate_expr.type_()),
                    selector: TypeMemberFnSelector::Negate,
                }
                .into(),
                arg_exprs: vec![expr],
            }
            .into(),
            kind => NegateExpr { kind, expr }.into(),
        }
    }

    fn compile_cast_expr<E: CompileExpr + Typed>(
        &mut self,
        cast_expr: &flattener::CastExpr<E>,
    ) -> Expr {
        let expr = cast_expr.expr.compile(self);

        let source_type_index = cast_expr.expr.type_();
        let target_type_index = cast_expr.type_;

        // All structs are represented with a common `Struct` type in Boogie. If
        // the source and target types are both structs, elide the cast.
        match (source_type_index, target_type_index) {
            (flattener::TypeIndex::Struct(_), flattener::TypeIndex::Struct(_)) => return expr,
            _ => (),
        }

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
        .into()
    }

    fn compile_bin_oper_expr(&mut self, bin_oper_expr: &flattener::BinOperExpr) -> Expr {
        let lhs = self.compile_pure_expr(&bin_oper_expr.lhs);
        let rhs = self.compile_pure_expr(&bin_oper_expr.rhs);

        let selector = TypeMemberFnSelector::BinOper(match bin_oper_expr.oper {
            BinOper::Arith(arith_bin_oper) => arith_bin_oper.into(),
            BinOper::Bitwise(bitwise_bin_oper) => bitwise_bin_oper.into(),
            BinOper::Compare(compare_bin_oper) => match compare_bin_oper {
                CompareBinOper::Numeric(numeric_compare_bin_oper) => {
                    numeric_compare_bin_oper.into()
                }
                // TODO(spinda): Use helper functions for floating-point.
                CompareBinOper::Eq | CompareBinOper::Neq => {
                    return BinOperExpr {
                        oper: compare_bin_oper.into(),
                        lhs,
                        rhs,
                    }
                    .into();
                }
            },
            BinOper::Logical(logical_bin_oper) => {
                return BinOperExpr {
                    oper: logical_bin_oper.into(),
                    lhs,
                    rhs,
                }
                .into();
            }
        });

        // Note that the type prefix for the helper function is the *operand*
        // type, not the output type, so we take the type of the left-hand side
        // rather than the operator expression itself.
        let lhs_type = bin_oper_expr.lhs.type_();
        let rhs_type = bin_oper_expr.rhs.type_();
        debug_assert_eq!(lhs_type, rhs_type);
        let type_ident = self.get_type_ident(lhs_type);

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

    fn take_synthetic_var_ident(
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
}

struct TmpRefInfo {
    ref_index: usize,
    used_for_in_ref: bool,
    used_for_out_ref: bool,
}

fn compile_literal(literal: flattener::Literal) -> Literal {
    match literal {
        flattener::Literal::Int32(n) => Literal::Bv32(n as u32),
        flattener::Literal::Int64(n) => Literal::Bv64(n as u64),
        flattener::Literal::UInt16(n) => Literal::Bv16(n),
        flattener::Literal::Double(n) => Literal::Float64(n),
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

// * Reference Helpers

fn generate_ref_type_ident(type_ident: UserTypeIdent) -> TypeIdent {
    TypeMemberTypeIdent {
        type_ident,
        selector: TypeMemberTypeSelector::Ref,
    }
    .into()
}

fn generate_ref_ctor_expr(
    type_ident: UserTypeIdent,
    ref_ctor_selector: RefCtorSelector,
) -> CallExpr {
    CallExpr {
        target: TypeMemberFnIdent {
            type_ident,
            selector: ref_ctor_selector.into(),
        }
        .into(),
        arg_exprs: Vec::new(),
    }
}

fn generate_local_ref_ctor_expr(
    type_ident: UserTypeIdent,
    local_call_index_expr: Expr,
) -> CallExpr {
    let mut ref_ctor_expr = generate_ref_ctor_expr(type_ident, RefCtorSelector::Local);
    ref_ctor_expr
        .arg_exprs
        .extend([ParamVarIdent::Frame.into(), local_call_index_expr]);
    ref_ctor_expr
}

// * Hacks

lazy_static! {
    // Temporary hack to implement a few functions in Boogie, until we implement
    // support for conditional compilation and polymorphism in Cachet.
    static ref BLOCKED_PATHS: HashSet<Path> = {
        HashSet::from([
            Ident::from("ValueReg").into(),
            Ident::from("MASM").nest("getValue".into()),
            Ident::from("MASM").nest("setValue".into()),
            Ident::from("MASM").nest("getInt32".into()),
            Ident::from("MASM").nest("setInt32".into()),
            Ident::from("MASM").nest("getBool".into()),
            Ident::from("MASM").nest("setBool".into()),
            Ident::from("MASM").nest("getObject".into()),
            Ident::from("MASM").nest("setObject".into()),
            Ident::from("CacheIR").nest("allocateValueReg".into()),
            Ident::from("CacheIR").nest("releaseValueReg".into()),
            Ident::from("CacheIR").nest("allocateReg".into()),
            Ident::from("CacheIR").nest("releaseReg".into()),
        ])
    };
}
