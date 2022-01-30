// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Deref, DerefMut};

use derive_more::Display;
use enum_map::EnumMap;
use fix_hidden_lifetime_bug::Captures;
use iterate::iterate;
use typed_index_collections::TiSlice;
use void::unreachable;

use cachet_lang::ast::{BuiltInVar, CastKind, CompareKind, Ident, Path, Spanned};
use cachet_lang::flattener::{self, Typed};
use cachet_util::MaybeOwned;

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

    BplCode(compiler.items.into())
}

struct Compiler<'a> {
    env: &'a flattener::Env,
    next_emit_id: usize,
    items: Vec<Item>,
}

impl<'a> Compiler<'a> {
    fn new(env: &'a flattener::Env) -> Self {
        Compiler {
            env,
            next_emit_id: 0,
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
                selector: IrMemberFnSelector::ExternalOpCtor,
            }
            .into(),
            attr: Some(FnAttr::Ctor),
            param_vars: TypedVars::default(),
            ret: op_type_item.ident.into(),
            value: None,
        };

        let pc_type_item = TypeItem {
            ident: IrMemberTypeIdent {
                ir_ident,
                selector: IrMemberTypeSelector::Pc,
            }
            .into(),
            attr: None,
            type_: Some(TypeIdent::Int.into()),
        };

        let pc_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::Pc,
            }
            .into(),
            type_: pc_type_item.ident.into(),
        });

        let ops_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::Ops,
            }
            .into(),
            type_: MapType {
                key_types: vec![pc_type_item.ident.into()],
                value_type: op_type_item.ident.into(),
            }
            .into(),
        });

        let incr_pc_stmt: Stmt = AssignStmt {
            lhs: pc_global_var_item.var.ident.into(),
            rhs: ArithExpr {
                kind: ArithKind::Add,
                lhs: pc_global_var_item.var.ident.into(),
                rhs: 1.into(),
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

        let emit_id_type_item = TypeItem {
            ident: IrMemberTypeIdent {
                ir_ident,
                selector: IrMemberTypeSelector::EmitId,
            }
            .into(),
            attr: None,
            type_: Some(TypeIdent::Int.into()),
        };

        let pc_emit_ids_global_var_item = GlobalVarItem::from(TypedVar {
            ident: IrMemberGlobalVarIdent {
                ir_ident,
                selector: IrMemberGlobalVarSelector::PcEmitIds,
            }
            .into(),
            type_: MapType {
                key_types: vec![pc_type_item.ident.into()],
                value_type: emit_id_type_item.ident.into(),
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
                    ident: ParamVarIdent::EmitId.into(),
                    type_: emit_id_type_item.ident.into(),
                },
                TypedVar {
                    ident: ParamVarIdent::Op.into(),
                    type_: op_type_item.ident.into(),
                },
            ]
            .into(),
            ret_vars: TypedVars::default(),
            body: Some(Body {
                local_vars: Vec::new(),
                stmts: vec![
                    AssignStmt {
                        lhs: IndexExpr {
                            base: pc_emit_ids_global_var_item.var.ident.into(),
                            key: pc_global_var_item.var.ident.into(),
                            value: None,
                        }
                        .into(),
                        rhs: ParamVarIdent::EmitId.into(),
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
            type_: Some(TypeIdent::Int.into()),
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
                value_type: pc_type_item.ident.into(),
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
                            rhs: 1.into(),
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
            pc_type_item.into(),
            pc_global_var_item.into(),
            ops_global_var_item.into(),
            step_proc_item.into(),
            emit_id_type_item.into(),
            pc_emit_ids_global_var_item.into(),
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

        let (param_vars, mut ret_vars) =
            self.compile_params(&callable_item.params, &callable_item.param_order);

        // Define datatype constructors for interpreter ops.
        let op_interprets_ir_ident = match (callable_index, callable_item.interprets) {
            (flattener::CallableIndex::Op(_), Some(ir_index)) => {
                let ir_ident = self.env[ir_index].ident.value.into();
                let ident = IrMemberFnIdent {
                    ir_ident,
                    selector: UserOpCtorIrMemberFnSelector::from(callable_ident).into(),
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

                Some(ir_ident)
            }
            _ => None,
        };

        let body = callable_item
            .body
            .as_ref()
            .map(|body| self.compile_body(op_interprets_ir_ident, &callable_item.params, body));

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
                    param_vars.push(self.compile_label_param(params[label_param_index].value));
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

    fn compile_label_param(&self, label_param_ident: Ident) -> TypedVar {
        TypedVar {
            ident: UserParamVarIdent::from(label_param_ident).into(),
            type_: IrMemberTypeIdent {
                // TODO(spinda): Give the labels IR annotations so this doesn't
                // have to be hardcoded.
                ir_ident: Ident::from("MASM").into(),
                selector: IrMemberTypeSelector::Label,
            }
            .into(),
        }
    }

    fn compile_body(
        &mut self,
        op_interprets_ir_ident: Option<IrIdent>,
        params: &flattener::Params,
        body: &flattener::Body,
    ) -> Body {
        let mut local_vars = self.compile_locals(&body.locals).collect();

        let mut scoped_compiler = ScopedCompiler::new(
            self,
            op_interprets_ir_ident,
            params,
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
        local_labels: &'b TiSlice<LocalLabelIndex, Spanned<Ident>>,
    ) -> impl 'b + Iterator<Item = LocalVar> + Captures<'a> {
        local_labels
            .iter_enumerated()
            .map(|(local_label_index, local_label_ident)| {
                self.compile_local_label(local_label_index, local_label_ident.value)
            })
    }

    fn compile_local_label(
        &self,
        local_label_index: LocalLabelIndex,
        local_label_ident: Ident,
    ) -> LocalVar {
        TypedVar {
            ident: LocalLabelVarIdent {
                ident: local_label_ident,
                index: local_label_index,
            }
            .into(),
            type_: IrMemberTypeIdent {
                // TODO(spinda): Give the labels IR annotations so this doesn't
                // have to be hardcoded.
                ir_ident: Ident::from("MASM").into(),
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

enum CallableRepr {
    Fn,
    Proc,
}

impl CallableRepr {
    fn for_callable(callable_item: &flattener::CallableItem) -> Self {
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
    op_interprets_ir_ident: Option<IrIdent>,
    params: &'b flattener::Params,
    locals: &'b flattener::Locals,
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
        op_interprets_ir_ident: Option<IrIdent>,
        params: &'b flattener::Params,
        locals: &'b flattener::Locals,
        local_vars: &'b mut Vec<LocalVar>,
    ) -> Self {
        ScopedCompiler {
            compiler,
            op_interprets_ir_ident,
            params,
            locals,
            local_vars,
            next_synthetic_var_indexes: MaybeOwned::Owned(EnumMap::default()),
            stmts: Vec::new(),
        }
    }

    fn recurse<'c>(&'c mut self) -> ScopedCompiler<'a, 'c> {
        ScopedCompiler {
            compiler: self.compiler,
            op_interprets_ir_ident: self.op_interprets_ir_ident,
            params: self.params,
            locals: self.locals,
            local_vars: self.local_vars,
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
                UserParamVarIdent::from(self.params[label_param_index].value).into()
            }
            flattener::LabelIndex::Local(local_label_index) => LocalLabelVarIdent {
                ident: self.locals[local_label_index].value,
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
            flattener::Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt),
            flattener::Stmt::Check(check_stmt) => self.compile_check_stmt(check_stmt),
            flattener::Stmt::Goto(goto_stmt) => self.compile_goto_stmt(goto_stmt),
            flattener::Stmt::Emit(call) => self.compile_emit_stmt(call),
            flattener::Stmt::Block(void, _) => unreachable(*void),
            flattener::Stmt::Invoke(invoke_stmt) => self.compile_invoke_stmt(invoke_stmt),
            flattener::Stmt::Assign(assign_stmt) => self.compile_assign_stmt(assign_stmt),
            flattener::Stmt::Ret(ret_stmt) => self.compile_ret_stmt(ret_stmt),
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: &flattener::LetStmt) {
        let lhs = LocalVarIdent {
            ident: self.locals[let_stmt.lhs].ident.value,
            index: let_stmt.lhs,
        }
        .into();

        let rhs = self.compile_expr(&let_stmt.rhs);

        self.stmts.push(AssignStmt { lhs, rhs }.into());
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

    fn compile_emit_stmt(&mut self, emit_stmt: &flattener::EmitStmt) {
        let ir_item = &self.env[emit_stmt.ir];

        let ir_ident = IrIdent::from(ir_item.ident.value);
        let op_ident = self.env[emit_stmt.call.target].path.value.ident();

        let (op_arg_exprs, _) = self.compile_args(&emit_stmt.call.args);

        match ir_item.emits {
            Some(_) => {
                // If the IR of the emitted op describes a compiler, call
                // directly into the other op.

                let target = UserFnIdent {
                    parent_ident: Some(ir_ident.ident),
                    fn_ident: op_ident,
                }
                .into();

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
                    selector: UserOpCtorIrMemberFnSelector::from(op_ident).into(),
                }
                .into();
                let op_ctor_call_expr = CallExpr {
                    target: op_ctor_target,
                    arg_exprs: op_arg_exprs,
                }
                .into();

                let emit_id = self.next_emit_id;
                self.next_emit_id += 1;

                let emit_target = IrMemberFnIdent {
                    ir_ident,
                    selector: IrMemberFnSelector::Emit,
                }
                .into();
                let emit_args = vec![emit_id.into(), op_ctor_call_expr];
                let emit_call_expr = CallExpr {
                    target: emit_target,
                    arg_exprs: emit_args,
                };

                self.stmts.push(emit_call_expr.into());
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

        let step_call = self.op_interprets_ir_ident.map(|ir_ident| {
            CallExpr {
                target: IrMemberFnIdent {
                    ir_ident,
                    selector: IrMemberFnSelector::Step,
                }
                .into(),
                arg_exprs: Vec::new(),
            }
            .into()
        });

        self.stmts
            .extend(iterate![..assign_stmt, ..step_call, Stmt::Ret]);
    }

    fn compile_expr(&mut self, expr: &flattener::Expr) -> Expr {
        match expr {
            flattener::Expr::Block(void, _) => unreachable(*void),
            flattener::Expr::Var(var_expr) => self.compile_var_expr(var_expr),
            flattener::Expr::Invoke(invoke_expr) => self.compile_invoke_expr(invoke_expr),
            flattener::Expr::Negate(negate_expr) => self.compile_negate_expr(&negate_expr).into(),
            flattener::Expr::Cast(cast_expr) => self.compile_cast_expr(&cast_expr).into(),
            flattener::Expr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).into()
            }
        }
    }

    fn compile_atom_expr(&mut self, atom_expr: &flattener::AtomExpr) -> Expr {
        match atom_expr {
            flattener::AtomExpr::Var(var_expr) => self.compile_var_expr(var_expr),
            flattener::AtomExpr::Negate(negate_expr) => {
                self.compile_negate_expr(&negate_expr).into()
            }
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
    ) -> NegateExpr {
        let expr = negate_expr.expr.compile(self);

        NegateExpr {
            kind: negate_expr.kind,
            expr,
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
                let var_param = &self.params[var_param_index];
                Some(UserParamVarIdent::from(var_param.ident.value).into())
            }
            flattener::VarIndex::OutParam(out_var_param_index) => {
                let out_var_param = &self.params[out_var_param_index];
                Some(UserParamVarIdent::from(out_var_param.ident.value).into())
            }
            flattener::VarIndex::Local(local_var_index) => {
                let local_var = &self.locals[local_var_index];
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
