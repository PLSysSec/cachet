// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::Deref;

use derive_more::Display;
use enumset::EnumSet;
use fix_hidden_lifetime_bug::Captures;
use iterate::iterate;
use lazy_static::lazy_static;
use typed_index_collections::{TiSlice, TiVec};

use cachet_lang::ast::{Ident, Spanned};
use cachet_lang::normalizer::{self, Typed};

use crate::cpp::ast::*;

mod ast;

#[derive(Clone, Debug)]
pub struct CompilerOutput {
    pub decls: CppCode,
    pub defs: CppCode,
}

#[derive(Clone, Debug, Display)]
pub struct CppCode(Code);

// TODO(spinda): Incorporate `env.decl_order`.

pub fn compile(env: &normalizer::Env) -> CompilerOutput {
    let mut compiler = Compiler::new(env);

    for enum_index in env.enum_items.keys() {
        compiler.compile_enum_item(enum_index);
    }

    for struct_index in env.struct_items.keys() {
        compiler.compile_struct_item(struct_index);
    }

    for global_var_index in env.global_var_items.keys() {
        compiler.compile_global_var_item(global_var_index);
    }

    for fn_index in env.fn_items.keys() {
        compiler.compile_fn_item(fn_index);
    }

    for op_index in env.op_items.keys() {
        compiler.compile_op_item(op_index);
    }

    let decls = iterate![
        CommentItem {
            text: "External forward declarations.".to_owned(),
        }
        .into(),
        ..compiler.external_decls,
        CommentItem {
            text: "Internal forward declarations.".to_owned(),
        }
        .into(),
        ..compiler.internal_decls,
    ]
    .collect();

    let defs = iterate![
        CommentItem {
            text: "Internal definitions.".to_owned(),
        }
        .into(),
        ..compiler.internal_defs,
    ]
    .collect();

    CompilerOutput {
        decls: CppCode(decls),
        defs: CppCode(defs),
    }
}

#[derive(Clone)]
struct ItemBuckets {
    built_in_type_namespace_items: normalizer::BuiltInTypeMap<NamespaceItem>,
    enum_namespace_items: TiVec<normalizer::EnumIndex, NamespaceItem>,
    struct_namespace_items: TiVec<normalizer::StructIndex, NamespaceItem>,
    ir_namespace_items: TiVec<normalizer::IrIndex, NamespaceItem>,
    top_level_items: Vec<Item>,
}

impl ItemBuckets {
    fn new(
        enum_items: &TiSlice<normalizer::EnumIndex, normalizer::EnumItem>,
        struct_items: &TiSlice<normalizer::StructIndex, normalizer::StructItem>,
        ir_items: &TiSlice<normalizer::IrIndex, normalizer::IrItem>,
    ) -> Self {
        let init_namespace_item = |ident| NamespaceItem {
            ident: NamespaceIdent {
                kind: NamespaceKind::Impl,
                ident,
            },
            items: Vec::new(),
        };

        ItemBuckets {
            built_in_type_namespace_items: normalizer::BUILT_IN_TYPES
                .map(|built_in_type| init_namespace_item(built_in_type.ident())),
            enum_namespace_items: enum_items
                .iter()
                .map(|enum_item| init_namespace_item(enum_item.ident.value))
                .collect(),
            struct_namespace_items: struct_items
                .iter()
                .map(|struct_item| init_namespace_item(struct_item.ident.value))
                .collect(),
            ir_namespace_items: ir_items
                .iter()
                .map(|ir_item| init_namespace_item(ir_item.ident.value))
                .collect(),
            top_level_items: Vec::new(),
        }
    }

    fn bucket_for(&mut self, parent_index: Option<normalizer::ParentIndex>) -> &mut Vec<Item> {
        match parent_index {
            Some(parent_index) => self.bucket_for_parent(parent_index),
            None => &mut self.top_level_items,
        }
    }

    fn bucket_for_parent(&mut self, parent_index: normalizer::ParentIndex) -> &mut Vec<Item> {
        match parent_index {
            normalizer::ParentIndex::Type(type_index) => self.bucket_for_type(type_index),
            normalizer::ParentIndex::Ir(ir_index) => self.bucket_for_ir(ir_index),
        }
    }

    fn bucket_for_type(&mut self, type_index: normalizer::TypeIndex) -> &mut Vec<Item> {
        match type_index {
            normalizer::TypeIndex::BuiltIn(built_in_type) => {
                self.bucket_for_built_in_type(built_in_type)
            }
            normalizer::TypeIndex::Enum(enum_index) => self.bucket_for_enum(enum_index),
            normalizer::TypeIndex::Struct(struct_index) => self.bucket_for_struct(struct_index),
        }
    }

    fn bucket_for_built_in_type(
        &mut self,
        built_in_type: normalizer::BuiltInType,
    ) -> &mut Vec<Item> {
        &mut self.built_in_type_namespace_items[built_in_type].items
    }

    fn bucket_for_enum(&mut self, enum_index: normalizer::EnumIndex) -> &mut Vec<Item> {
        &mut self.enum_namespace_items[enum_index].items
    }

    fn bucket_for_struct(&mut self, struct_index: normalizer::StructIndex) -> &mut Vec<Item> {
        &mut self.struct_namespace_items[struct_index].items
    }

    fn bucket_for_ir(&mut self, ir_index: normalizer::IrIndex) -> &mut Vec<Item> {
        &mut self.ir_namespace_items[ir_index].items
    }
}

impl IntoIterator for ItemBuckets {
    type Item = Item;
    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let ItemBuckets {
            built_in_type_namespace_items,
            enum_namespace_items,
            struct_namespace_items,
            ir_namespace_items,
            top_level_items,
        } = self;

        iterate![
            ..iterate![
                ..built_in_type_namespace_items,
                ..enum_namespace_items,
                ..struct_namespace_items,
                ..ir_namespace_items,
            ]
            .filter(|namespace_item| !namespace_item.items.is_empty())
            .map(Into::into),
            ..top_level_items
        ]
    }
}

struct Compiler<'a> {
    env: &'a normalizer::Env,
    external_decls: ItemBuckets,
    internal_decls: ItemBuckets,
    internal_defs: ItemBuckets,
}

impl<'a> Compiler<'a> {
    fn new(env: &'a normalizer::Env) -> Self {
        let external_decls = ItemBuckets::new(&env.enum_items, &env.struct_items, &env.ir_items);
        let internal_decls = external_decls.clone();
        let internal_defs = external_decls.clone();

        Compiler {
            env,
            external_decls,
            internal_decls,
            internal_defs,
        }
    }

    fn compile_enum_item(&mut self, enum_index: normalizer::EnumIndex) {
        let enum_item = &self.env[enum_index];

        let variant_decls = enum_item.variants.iter().map(|variant| {
            FnItem {
                path: VariantFnPath {
                    parent: enum_item.ident.value,
                    ident: VariantFnIdent::from(variant.value),
                }
                .into(),
                is_fully_qualified: false,
                is_inline: true,
                params: Vec::new(),
                ret: TypeMemberTypePath {
                    parent: enum_item.ident.value,
                    ident: ExprTag::Ref.into(),
                }
                .into(),
                body: None,
            }
            .into()
        });

        self.external_decls
            .bucket_for_enum(enum_index)
            .extend(variant_decls);
    }

    fn compile_struct_item(&mut self, struct_index: normalizer::StructIndex) {
        let struct_item = &self.env[struct_index];

        if let Some(supertype_index) = struct_item.supertype {
            let supertype_ident = self.get_type_ident(supertype_index);

            self.external_decls.bucket_for_struct(struct_index).extend([
                generate_cast_fn_decl(
                    CastExprKind::Downcast,
                    supertype_ident,
                    struct_item.ident.value,
                    ExprTag::Val,
                )
                .into(),
                generate_cast_fn_decl(
                    CastExprKind::Downcast,
                    supertype_ident,
                    struct_item.ident.value,
                    ExprTag::Ref,
                )
                .into(),
                generate_cast_fn_decl(
                    CastExprKind::Upcast,
                    supertype_ident,
                    struct_item.ident.value,
                    ExprTag::Val,
                )
                .into(),
                generate_cast_fn_decl(
                    CastExprKind::Upcast,
                    supertype_ident,
                    struct_item.ident.value,
                    ExprTag::Ref,
                )
                .into(),
            ]);
        }
    }

    fn compile_global_var_item(&mut self, global_var_index: normalizer::GlobalVarIndex) {
        let global_var_item = &self.env[global_var_index];
        let global_var_type_ident = self.get_type_ident(global_var_item.type_);

        let global_var_fn_decl = FnItem {
            path: GlobalVarFnPath {
                parent: global_var_item
                    .parent
                    .map(|parent| self.get_parent_ident(parent)),
                ident: GlobalVarFnIdent::from(global_var_item.path.value.ident()),
            }
            .into(),
            is_fully_qualified: false,
            is_inline: true,
            params: self
                .generate_context_params(global_var_item.parent)
                .collect(),
            ret: TypeMemberTypePath {
                parent: global_var_type_ident,
                ident: global_var_tag(global_var_item.is_mut).into(),
            }
            .into(),
            body: None,
        }
        .into();

        self.external_decls
            .bucket_for(global_var_item.parent)
            .push(global_var_fn_decl);
    }

    fn compile_fn_item(&mut self, fn_index: normalizer::FnIndex) {
        let fn_item = &self.env[fn_index];

        let fn_decl = self.compile_fn_decl(fn_item);

        match &fn_item.body {
            None => {
                self.external_decls
                    .bucket_for(fn_item.parent)
                    .push(fn_decl.into());
            }
            Some(body) => {
                let mut fn_def = fn_decl.clone();
                fn_def.is_fully_qualified = true;
                fn_def.body = Some(self.compile_body(None, None, &fn_item.params, body));

                self.internal_decls
                    .bucket_for(fn_item.parent)
                    .push(fn_decl.into());
                self.internal_defs
                    .bucket_for(fn_item.parent)
                    .push(fn_def.into());
            }
        }
    }

    fn compile_op_item(&mut self, op_index: normalizer::OpIndex) {
        let op_item = &self.env[op_index];

        let parent_ir_item = &self.env[op_item.parent];

        // TODO(spinda): Get rid of the unfortunate cloning dance required here.
        // Should become unnecessary once `FnItem` and `OpItem` are merged.
        let fn_params = normalizer::FnParams {
            params: op_item.params.clone(),
            out_var_params: TiVec::new(),
        };

        let op_decl = self.compile_op_decl(op_item);

        let mut op_def = op_decl.clone();
        op_def.is_fully_qualified = true;
        op_def.body = Some(self.compile_body(
            Some(op_item.parent),
            parent_ir_item.emits,
            &fn_params,
            &op_item.body,
        ));

        self.internal_decls
            .bucket_for_ir(op_item.parent)
            .push(op_decl.into());
        self.internal_defs
            .bucket_for_ir(op_item.parent)
            .push(op_def.into());
    }

    fn compile_fn_decl(&mut self, fn_item: &normalizer::FnItem) -> FnItem {
        let path = UserFnPath {
            parent: fn_item
                .parent
                .map(|parent_index| self.get_parent_ident(parent_index)),
            ident: fn_item.path.value.ident().into(),
        }
        .into();

        let params = self
            .generate_context_params(fn_item.parent)
            .chain(self.compile_fn_params(&fn_item.params, &fn_item.param_order))
            .collect();

        let ret = if fn_item.ret == normalizer::BuiltInType::Unit.into() {
            Type::Void
        } else {
            TypeMemberTypePath {
                parent: self.get_type_ident(fn_item.ret),
                ident: ExprTag::Val.into(),
            }
            .into()
        };

        FnItem {
            path,
            is_fully_qualified: false,
            is_inline: false,
            params,
            ret,
            body: None,
        }
    }

    fn compile_op_decl(&mut self, op_item: &normalizer::OpItem) -> FnItem {
        let path = OpFnPath {
            parent: self.env[op_item.parent].ident.value,
            ident: op_item.path.value.ident().into(),
        }
        .into();

        let params = self
            .generate_context_params(Some(op_item.parent.into()))
            .chain(self.compile_params(&op_item.params, &op_item.param_order))
            .collect();

        FnItem {
            path,
            is_fully_qualified: false,
            is_inline: false,
            params,
            ret: Type::Void,
            body: None,
        }
    }

    fn compile_fn_params<'c>(
        &'c mut self,
        fn_params: &'c normalizer::FnParams,
        fn_param_order: &'c [normalizer::FnParamIndex],
    ) -> impl 'c + Iterator<Item = Param> + Captures<'a> {
        fn_param_order
            .iter()
            .map(|fn_param_index| self.compile_fn_param(fn_params, *fn_param_index))
    }

    fn compile_params<'c>(
        &'c mut self,
        params: &'c normalizer::Params,
        param_order: &'c [normalizer::ParamIndex],
    ) -> impl 'c + Iterator<Item = Param> + Captures<'a> {
        param_order
            .iter()
            .map(|param_index| self.compile_param(params, *param_index))
    }

    fn compile_fn_param(
        &mut self,
        fn_params: &normalizer::FnParams,
        fn_param_index: normalizer::FnParamIndex,
    ) -> Param {
        match fn_param_index {
            normalizer::FnParamIndex::Param(param_index) => {
                self.compile_param(&fn_params.params, param_index)
            }
            normalizer::FnParamIndex::OutVar(out_var_param_index) => {
                self.compile_out_var_param(&fn_params[out_var_param_index])
            }
        }
    }

    fn compile_param(
        &mut self,
        params: &normalizer::Params,
        param_index: normalizer::ParamIndex,
    ) -> Param {
        match param_index {
            normalizer::ParamIndex::Var(var_param_index) => {
                self.compile_var_param(&params[var_param_index])
            }
            normalizer::ParamIndex::Label(label_param_index) => {
                self.compile_label_param(params[label_param_index].value)
            }
        }
    }

    fn compile_var_param(&mut self, var_param: &normalizer::VarParam) -> Param {
        let type_ = TypeMemberTypePath {
            parent: self.get_type_ident(var_param.type_),
            ident: ExprTag::Ref.into(),
        }
        .into();

        // TODO(spinda): Create local variables for mutable parameters.

        Param {
            ident: UserParamIdent::from(var_param.ident.value).into(),
            type_,
        }
    }

    fn compile_out_var_param(&mut self, out_var_param: &normalizer::OutVarParam) -> Param {
        Param {
            ident: UserParamIdent::from(out_var_param.ident.value).into(),
            type_: TypeMemberTypePath {
                parent: self.get_type_ident(out_var_param.type_),
                ident: ExprTag::MutRef.into(),
            }
            .into(),
        }
    }

    fn compile_label_param(&mut self, label_param_ident: Ident) -> Param {
        Param {
            ident: UserParamIdent::from(label_param_ident).into(),
            type_: IrMemberTypePath {
                // TODO(spinda): Give the labels IR annotations so this doesn't
                // have to be hardcoded.
                parent: "MASM".into(),
                ident: IrMemberTypeIdent::LabelRef,
            }
            .into(),
        }
    }

    fn compile_body(
        &mut self,
        parent_ir_index: Option<normalizer::IrIndex>,
        emits_ir_index: Option<normalizer::IrIndex>,
        fn_params: &normalizer::FnParams,
        body: &normalizer::Body,
    ) -> Block {
        ScopedCompiler::new(
            self,
            parent_ir_index,
            emits_ir_index,
            fn_params,
            &body.local_vars,
            &body.local_labels,
        )
        .compile_block_stmts(&body.stmts)
    }

    fn generate_context_params(
        &self,
        parent_index: Option<normalizer::ParentIndex>,
    ) -> impl Iterator<Item = Param> {
        let ir_context_param = parent_index.and_then(|parent_index| match parent_index {
            normalizer::ParentIndex::Type(_) => None,
            normalizer::ParentIndex::Ir(ir_index) => {
                let ir_item = &self.env[ir_index];
                let ir_context_param_ident = match ir_item.emits {
                    None => IrContextParamIdent::Interpreter,
                    Some(_) => IrContextParamIdent::Compiler,
                };
                Some(Param {
                    ident: ir_context_param_ident.into(),
                    type_: IrMemberTypePath {
                        parent: ir_item.ident.value,
                        ident: ir_context_param_ident.type_ident(),
                    }
                    .into(),
                })
            }
        });

        iterate![
            Param {
                ident: ParamIdent::Context.into(),
                type_: HelperTypeIdent::ContextRef.into(),
            },
            ..ir_context_param,
        ]
    }

    fn get_parent_ident(&self, parent_index: normalizer::ParentIndex) -> Ident {
        match parent_index {
            normalizer::ParentIndex::Type(type_index) => self.get_type_ident(type_index),
            normalizer::ParentIndex::Ir(ir_index) => self.env[ir_index].ident.value,
        }
    }

    fn get_type_ident(&self, type_index: normalizer::TypeIndex) -> Ident {
        match type_index {
            normalizer::TypeIndex::BuiltIn(built_in_type) => built_in_type.ident(),
            normalizer::TypeIndex::Enum(enum_index) => self.env[enum_index].ident.value,
            normalizer::TypeIndex::Struct(struct_index) => self.env[struct_index].ident.value,
        }
    }
}

fn generate_cast_fn_decl(
    kind: CastExprKind,
    supertype_ident: Ident,
    subtype_ident: Ident,
    param_tag: ExprTag,
) -> FnItem {
    let subtype_path = TypeMemberTypePath {
        parent: subtype_ident,
        ident: param_tag.into(),
    };
    let supertype_path = TypeMemberTypePath {
        parent: supertype_ident,
        ident: param_tag.into(),
    };

    let (source_type_path, target_type_path) = match kind {
        CastExprKind::Downcast => (supertype_path, subtype_path),
        CastExprKind::Upcast => (subtype_path, supertype_path),
    };

    lazy_static! {
        static ref IN_PARAM_IDENT: Ident = "in".into();
    }

    FnItem {
        path: TypeMemberFnPath {
            parent: subtype_ident,
            ident: CastTypeMemberFnIdent {
                kind,
                supertype: supertype_ident,
            }
            .into(),
        }
        .into(),
        is_fully_qualified: false,
        is_inline: true,
        params: vec![Param {
            ident: UserParamIdent::from(*IN_PARAM_IDENT).into(),
            type_: source_type_path.into(),
        }],
        ret: target_type_path.into(),
        body: None,
    }
}

struct ScopedCompiler<'a, 'b> {
    compiler: &'b Compiler<'a>,
    parent_ir_index: Option<normalizer::IrIndex>,
    emits_ir_index: Option<normalizer::IrIndex>,
    fn_params: &'b normalizer::FnParams,
    local_vars: &'b TiSlice<normalizer::LocalVarIndex, normalizer::LocalVar>,
    local_labels: &'b TiSlice<normalizer::LocalLabelIndex, Spanned<Ident>>,
}

impl<'a> Deref for ScopedCompiler<'a, '_> {
    type Target = Compiler<'a>;

    fn deref(&self) -> &Self::Target {
        self.compiler
    }
}

impl<'a, 'b> ScopedCompiler<'a, 'b> {
    fn new(
        compiler: &'b Compiler<'a>,
        parent_ir_index: Option<normalizer::IrIndex>,
        emits_ir_index: Option<normalizer::IrIndex>,
        fn_params: &'b normalizer::FnParams,
        local_vars: &'b TiSlice<normalizer::LocalVarIndex, normalizer::LocalVar>,
        local_labels: &'b TiSlice<normalizer::LocalLabelIndex, Spanned<Ident>>,
    ) -> Self {
        ScopedCompiler {
            compiler,
            parent_ir_index,
            emits_ir_index,
            fn_params,
            local_vars,
            local_labels,
        }
    }

    fn compile_block_stmts(&self, stmts: &[normalizer::Stmt]) -> Block {
        let mut compiled_stmts = Vec::with_capacity(stmts.len());

        for stmt in stmts {
            self.compile_stmt(stmt, &mut compiled_stmts);
        }

        compiled_stmts.into()
    }

    fn compile_stmt(&self, stmt: &normalizer::Stmt, compiled_stmts: &mut Vec<Stmt>) {
        match stmt {
            normalizer::Stmt::Let(let_stmt) => self.compile_let_stmt(let_stmt, compiled_stmts),
            normalizer::Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt, compiled_stmts),
            normalizer::Stmt::Check(check_stmt) => {
                self.compile_check_stmt(check_stmt, compiled_stmts)
            }
            normalizer::Stmt::Goto(goto_stmt) => self.compile_goto_stmt(goto_stmt, compiled_stmts),
            normalizer::Stmt::Emit(emit_stmt) => self.compile_emit_stmt(emit_stmt, compiled_stmts),
            normalizer::Stmt::Block(block_stmt) => {
                self.compile_block_stmt(block_stmt, compiled_stmts)
            }
            normalizer::Stmt::Call(call_stmt) => self.compile_call_stmt(call_stmt, compiled_stmts),
            normalizer::Stmt::Assign(assign_stmt) => {
                self.compile_assign_stmt(assign_stmt, compiled_stmts)
            }
            normalizer::Stmt::Ret(ret_stmt) => self.compile_ret_stmt(ret_stmt, compiled_stmts),
        }
    }

    fn compile_let_stmt(&self, let_stmt: &normalizer::LetStmt, compiled_stmts: &mut Vec<Stmt>) {
        let lhs = LocalVarIdent {
            ident: self.local_vars[let_stmt.lhs].ident.value,
            index: let_stmt.lhs,
        }
        .into();

        let type_ = TypeMemberTypePath {
            parent: self.get_type_ident(let_stmt.rhs.type_()),
            ident: ExprTag::Local.into(),
        }
        .into();

        let rhs = Some(self.use_expr(self.compile_expr(&let_stmt.rhs), ExprTag::Local.into()));

        compiled_stmts.push(LetStmt { lhs, type_, rhs }.into());
    }

    fn compile_if_stmt(&self, if_stmt: &normalizer::IfStmt, compiled_stmts: &mut Vec<Stmt>) {
        let cond = self.use_expr(
            self.compile_expr(&if_stmt.cond),
            ExprTag::Ref | ExprTag::Val,
        );

        let then = self.compile_block_stmts(&if_stmt.then);

        let else_ = if_stmt
            .else_
            .as_ref()
            .map(|else_| self.compile_block_stmts(else_));

        compiled_stmts.push(IfStmt { cond, then, else_ }.into())
    }

    fn compile_check_stmt(
        &self,
        check_stmt: &normalizer::CheckStmt,
        compiled_stmts: &mut Vec<Stmt>,
    ) {
        let cond = self.use_expr(
            self.compile_expr(&check_stmt.cond),
            ExprTag::Ref | ExprTag::Val,
        );

        compiled_stmts.push(
            Expr::from(CallExpr {
                target: HelperFnIdent::Assert.into(),
                args: vec![cond],
            })
            .into(),
        )
    }

    fn compile_goto_stmt(&self, goto_stmt: &normalizer::GotoStmt, compiled_stmts: &mut Vec<Stmt>) {
        compiled_stmts.extend([
            Expr::from(CallExpr {
                target: IrMemberFnPath {
                    parent: self.env[self.parent_ir_index.unwrap()].ident.value,
                    ident: IrMemberFnIdent::Goto,
                }
                .into(),
                args: vec![
                    ParamIdent::Context.into(),
                    IrContextParamIdent::Interpreter.into(),
                    self.get_label_ident(goto_stmt.label).into(),
                ],
            })
            .into(),
            RetStmt { value: None }.into(),
        ]);
    }

    fn compile_emit_stmt(&self, emit_stmt: &normalizer::EmitStmt, compiled_stmts: &mut Vec<Stmt>) {
        let args = self.compile_args(&emit_stmt.args).collect::<Vec<_>>();

        compiled_stmts.push(
            Expr::from(CallExpr {
                target: IrMemberFnPath {
                    parent: self.env[self.emits_ir_index.unwrap()].ident.value,
                    ident: EmitOpIrMemberFnIdent::from(OpFnIdent::from(
                        self.env[emit_stmt.target].path.value.ident(),
                    ))
                    .into(),
                }
                .into(),
                args: iterate![
                    ParamIdent::Context.into(),
                    CallExpr {
                        target: IrMemberFnPath {
                            parent: self.env[self.parent_ir_index.unwrap()].ident.value,
                            ident: IrMemberFnIdent::GetOutput,
                        }
                        .into(),
                        args: vec![IrContextParamIdent::Compiler.into()],
                    }
                    .into(),
                    ..args,
                ]
                .collect(),
            })
            .into(),
        );
    }

    fn compile_block_stmt(
        &self,
        block_stmt: &normalizer::BlockStmt,
        compiled_stmts: &mut Vec<Stmt>,
    ) {
        let stmts = self.compile_block_stmts(&block_stmt.stmts);

        compiled_stmts.push(BlockStmt::from(stmts).into());
    }

    fn compile_call_stmt(&self, fn_call: &normalizer::FnCall, compiled_stmts: &mut Vec<Stmt>) {
        compiled_stmts.push(Expr::from(self.compile_call_expr(fn_call).expr).into());
    }

    fn compile_assign_stmt(
        &self,
        assign_stmt: &normalizer::AssignStmt,
        compiled_stmts: &mut Vec<Stmt>,
    ) {
        let lhs = self.use_expr(
            self.compile_var_use(assign_stmt.lhs),
            ExprTag::MutRef.into(),
        );

        let rhs = self.use_expr(
            self.compile_expr(&assign_stmt.rhs),
            ExprTag::MutRef | ExprTag::Ref | ExprTag::Local | ExprTag::Val,
        );

        compiled_stmts.push(match assign_stmt.lhs {
            normalizer::VarIndex::OutParam(_) => Expr::from(CallExpr {
                target: TypeMemberFnPath {
                    parent: self.get_type_ident(assign_stmt.rhs.type_()),
                    ident: TypeMemberFnIdent::SetMutRef,
                }
                .into(),
                args: vec![lhs, rhs],
            })
            .into(),
            _ => Expr::from(AssignExpr { lhs, rhs }).into(),
        });
    }

    fn compile_ret_stmt(&self, ret_stmt: &normalizer::RetStmt, compiled_stmts: &mut Vec<Stmt>) {
        let value = self.use_expr(self.compile_expr(&ret_stmt.value), ExprTag::Val.into());

        compiled_stmts.push(RetStmt { value: Some(value) }.into());
    }

    fn compile_fn_args<'c>(
        &'c self,
        fn_args: &'c [normalizer::FnArg],
    ) -> impl 'c + Iterator<Item = Expr> {
        fn_args.iter().map(|fn_arg| self.compile_fn_arg(fn_arg))
    }

    fn compile_args<'c>(&'c self, args: &'c [normalizer::Arg]) -> impl 'c + Iterator<Item = Expr> {
        args.iter().map(|arg| self.compile_arg(arg))
    }

    fn compile_fn_arg(&self, fn_arg: &normalizer::FnArg) -> Expr {
        match fn_arg {
            normalizer::FnArg::Arg(arg) => self.compile_arg(arg),
            normalizer::FnArg::OutVar(out_var_arg) => self.use_expr(
                self.compile_out_var_arg(out_var_arg),
                ExprTag::MutRef.into(),
            ),
        }
    }

    fn compile_arg(&self, arg: &normalizer::Arg) -> Expr {
        match arg {
            normalizer::Arg::Expr(atom_expr) => {
                self.use_expr(self.compile_atom_expr(atom_expr), ExprTag::Ref.into())
            }
            normalizer::Arg::Label(label_index) => self.get_label_ident(*label_index).into(),
        }
    }

    fn compile_out_var_arg(&self, out_var_arg: &normalizer::OutVarArg) -> TaggedExpr {
        // TODO(spinda): Handle out-parameter upcasting.
        match out_var_arg.out_var {
            normalizer::OutVar::Out(var_index) => self.compile_var_use(var_index.value),
            // TODO(spinda): Eliminate these in the normalizer.
            normalizer::OutVar::OutLet(_) => unimplemented!(),
        }
    }

    fn compile_expr(&self, expr: &normalizer::Expr) -> TaggedExpr {
        match expr {
            normalizer::Expr::Block(block_expr) => {
                self.compile_block_expr(&block_expr).map(Expr::from)
            }
            normalizer::Expr::Var(var_expr) => self.compile_var_expr(var_expr),
            normalizer::Expr::Call(fn_call) => self.compile_call_expr(fn_call).map(Expr::from),
            normalizer::Expr::Negate(negate_expr) => {
                self.compile_negate_expr(&negate_expr).map(Expr::from)
            }
            normalizer::Expr::Cast(cast_expr) => {
                self.compile_cast_expr(&cast_expr).map(Expr::from)
            }
            normalizer::Expr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).map(Expr::from)
            }
        }
    }

    fn compile_atom_expr(&self, atom_expr: &normalizer::AtomExpr) -> TaggedExpr {
        match atom_expr {
            normalizer::AtomExpr::Var(var_expr) => self.compile_var_expr(var_expr),
            normalizer::AtomExpr::Negate(negate_expr) => {
                self.compile_negate_expr(&negate_expr).map(Expr::from)
            }
            normalizer::AtomExpr::Cast(cast_expr) => {
                self.compile_cast_expr(&cast_expr).map(Expr::from)
            }
            normalizer::AtomExpr::Compare(compare_expr) => {
                self.compile_compare_expr(&compare_expr).map(Expr::from)
            }
        }
    }

    fn use_expr(&self, expr: TaggedExpr, tags: EnumSet<ExprTag>) -> Expr {
        // TODO(spinda): Auto-expand tags for built-in types.
        if expr.tags.is_disjoint(tags) {
            if let Some(preferred_tag) = tags.iter().nth(0) {
                // TODO(spinda): Check tag conversion validity.
                return CallExpr {
                    target: TypeMemberFnPath {
                        parent: self.get_type_ident(expr.type_),
                        ident: ToTagTypeMemberFnIdent::from(preferred_tag).into(),
                    }
                    .into(),
                    args: iterate![
                        ..if preferred_tag == ExprTag::Local {
                            Some(ParamIdent::Context.into())
                        } else {
                            None
                        },
                        expr.expr,
                    ]
                    .collect(),
                }
                .into();
            }
        }

        expr.expr
    }

    fn compile_block_expr(&self, block_expr: &normalizer::BlockExpr) -> TaggedExpr<BlockExpr> {
        let mut block = self.compile_block_stmts(&block_expr.stmts);
        block.stmts.push(
            self.use_expr(self.compile_expr(&block_expr.value), ExprTag::Val.into())
                .into(),
        );
        TaggedExpr {
            expr: block.into(),
            type_: block_expr.type_(),
            tags: ExprTag::Val.into(),
        }
    }

    fn compile_var_expr(&self, var_expr: &normalizer::VarExpr) -> TaggedExpr {
        self.compile_var_use(var_expr.var)
    }

    fn compile_var_use(&self, var_index: normalizer::VarIndex) -> TaggedExpr {
        match var_index {
            normalizer::VarIndex::BuiltIn(built_in_var) => TaggedExpr {
                expr: CallExpr {
                    target: GlobalVarFnPath::from(GlobalVarFnIdent::from(built_in_var.ident()))
                        .into(),
                    args: self.generate_context_args(None).collect(),
                }
                .into(),
                type_: built_in_var.type_(),
                tags: ExprTag::Ref.into(),
            },
            normalizer::VarIndex::EnumVariant(enum_variant_index) => {
                let enum_item = &self.env[enum_variant_index.enum_index];
                TaggedExpr {
                    expr: CallExpr {
                        target: VariantFnPath {
                            parent: enum_item.ident.value,
                            ident: enum_item.variants[enum_variant_index.variant_index]
                                .value
                                .into(),
                        }
                        .into(),
                        args: self.generate_context_args(None).collect(),
                    }
                    .into(),
                    type_: enum_variant_index.enum_index.into(),
                    tags: ExprTag::Ref.into(),
                }
            }
            normalizer::VarIndex::Global(global_var_index) => {
                let global_var_item = &self.env[global_var_index];
                TaggedExpr {
                    expr: CallExpr {
                        target: GlobalVarFnPath {
                            parent: global_var_item
                                .parent
                                .map(|parent| self.get_parent_ident(parent)),
                            ident: global_var_item.path.value.ident().into(),
                        }
                        .into(),
                        args: self.generate_context_args(global_var_item.parent).collect(),
                    }
                    .into(),
                    type_: global_var_item.type_,
                    tags: global_var_tag(global_var_item.is_mut).into(),
                }
            }
            normalizer::VarIndex::Param(var_param_index) => {
                let var_param = &self.fn_params[var_param_index];
                TaggedExpr {
                    expr: UserParamIdent::from(var_param.ident.value).into(),
                    type_: var_param.type_,
                    tags: ExprTag::Ref.into(),
                }
            }
            normalizer::VarIndex::OutParam(out_var_param_index) => {
                let out_var_param = &self.fn_params[out_var_param_index];
                TaggedExpr {
                    expr: UserParamIdent::from(out_var_param.ident.value).into(),
                    type_: out_var_param.type_,
                    tags: ExprTag::MutRef.into(),
                }
            }
            normalizer::VarIndex::Local(local_var_index) => {
                let local_var = &self.local_vars[local_var_index];
                TaggedExpr {
                    expr: LocalVarIdent {
                        ident: local_var.ident.value,
                        index: local_var_index,
                    }
                    .into(),
                    type_: local_var.type_,
                    tags: ExprTag::Local.into(),
                }
            }
        }
    }

    fn compile_call_expr(&self, fn_call: &normalizer::FnCall) -> TaggedExpr<CallExpr> {
        let fn_item = &self.env[fn_call.target];

        let args = self
            .generate_context_args(fn_item.parent)
            .chain(self.compile_fn_args(&fn_call.args))
            .collect();

        TaggedExpr {
            expr: CallExpr {
                target: UserFnPath {
                    parent: fn_item
                        .parent
                        .map(|parent_index| self.get_parent_ident(parent_index)),
                    ident: fn_item.path.value.ident().into(),
                }
                .into(),
                args,
            },
            type_: fn_call.type_(),
            tags: ExprTag::Val.into(),
        }
    }

    fn compile_negate_expr<E: CompileExpr + Typed>(
        &self,
        negate_expr: &normalizer::NegateExpr<E>,
    ) -> TaggedExpr<NegateExpr> {
        let expr = self.use_expr(negate_expr.expr.compile(self), ExprTag::Ref | ExprTag::Val);

        // TODO(spinda): Replace this with a helper function call.
        TaggedExpr {
            expr: NegateExpr {
                kind: negate_expr.kind,
                expr,
            },
            type_: negate_expr.type_(),
            tags: ExprTag::Ref | ExprTag::Val,
        }
    }

    fn compile_cast_expr<E: CompileExpr + Typed>(
        &self,
        cast_expr: &normalizer::CastExpr<E>,
    ) -> TaggedExpr<CallExpr> {
        let expr = self.use_expr(cast_expr.expr.compile(self), ExprTag::Ref.into());

        let source_type_index = cast_expr.expr.type_();
        let target_type_index = cast_expr.type_;

        let (supertype_index, subtype_index) = match cast_expr.kind {
            CastExprKind::Downcast => (source_type_index, target_type_index),
            CastExprKind::Upcast => (target_type_index, source_type_index),
        };

        let supertype_ident = self.get_type_ident(supertype_index);
        let subtype_ident = self.get_type_ident(subtype_index);

        TaggedExpr {
            expr: CallExpr {
                target: TypeMemberFnPath {
                    parent: subtype_ident,
                    ident: CastTypeMemberFnIdent {
                        kind: cast_expr.kind,
                        supertype: supertype_ident,
                    }
                    .into(),
                }
                .into(),
                args: vec![expr],
            },
            type_: cast_expr.type_(),
            tags: ExprTag::Ref.into(),
        }
    }

    fn compile_compare_expr(
        &self,
        compare_expr: &normalizer::CompareExpr,
    ) -> TaggedExpr<CallExpr> {
        let lhs_type = compare_expr.lhs.type_();
        let rhs_type = compare_expr.rhs.type_();
        debug_assert_eq!(lhs_type, rhs_type);

        let lhs = self.use_expr(
            self.compile_atom_expr(&compare_expr.lhs),
            ExprTag::Ref.into(),
        );
        let rhs = self.use_expr(
            self.compile_atom_expr(&compare_expr.rhs),
            ExprTag::Ref.into(),
        );

        TaggedExpr {
            expr: CallExpr {
                target: TypeMemberFnPath {
                    parent: self.get_type_ident(lhs_type),
                    ident: CompareTypeMemberFnIdent::from(compare_expr.kind).into(),
                }
                .into(),
                args: vec![lhs, rhs],
            },
            type_: compare_expr.type_(),
            tags: ExprTag::Ref | ExprTag::Val,
        }
    }

    fn generate_context_args(
        &self,
        parent_index: Option<normalizer::ParentIndex>,
    ) -> impl Iterator<Item = Expr> {
        self.generate_context_params(parent_index)
            .map(|param| param.ident.into())
    }

    fn get_label_ident(&self, label_index: normalizer::LabelIndex) -> VarIdent {
        match label_index {
            normalizer::LabelIndex::Param(label_param_index) => {
                UserParamIdent::from(self.fn_params[label_param_index].value).into()
            }
            normalizer::LabelIndex::Local(local_label_index) => LocalLabelVarIdent {
                ident: self.local_labels[local_label_index].value,
                index: local_label_index,
            }
            .into(),
        }
    }
}

trait CompileExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &ScopedCompiler<'a, 'b>) -> TaggedExpr;
}

impl CompileExpr for normalizer::Expr {
    fn compile<'a, 'b>(&self, scoped_compiler: &ScopedCompiler<'a, 'b>) -> TaggedExpr {
        scoped_compiler.compile_expr(self)
    }
}

impl CompileExpr for normalizer::AtomExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &ScopedCompiler<'a, 'b>) -> TaggedExpr {
        scoped_compiler.compile_atom_expr(self)
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}", expr)]
pub struct TaggedExpr<T = Expr> {
    expr: T,
    type_: normalizer::TypeIndex,
    tags: EnumSet<ExprTag>,
}

impl<T> TaggedExpr<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> TaggedExpr<U> {
        TaggedExpr {
            expr: f(self.expr),
            type_: self.type_,
            tags: self.tags,
        }
    }
}

fn global_var_tag(is_mut: bool) -> ExprTag {
    if is_mut {
        ExprTag::MutRef
    } else {
        ExprTag::Ref
    }
}
