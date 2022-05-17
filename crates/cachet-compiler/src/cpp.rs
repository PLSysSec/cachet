// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::{hash_map, HashMap};
use std::iter;
use std::mem;
use std::ops::Deref;

use derive_more::{Display, From};
use enum_map::EnumMap;
use fix_hidden_lifetime_bug::Captures;
use iterate::iterate;
use typed_index_collections::{TiSlice, TiVec};

use cachet_util::MaybeOwned;

use cachet_lang::ast::{BinOper, CastKind, CompareBinOper, Ident, Path, VarParamKind, VarRefKind};
use cachet_lang::built_in::{BuiltInType, BuiltInVar};
use cachet_lang::normalizer::{self, HasAttrs, Typed};

use crate::cpp::ast::*;

mod ast;

// * C++ Compilation Entry Point

#[derive(Clone, Debug)]
pub struct CppCompilerOutput {
    pub decls: CppCode,
    pub defs: CppCode,
}

#[derive(Clone, Debug, Display)]
pub struct CppCode(Code);

pub fn compile(env: &normalizer::Env) -> CppCompilerOutput {
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
        compiler.compile_callable_item(fn_index.into());
    }

    for op_index in env.op_items.keys() {
        compiler.compile_callable_item(op_index.into());
    }

    let decls = iterate![
        CommentItem::from("External forward declarations.").into(),
        ..compiler.external_decls,
        CommentItem::from("Internal forward declarations.").into(),
        ..compiler.internal_decls,
    ]
    .collect();

    let defs = iterate![
        CommentItem::from("Internal definitions.").into(),
        ..compiler.internal_defs,
    ]
    .collect();

    CppCompilerOutput {
        decls: CppCode(decls),
        defs: CppCode(defs),
    }
}

// * Organizing C++ Declarations and Definitions

#[derive(Clone, Copy, From)]
enum ItemBucketIndex {
    #[from(types(BuiltInType, normalizer::EnumIndex, normalizer::StructIndex))]
    Type(normalizer::TypeIndex),
    #[from(types(normalizer::IrIndex))]
    Ir(IrItemBucketIndex),
    TopLevel,
}

impl From<normalizer::ParentIndex> for ItemBucketIndex {
    fn from(parent_index: normalizer::ParentIndex) -> Self {
        match parent_index {
            normalizer::ParentIndex::Type(type_index) => type_index.into(),
            normalizer::ParentIndex::Ir(ir_index) => ir_index.into(),
        }
    }
}

impl From<Option<normalizer::ParentIndex>> for ItemBucketIndex {
    fn from(parent_index: Option<normalizer::ParentIndex>) -> Self {
        match parent_index {
            Some(parent_index) => parent_index.into(),
            None => Self::TopLevel,
        }
    }
}

#[derive(Clone, Copy)]
struct IrItemBucketIndex {
    ir_index: normalizer::IrIndex,
    selector: Option<IrMemberFlagSelector>,
}

impl From<normalizer::IrIndex> for IrItemBucketIndex {
    fn from(ir_index: normalizer::IrIndex) -> Self {
        Self {
            ir_index,
            selector: None,
        }
    }
}

#[derive(Clone)]
struct ItemBuckets {
    built_in_type_namespace_items: [NamespaceItem; BuiltInType::COUNT],
    enum_namespace_items: TiVec<normalizer::EnumIndex, NamespaceItem>,
    struct_namespace_items: TiVec<normalizer::StructIndex, NamespaceItem>,
    ir_item_buckets: TiVec<normalizer::IrIndex, IrItemBuckets>,
    top_level_items: Vec<Item>,
}

impl ItemBuckets {
    fn new(
        enum_items: &TiSlice<normalizer::EnumIndex, normalizer::EnumItem>,
        struct_items: &TiSlice<normalizer::StructIndex, normalizer::StructItem>,
        ir_items: &TiSlice<normalizer::IrIndex, normalizer::IrItem>,
    ) -> Self {
        let init_namespace_item = |ident: Ident| NamespaceItem {
            ident: NamespaceIdent {
                kind: NamespaceKind::Impl,
                ident,
            },
            items: Vec::new(),
        };

        ItemBuckets {
            built_in_type_namespace_items: BuiltInType::ALL
                .map(|built_in_type| init_namespace_item(built_in_type.ident())),
            enum_namespace_items: enum_items
                .iter()
                .map(|enum_item| init_namespace_item(enum_item.ident.value))
                .collect(),
            struct_namespace_items: struct_items
                .iter()
                .map(|struct_item| init_namespace_item(struct_item.ident.value))
                .collect(),
            ir_item_buckets: ir_items
                .iter()
                .map(|ir_item| IrItemBuckets::new(ir_item.ident.value))
                .collect(),
            top_level_items: Vec::new(),
        }
    }

    fn bucket_for(&mut self, item_bucket_index: ItemBucketIndex) -> &mut Vec<Item> {
        match item_bucket_index {
            ItemBucketIndex::Type(type_index) => self.bucket_for_type(type_index),
            ItemBucketIndex::Ir(ir_item_bucket_index) => self.bucket_for_ir(ir_item_bucket_index),
            ItemBucketIndex::TopLevel => &mut self.top_level_items,
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

    fn bucket_for_built_in_type(&mut self, built_in_type: BuiltInType) -> &mut Vec<Item> {
        &mut self.built_in_type_namespace_items[built_in_type.index()].items
    }

    fn bucket_for_enum(&mut self, enum_index: normalizer::EnumIndex) -> &mut Vec<Item> {
        &mut self.enum_namespace_items[enum_index].items
    }

    fn bucket_for_struct(&mut self, struct_index: normalizer::StructIndex) -> &mut Vec<Item> {
        &mut self.struct_namespace_items[struct_index].items
    }

    fn bucket_for_ir(&mut self, ir_item_bucket_index: IrItemBucketIndex) -> &mut Vec<Item> {
        self.ir_item_buckets[ir_item_bucket_index.ir_index]
            .bucket_for(ir_item_bucket_index.selector)
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
            ir_item_buckets,
            top_level_items,
        } = self;

        iterate![
            ..iterate![
                ..built_in_type_namespace_items,
                ..enum_namespace_items,
                ..struct_namespace_items,
            ]
            .filter(|namespace_item| !namespace_item.items.is_empty())
            .map(Into::into),
            ..ir_item_buckets.into_iter().flatten(),
            ..top_level_items
        ]
    }
}

#[derive(Clone)]
struct IrItemBuckets {
    ident: Ident,
    items_for_selectors: EnumMap<IrMemberFlagSelector, Vec<Item>>,
    misc_items: Vec<Item>,
}

impl IrItemBuckets {
    fn new(ident: Ident) -> Self {
        IrItemBuckets {
            ident,
            items_for_selectors: EnumMap::default(),
            misc_items: Vec::new(),
        }
    }

    fn bucket_for(&mut self, selector: Option<IrMemberFlagSelector>) -> &mut Vec<Item> {
        match selector {
            Some(selector) => &mut self.items_for_selectors[selector],
            None => &mut self.misc_items,
        }
    }
}

impl IntoIterator for IrItemBuckets {
    type Item = Item;
    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let IrItemBuckets {
            ident,
            items_for_selectors,
            misc_items,
        } = self;

        let impl_namespace_ident = NamespaceIdent {
            kind: NamespaceKind::Impl,
            ident,
        };

        let if_def_items = items_for_selectors
            .into_iter()
            .filter_map(move |(selector, items)| {
                if items.is_empty() {
                    None
                } else {
                    Some(
                        IfDefItem {
                            cond: IrMemberFlagIdent { ident, selector }.into(),
                            then: vec![
                                NamespaceItem {
                                    ident: impl_namespace_ident,
                                    items,
                                }
                                .into(),
                            ]
                            .into(),
                        }
                        .into(),
                    )
                }
            });

        let misc_namespace_item = if misc_items.is_empty() {
            None
        } else {
            Some(
                NamespaceItem {
                    ident: impl_namespace_ident,
                    items: misc_items,
                }
                .into(),
            )
        };

        iterate![..if_def_items, ..misc_namespace_item]
    }
}

// * Top-Level C++ Compiler

const CONTEXT_PARAM: Param = Param {
    ident: ParamVarIdent::Context,
    type_: Type::Path(TypePath::Helper(HelperTypeIdent::ContextRef)),
};
const CONTEXT_ARG: Expr = Expr::Var(VarIdent::Param(CONTEXT_PARAM.ident));

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

        let variant_decls = enum_item.variants.iter().map(|variant_path| {
            let path = TypeMemberFnPath {
                parent: enum_item.ident.value,
                ident: VariantTypeMemberFnIdent::from(variant_path.value.ident()).into(),
            }
            .into();

            let ret = TypeMemberTypePath {
                parent: enum_item.ident.value,
                ident: VarRefKind::In.into(),
            }
            .into();

            FnItem {
                path,
                is_fully_qualified: false,
                is_inline: true,
                params: Vec::new(),
                ret,
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

            // TODO(spinda): For now we assume subtyping rules are bivariant, but
            // types ought to be able to specify their own variances.
            self.external_decls.bucket_for_struct(struct_index).extend(
                [
                    TypeRepr::Value,
                    VarRefKind::In.into(),
                    VarRefKind::Out.into(),
                ]
                .into_iter()
                .flat_map(|param_repr| {
                    [CastKind::Downcast, CastKind::Upcast]
                        .into_iter()
                        .map(move |cast_kind| {
                            generate_cast_fn_decl(
                                cast_kind,
                                supertype_ident,
                                struct_item.ident.value,
                                param_repr,
                            )
                            .into()
                        })
                }),
            );
        }
    }

    fn compile_global_var_item(&mut self, global_var_index: normalizer::GlobalVarIndex) {
        let global_var_item = &self.env[global_var_index];
        if global_var_item.is_prelude() {
            return;
        }

        let global_var_parent_ident = global_var_item.path.value.parent().map(Path::ident);
        let global_var_ident = global_var_item.path.value.ident();
        let path = GlobalVarFnPath {
            parent: global_var_parent_ident,
            ident: global_var_ident.into(),
        }
        .into();

        let type_ident = self.get_type_ident(global_var_item.type_);

        let ret = TypeMemberTypePath {
            parent: type_ident,
            ident: get_global_var_repr(global_var_item.is_mut).into(),
        }
        .into();

        let fn_decl = FnItem {
            path,
            is_fully_qualified: false,
            is_inline: true,
            params: vec![CONTEXT_PARAM],
            ret,
            body: None,
        }
        .into();

        self.external_decls
            .bucket_for(global_var_item.parent.into())
            .push(fn_decl);
    }

    fn compile_callable_item(&mut self, callable_index: normalizer::CallableIndex) {
        let callable_item = &self.env[callable_index];
        if callable_item.is_prelude() {
            return;
        }

        let callable_parent_ident = callable_item.path.value.parent().map(Path::ident);
        let callable_ident = callable_item.path.value.ident();
        let path = UserFnPath {
            parent: callable_parent_ident,
            ident: match callable_index {
                normalizer::CallableIndex::Fn(_) => FnUserFnIdent::from(callable_ident).into(),
                normalizer::CallableIndex::Op(_) => OpUserFnIdent::from(callable_ident).into(),
            },
        }
        .into();

        let params = iter::once(CONTEXT_PARAM)
            .chain(self.compile_params(&callable_item.params, &callable_item.param_order))
            .collect();

        let ret = match callable_item.ret {
            Some(ret) => TypeMemberTypePath {
                parent: self.get_type_ident(ret),
                ident: TypeRepr::Value.into(),
            }
            .into(),
            None => Type::Void,
        };

        let fn_decl = FnItem {
            path,
            is_fully_qualified: false,
            is_inline: false,
            params,
            ret,
            body: None,
        };

        let item_bucket_index = match callable_index {
            normalizer::CallableIndex::Fn(_) => callable_item.parent.into(),
            normalizer::CallableIndex::Op(_) => {
                let ir_index = match callable_item.parent {
                    Some(normalizer::ParentIndex::Ir(ir_index)) => ir_index,
                    _ => panic!("op with no parent IR"),
                };
                let ir_item = &self.env[ir_index];
                let ir_ident = ir_item.ident.value;

                // Declare emit helper functions for ops.

                let mut emit_fn_decl = fn_decl.clone();

                emit_fn_decl.path = IrMemberFnPath {
                    parent: ir_ident,
                    ident: EmitIrMemberFnIdent::from(callable_ident).into(),
                }
                .into();

                emit_fn_decl.params.insert(
                    1,
                    Param {
                        ident: ParamVarIdent::Ops,
                        type_: IrMemberTypePath {
                            parent: ir_ident,
                            ident: IrMemberTypeIdent::OpsRef,
                        }
                        .into(),
                    },
                );

                self.external_decls
                    .bucket_for_ir(IrItemBucketIndex {
                        ir_index,
                        selector: Some(IrMemberFlagSelector::Emit),
                    })
                    .push(emit_fn_decl.into());

                // Put the rest of the compiled items in either the interpreter
                // or compiler bucket.

                IrItemBucketIndex {
                    ir_index,
                    selector: Some(match ir_item.emits {
                        None => IrMemberFlagSelector::Interpreter,
                        Some(_) => IrMemberFlagSelector::Compiler,
                    }),
                }
                .into()
            }
        };

        match &callable_item.body {
            None => {
                self.external_decls
                    .bucket_for(item_bucket_index)
                    .push(fn_decl.into());
            }
            Some(body) => {
                let mut fn_def = fn_decl.clone();
                fn_def.is_fully_qualified = true;
                fn_def.body =
                    Some(self.compile_body(callable_item.parent, &callable_item.params, body));

                self.internal_decls
                    .bucket_for(item_bucket_index)
                    .push(fn_decl.into());
                self.internal_defs
                    .bucket_for(item_bucket_index)
                    .push(fn_def.into());
            }
        }
    }

    fn compile_params<'b>(
        &'b mut self,
        params: &'b normalizer::Params,
        param_order: &'b [normalizer::ParamIndex],
    ) -> impl 'b + Iterator<Item = Param> + Captures<'a> {
        param_order
            .iter()
            .map(|param_index| self.compile_param(params, *param_index))
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
                self.compile_label_param(&params[label_param_index])
            }
        }
    }

    fn compile_var_param(&mut self, var_param: &normalizer::VarParam) -> Param {
        let ident = UserParamVarIdent::from(var_param.ident.value).into();

        let type_ = TypeMemberTypePath {
            parent: self.get_type_ident(var_param.type_),
            ident: get_var_param_repr(var_param).into(),
        }
        .into();

        Param { ident, type_ }
    }

    fn compile_label_param(&mut self, label_param: &normalizer::LabelParam) -> Param {
        let ident = UserParamVarIdent::from(label_param.label.ident.value).into();

        let type_ = IrMemberTypePath {
            parent: self.env[label_param.label.ir].ident.value,
            ident: if label_param.is_out_ref {
                IrMemberTypeIdent::LabelMutRef
            } else {
                IrMemberTypeIdent::LabelRef
            },
        }
        .into();

        Param { ident, type_ }
    }

    fn compile_body(
        &mut self,
        parent_index: Option<normalizer::ParentIndex>,
        params: &normalizer::Params,
        body: &normalizer::Body,
    ) -> Block {
        let mut scoped_compiler = ScopedCompiler::new(self, parent_index, params, &body.locals);
        scoped_compiler.compile_stmts(&body.stmts);
        scoped_compiler.stmts.into()
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
    kind: CastKind,
    supertype_ident: Ident,
    subtype_ident: Ident,
    param_repr: TypeRepr,
) -> FnItem {
    let subtype_path = TypeMemberTypePath {
        parent: subtype_ident,
        ident: param_repr.into(),
    };
    let supertype_path = TypeMemberTypePath {
        parent: supertype_ident,
        ident: param_repr.into(),
    };

    let (source_type_path, target_type_path) = match kind {
        CastKind::Downcast => (supertype_path, subtype_path),
        CastKind::Upcast => (subtype_path, supertype_path),
    };

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
            ident: ParamVarIdent::In,
            type_: source_type_path.into(),
        }],
        ret: target_type_path.into(),
        body: None,
    }
}

// * Scoped C++ Compiler

// TODO(spinda): Make this hold a `CallableItem` reference instead of broken-out
// fields.
struct ScopedCompiler<'a, 'b> {
    compiler: &'b Compiler<'a>,
    parent_index: Option<normalizer::ParentIndex>,
    params: &'b normalizer::Params,
    locals: &'b normalizer::Locals,
    next_synthetic_var_indexes: MaybeOwned<'b, EnumMap<SyntheticVarKind, usize>>,
    stmts: Vec<Stmt>,
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
        parent_index: Option<normalizer::ParentIndex>,
        params: &'b normalizer::Params,
        locals: &'b normalizer::Locals,
    ) -> Self {
        ScopedCompiler {
            compiler,
            parent_index,
            params,
            locals,
            next_synthetic_var_indexes: MaybeOwned::Owned(EnumMap::default()),
            stmts: Vec::new(),
        }
    }

    fn recurse<'c>(&'c mut self) -> ScopedCompiler<'a, 'c> {
        ScopedCompiler {
            compiler: self.compiler,
            parent_index: self.parent_index,
            params: self.params,
            locals: self.locals,
            next_synthetic_var_indexes: MaybeOwned::Borrowed(&mut self.next_synthetic_var_indexes),
            stmts: Vec::new(),
        }
    }

    fn compile_args(
        &mut self,
        params: &normalizer::Params,
        param_order: &[normalizer::ParamIndex],
        args: &[normalizer::Arg],
        compiled_args: &mut Vec<Expr>,
    ) {
        let mut stable_var_tmp_locals = HashMap::new();
        compiled_args.extend(
            param_order
                .iter()
                .zip(args.iter())
                .map(|(param_index, arg)| {
                    self.compile_arg(params, *param_index, arg, &mut stable_var_tmp_locals)
                }),
        );
    }

    fn compile_arg(
        &mut self,
        params: &normalizer::Params,
        param_index: normalizer::ParamIndex,
        arg: &normalizer::Arg,
        stable_var_tmp_locals: &mut HashMap<normalizer::VarIndex, SyntheticVarIdent>,
    ) -> Expr {
        match arg {
            normalizer::Arg::Expr(pure_expr) => {
                self.compile_expr_arg(params, param_index, pure_expr, stable_var_tmp_locals)
            }
            normalizer::Arg::VarRef(var_ref_arg) => self.compile_var_ref_arg(var_ref_arg),
            normalizer::Arg::Label(label_arg) => self.compile_label_arg(label_arg),
        }
    }

    fn compile_expr_arg(
        &mut self,
        params: &normalizer::Params,
        param_index: normalizer::ParamIndex,
        expr: &normalizer::PureExpr,
        stable_var_tmp_locals: &mut HashMap<normalizer::VarIndex, SyntheticVarIdent>,
    ) -> Expr {
        let var_param_index = match param_index {
            normalizer::ParamIndex::Var(var_param_index) => var_param_index,
            normalizer::ParamIndex::Label(_) => {
                panic!("expression argument passed for label parameter")
            }
        };
        let var_param = &params[var_param_index];

        let tagged_expr = self.compile_expr_arg_impl(var_param, expr, stable_var_tmp_locals);
        self.use_expr(tagged_expr, get_var_param_repr(var_param))
    }

    fn compile_expr_arg_impl(
        &mut self,
        var_param: &normalizer::VarParam,
        expr: &normalizer::PureExpr,
        stable_var_tmp_locals: &mut HashMap<normalizer::VarIndex, SyntheticVarIdent>,
    ) -> TaggedExpr<Expr> {
        match expr {
            // Ensure casts stay at the argument site, so that multiple casts of
            // the same variable can potentially share the same synthetic local
            // (see below).
            normalizer::PureExpr::Cast(cast_expr) => {
                let tagged_expr =
                    self.compile_expr_arg_impl(var_param, &cast_expr.expr, stable_var_tmp_locals);
                self.compile_cast(cast_expr.kind, tagged_expr, cast_expr.type_, VarRefKind::In)
                    .map(Into::into)
            }
            expr => {
                let mut tagged_expr = self.compile_pure_expr(expr);

                let mut needs_synthetic_var = false;
                let mut synthetic_var_is_stable = false;

                // Variable parameters assigned `Ref` representations may need
                // special handling.
                if let TypeRepr::Ref(var_ref_kind) = get_var_param_repr(var_param) {
                    // If we have a `Value` but we want a `Ref`, we'll need to
                    // promote to a synthetic `Local` that we can take
                    // a reference to.
                    if let TypeRepr::Value = tagged_expr.repr {
                        needs_synthetic_var = true;
                    // Value parameters of non-built-in types are emulated with
                    // reference parameters, with storage for the parameter
                    // values delegated to the caller. In such cases, we may
                    // need to generate a synthetic local variable in the caller
                    // in order to fully provide value parameter semantics.
                    } else if let VarParamKind::Value { .. } = var_param.kind {
                        // When the parameter expects a read-only reference, we
                        // can avoid creating a synthetic local in the caller if
                        // the argument we have points stably to a value.
                        // Otherwise, we either need an independent variable to
                        // be mutated by the callee, or we need to capture
                        // a snapshot of an unstable argument expression's value
                        // at the time of the call - both cases which we handle
                        // with a synthetic local.
                        if !(var_ref_kind == VarRefKind::In && tagged_expr.is_stable) {
                            needs_synthetic_var = true;
                        }
                    }

                    // If we need to create a synthetic variable, it can be
                    // considered stable (unchanging) if only read-only
                    // references point to it.
                    if needs_synthetic_var {
                        if let VarRefKind::In = var_ref_kind {
                            synthetic_var_is_stable = true;
                        }
                    }
                }

                if needs_synthetic_var {
                    // Stable synthetic variables can be shared between multiple
                    // read-only references to the same variable in the same
                    // batch of arguments.
                    let mut var_tmp_local_entry = None;
                    if synthetic_var_is_stable {
                        if let normalizer::PureExpr::Var(var_expr) = expr {
                            var_tmp_local_entry = Some(stable_var_tmp_locals.entry(var_expr.var));
                        }
                    }

                    // Use a cached synthetic variable if we have one available;
                    // otherwise, generate a fresh one.
                    let (synthetic_var_ident, is_fresh_synthetic_var) = match &var_tmp_local_entry
                    {
                        Some(hash_map::Entry::Occupied(occupied_entry)) => {
                            (*occupied_entry.get(), false)
                        }
                        _ => (
                            self.take_synthetic_var_ident(
                                SyntheticVarKind::Arg,
                                Some(var_param.ident.value),
                            ),
                            true,
                        ),
                    };

                    // Cache the synthetic variable if we can.
                    if let Some(hash_map::Entry::Vacant(vacant_entry)) = var_tmp_local_entry {
                        vacant_entry.insert(synthetic_var_ident);
                    }

                    let synthetic_var_type = tagged_expr.type_;
                    let synthetic_var_value = mem::replace(
                        &mut tagged_expr,
                        TaggedExpr {
                            expr: synthetic_var_ident.into(),
                            type_: synthetic_var_type,
                            repr: TypeRepr::Local,
                            // The synthetic variable can be considered stable
                            // (unchanging) if the sole reference pointing to it
                            // is read-only.
                            is_stable: synthetic_var_is_stable,
                        },
                    );
                    if is_fresh_synthetic_var {
                        self.init_local_var(
                            synthetic_var_ident.into(),
                            synthetic_var_type,
                            synthetic_var_value,
                        );
                    }
                }

                tagged_expr
            }
        }
    }

    fn compile_var_ref_arg(&mut self, var_ref_arg: &normalizer::VarRefArg) -> Expr {
        let tagged_expr = match &var_ref_arg.var_ref {
            normalizer::VarRef::Free(free_var_ref) => {
                self.compile_var_access(free_var_ref.var.value)
            }
            // TODO(spinda): Eliminate these in the normalizer.
            normalizer::VarRef::FreshOut(_) => unimplemented!(),
        };

        let var_ref_kind = var_ref_arg.var_ref.kind();
        let tagged_expr = var_ref_arg.cast_route.iter().fold(
            tagged_expr,
            |tagged_expr, (cast_kind, target_type_index)| {
                self.compile_cast(*cast_kind, tagged_expr, *target_type_index, var_ref_kind)
                    .map(Into::into)
            },
        );

        self.use_expr(tagged_expr, var_ref_kind.into())
    }

    fn compile_label_arg(&self, label_arg: &normalizer::LabelArg) -> Expr {
        match label_arg.label {
            normalizer::LabelIndex::Param(label_param_index) => {
                UserParamVarIdent::from(self.params[label_param_index].label.ident.value).into()
            }
            normalizer::LabelIndex::Local(local_label_index) => CallExpr {
                target: IrMemberFnPath {
                    parent: self.env[label_arg.ir].ident.value,
                    ident: if label_arg.is_out_ref {
                        IrMemberFnIdent::ToLabelMutRef
                    } else {
                        IrMemberFnIdent::ToLabelRef
                    },
                }
                .into(),
                args: vec![
                    LocalLabelVarIdent {
                        ident: self.locals[local_label_index].ident.value,
                        index: local_label_index,
                    }
                    .into(),
                ],
            }
            .into(),
        }
    }

    fn compile_internal_label_arg(
        &self,
        label_index: normalizer::LabelIndex,
        is_out: bool,
    ) -> Expr {
        self.compile_label_arg(&normalizer::LabelArg {
            label: label_index,
            is_out_ref: is_out,
            ir: self.get_label_ir(label_index),
        })
    }

    fn compile_invocation(&mut self, invoke_expr: &normalizer::InvokeExpr) -> (Vec<Stmt>, Expr) {
        // Compiling the arguments may produce temporary variables. Confine them
        // to a temporary scope.
        let mut scoped_compiler = self.recurse();

        let fn_item = &scoped_compiler.env[invoke_expr.call.target];

        let parent = fn_item.path.value.parent().map(Path::ident);
        let ident = FnUserFnIdent::from(fn_item.path.value.ident()).into();
        let target = UserFnPath { parent, ident }.into();

        let mut args = vec![CONTEXT_ARG];
        scoped_compiler.compile_args(
            &fn_item.params,
            &fn_item.param_order,
            &invoke_expr.call.args,
            &mut args,
        );

        (scoped_compiler.stmts, CallExpr { target, args }.into())
    }

    fn compile_block(&mut self, stmts: &[normalizer::Stmt]) -> Block {
        let mut scoped_compiler = self.recurse();
        scoped_compiler.compile_stmts(stmts);
        scoped_compiler.stmts.into()
    }

    fn compile_stmts(&mut self, stmts: &[normalizer::Stmt]) {
        self.stmts.reserve(stmts.len());
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
    }

    fn compile_stmt(&mut self, stmt: &normalizer::Stmt) {
        match stmt {
            normalizer::Stmt::Let(let_stmt) => self.compile_let_stmt(let_stmt),
            normalizer::Stmt::Label(label_stmt) => self.compile_label_stmt(label_stmt),
            normalizer::Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt),
            normalizer::Stmt::Check(check_stmt) => self.compile_check_stmt(check_stmt),
            normalizer::Stmt::Goto(goto_stmt) => self.compile_goto_stmt(goto_stmt),
            normalizer::Stmt::Bind(bind_stmt) => self.compile_bind_stmt(bind_stmt),
            normalizer::Stmt::Emit(call) => self.compile_emit_stmt(call),
            normalizer::Stmt::Block(_, block_stmt) => self.compile_block_stmt(block_stmt),
            normalizer::Stmt::Invoke(invoke_stmt) => self.compile_invoke_stmt(invoke_stmt),
            normalizer::Stmt::Assign(assign_stmt) => self.compile_assign_stmt(assign_stmt),
            normalizer::Stmt::Ret(ret_stmt) => self.compile_ret_stmt(ret_stmt),
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: &normalizer::LetStmt) {
        let local_var = &self.locals[let_stmt.lhs];

        let lhs = LocalVarIdent {
            ident: local_var.ident.value,
            index: let_stmt.lhs,
        }
        .into();

        let rhs = self.compile_expr(&let_stmt.rhs);

        self.init_local_var(lhs, local_var.type_, rhs);
    }

    fn compile_label_stmt(&mut self, label_stmt: &normalizer::LabelStmt) {
        let local_label = &self.locals[label_stmt.label];
        let ir_ident = self.env[local_label.ir].ident.value;

        let lhs = LocalLabelVarIdent {
            ident: local_label.ident.value,
            index: label_stmt.label,
        }
        .into();

        let type_ = IrMemberTypePath {
            parent: ir_ident,
            ident: IrMemberTypeIdent::LabelLocal,
        }
        .into();

        let rhs = Some(
            CallExpr {
                target: IrMemberFnPath {
                    parent: ir_ident,
                    ident: IrMemberFnIdent::NewLabel,
                }
                .into(),
                args: vec![CONTEXT_ARG],
            }
            .into(),
        );

        self.stmts.push(LetStmt { lhs, type_, rhs }.into());
    }

    fn compile_if_stmt(&mut self, if_stmt: &normalizer::IfStmt) {
        let if_ = self.compile_if_stmt_recurse(if_stmt);
        self.stmts.push(if_.into());
    }

    fn compile_if_stmt_recurse(&mut self, if_stmt: &normalizer::IfStmt) -> IfStmt {
        let tagged_cond = self.compile_expr(&if_stmt.cond);
        let cond = self.use_expr(tagged_cond, TypeRepr::Value);

        let then = self.compile_block(&if_stmt.then);

        let else_ = if_stmt.else_.as_ref().map(|else_| match else_ {
            normalizer::ElseClause::ElseIf(else_if) => {
                ast::ElseClause::ElseIf(Box::new(self.compile_if_stmt_recurse(&*else_if)))
            }
            normalizer::ElseClause::Else(else_block) => {
                ast::ElseClause::Else(self.compile_block(else_block))
            }
        });

        IfStmt { cond, then, else_ }.into()
    }

    fn compile_check_stmt(&mut self, check_stmt: &normalizer::CheckStmt) {
        let tagged_cond = self.compile_expr(&check_stmt.cond);
        let cond = self.use_expr(tagged_cond, TypeRepr::Value);

        self.stmts.push(
            Expr::from(CallExpr {
                target: HelperFnIdent::Assert.into(),
                args: vec![cond],
            })
            .into(),
        );
    }

    fn compile_goto_stmt(&mut self, goto_stmt: &normalizer::GotoStmt) {
        let ir_ident = self.env[goto_stmt.ir].ident.value;
        let target = IrMemberFnPath {
            parent: ir_ident,
            ident: IrMemberFnIdent::GotoLabel,
        }
        .into();

        let args = vec![
            CONTEXT_ARG,
            self.compile_internal_label_arg(goto_stmt.label, false),
        ];

        self.stmts.extend([
            Expr::from(CallExpr { target, args }).into(),
            RetStmt { value: None }.into(),
        ]);
    }

    fn compile_bind_stmt(&mut self, bind_stmt: &normalizer::BindStmt) {
        let ir_ident = self.env[bind_stmt.ir].ident.value;
        let target = IrMemberFnPath {
            parent: ir_ident,
            ident: IrMemberFnIdent::BindLabel,
        }
        .into();

        let args = vec![
            CONTEXT_ARG,
            self.generate_ops_arg(),
            self.compile_internal_label_arg(bind_stmt.label, true),
        ];

        self.stmts.extend([
            Expr::from(CallExpr { target, args }).into(),
            RetStmt { value: None }.into(),
        ]);
    }

    fn compile_emit_stmt(&mut self, emit_stmt: &normalizer::EmitStmt) {
        let mut scoped_compiler = self.recurse();

        let ir_item = &scoped_compiler.env[emit_stmt.ir];
        let op_item = &scoped_compiler.env[emit_stmt.call.target];

        let ir_ident = ir_item.ident.value;
        let op_ident = op_item.path.value.ident();

        let target = IrMemberFnPath {
            parent: ir_ident,
            ident: EmitIrMemberFnIdent::from(op_ident).into(),
        }
        .into();

        let mut args = vec![CONTEXT_ARG, scoped_compiler.generate_ops_arg()];
        scoped_compiler.compile_args(
            &op_item.params,
            &op_item.param_order,
            &emit_stmt.call.args,
            &mut args,
        );

        // Compiling the arguments may produce temporary variables. If so,
        // confine them to a block wrapped around the call.
        let call_stmt = Expr::from(CallExpr { target, args }).into();
        let mut stmts = scoped_compiler.stmts;
        self.stmts.push(if stmts.is_empty() {
            call_stmt
        } else {
            stmts.push(call_stmt);
            stmts.into()
        });
    }

    fn compile_block_stmt(&mut self, block_stmt: &normalizer::BlockStmt) {
        let stmts = self.compile_block(&block_stmt.stmts);

        self.stmts.push(BlockStmt::from(stmts).into());
    }

    fn compile_invoke_stmt(&mut self, invoke_stmt: &normalizer::InvokeStmt) {
        let (mut stmts, expr) = self.compile_invocation(invoke_stmt);
        self.stmts.push(if stmts.is_empty() {
            expr.into()
        } else {
            stmts.push(expr.into());
            stmts.into()
        });
    }

    fn compile_assign_stmt(&mut self, assign_stmt: &normalizer::AssignStmt) {
        let lhs = self.compile_var_access(assign_stmt.lhs);
        debug_assert!(
            match lhs.repr {
                TypeRepr::Local | TypeRepr::Ref(VarRefKind::Out) => true,
                TypeRepr::Value | TypeRepr::Ref(VarRefKind::In) => false,
            },
            "left-hand side of an assignment must be assignable"
        );

        let rhs = self.compile_expr(&assign_stmt.rhs);
        debug_assert!(
            match rhs.repr {
                TypeRepr::Value | TypeRepr::Local | TypeRepr::Ref(VarRefKind::In) => true,
                TypeRepr::Ref(VarRefKind::Out) => false,
            },
            "right-hand side of an assignment must be readable"
        );

        debug_assert_eq!(lhs.type_, rhs.type_);

        self.stmts.push(
            Expr::from(CallExpr {
                target: TypeMemberFnPath {
                    parent: self.get_type_ident(lhs.type_),
                    ident: TypeMemberFnIdent::Assign,
                }
                .into(),
                args: vec![lhs.expr, rhs.expr],
            })
            .into(),
        );
    }

    fn compile_ret_stmt(&mut self, ret_stmt: &normalizer::RetStmt) {
        let value = ret_stmt.value.as_ref().map(|value| {
            let tagged_expr = self.compile_expr(value);
            self.use_expr(tagged_expr, TypeRepr::Value)
        });

        self.stmts.push(RetStmt { value }.into());
    }

    fn compile_expr(&mut self, expr: &normalizer::Expr) -> TaggedExpr {
        match expr {
            normalizer::Expr::Block(_, block_expr) => self.compile_block_expr(&block_expr),
            normalizer::Expr::Literal(literal) => self.compile_literal(literal).map(Expr::from),
            normalizer::Expr::Var(var_expr) => self.compile_var_expr(var_expr),
            normalizer::Expr::Invoke(invoke_expr) => self.compile_invoke_expr(invoke_expr),
            normalizer::Expr::FieldAccess(field_access_expr) => self
                .compile_field_access_expr(&field_access_expr)
                .map(Expr::from),
            normalizer::Expr::Negate(negate_expr) => {
                self.compile_negate_expr(&negate_expr).map(Expr::from)
            }
            normalizer::Expr::Cast(cast_expr) => {
                self.compile_cast_expr(&cast_expr).map(Expr::from)
            }
            normalizer::Expr::BinOper(bin_oper_expr) => {
                self.compile_bin_oper_expr(&bin_oper_expr).map(Expr::from)
            }
        }
    }

    fn compile_pure_expr(&mut self, pure_expr: &normalizer::PureExpr) -> TaggedExpr {
        match pure_expr {
            normalizer::PureExpr::Block(_, block_expr) => {
                self.compile_pure_expr(&block_expr.value)
            }
            normalizer::PureExpr::Literal(literal) => {
                self.compile_literal(literal).map(Expr::from)
            }
            normalizer::PureExpr::Var(var_expr) => self.compile_var_expr(var_expr),
            normalizer::PureExpr::FieldAccess(field_access_expr) => self
                .compile_field_access_expr(&field_access_expr)
                .map(Expr::from),
            normalizer::PureExpr::Negate(negate_expr) => {
                self.compile_negate_expr(&negate_expr).map(Expr::from)
            }
            normalizer::PureExpr::Cast(cast_expr) => {
                self.compile_cast_expr(&cast_expr).map(Expr::from)
            }
            normalizer::PureExpr::BinOper(bin_oper_expr) => {
                self.compile_bin_oper_expr(&bin_oper_expr).map(Expr::from)
            }
        }
    }

    fn compile_block_expr(&mut self, block_expr: &normalizer::BlockExpr) -> TaggedExpr<Expr> {
        let mut block = self.compile_block(&block_expr.stmts);

        let tagged_value = self.compile_expr(&block_expr.value);
        if block.stmts.is_empty() {
            return tagged_value;
        }
        block
            .stmts
            .push(self.use_expr(tagged_value, TypeRepr::Value).into());

        TaggedExpr {
            expr: block.into(),
            type_: block_expr.type_(),
            repr: TypeRepr::Value,
            is_stable: true,
        }
    }

    fn compile_literal(&self, literal: &normalizer::Literal) -> TaggedExpr<Literal> {
        TaggedExpr {
            expr: match literal {
                normalizer::Literal::Int32(n) => Literal::Int32(*n),
                normalizer::Literal::Int64(n) => Literal::Int64(*n),
                normalizer::Literal::UInt16(n) => Literal::UInt16(*n),
                normalizer::Literal::Double(n) => Literal::Double(*n),
            },
            type_: literal.type_(),
            repr: TypeRepr::Value,
            is_stable: true,
        }
    }

    fn compile_var_expr(&self, var_expr: &normalizer::VarExpr) -> TaggedExpr {
        self.compile_var_read(var_expr.var)
    }

    fn compile_var_read(&self, var_index: normalizer::VarIndex) -> TaggedExpr {
        // Inline `true` and `false` constants.
        match var_index {
            normalizer::VarIndex::BuiltIn(
                built_in_var @ (BuiltInVar::True | BuiltInVar::False),
            ) => TaggedExpr {
                expr: (built_in_var == BuiltInVar::True).into(),
                type_: built_in_var.type_(),
                repr: TypeRepr::Value,
                is_stable: true,
            },
            var_index => self.compile_var_access(var_index),
        }
    }

    fn compile_var_access(&self, var_index: normalizer::VarIndex) -> TaggedExpr {
        match var_index {
            normalizer::VarIndex::BuiltIn(built_in_var) => TaggedExpr {
                expr: CallExpr {
                    target: GlobalVarFnPath::from(GlobalVarFnIdent::from(built_in_var.ident()))
                        .into(),
                    args: vec![CONTEXT_ARG],
                }
                .into(),
                type_: built_in_var.type_(),
                repr: VarRefKind::In.into(),
                is_stable: true,
            },
            normalizer::VarIndex::EnumVariant(enum_variant_index) => {
                let enum_item = &self.env[enum_variant_index.enum_index];
                let variant_path = enum_item[enum_variant_index.variant_index];

                TaggedExpr {
                    expr: CallExpr {
                        target: TypeMemberFnPath {
                            parent: enum_item.ident.value,
                            ident: VariantTypeMemberFnIdent::from(variant_path.value.ident())
                                .into(),
                        }
                        .into(),
                        args: vec![CONTEXT_ARG],
                    }
                    .into(),
                    type_: enum_variant_index.enum_index.into(),
                    repr: VarRefKind::In.into(),
                    is_stable: true,
                }
            }
            normalizer::VarIndex::Global(global_var_index) => {
                let global_var_item = &self.env[global_var_index];

                TaggedExpr {
                    expr: CallExpr {
                        target: GlobalVarFnPath {
                            parent: global_var_item.path.value.parent().map(Path::ident),
                            ident: global_var_item.path.value.ident().into(),
                        }
                        .into(),
                        args: vec![CONTEXT_ARG],
                    }
                    .into(),
                    type_: global_var_item.type_,
                    repr: get_global_var_repr(global_var_item.is_mut).into(),
                    is_stable: !global_var_item.is_mut,
                }
            }
            normalizer::VarIndex::Param(var_param_index) => {
                let var_param = &self.params[var_param_index];

                TaggedExpr {
                    expr: UserParamVarIdent::from(var_param.ident.value).into(),
                    type_: var_param.type_,
                    repr: get_var_param_repr(var_param),
                    is_stable: match var_param.kind {
                        VarParamKind::Value { is_mut } => !is_mut,
                        VarParamKind::Ref(_) => false,
                    },
                }
            }
            normalizer::VarIndex::Local(local_var_index) => {
                let local_var = &self.locals[local_var_index];

                TaggedExpr {
                    expr: LocalVarIdent {
                        ident: local_var.ident.value,
                        index: local_var_index,
                    }
                    .into(),
                    type_: local_var.type_,
                    repr: TypeRepr::Local.into(),
                    is_stable: !local_var.is_mut,
                }
            }
        }
    }

    fn compile_invoke_expr(&mut self, invoke_expr: &normalizer::InvokeExpr) -> TaggedExpr<Expr> {
        let (mut stmts, expr) = self.compile_invocation(invoke_expr);
        TaggedExpr {
            expr: if stmts.is_empty() {
                expr.into()
            } else {
                stmts.push(expr.into());
                stmts.into()
            },
            type_: invoke_expr.type_(),
            repr: TypeRepr::Value,
            is_stable: true,
        }
    }

    fn compile_field_access_expr<E: CompileExpr + Typed>(
        &mut self,
        field_access_expr: &normalizer::FieldAccessExpr<E>,
    ) -> TaggedExpr<ArrowExpr> {
        let parent_expr = field_access_expr.parent.compile(self).expr;
        let parent_type = field_access_expr.parent.type_();

        TaggedExpr {
            expr: ArrowExpr {
                parent: CallExpr {
                    target: TypeMemberFnPath {
                        parent: self.get_type_ident(parent_type),
                        ident: TypeMemberFnIdent::Fields,
                    }
                    .into(),
                    args: vec![parent_expr],
                }
                .into(),
                member: self.env[field_access_expr.field].ident.value,
            },
            type_: field_access_expr.type_(),
            repr: TypeRepr::Value,
            is_stable: true,
        }
    }

    fn compile_negate_expr<E: CompileExpr + Typed>(
        &mut self,
        negate_expr: &normalizer::NegateExpr<E>,
    ) -> TaggedExpr<NegateExpr> {
        debug_assert!(match negate_expr.expr.type_() {
            normalizer::TypeIndex::BuiltIn(_) => true,
            _ => false,
        });

        let tagged_expr = negate_expr.expr.compile(self);
        let expr = self.use_expr(tagged_expr, TypeRepr::Value);

        TaggedExpr {
            expr: NegateExpr {
                kind: negate_expr.kind,
                expr,
            },
            type_: negate_expr.type_(),
            repr: TypeRepr::Value,
            is_stable: true,
        }
    }

    fn compile_cast_expr<E: CompileExpr + Typed>(
        &mut self,
        cast_expr: &normalizer::CastExpr<E>,
    ) -> TaggedExpr<CallExpr> {
        let tagged_expr = cast_expr.expr.compile(self);
        self.compile_cast(cast_expr.kind, tagged_expr, cast_expr.type_, VarRefKind::In)
    }

    fn compile_cast(
        &mut self,
        kind: CastKind,
        tagged_expr: TaggedExpr,
        target_type_index: normalizer::TypeIndex,
        target_var_ref_kind: VarRefKind,
    ) -> TaggedExpr<CallExpr> {
        let source_type_index = tagged_expr.type_;
        let is_stable = tagged_expr.is_stable;
        let repr = match tagged_expr.repr {
            TypeRepr::Value => TypeRepr::Value,
            TypeRepr::Local | TypeRepr::Ref(_) => target_var_ref_kind.into(),
        };
        let expr = self.use_expr(tagged_expr, repr);

        let (supertype_index, subtype_index) = match kind {
            CastKind::Downcast => (source_type_index, target_type_index),
            CastKind::Upcast => (target_type_index, source_type_index),
        };

        let supertype_ident = self.get_type_ident(supertype_index);
        let subtype_ident = self.get_type_ident(subtype_index);

        TaggedExpr {
            expr: CallExpr {
                target: TypeMemberFnPath {
                    parent: subtype_ident,
                    ident: CastTypeMemberFnIdent {
                        kind,
                        supertype: supertype_ident,
                    }
                    .into(),
                }
                .into(),
                args: vec![expr],
            },
            type_: target_type_index,
            repr,
            is_stable,
        }
    }

    fn compile_bin_oper_expr(&mut self, bin_oper_expr: &normalizer::BinOperExpr) -> TaggedExpr {
        let lhs = self.compile_bin_oper_operand_expr(bin_oper_expr.oper, &bin_oper_expr.lhs);
        let rhs = self.compile_bin_oper_operand_expr(bin_oper_expr.oper, &bin_oper_expr.rhs);

        let lhs_type = bin_oper_expr.lhs.type_();
        let rhs_type = bin_oper_expr.rhs.type_();
        debug_assert_eq!(lhs_type, rhs_type);

        let expr = if let normalizer::TypeIndex::BuiltIn(_) = lhs_type {
            // For built-in types, we can directly use C++ operators.
            BinOperExpr {
                oper: bin_oper_expr.oper,
                lhs,
                rhs,
            }
            .into()
        } else {
            // Otherwise, we should call out to a helper function.
            CallExpr {
                target: TypeMemberFnPath {
                    parent: self.get_type_ident(lhs_type),
                    ident: match bin_oper_expr.oper {
                        BinOper::Compare(CompareBinOper::Eq) => CompareTypeMemberFnIdent::Eq,
                        BinOper::Compare(CompareBinOper::Neq) => CompareTypeMemberFnIdent::Neq,
                        _ => panic!("binary operator only valid for built-in types"),
                    }
                    .into(),
                }
                .into(),
                args: vec![lhs, rhs],
            }
            .into()
        };

        TaggedExpr {
            expr,
            type_: bin_oper_expr.type_(),
            repr: TypeRepr::Value,
            is_stable: true,
        }
    }

    fn compile_bin_oper_operand_expr(
        &mut self,
        bin_oper: BinOper,
        expr: &normalizer::PureExpr,
    ) -> Expr {
        let tagged_expr = self.compile_pure_expr(expr);
        match bin_oper {
            // Equality comparison operators can take either `Value` or `Ref`
            // operands.
            BinOper::Compare(CompareBinOper::Eq | CompareBinOper::Neq) => match tagged_expr.repr {
                TypeRepr::Value | TypeRepr::Ref(_) => tagged_expr.expr,
                TypeRepr::Local => self.use_expr(tagged_expr, TypeRepr::Ref(VarRefKind::In)),
            },
            // Other comparison operators (which operate over built-in types)
            // take `Value` operands.
            _ => self.use_expr(tagged_expr, TypeRepr::Value),
        }
    }

    fn use_expr(&self, tagged_expr: TaggedExpr, desired_repr: TypeRepr) -> Expr {
        if tagged_expr.repr == desired_repr {
            return tagged_expr.expr;
        }

        debug_assert!(
            tagged_expr.repr.can_convert_to(desired_repr),
            "can't convert from {} to {}",
            tagged_expr.repr,
            desired_repr
        );

        if let normalizer::TypeIndex::BuiltIn(_) = tagged_expr.type_ {
            // Built-in types are known to need no explicit tag conversions.
            return tagged_expr.expr;
        }

        CallExpr {
            target: TypeMemberFnPath {
                parent: self.get_type_ident(tagged_expr.type_),
                ident: ToReprTypeMemberFnIdent::from(desired_repr).into(),
            }
            .into(),
            args: iterate![
                ..if desired_repr == TypeRepr::Local {
                    Some(CONTEXT_ARG)
                } else {
                    None
                },
                tagged_expr.expr,
            ]
            .collect(),
        }
        .into()
    }

    fn init_local_var(&mut self, lhs: VarIdent, lhs_type: normalizer::TypeIndex, rhs: TaggedExpr) {
        let type_ = TypeMemberTypePath {
            parent: self.get_type_ident(lhs_type),
            ident: TypeRepr::Local.into(),
        }
        .into();

        let rhs = Some(self.use_expr(rhs, TypeRepr::Local));

        self.stmts.push(LetStmt { lhs, type_, rhs }.into());
    }

    fn generate_ops_arg(&self) -> Expr {
        match self.parent_index {
            Some(normalizer::ParentIndex::Ir(ir_index)) => CallExpr {
                target: IrMemberFnPath {
                    parent: self.env[ir_index].ident.value,
                    ident: IrMemberFnIdent::GetOutput,
                }
                .into(),
                args: vec![CONTEXT_ARG],
            }
            .into(),
            _ => ParamVarIdent::Ops.into(),
        }
    }

    fn get_label_ir(&self, label_index: normalizer::LabelIndex) -> normalizer::IrIndex {
        match label_index {
            normalizer::LabelIndex::Param(label_param_index) => {
                self.params[label_param_index].label.ir
            }
            normalizer::LabelIndex::Local(local_label_index) => self.locals[local_label_index].ir,
        }
    }

    fn take_synthetic_var_ident(
        &mut self,
        kind: SyntheticVarKind,
        ident: Option<Ident>,
    ) -> SyntheticVarIdent {
        let index = &mut self.next_synthetic_var_indexes[kind];
        let ident = SyntheticVarIdent {
            kind,
            ident,
            index: *index,
        };
        *index += 1;
        ident
    }
}

trait CompileExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> TaggedExpr;
}

impl CompileExpr for normalizer::Expr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> TaggedExpr {
        scoped_compiler.compile_expr(self)
    }
}

impl CompileExpr for normalizer::PureExpr {
    fn compile<'a, 'b>(&self, scoped_compiler: &mut ScopedCompiler<'a, 'b>) -> TaggedExpr {
        scoped_compiler.compile_pure_expr(self)
    }
}

// * Tagged C++ Expressions

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}", expr)]
struct TaggedExpr<T = Expr> {
    expr: T,
    type_: normalizer::TypeIndex,
    repr: TypeRepr,
    is_stable: bool,
}

impl<T> TaggedExpr<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> TaggedExpr<U> {
        TaggedExpr {
            expr: f(self.expr),
            type_: self.type_,
            repr: self.repr,
            is_stable: self.is_stable,
        }
    }
}

fn get_global_var_repr(is_mut: bool) -> TypeRepr {
    TypeRepr::Ref(if is_mut {
        // TODO(spinda): This should really be `VarRefKind::Mut`, but that's not
        // a thing quite yet.
        VarRefKind::Out
    } else {
        VarRefKind::In
    })
}

fn get_var_param_repr(var_param: &normalizer::VarParam) -> TypeRepr {
    match var_param.kind {
        VarParamKind::Value { is_mut } => match var_param.type_ {
            normalizer::TypeIndex::BuiltIn(_) => TypeRepr::Value,
            normalizer::TypeIndex::Enum(_) | normalizer::TypeIndex::Struct(_) => if is_mut {
                // TODO(spinda): Should be `VarRefKind::Mut`.
                VarRefKind::Out
            } else {
                VarRefKind::In
            }
            .into(),
        },
        VarParamKind::Ref(var_ref_kind) => var_ref_kind.into(),
    }
}
