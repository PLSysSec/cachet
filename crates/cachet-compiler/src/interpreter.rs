// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt;
use std::iter::{FromIterator, IntoIterator};
use std::ops::{Deref, DerefMut};

use iterate::iterate;

use cachet_lang::type_checker::*;

use crate::cpp;

#[derive(Clone, Debug)]
pub struct Interpreter {
    pub fwd_decls: Code,
    pub defs: Code,
}

impl Interpreter {
    pub fn generate(env: &Env) -> Interpreter {
        let (fwd_decls, defs) = Generator { env }.generate_code();
        Interpreter {
            fwd_decls: Code(fwd_decls),
            defs: Code(defs),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Code(cpp::Code);

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.0, f)
    }
}

const TYPE_NAMESPACE_IDENT_PREFIX: &'static str = "Type";
const IMPL_NAMESPACE_IDENT_PREFIX: &'static str = "Impl";

const VAL_TYPE_IDENT: &'static str = "Val";
const TO_VAL_HELPER_IDENT: &'static str = "ToVal";

const LOCAL_TYPE_IDENT: &'static str = "Local";
const EMPTY_LOCAL_HELPER_IDENT: &'static str = "EmptyLocal";
const TO_LOCAL_HELPER_IDENT: &'static str = "ToLocal";

const REF_TYPE_IDENT: &'static str = "Ref";
const TO_REF_HELPER_IDENT: &'static str = "ToRef";

const OUT_REF_TYPE_IDENT: &'static str = "OutRef";
const TO_OUT_REF_HELPER_IDENT: &'static str = "ToOutRef";
const SET_OUT_REF_HELPER_IDENT: &'static str = "SetOutRef";

const COMPARE_EQ_HELPER_IDENT: &'static str = "CompareEq";
const COMPARE_NEQ_HELPER_IDENT: &'static str = "CompareNeq";
const COMPARE_LTE_HELPER_IDENT: &'static str = "CompareLte";
const COMPARE_GTE_HELPER_IDENT: &'static str = "CompareGte";
const COMPARE_LT_HELPER_IDENT: &'static str = "CompareLt";
const COMPARE_GT_HELPER_IDENT: &'static str = "CompareGt";

const DOWNCAST_FN_IDENT_PREFIX: &'static str = "From";
const UPCAST_FN_IDENT_PREFIX: &'static str = "To";

const VARIANT_VAR_IDENT_PREFIX: &'static str = "Variant";
const CONST_VAR_IDENT_PREFIX: &'static str = "Const";

const FN_IDENT_PREFIX: &'static str = "Fn";
const OP_IDENT_PREFIX: &'static str = "Op";

const CONTEXT_TYPE_IDENT: &'static str = "Cachet_Context";
const CONTEXT_PARAM_VAR_IDENT: &'static str = "cx";

const PARAM_VAR_IDENT_PREFIX: &'static str = "param";
const LOCAL_VAR_IDENT_PREFIX: &'static str = "local";

const RESULT_VAR_IDENT_PREFIX: &'static str = "result";
const RET_VAR_IDENT_PREFIX: &'static str = "ret";
const OUT_VAR_IDENT_PREFIX: &'static str = "out";
const TEMP_VAR_IDENT_PREFIX: &'static str = "temp";

const RESULT_TYPE_IDENT: &'static str = "Cachet_Result";
const ERR_TYPE_IDENT: &'static str = "Cachet_Err";
const OK_FN_IDENT: &'static str = "Cachet_Ok";
const IS_OK_FN_IDENT: &'static str = "Cachet_IsOk";
const UNWRAP_FN_IDENT: &'static str = "Cachet_Unwrap";

const ASSERT_FN_IDENT: &'static str = "Cachet_Assert";
const BAIL_TYPE_IDENT: &'static str = "Cachet_Bail";

#[derive(Clone)]
struct DefBuckets {
    built_in_type_namespaces: BuiltInTypeMap<cpp::NamespaceDef>,
    enum_namespaces: Vec<cpp::NamespaceDef>,
    struct_namespaces: Vec<cpp::NamespaceDef>,
    top_defs: Vec<cpp::Def>,
}

impl DefBuckets {
    fn new(env: &Env, namespace_ident_prefix: &str) -> Self {
        DefBuckets {
            built_in_type_namespaces: BUILT_IN_TYPES.map(|built_in_type| cpp::NamespaceDef {
                ident: format!("{}_{}", namespace_ident_prefix, built_in_type.ident()),
                defs: Vec::new(),
            }),
            enum_namespaces: env
                .enum_defs
                .iter()
                .map(|enum_def| cpp::NamespaceDef {
                    ident: format!("{}_{}", namespace_ident_prefix, &enum_def.ident),
                    defs: Vec::new(),
                })
                .collect(),
            struct_namespaces: env
                .struct_defs
                .iter()
                .map(|struct_def| cpp::NamespaceDef {
                    ident: format!("{}_{}", namespace_ident_prefix, &struct_def.ident),
                    defs: Vec::new(),
                })
                .collect(),
            top_defs: Vec::new(),
        }
    }

    fn bucket_for(&mut self, parent_type: Option<TypeIndex>) -> &mut Vec<cpp::Def> {
        match parent_type {
            Some(parent_type) => self.bucket_for_type(parent_type),
            None => &mut self.top_defs,
        }
    }

    fn bucket_for_type(&mut self, parent_type: TypeIndex) -> &mut Vec<cpp::Def> {
        match parent_type {
            TypeIndex::BuiltIn(built_in_type) => self.bucket_for_built_in_type(built_in_type),
            TypeIndex::Enum(enum_index) => self.bucket_for_enum_type(enum_index),
            TypeIndex::Struct(struct_index) => self.bucket_for_struct_type(struct_index),
        }
    }

    fn bucket_for_built_in_type(&mut self, built_in_type: BuiltInType) -> &mut Vec<cpp::Def> {
        &mut self.built_in_type_namespaces[built_in_type].defs
    }

    fn bucket_for_enum_type(&mut self, enum_index: EnumIndex) -> &mut Vec<cpp::Def> {
        &mut self.enum_namespaces[enum_index].defs
    }

    fn bucket_for_struct_type(&mut self, struct_index: StructIndex) -> &mut Vec<cpp::Def> {
        &mut self.struct_namespaces[struct_index].defs
    }
}

impl IntoIterator for DefBuckets {
    type Item = cpp::Def;
    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let DefBuckets {
            built_in_type_namespaces,
            enum_namespaces,
            struct_namespaces,
            top_defs,
        } = self;

        iterate![
            ..iterate![
                ..built_in_type_namespaces,
                ..enum_namespaces,
                ..struct_namespaces
            ]
            .filter(|namespace_def| !namespace_def.defs.is_empty())
            .map(Into::into),
            ..top_defs
        ]
    }
}

struct Generator<'a> {
    env: &'a Env,
}

impl<'a> Generator<'a> {
    fn generate_code(&self) -> (cpp::Code, cpp::Code) {
        let mut external_fwd_decls = DefBuckets::new(self.env, IMPL_NAMESPACE_IDENT_PREFIX);
        let mut internal_fwd_decls = external_fwd_decls.clone();
        let mut internal_defs = external_fwd_decls.clone();

        for (enum_index, enum_def) in self.env.enum_defs.iter().enumerate() {
            let type_namespace_ident = generate_type_namespace_ident(&enum_def.ident.value);
            let val_type = generate_type(type_namespace_ident, TypeKind::Val);

            let variant_fwd_decls = enum_def.variants.iter().map(|variant| {
                cpp::CommentDef {
                    text: format!(
                        "inline constexpr {} {};",
                        val_type,
                        generate_variant_var_ident(&variant.value)
                    ),
                }
                .into()
            });

            external_fwd_decls
                .bucket_for_enum_type(enum_index)
                .extend(variant_fwd_decls);
        }

        for (struct_index, struct_def) in self.env.struct_defs.iter().enumerate() {
            if let Some(subtype_of) = struct_def.subtype_of {
                let type_namespace_ident = generate_type_namespace_ident(&struct_def.ident.value);

                let downcast_fn_fwd_decl = self
                    .generate_cast_fn_fwd_decl(
                        CastExprKind::Downcast,
                        type_namespace_ident.clone(),
                        subtype_of,
                    )
                    .into();

                let upcast_fn_fwd_decl = self
                    .generate_cast_fn_fwd_decl(
                        CastExprKind::Upcast,
                        type_namespace_ident.clone(),
                        subtype_of,
                    )
                    .into();

                external_fwd_decls
                    .bucket_for_struct_type(struct_index)
                    .extend([downcast_fn_fwd_decl, upcast_fn_fwd_decl]);
            }
        }

        for fn_def in &self.env.fn_defs {
            let fn_fwd_decl =
                self.generate_fn_fwd_decl(generate_fn_ident(&fn_def.sig.ident.value), &fn_def.sig);

            match &fn_def.body {
                None => {
                    external_fwd_decls
                        .bucket_for(fn_def.parent_type)
                        .push(fn_fwd_decl.into());
                }
                Some(body) => {
                    let mut cpp_fn_def = fn_fwd_decl.clone();
                    cpp_fn_def.body = Some(self.generate_fn_body(&fn_def.sig, body));

                    internal_fwd_decls
                        .bucket_for(fn_def.parent_type)
                        .push(fn_fwd_decl.into());
                    internal_defs
                        .bucket_for(fn_def.parent_type)
                        .push(cpp_fn_def.into());
                }
            }
        }

        for op_def in &self.env.op_defs {
            let op_fwd_decl = self.generate_fn_fwd_decl(
                format!("{}_{}", OP_IDENT_PREFIX, op_def.sig.ident),
                &op_def.sig,
            );

            let mut cpp_op_def = op_fwd_decl.clone();
            cpp_op_def.body = Some(self.generate_fn_body(&op_def.sig, &op_def.body));

            internal_fwd_decls.top_defs.push(op_fwd_decl.into());
            internal_defs.top_defs.push(cpp_op_def.into());
        }

        let fwd_decls = cpp::Code {
            defs: iterate![
                cpp::CommentDef {
                    text: "External forward declarations.".to_owned(),
                }
                .into(),
                ..external_fwd_decls,
                cpp::CommentDef {
                    text: "Internal forward declarations.".to_owned(),
                }
                .into(),
                ..internal_fwd_decls,
            ]
            .collect(),
        };

        let defs = cpp::Code {
            defs: iterate![
                cpp::CommentDef {
                    text: "Internal definitions.".to_owned(),
                }
                .into(),
                ..internal_defs,
            ]
            .collect(),
        };

        (fwd_decls, defs)
    }

    fn generate_cast_fn_ident(&self, kind: CastExprKind, subtype_of: TypeIndex) -> Ident {
        format!(
            "{}_{}",
            match kind {
                CastExprKind::Downcast => DOWNCAST_FN_IDENT_PREFIX,
                CastExprKind::Upcast => UPCAST_FN_IDENT_PREFIX,
            },
            self.get_type_ident(subtype_of)
        )
    }

    fn generate_cast_fn_fwd_decl(
        &self,
        kind: CastExprKind,
        child_type_namespace_ident: Ident,
        parent_type_index: TypeIndex,
    ) -> cpp::FnDef {
        let child_type = generate_type(child_type_namespace_ident, TypeKind::Val);
        let parent_type = self.generate_type(parent_type_index, TypeKind::Val);

        let (source_type, target_type) = match kind {
            CastExprKind::Downcast => (parent_type, child_type),
            CastExprKind::Upcast => (child_type, parent_type),
        };

        cpp::FnDef {
            path: cpp::Path::from_ident(self.generate_cast_fn_ident(kind, parent_type_index)),
            is_inline: true,
            params: vec![cpp::Param {
                ident: "in".to_owned(),
                type_: source_type,
            }],
            ret: target_type,
            body: None,
        }
    }

    fn generate_fn_fwd_decl(&self, ident: Ident, sig: &Sig) -> cpp::FnDef {
        let path = cpp::Path::from_ident(ident);

        let params = sig
            .param_vars
            .iter()
            .map(|param_var| self.generate_fn_param(param_var));
        let params = iterate![
            cpp::Param {
                ident: CONTEXT_PARAM_VAR_IDENT.to_owned(),
                type_: cpp::Path::from_ident(CONTEXT_TYPE_IDENT).to_owned().into()
            },
            ..params
        ]
        .collect();

        let mut ret = match sig.ret {
            // Infallible functions that return Unit instead return void when
            // translated to C++.
            TypeIndex::BuiltIn(BuiltInType::Unit) if !sig.is_fallible => cpp::Type::Void,
            _ => self.generate_type(sig.ret, TypeKind::Val),
        };
        // Fallible functions return, e.g., Result<Type_Unit::Val> or
        // Result<Type_Int32::Val> when translated to C++.
        if sig.is_fallible {
            ret = generate_result_type(ret).into();
        }

        cpp::FnDef {
            path,
            is_inline: false,
            params,
            ret,
            body: None,
        }
    }

    fn generate_fn_body(&self, sig: &Sig, body: &Body) -> cpp::Block {
        ScopedGenerator::new(self, sig, &body.local_vars).generate_body_block(&body.block)
    }

    fn generate_fn_param(&self, param_var: &ParamVar) -> cpp::Param {
        cpp::Param {
            ident: generate_param_var_ident(&param_var.ident.value),
            type_: self.generate_type(
                param_var.type_,
                if param_var.is_out {
                    TypeKind::OutRef
                } else {
                    TypeKind::Ref
                },
            ),
        }
    }

    fn get_type_ident(&self, type_index: TypeIndex) -> &str {
        match type_index {
            TypeIndex::BuiltIn(built_in_type) => built_in_type.ident(),
            TypeIndex::Enum(enum_index) => &self.env.enum_defs[enum_index].ident.value,
            TypeIndex::Struct(struct_index) => &self.env.struct_defs[struct_index].ident.value,
        }
    }

    fn generate_type_namespace_ident(&self, type_index: TypeIndex) -> Ident {
        generate_type_namespace_ident(self.get_type_ident(type_index))
    }

    fn generate_impl_namespace_ident(&self, type_index: TypeIndex) -> Ident {
        generate_impl_namespace_ident(self.get_type_ident(type_index))
    }

    fn generate_type(&self, type_index: TypeIndex, type_kind: TypeKind) -> cpp::Type {
        generate_type(self.generate_type_namespace_ident(type_index), type_kind)
    }

    fn generate_helper_call(
        &self,
        type_index: TypeIndex,
        ident: Ident,
        args: Vec<cpp::Expr>,
    ) -> cpp::CallExpr {
        cpp::CallExpr {
            target: cpp::Path::new(Some(self.generate_type_namespace_ident(type_index)), ident)
                .into(),
            args,
        }
    }

    fn convert_expr_to_kind(
        &self,
        kind: ExprKind,
        expr: TaggedExpr,
        type_: TypeIndex,
    ) -> cpp::Expr {
        if kind.accepts_tag(expr.tag) {
            expr.value
        } else {
            assert!(kind.can_convert_from_tag(expr.tag));
            self.generate_helper_call(
                type_,
                kind.convert_helper_ident().to_owned(),
                iterate![
                    ..if kind.convert_helper_needs_context() {
                        Some(
                            cpp::Path::from_ident(CONTEXT_PARAM_VAR_IDENT)
                                .to_owned()
                                .into(),
                        )
                    } else {
                        None
                    },
                    expr.value
                ]
                .collect(),
            )
            .into()
        }
    }
}

#[derive(Clone, Copy)]
enum TypeKind {
    Val,
    Local,
    Ref,
    OutRef,
}

impl TypeKind {
    fn type_ident(&self) -> &'static str {
        match self {
            TypeKind::Val => VAL_TYPE_IDENT,
            TypeKind::Local => LOCAL_TYPE_IDENT,
            TypeKind::Ref => REF_TYPE_IDENT,
            TypeKind::OutRef => OUT_REF_TYPE_IDENT,
        }
    }
}

fn generate_type_namespace_ident(ident: &str) -> Ident {
    format!("{}_{}", TYPE_NAMESPACE_IDENT_PREFIX, ident)
}

fn generate_impl_namespace_ident(ident: &str) -> Ident {
    format!("{}_{}", IMPL_NAMESPACE_IDENT_PREFIX, ident)
}

fn generate_type(type_namespace_ident: Ident, type_kind: TypeKind) -> cpp::Type {
    cpp::Path::new(
        Some(type_namespace_ident),
        type_kind.type_ident().to_owned(),
    )
    .into()
}

fn generate_result_type(inner: cpp::Type) -> cpp::TemplateType {
    cpp::TemplateType {
        inner: cpp::Path::from_ident(RESULT_TYPE_IDENT).to_owned().into(),
        args: vec![inner],
    }
}

fn generate_variant_var_ident(ident: &str) -> Ident {
    format!("{}_{}", VARIANT_VAR_IDENT_PREFIX, ident)
}

fn generate_fn_ident(ident: &str) -> Ident {
    format!("{}_{}", FN_IDENT_PREFIX, ident)
}

fn generate_param_var_ident(ident: &str) -> Ident {
    format!("{}_{}", PARAM_VAR_IDENT_PREFIX, ident)
}

struct ScopedGenerator<'a, 'b> {
    generator: &'a Generator<'b>,
    sig: &'a Sig,
    local_vars: &'a [LocalVar],
    num_synthetic_vars: usize,
}

impl<'a> Deref for ScopedGenerator<'_, 'a> {
    type Target = Generator<'a>;

    fn deref(&self) -> &Self::Target {
        self.generator
    }
}

impl<'a, 'b> ScopedGenerator<'a, 'b> {
    fn new(generator: &'a Generator<'b>, sig: &'a Sig, local_vars: &'a [LocalVar]) -> Self {
        ScopedGenerator {
            generator,
            sig,
            local_vars,
            num_synthetic_vars: 0,
        }
    }

    fn generate_body_block(&mut self, body_block: &Block) -> cpp::Block {
        let will_use_value =
            self.sig.is_fallible || body_block.type_() != TypeIndex::BuiltIn(BuiltInType::Unit);

        let (mut block, value) = self.generate_block(&body_block);
        if will_use_value {
            self.push_block_value(&mut block, value, body_block.value.type_());
        } else {
            block
                .stmts
                .push(generate_unused_value_expr(value.value).into());
        }

        if will_use_value {
            let mut expr = block.into();
            if self.sig.is_fallible {
                expr = cpp::CallExpr {
                    target: cpp::Path::from_ident(OK_FN_IDENT).to_owned().into(),
                    args: vec![expr],
                }
                .into();
            }
            cpp::Block::from(vec![cpp::RetStmt::from(expr).into()])
        } else {
            block
        }
    }

    fn generate_block(&mut self, block: &Block) -> (cpp::Block, TaggedExpr) {
        let mut stmt_expr_generator = StmtExprGenerator::new(self);
        let mut stmts = Vec::with_capacity(block.stmts.len());

        for stmt in &block.stmts {
            let stmt = stmt_expr_generator.generate_stmt(stmt);
            let deferred_stmts = stmt_expr_generator.drain_deferred_stmts();
            stmts.extend(iterate![..deferred_stmts, stmt]);
        }

        let value = stmt_expr_generator.generate_expr(&block.value);
        stmts.extend(stmt_expr_generator.drain_deferred_stmts());

        (cpp::Block { stmts }, value)
    }

    fn push_block_value(&mut self, block: &mut cpp::Block, value: TaggedExpr, type_: TypeIndex) {
        block.stmts.push(
            self.convert_expr_to_kind(ExprKind::Val, value, type_)
                .into(),
        );
    }

    fn generate_param_var_ident(&self, param_var_index: ParamVarIndex) -> Ident {
        generate_param_var_ident(&self.sig.param_vars[param_var_index].ident.value)
    }

    fn generate_synthetic_var_stmt(
        &mut self,
        synthetic_var_ident_prefix: &str,
        synthetic_var_type: cpp::Type,
        synthetic_var_init: Option<cpp::Expr>,
    ) -> cpp::VarStmt {
        let synthetic_var_index = self.num_synthetic_vars;
        self.num_synthetic_vars += 1;
        let synthetic_var_ident =
            format!("{}_{}", synthetic_var_ident_prefix, synthetic_var_index);

        cpp::VarStmt {
            ident: synthetic_var_ident,
            type_: synthetic_var_type,
            init: synthetic_var_init,
        }
    }
}

fn generate_unused_value_expr(expr: cpp::Expr) -> cpp::Expr {
    // This useless cast to void silences the compiler warning that the
    // value goes unused.
    cpp::CastExpr {
        kind: cpp::CastExprKind::CStyle,
        expr,
        type_: cpp::Type::Void,
    }
    .into()
}

struct StmtExprGenerator<'a, 'b, 'c> {
    scoped_generator: &'a mut ScopedGenerator<'b, 'c>,
    deferred_stmts: Vec<cpp::Stmt>,
}

impl<'a, 'b> Deref for StmtExprGenerator<'_, 'a, 'b> {
    type Target = ScopedGenerator<'a, 'b>;

    fn deref(&self) -> &Self::Target {
        self.scoped_generator
    }
}

impl<'a, 'b> DerefMut for StmtExprGenerator<'_, 'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.scoped_generator
    }
}

impl<'a, 'b, 'c> StmtExprGenerator<'a, 'b, 'c> {
    fn new(scoped_generator: &'a mut ScopedGenerator<'b, 'c>) -> Self {
        StmtExprGenerator {
            scoped_generator,
            deferred_stmts: Vec::new(),
        }
    }

    fn drain_deferred_stmts(&mut self) -> impl '_ + Iterator<Item = cpp::Stmt> {
        self.deferred_stmts.drain(..)
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> cpp::Stmt {
        match stmt {
            Stmt::Let(let_stmt) => self.generate_let_stmt(let_stmt).into(),
            Stmt::Check(check_stmt) => self.generate_check_stmt(check_stmt),
            // We don't care about the tags of expressions used as statements,
            // as their values go unused anyway.
            Stmt::Expr(expr) => self.generate_expr_stmt(expr).into(),
        }
    }

    fn generate_let_stmt(&mut self, let_stmt: &LetStmt) -> cpp::VarStmt {
        let local_var = &self.local_vars[let_stmt.lhs];
        cpp::VarStmt {
            ident: generate_local_var_ident(&local_var.ident, let_stmt.lhs),
            type_: self.generate_type(local_var.type_, TypeKind::Local),
            init: Some(self.generate_expr_of_kind(ExprKind::Local, &let_stmt.rhs)),
        }
    }

    fn generate_check_stmt(&mut self, check_stmt: &CheckStmt) -> cpp::Stmt {
        // The condition value is always of the built-in boolean type, so we
        // can ignore the expression tag here.
        let cond = self.generate_expr(&check_stmt.cond).value;

        match check_stmt.kind {
            CheckStmtKind::Assert => cpp::Expr::from(cpp::CallExpr {
                target: cpp::Path::from_ident(ASSERT_FN_IDENT).to_owned().into(),
                args: vec![cond],
            })
            .into(),
            CheckStmtKind::Guard => generate_guard_stmt(cond).into(),
        }
    }

    fn generate_expr_stmt(&mut self, expr: &Expr) -> cpp::ExprStmt {
        generate_unused_value_expr(self.generate_expr(expr).value).into()
    }

    fn generate_expr(&mut self, expr: &Expr) -> TaggedExpr {
        match expr {
            Expr::Block(block_expr) => self.generate_block_expr(block_expr),
            Expr::Var(var_expr) => self.generate_var_expr(var_expr).map(Into::into),
            Expr::Call(call_expr) => self.generate_call_expr(call_expr),
            Expr::Cast(cast_expr) => self.generate_cast_expr(cast_expr),
            Expr::Compare(compare_expr) => self.generate_compare_expr(compare_expr),
            Expr::Assign(assign_expr) => self.generate_assign_expr(assign_expr).map(Into::into),
        }
    }

    fn generate_expr_of_kind(&mut self, kind: ExprKind, expr: &Expr) -> cpp::Expr {
        let cpp_expr = self.generate_expr(expr);
        self.convert_expr_to_kind(kind, cpp_expr, expr.type_())
    }

    fn generate_block_expr(&mut self, block_expr: &BlockExpr) -> TaggedExpr<cpp::Expr> {
        let (mut block, value) = self.generate_block(&block_expr.block);

        if block.stmts.is_empty() {
            // Eliminate blocks that contain only a value at the end and no
            // other statements (including deferred statements resulting from
            // the value expression).
            value
        } else {
            self.push_block_value(&mut block, value, block_expr.block.value.type_());
            TaggedExpr::new(
                ExprTag::RVal {
                    is_clobberable: true,
                },
                block.into(),
            )
        }
    }

    fn generate_var_expr(&mut self, var_expr: &VarExpr) -> TaggedExpr<cpp::Path> {
        match var_expr.index {
            VarIndex::BuiltIn(built_in_var) => {
                TaggedExpr::new(ExprTag::LVal, generate_built_in_var_path(built_in_var))
            }
            VarIndex::EnumVariant(enum_variant_index) => {
                let enum_def = &self.env.enum_defs[enum_variant_index.enum_index];
                TaggedExpr::new(
                    ExprTag::LVal,
                    cpp::Path::new(
                        Some(generate_impl_namespace_ident(&enum_def.ident.value)),
                        generate_variant_var_ident(
                            &enum_def.variants[enum_variant_index.variant_index].value,
                        ),
                    ),
                )
            }
            VarIndex::Const(const_index) => {
                let const_def = &self.env.const_defs[const_index];
                TaggedExpr::new(
                    ExprTag::LVal,
                    cpp::Path::new(
                        const_def
                            .parent_type
                            .map(|parent_type| self.generate_impl_namespace_ident(parent_type)),
                        generate_const_var_ident(&const_def.ident.value),
                    ),
                )
            }
            VarIndex::ScopedVar(scoped_var_index) => match scoped_var_index {
                ScopedVarIndex::ParamVar(param_var_index) => TaggedExpr::new(
                    ExprTag::LRef,
                    cpp::Path::from_ident(self.generate_param_var_ident(param_var_index)),
                ),
                ScopedVarIndex::LocalVar(local_var_index) => TaggedExpr::new(
                    ExprTag::LLocal,
                    cpp::Path::from_ident(generate_local_var_ident(
                        &self.local_vars[local_var_index].ident,
                        local_var_index,
                    )),
                ),
            },
        }
    }

    fn generate_call_expr(&mut self, call_expr: &CallExpr) -> TaggedExpr {
        const CALL_EXPR_TAG: ExprTag = ExprTag::RVal {
            is_clobberable: true,
        };

        let target_fn_def = &self.env.fn_defs[call_expr.target];
        let target = cpp::Path::new(
            target_fn_def
                .parent_type
                .map(|parent_type| self.generate_impl_namespace_ident(parent_type)),
            generate_fn_ident(&target_fn_def.sig.ident.value),
        )
        .into();

        let mut expr_scaffold = ExprScaffold::new(CALL_EXPR_TAG);
        let mut deferred_pre_call_stmts = Vec::new();
        let mut deferred_post_call_stmts = Vec::new();
        let args = call_expr.args.iter().map(|arg| {
            self.generate_call_expr_arg(
                arg,
                &mut expr_scaffold,
                &mut deferred_pre_call_stmts,
                &mut deferred_post_call_stmts,
            )
        });
        let args = iterate![
            cpp::Path::from_ident(CONTEXT_PARAM_VAR_IDENT)
                .to_owned()
                .into(),
            ..args
        ]
        .collect();
        let cpp_call_expr = expr_scaffold
            .build(cpp::CallExpr { target, args }.into())
            .value;

        let cpp_call_expr = if call_expr.is_fallible {
            // Fallible functions translated to C++ return, e.g.,
            // Result<Type_Unit::Val> or Result<Type_Int32::Val>. We need to
            // unwrap the Result to get at the actual value, and also insert a
            // guard if the call expression is itself inside a fallible
            // function.

            if self.sig.is_fallible {
                // If the call expression is inside a fallible function, we save
                // the return value to a local, guard on the local, and then
                // unwrap it.

                let result_var_type =
                    generate_result_type(self.generate_type(call_expr.type_(), TypeKind::Val))
                        .into();

                let result_var_stmt = self.generate_synthetic_var_stmt(
                    RESULT_VAR_IDENT_PREFIX,
                    result_var_type,
                    Some(cpp_call_expr),
                );

                let result_var_expr =
                    cpp::Expr::from(cpp::Path::from_ident(result_var_stmt.ident.clone()));

                let result_guard_stmt = generate_guard_stmt(
                    cpp::CallExpr {
                        target: cpp::Path::from_ident(IS_OK_FN_IDENT).to_owned().into(),
                        args: vec![result_var_expr.clone()],
                    }
                    .into(),
                );

                let result_unwrap_expr = cpp::Expr::from(generate_unwrap_call(
                    cpp::CastExpr {
                        kind: cpp::FunctionalCastExprKind::Static.into(),
                        expr: result_var_expr,
                        type_: cpp::RefType {
                            inner: result_var_stmt.type_.clone(),
                            value_category: cpp::ValueCategory::RValue,
                        }
                        .into(),
                    }
                    .into(),
                ));

                cpp::Block::from(vec![
                    result_var_stmt.into(),
                    result_guard_stmt.into(),
                    result_unwrap_expr.into(),
                ])
                .into()
            } else {
                // Otherwise, we simply unwrap the return value directly.
                generate_unwrap_call(cpp_call_expr).into()
            }
        } else if let TypeIndex::BuiltIn(BuiltInType::Unit) = target_fn_def.sig.ret {
            // Infallible calls that return Unit in Cachet instead return
            // void in C++, requiring adapting when the call expression is
            // used as a value.
            cpp::CommaExpr {
                // Even though the left-hand side of the comma expression is
                // already of type void, this redundant cast is necessary to
                // silence a compiler warning.
                lhs: generate_unused_value_expr(cpp_call_expr),
                rhs: generate_built_in_var_path(BuiltInVar::Unit).into(),
            }
            .into()
        } else {
            cpp_call_expr
        };

        let cpp_call_expr = if !deferred_pre_call_stmts.is_empty()
            || !deferred_pre_call_stmts.is_empty()
        {
            let ret_var_type = self.generate_type(call_expr.type_(), TypeKind::Val);

            let ret_var_stmt = self.generate_synthetic_var_stmt(
                RET_VAR_IDENT_PREFIX,
                ret_var_type,
                Some(cpp_call_expr),
            );

            let ret_var_expr = cpp::Expr::from(cpp::Path::from_ident(ret_var_stmt.ident.clone()));

            cpp::Block {
                stmts: iterate![
                    ..deferred_pre_call_stmts,
                    ret_var_stmt.into(),
                    ..deferred_post_call_stmts,
                    ret_var_expr.into(),
                ]
                .collect(),
            }
            .into()
        } else {
            cpp_call_expr
        };

        TaggedExpr::new(CALL_EXPR_TAG, cpp_call_expr)
    }

    fn generate_call_expr_arg(
        &mut self,
        arg: &CallExprArg,
        expr_scaffold: &mut ExprScaffold,
        deferred_pre_call_stmts: &mut Vec<cpp::Stmt>,
        deferred_post_call_stmts: &mut Vec<cpp::Stmt>,
    ) -> cpp::Expr {
        match arg {
            CallExprArg::Expr(expr) => expr_scaffold.push_intermediate(self, ExprKind::Ref, expr),
            CallExprArg::OutRef(out_ref) => self.generate_out_ref_call_expr_arg(
                out_ref,
                deferred_pre_call_stmts,
                deferred_post_call_stmts,
            ),
        }
    }

    fn generate_out_ref_call_expr_arg(
        &mut self,
        out_ref: &OutRef,
        deferred_pre_call_stmts: &mut Vec<cpp::Stmt>,
        deferred_post_call_stmts: &mut Vec<cpp::Stmt>,
    ) -> cpp::Expr {
        let out_ref_expr = match out_ref.index {
            ScopedVarIndex::ParamVar(param_var_index) => {
                cpp::Path::from_ident(self.generate_param_var_ident(param_var_index)).into()
            }
            ScopedVarIndex::LocalVar(local_var_index) => {
                let local_var = &self.local_vars[local_var_index];
                let local_var_ident = generate_local_var_ident(&local_var.ident, local_var_index);
                let local_var_type_namespace_ident =
                    self.generate_type_namespace_ident(local_var.type_);

                self.deferred_stmts.push(
                    cpp::VarStmt {
                        ident: local_var_ident.clone(),
                        type_: generate_type(
                            local_var_type_namespace_ident.clone(),
                            TypeKind::Local,
                        ),
                        init: Some(
                            generate_empty_local_var_init(local_var_type_namespace_ident.clone())
                                .into(),
                        ),
                    }
                    .into(),
                );

                self.generate_out_ref_expr_from_local_var(
                    local_var_ident,
                    local_var_type_namespace_ident,
                )
                .into()
            }
        };

        match out_ref.upcast_route.last().copied() {
            None => out_ref_expr,
            Some(target_type_index) => {
                let source_type_index = out_ref.type_;

                // The type of the out-parameter is a subtype of the type of
                // the out-ref we're passing into it. We need to use an
                // intermediate variable to capture the out-value, and then
                // assign to the original out-ref the result of upcasting
                // the intermediate.

                let out_var_type_namespace_ident =
                    self.generate_type_namespace_ident(source_type_index);

                let out_var_stmt = self.generate_synthetic_var_stmt(
                    OUT_VAR_IDENT_PREFIX,
                    generate_type(out_var_type_namespace_ident.clone(), TypeKind::Local),
                    Some(
                        generate_empty_local_var_init(out_var_type_namespace_ident.clone()).into(),
                    ),
                );

                let out_var_ident = out_var_stmt.ident.clone();

                deferred_pre_call_stmts.push(out_var_stmt.into());

                let out_var_expr = self.convert_expr_to_kind(
                    ExprKind::Val,
                    TaggedExpr::new(
                        ExprTag::LLocal,
                        cpp::Expr::from(cpp::Path::from_ident(out_var_ident.clone())),
                    ),
                    source_type_index,
                );

                let out_var_upcast_pairs =
                    iterate![source_type_index, ..out_ref.upcast_route.iter().copied()]
                        .zip(out_ref.upcast_route.iter().copied());

                let out_var_upcast_expr =
                    out_var_upcast_pairs.fold(out_var_expr, |expr, (from, to)| {
                        self.generate_cpp_cast_expr(CastExprKind::Upcast, expr, from, to)
                            .into()
                    });

                deferred_post_call_stmts.push(
                    cpp::Expr::from(self.generate_out_ref_set_expr(
                        out_ref_expr,
                        target_type_index,
                        out_var_upcast_expr,
                    ))
                    .into(),
                );

                self.generate_out_ref_expr_from_local_var(
                    out_var_ident,
                    out_var_type_namespace_ident,
                )
                .into()
            }
        }
    }

    fn generate_out_ref_expr_from_local_var(
        &mut self,
        local_var_ident: Ident,
        local_var_type_namespace_ident: Ident,
    ) -> cpp::CallExpr {
        cpp::CallExpr {
            target: cpp::Path::new(
                Some(local_var_type_namespace_ident),
                TO_OUT_REF_HELPER_IDENT.to_owned(),
            )
            .into(),
            args: vec![cpp::Path::from_ident(local_var_ident).into()],
        }
    }

    fn generate_out_ref_set_expr(
        &mut self,
        out_ref_expr: cpp::Expr,
        out_ref_type: TypeIndex,
        rhs_expr: cpp::Expr,
    ) -> cpp::CallExpr {
        self.generate_helper_call(
            out_ref_type,
            SET_OUT_REF_HELPER_IDENT.to_owned(),
            vec![out_ref_expr, rhs_expr],
        )
    }

    fn generate_cast_expr(&mut self, cast_expr: &CastExpr) -> TaggedExpr {
        let mut expr_scaffold = ExprScaffold::new(ExprTag::RVal {
            is_clobberable: false,
        });
        let expr = expr_scaffold.push_intermediate(self, ExprKind::Val, &cast_expr.expr);

        let source_type = cast_expr.expr.type_();
        let target_type = cast_expr.type_;

        expr_scaffold.build(
            self.generate_cpp_cast_expr(cast_expr.kind, expr, source_type, target_type)
                .into(),
        )
    }

    fn generate_cpp_cast_expr(
        &mut self,
        kind: CastExprKind,
        expr: cpp::Expr,
        source_type: TypeIndex,
        target_type: TypeIndex,
    ) -> cpp::CallExpr {
        let (parent_type, child_type) = match kind {
            CastExprKind::Downcast => (source_type, target_type),
            CastExprKind::Upcast => (target_type, source_type),
        };

        cpp::CallExpr {
            target: cpp::Path::new(
                Some(self.generate_impl_namespace_ident(child_type)),
                self.generate_cast_fn_ident(kind, parent_type),
            )
            .into(),
            args: vec![expr],
        }
    }

    fn generate_compare_expr(&mut self, compare_expr: &CompareExpr) -> TaggedExpr {
        let mut expr_scaffold = ExprScaffold::new(ExprTag::RVal {
            is_clobberable: false,
        });
        let lhs = expr_scaffold.push_intermediate(self, ExprKind::Val, &compare_expr.lhs);
        let rhs = expr_scaffold.push_intermediate(self, ExprKind::Val, &compare_expr.rhs);
        expr_scaffold.build(
            self.generate_helper_call(
                compare_expr.lhs.type_(),
                match compare_expr.kind {
                    CompareExprKind::Eq => COMPARE_EQ_HELPER_IDENT,
                    CompareExprKind::Neq => COMPARE_NEQ_HELPER_IDENT,
                    CompareExprKind::Lte => COMPARE_LTE_HELPER_IDENT,
                    CompareExprKind::Gte => COMPARE_GTE_HELPER_IDENT,
                    CompareExprKind::Lt => COMPARE_LT_HELPER_IDENT,
                    CompareExprKind::Gt => COMPARE_GT_HELPER_IDENT,
                }
                .to_owned(),
                vec![lhs, rhs],
            )
            .into(),
        )
    }

    fn generate_assign_expr(&mut self, assign_expr: &AssignExpr) -> TaggedExpr<cpp::CallExpr> {
        let param_var = &self.sig.param_vars[assign_expr.lhs];
        let rhs_expr = self.generate_expr(&assign_expr.rhs);
        TaggedExpr::new(
            ExprTag::RVal {
                is_clobberable: rhs_expr.tag.is_clobberable(),
            },
            self.generate_out_ref_set_expr(
                cpp::Path::from_ident(generate_param_var_ident(&param_var.ident.value)).into(),
                param_var.type_,
                // The SetOutRef helper should be able to accept expressions
                // with any tag, so we don't need to do any kind/tag
                // conversion here.
                rhs_expr.value,
            ),
        )
    }
}

fn generate_built_in_var_path(built_in_var: BuiltInVar) -> cpp::Path {
    cpp::Path::new(None, generate_const_var_ident(built_in_var.ident()))
}

fn generate_const_var_ident(ident: &str) -> Ident {
    format!("{}_{}", CONST_VAR_IDENT_PREFIX, ident)
}

fn generate_local_var_ident(ident: &str, index: LocalVarIndex) -> Ident {
    format!("{}_{}_{}", LOCAL_VAR_IDENT_PREFIX, ident, index)
}

fn generate_unwrap_call(result_expr: cpp::Expr) -> cpp::CallExpr {
    cpp::CallExpr {
        target: cpp::Path::from_ident(UNWRAP_FN_IDENT).to_owned().into(),
        args: vec![result_expr],
    }
}

fn generate_guard_stmt(cond: cpp::Expr) -> cpp::IfStmt {
    cpp::IfStmt {
        cond: cpp::UnaryExpr {
            kind: cpp::UnaryExprKind::BoolNot,
            inner: cond,
        }
        .into(),
        body: cpp::Block::from(vec![
            cpp::RetStmt::from(cpp::Expr::from(cpp::CallExpr {
                target: cpp::Path::from_ident(ERR_TYPE_IDENT).to_owned().into(),
                args: vec![
                    cpp::CallExpr {
                        target: cpp::Path::from_ident(BAIL_TYPE_IDENT).to_owned().into(),
                        args: Vec::new(),
                    }
                    .into(),
                ],
            }))
            .into(),
        ])
        .into(),
    }
}

fn generate_empty_local_var_init(type_namespace_ident: Ident) -> cpp::CallExpr {
    cpp::CallExpr {
        target: cpp::Path::new(
            Some(type_namespace_ident.clone()),
            EMPTY_LOCAL_HELPER_IDENT.to_owned(),
        )
        .into(),
        args: vec![
            cpp::Path::from_ident(CONTEXT_PARAM_VAR_IDENT)
                .to_owned()
                .into(),
        ],
    }
}

#[derive(Clone, Copy)]
enum ExprTag {
    LVal,
    RVal { is_clobberable: bool },
    LLocal,
    LRef,
}

impl ExprTag {
    fn is_clobberable(self) -> bool {
        match self {
            ExprTag::RVal { is_clobberable } => is_clobberable,
            ExprTag::LVal | ExprTag::LLocal | ExprTag::LRef => false,
        }
    }

    fn mark_clobberable(&mut self) {
        match self {
            ExprTag::RVal { is_clobberable } => {
                *is_clobberable = true;
            }
            ExprTag::LVal | ExprTag::LLocal | ExprTag::LRef => (),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ExprKind {
    Val,
    Local,
    Ref,
}

impl ExprKind {
    const fn convert_helper_ident(self) -> &'static str {
        match self {
            ExprKind::Val => TO_VAL_HELPER_IDENT,
            ExprKind::Local => TO_LOCAL_HELPER_IDENT,
            ExprKind::Ref => TO_REF_HELPER_IDENT,
        }
    }

    const fn convert_helper_needs_context(self) -> bool {
        match self {
            ExprKind::Local => true,
            ExprKind::Val | ExprKind::Ref => false,
        }
    }

    const fn accepts_tag(self, tag: ExprTag) -> bool {
        match (self, tag) {
            (ExprKind::Val, ExprTag::LVal | ExprTag::RVal { .. }) => true,
            (ExprKind::Ref, ExprTag::LRef) => true,
            _ => false,
        }
    }

    const fn can_convert_from_tag(self, tag: ExprTag) -> bool {
        match (self, tag) {
            (ExprKind::Val, ExprTag::LLocal) => true,
            (ExprKind::Val, ExprTag::LRef) => true,
            (ExprKind::Local, ExprTag::LVal | ExprTag::RVal { .. }) => true,
            (ExprKind::Local, ExprTag::LLocal) => true,
            (ExprKind::Local, ExprTag::LRef) => true,
            (ExprKind::Ref, ExprTag::LVal) => true,
            (ExprKind::Ref, ExprTag::LLocal) => true,
            _ => false,
        }
    }

    const fn requires_temp_for_tag(self, tag: ExprTag) -> bool {
        match (self, tag) {
            (ExprKind::Ref, ExprTag::RVal { .. }) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy)]
struct TaggedExpr<T = cpp::Expr> {
    tag: ExprTag,
    value: T,
}

impl<T> TaggedExpr<T> {
    fn new(tag: ExprTag, value: T) -> Self {
        TaggedExpr { tag, value }
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> TaggedExpr<U> {
        TaggedExpr {
            tag: self.tag,
            value: f(self.value),
        }
    }
}

struct ExprScaffold {
    tag: ExprTag,
    has_undeferred_clobberable_intermediate: bool,
    deferred_temp_var_stmts: Vec<cpp::VarStmt>,
}

impl ExprScaffold {
    fn new(tag: ExprTag) -> Self {
        ExprScaffold {
            tag,
            has_undeferred_clobberable_intermediate: false,
            deferred_temp_var_stmts: Vec::new(),
        }
    }

    fn push_intermediate<'a, 'b, 'c>(
        &mut self,
        stmt_expr_generator: &mut StmtExprGenerator<'a, 'b, 'c>,
        kind: ExprKind,
        expr: &Expr,
    ) -> cpp::Expr {
        let cpp_expr = stmt_expr_generator.generate_expr(expr);
        let expr_type = expr.type_();

        let is_clobberable = cpp_expr.tag.is_clobberable();
        if is_clobberable {
            self.tag.mark_clobberable();
        }

        let must_use_temp = kind.requires_temp_for_tag(cpp_expr.tag)
            || (is_clobberable && self.has_undeferred_clobberable_intermediate);
        let cpp_expr = if must_use_temp {
            let temp_var_init =
                stmt_expr_generator.convert_expr_to_kind(ExprKind::Local, cpp_expr, expr_type);

            let temp_var_type = stmt_expr_generator.generate_type(expr_type, TypeKind::Local);

            let temp_var_stmt = stmt_expr_generator.generate_synthetic_var_stmt(
                TEMP_VAR_IDENT_PREFIX,
                temp_var_type,
                Some(temp_var_init),
            );

            let temp_var_expr =
                cpp::Expr::from(cpp::Path::from_ident(temp_var_stmt.ident.clone()));

            self.deferred_temp_var_stmts.push(temp_var_stmt);

            TaggedExpr::new(ExprTag::LLocal, temp_var_expr)
        } else {
            self.has_undeferred_clobberable_intermediate =
                self.has_undeferred_clobberable_intermediate || is_clobberable;
            cpp_expr
        };

        stmt_expr_generator.convert_expr_to_kind(kind, cpp_expr, expr_type)
    }

    fn build(self, expr: cpp::Expr) -> TaggedExpr {
        TaggedExpr::new(
            self.tag,
            if self.deferred_temp_var_stmts.is_empty() {
                expr
            } else {
                cpp::Block::from_iter(iterate![
                    ..self.deferred_temp_var_stmts.into_iter().map(Into::into),
                    cpp::Stmt::from(expr)
                ])
                .into()
            },
        )
    }
}
