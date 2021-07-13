// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;

use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

use codespan::Span;
use enumset::EnumSet;

use crate::frontend::parser::{self, Path};
use crate::frontend::FrontendError;

pub use crate::frontend::resolver::ast::*;
pub use crate::frontend::resolver::error::*;

pub fn resolve(spec: parser::Spec) -> Result<Env, ResolveErrors> {
    let mut registry = Registry::new();
    registry.register_spec(spec);

    let mut resolver = Resolver::new(
        registry.errors,
        registry.items,
        registry.impl_def_parent_types,
    );

    let enum_defs = registry.enum_defs;

    let struct_defs: Option<_> = collect_eager(
        registry
            .struct_defs
            .into_iter()
            .map(|struct_def| resolver.resolve_struct_def(struct_def)),
    );

    let const_defs: Option<_> = collect_eager(
        registry
            .const_defs
            .into_iter()
            .map(|const_def| resolver.resolve_const_def(const_def)),
    );

    let fn_defs: Option<_> = collect_eager(
        registry
            .fn_defs
            .into_iter()
            .map(|fn_def| resolver.resolve_fn_def(fn_def)),
    );

    let op_defs: Option<_> = collect_eager(
        registry
            .op_defs
            .into_iter()
            .map(|op_def| resolver.resolve_op_def(op_def)),
    );

    if resolver.errors.is_empty() {
        Ok(Env {
            enum_defs,
            struct_defs: struct_defs.unwrap(),
            const_defs: const_defs.unwrap(),
            fn_defs: fn_defs.unwrap(),
            op_defs: op_defs.unwrap(),
        })
    } else {
        resolver.errors.sort_by_key(|error| error.span());
        Err(ResolveErrors(resolver.errors))
    }
}

struct RegisteredDef<T> {
    def: T,
    impl_def_index: Option<usize>,
}

struct RegisteredItem {
    first_defined_at: Option<Span>,
    status: RegisteredItemStatus,
}

enum RegisteredItemStatus {
    DefinedOnce(ItemIndex),
    Duplicated(EnumSet<ItemKind>),
}

#[derive(Clone, Copy)]
enum ItemIndex {
    Type(TypeIndex),
    Fn(FnIndex),
    Var(VarIndex),
    Op(OpIndex),
}

impl ItemIndex {
    fn kind(&self) -> ItemKind {
        match self {
            ItemIndex::Type(_) => ItemKind::Type,
            ItemIndex::Fn(_) => ItemKind::Fn,
            ItemIndex::Var(_) => ItemKind::Var,
            ItemIndex::Op(_) => ItemKind::Op,
        }
    }
}

struct Registry {
    errors: Vec<ResolveError>,
    items: HashMap<Path, RegisteredItem>,
    enum_defs: Vec<parser::EnumDef>,
    struct_defs: Vec<parser::StructDef>,
    const_defs: Vec<RegisteredDef<parser::ConstDef>>,
    fn_defs: Vec<RegisteredDef<parser::FnDef>>,
    op_defs: Vec<parser::OpDef>,
    impl_def_parent_types: Vec<Spanned<Path>>,
}

impl Registry {
    fn new() -> Self {
        let mut items = HashMap::with_capacity(BUILT_IN_TYPES.len() + BUILT_IN_VARS.len());
        for built_in_type in BUILT_IN_TYPES {
            items.insert(
                Path::from_ident(built_in_type.ident().into()),
                RegisteredItem {
                    first_defined_at: None,
                    status: RegisteredItemStatus::DefinedOnce(ItemIndex::Type(
                        built_in_type.into(),
                    )),
                },
            );
        }
        for built_in_var in BUILT_IN_VARS {
            items.insert(
                Path::from_ident(built_in_var.ident().into()),
                RegisteredItem {
                    first_defined_at: None,
                    status: RegisteredItemStatus::DefinedOnce(ItemIndex::Var(built_in_var.into())),
                },
            );
        }

        Registry {
            errors: Vec::new(),
            items,
            enum_defs: Vec::new(),
            struct_defs: Vec::new(),
            const_defs: Vec::new(),
            fn_defs: Vec::new(),
            op_defs: Vec::new(),
            impl_def_parent_types: Vec::new(),
        }
    }

    fn register_spec(&mut self, spec: parser::Spec) {
        for top_def in spec {
            self.register_top_def(top_def);
        }
    }

    fn register_top_def(&mut self, top_def: parser::TopDef) {
        match top_def {
            parser::TopDef::Enum(enum_def) => {
                let enum_index = self.enum_defs.len();
                self.register_item(
                    None,
                    enum_def.ident.clone(),
                    ItemIndex::Type(TypeIndex::Enum(enum_index)),
                );
                for (variant_index, variant) in enum_def.variants.iter().enumerate() {
                    self.register_item(
                        Some(enum_def.ident.clone()),
                        variant.clone(),
                        ItemIndex::Var(VarIndex::EnumVariant(EnumVariantIndex {
                            enum_index,
                            variant_index,
                        })),
                    );
                }
                self.enum_defs.push(enum_def);
            }
            parser::TopDef::Struct(struct_def) => {
                self.register_item(
                    None,
                    struct_def.ident.clone(),
                    ItemIndex::Type(TypeIndex::Struct(self.struct_defs.len())),
                );
                self.struct_defs.push(struct_def);
            }
            parser::TopDef::Impl(impl_def) => {
                // The nested defs will only be accessible if the path supplied
                // for the parent type is valid, i.e., it doesn't have its own
                // parent type specified.
                if impl_def.parent_type.value.parent_type.is_none() {
                    let impl_def_index = self.impl_def_parent_types.len();
                    let parent_type = impl_def
                        .parent_type
                        .as_ref()
                        .map(|parent_type| &parent_type.ident);
                    for def in impl_def.defs {
                        self.register_def(def, Some((impl_def_index, parent_type.cloned())));
                    }
                }

                self.impl_def_parent_types.push(impl_def.parent_type);
            }
            parser::TopDef::Def(def) => {
                self.register_def(def, None);
            }
            parser::TopDef::Op(op_def) => {
                self.register_item(
                    None,
                    op_def.sig.ident.clone(),
                    ItemIndex::Op(self.op_defs.len()),
                );
                self.op_defs.push(op_def);
            }
        }
    }

    fn register_def(&mut self, def: parser::Def, impl_info: Option<(usize, Spanned<Ident>)>) {
        let (impl_def_index, parent_type) = match impl_info {
            Some((impl_def_index, parent_type)) => (Some(impl_def_index), Some(parent_type)),
            None => (None, None),
        };

        match def {
            parser::Def::Const(const_def) => {
                self.register_item(
                    parent_type,
                    const_def.ident.clone(),
                    ItemIndex::Var(VarIndex::Const(self.const_defs.len())),
                );
                self.const_defs.push(RegisteredDef {
                    def: const_def,
                    impl_def_index,
                });
            }
            parser::Def::Fn(fn_def) => {
                self.register_item(
                    parent_type,
                    fn_def.sig.ident.clone(),
                    ItemIndex::Fn(self.fn_defs.len()),
                );
                self.fn_defs.push(RegisteredDef {
                    def: fn_def,
                    impl_def_index,
                });
            }
        }
    }

    fn register_item(
        &mut self,
        parent_type: Option<Spanned<Ident>>,
        ident: Spanned<Ident>,
        index: ItemIndex,
    ) {
        let parent_type_span = parent_type.as_ref().map(|parent_type| parent_type.span);
        let ident_span = ident.span;
        let path = Path::new(
            parent_type.map(|parent_type| parent_type.value),
            ident.value,
        );
        match self.items.entry(path) {
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(RegisteredItem {
                    first_defined_at: Some(ident_span),
                    status: RegisteredItemStatus::DefinedOnce(index),
                });
            }
            Entry::Occupied(mut occupied_entry) => {
                let Path { parent_type, ident } = occupied_entry.key().clone();
                self.errors.push(ResolveError::DuplicateDef {
                    parent_type: parent_type
                        .map(|parent_type| Spanned::new(parent_type_span.unwrap(), parent_type)),
                    ident: Spanned::new(ident_span, ident),
                    first_defined_at: occupied_entry.get().first_defined_at,
                });

                let status = &mut occupied_entry.get_mut().status;
                match status {
                    RegisteredItemStatus::DefinedOnce(registered_index) => {
                        *status = RegisteredItemStatus::Duplicated(
                            registered_index.kind() | index.kind(),
                        );
                    }
                    RegisteredItemStatus::Duplicated(kinds) => {
                        kinds.insert(index.kind());
                    }
                }
            }
        }
    }
}

enum LookupResult<T> {
    Found {
        path: Spanned<Path>,
        defined_at: Option<Span>,
        index: Option<T>,
    },
    Failure,
}

impl<T> LookupResult<T> {
    fn found(self) -> Option<T> {
        match self {
            LookupResult::Found { index, .. } => index,
            _ => None,
        }
    }

    fn found_spanned(self) -> Option<Spanned<T>> {
        match self {
            LookupResult::Found { path, index, .. } => {
                index.map(|index| Spanned::new(path.span, index))
            }
            _ => None,
        }
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> LookupResult<U> {
        match self {
            LookupResult::Found {
                path,
                defined_at,
                index,
            } => LookupResult::Found {
                path,
                defined_at,
                index: index.map(f),
            },
            LookupResult::Failure => LookupResult::Failure,
        }
    }
}

struct Resolver {
    errors: Vec<ResolveError>,
    items: HashMap<Path, RegisteredItem>,
    impl_def_parent_types: Vec<Option<TypeIndex>>,
}

impl Resolver {
    fn new(
        errors: Vec<ResolveError>,
        items: HashMap<Path, RegisteredItem>,
        impl_def_parent_types: Vec<Spanned<Path>>,
    ) -> Self {
        let mut resolver = Resolver {
            errors,
            items,
            impl_def_parent_types: Vec::with_capacity(impl_def_parent_types.len()),
        };

        for parent_type in impl_def_parent_types {
            let parent_type = resolver.lookup_type_global(parent_type).found();
            resolver.impl_def_parent_types.push(parent_type);
        }

        resolver
    }

    fn resolve_struct_def(&mut self, struct_def: parser::StructDef) -> Option<StructDef> {
        let subtype_of = match struct_def.subtype_of {
            None => Some(None),
            Some(subtype_of) => self.lookup_type_global(subtype_of).found().map(Some),
        };

        Some(StructDef {
            ident: struct_def.ident,
            subtype_of: subtype_of?,
        })
    }

    fn resolve_const_def(
        &mut self,
        registered_const_def: RegisteredDef<parser::ConstDef>,
    ) -> Option<ConstDef> {
        let (const_def, parent_type) = self.resolve_registered_def(registered_const_def);

        let type_ = self.lookup_type_global(const_def.type_).found();

        Some(ConstDef {
            ident: const_def.ident,
            parent_type: parent_type?,
            type_: type_?,
        })
    }

    fn resolve_fn_def(
        &mut self,
        registered_fn_def: RegisteredDef<parser::FnDef>,
    ) -> Option<FnDef> {
        let (fn_def, parent_type) = self.resolve_registered_def(registered_fn_def);

        let body = match fn_def.body {
            None => Some(None),
            Some(body) => self
                .resolve_body(body, fn_def.sig.param_vars.as_ref())
                .map(Some),
        };

        let sig = self.resolve_sig(fn_def.sig);

        Some(FnDef {
            sig: sig?,
            parent_type: parent_type?,
            is_unsafe: fn_def.is_unsafe,
            body: body?,
        })
    }

    fn resolve_op_def(&mut self, op_def: parser::OpDef) -> Option<OpDef> {
        let body = self.resolve_body(op_def.body, op_def.sig.param_vars.as_ref());

        let sig = self.resolve_sig(op_def.sig);

        Some(OpDef {
            sig: sig?,
            body: body?,
        })
    }

    fn resolve_sig(&mut self, sig: parser::Sig) -> Option<Sig> {
        let param_vars = self.resolve_param_vars(sig.param_vars);

        let ret = self.resolve_fn_ret(sig.ret);

        Some(Sig {
            ident: sig.ident,
            is_fallible: sig.is_fallible,
            param_vars: param_vars?,
            ret: ret?,
        })
    }

    fn resolve_registered_def<T>(
        &self,
        registered_def: RegisteredDef<T>,
    ) -> (T, Option<Option<TypeIndex>>) {
        (
            registered_def.def,
            match registered_def.impl_def_index {
                None => Some(None),
                Some(impl_def_index) => self.impl_def_parent_types[impl_def_index].map(Some),
            },
        )
    }

    fn resolve_body(
        &mut self,
        body: Spanned<parser::Block>,
        param_vars: &[parser::ParamVar],
    ) -> Option<Body> {
        let mut local_vars = Vec::new();

        let mut scoped_resolver = ScopedResolver {
            resolver: self,
            local_vars: &mut local_vars,
            scoped_vars: Cow::Owned(
                param_vars
                    .iter()
                    .enumerate()
                    .map(|(index, param_var)| {
                        (
                            param_var.ident.value.clone(),
                            Spanned::new(param_var.ident.span, ScopedVarIndex::ParamVar(index)),
                        )
                    })
                    .collect(),
            ),
        };

        let block = map_spanned(body, |body| scoped_resolver.resolve_block(body));

        Some(Body {
            local_vars,
            block: block?,
        })
    }

    fn resolve_param_vars(&mut self, param_vars: Vec<parser::ParamVar>) -> Option<Vec<ParamVar>> {
        collect_eager(
            param_vars
                .into_iter()
                .map(|param_var| self.resolve_param_var(param_var)),
        )
    }

    fn resolve_param_var(&mut self, param_var: parser::ParamVar) -> Option<ParamVar> {
        let type_ = self.lookup_type_global(param_var.type_).found();

        Some(ParamVar {
            ident: param_var.ident,
            is_out: param_var.is_out,
            type_: type_?,
        })
    }

    fn resolve_fn_ret(&mut self, ret: Option<Spanned<Path>>) -> Option<TypeIndex> {
        match ret {
            None => Some(BuiltInType::Unit.into()),
            Some(ret) => self.lookup_type_global(ret).found(),
        }
    }

    fn lookup_type_global(&mut self, path: Spanned<Path>) -> LookupResult<TypeIndex> {
        self.lookup_global(path, ItemKind::Type.into())
            .map(|index| match index {
                ItemIndex::Type(index) => index,
                _ => unreachable!(),
            })
    }

    fn lookup_fn_global(&mut self, path: Spanned<Path>) -> LookupResult<FnIndex> {
        self.lookup_global(path, ItemKind::Fn.into())
            .map(|index| match index {
                ItemIndex::Fn(index) => index,
                _ => unreachable!(),
            })
    }

    fn lookup_var_global(&mut self, path: Spanned<Path>) -> LookupResult<VarIndex> {
        self.lookup_global(path, ItemKind::Var.into())
            .map(|index| match index {
                ItemIndex::Var(index) => index,
                _ => unreachable!(),
            })
    }

    fn lookup_global(
        &mut self,
        path: Spanned<Path>,
        expected: EnumSet<ItemKind>,
    ) -> LookupResult<ItemIndex> {
        let registered_item = match self.items.get(&path.value) {
            Some(registered_item) => registered_item,
            None => {
                self.errors.push(ResolveError::Undefined { path, expected });
                return LookupResult::Failure;
            }
        };

        let (index, found) = match registered_item.status {
            RegisteredItemStatus::DefinedOnce(index) => (Some(index), index.kind().into()),
            RegisteredItemStatus::Duplicated(found) => (None, found),
        };

        if found.is_disjoint(expected) {
            self.errors.push(ResolveError::WrongKind {
                path,
                expected,
                found,
                defined_at: registered_item.first_defined_at,
            });
            LookupResult::Failure
        } else {
            LookupResult::Found {
                path,
                defined_at: if found.len() == 1 {
                    registered_item.first_defined_at
                } else {
                    None
                },
                index,
            }
        }
    }
}

struct ScopedResolver<'a> {
    resolver: &'a mut Resolver,
    local_vars: &'a mut Vec<LocalVar>,
    scoped_vars: Cow<'a, HashMap<Ident, Spanned<ScopedVarIndex>>>,
}

impl Deref for ScopedResolver<'_> {
    type Target = Resolver;

    fn deref(&self) -> &Self::Target {
        self.resolver
    }
}

impl DerefMut for ScopedResolver<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.resolver
    }
}

impl<'a> ScopedResolver<'a> {
    fn resolve_block(&mut self, block: parser::Block) -> Option<Block> {
        let stmts: Option<_> =
            collect_eager(block.stmts.into_iter().map(|stmt| self.resolve_stmt(stmt)));

        let value = match block.value {
            None => Some(None),
            Some(value) => self.resolve_expr(value).map(Some),
        };

        Some(Block {
            stmts: stmts?,
            value: value?,
        })
    }

    fn resolve_inner_block(&mut self, block: parser::Block) -> Option<Block> {
        let mut scoped_resolver = ScopedResolver {
            resolver: self.resolver,
            local_vars: self.local_vars,
            scoped_vars: Cow::Borrowed(&self.scoped_vars),
        };
        scoped_resolver.resolve_block(block)
    }

    fn resolve_stmt(&mut self, stmt: parser::Stmt) -> Option<Stmt> {
        match stmt {
            parser::Stmt::Let(let_stmt) => self.resolve_let_stmt(let_stmt).map(Stmt::from),
            parser::Stmt::Check(check_stmt) => self.resolve_check_stmt(check_stmt).map(Stmt::from),
            parser::Stmt::Expr(expr) => self.resolve_expr(expr).map(Stmt::from),
        }
    }

    fn resolve_let_stmt(&mut self, let_stmt: parser::LetStmt) -> Option<LetStmt> {
        let lhs = self.bind_local_var(
            let_stmt.lhs,
            let_stmt.type_,
            |scoped_resolver, ident, index| scoped_resolver.insert_into_scope(ident, index),
        );

        let rhs = map_spanned(let_stmt.rhs, |rhs| self.resolve_expr(rhs));

        Some(LetStmt {
            lhs: lhs?,
            rhs: rhs?,
        })
    }

    fn resolve_check_stmt(&mut self, check_stmt: parser::CheckStmt) -> Option<CheckStmt> {
        let cond = map_spanned(check_stmt.cond, |cond| self.resolve_expr(cond));

        Some(CheckStmt {
            kind: check_stmt.kind,
            cond: cond?,
        })
    }

    fn resolve_expr(&mut self, expr: parser::Expr) -> Option<Expr> {
        match expr {
            parser::Expr::Block(block_expr) => {
                self.resolve_block_expr(*block_expr).map(Expr::from)
            }
            parser::Expr::Var(path) => {
                self.lookup_var_scoped(path).found_spanned().map(Expr::from)
            }
            parser::Expr::Call(call_expr) => self.resolve_call_expr(call_expr).map(Expr::from),
            parser::Expr::Cast(cast_expr) => self.resolve_cast_expr(*cast_expr).map(Expr::from),
            parser::Expr::Compare(compare_expr) => {
                self.resolve_compare_expr(*compare_expr).map(Expr::from)
            }
            parser::Expr::Assign(assign_expr) => {
                self.resolve_assign_expr(*assign_expr).map(Expr::from)
            }
        }
    }

    fn resolve_block_expr(&mut self, block_expr: parser::BlockExpr) -> Option<BlockExpr> {
        let block = self.resolve_inner_block(block_expr.block);

        Some(BlockExpr {
            kind: block_expr.kind,
            block: block?,
        })
    }

    fn resolve_call_expr(&mut self, call_expr: parser::CallExpr) -> Option<CallExpr> {
        let target = self.lookup_fn_scoped(call_expr.target).found_spanned();

        let mut deferred_scope_insertions = Vec::new();
        let args = map_spanned(call_expr.args, |args| {
            collect_eager(
                args.into_iter()
                    .map(|arg| self.resolve_call_expr_arg(arg, &mut deferred_scope_insertions)),
            )
        });
        for (ident, index) in deferred_scope_insertions {
            self.insert_into_scope(ident, index);
        }

        Some(CallExpr {
            target: target?,
            args: args?,
        })
    }

    fn resolve_call_expr_arg(
        &mut self,
        arg: parser::CallExprArg,
        deferred_scope_insertions: &mut Vec<(Ident, Spanned<LocalVarIndex>)>,
    ) -> Option<CallExprArg> {
        match arg {
            parser::CallExprArg::Expr(expr) => {
                map_spanned(expr, |expr| self.resolve_expr(expr)).map(CallExprArg::from)
            }
            parser::CallExprArg::OutRef(out_ref) => self
                .lookup_out_param_var_scoped(out_ref)
                .found_spanned()
                .map(|index| CallExprArg::OutRef(index.map(ScopedVarIndex::ParamVar))),
            parser::CallExprArg::LetOutRef(let_out_ref) => {
                let span = let_out_ref.ident.span;
                self.bind_local_var(let_out_ref.ident, let_out_ref.type_, |_, ident, index| {
                    deferred_scope_insertions.push((ident, index));
                })
                .map(|index| {
                    CallExprArg::OutRef(Spanned::new(span, ScopedVarIndex::LocalVar(index)))
                })
            }
        }
    }

    fn resolve_cast_expr(&mut self, cast_expr: parser::CastExpr) -> Option<CastExpr> {
        let expr = map_spanned(cast_expr.expr, |expr| self.resolve_expr(expr));

        let type_ = self.lookup_type_scoped(cast_expr.type_).found_spanned();

        Some(CastExpr {
            expr: expr?,
            type_: type_?,
        })
    }

    fn resolve_compare_expr(&mut self, compare_expr: parser::CompareExpr) -> Option<CompareExpr> {
        let lhs = map_spanned(compare_expr.lhs, |lhs| self.resolve_expr(lhs));

        let rhs = map_spanned(compare_expr.rhs, |rhs| self.resolve_expr(rhs));

        Some(CompareExpr {
            kind: compare_expr.kind,
            lhs: lhs?,
            rhs: rhs?,
        })
    }

    fn resolve_assign_expr(&mut self, assign_expr: parser::AssignExpr) -> Option<AssignExpr> {
        let lhs = self
            .lookup_out_param_var_scoped(assign_expr.lhs)
            .found_spanned();

        let rhs = map_spanned(assign_expr.rhs, |rhs| self.resolve_expr(rhs));

        Some(AssignExpr {
            lhs: lhs?,
            rhs: rhs?,
        })
    }

    fn bind_local_var(
        &mut self,
        ident: Spanned<Ident>,
        type_: Option<Spanned<Path>>,
        mut insert_into_scope: impl FnMut(&mut Self, Ident, Spanned<LocalVarIndex>),
    ) -> Option<LocalVarIndex> {
        let (type_, did_resolve_type) = match type_ {
            None => (None, true),
            Some(type_) => {
                let type_ = self.lookup_type_scoped(type_).found();
                (type_, type_.is_some())
            }
        };

        let index = self.local_vars.len();
        self.local_vars.push(LocalVar {
            ident: ident.value.clone(),
            type_,
        });
        insert_into_scope(self, ident.value, Spanned::new(ident.span, index));

        if did_resolve_type { Some(index) } else { None }
    }

    fn insert_into_scope(&mut self, ident: Ident, index: Spanned<LocalVarIndex>) {
        self.scoped_vars
            .to_mut()
            .insert(ident, index.map(ScopedVarIndex::LocalVar));
    }

    fn lookup_type_scoped(&mut self, path: Spanned<Path>) -> LookupResult<TypeIndex> {
        if path.value.parent_type.is_none() {
            if let Some(index) = self.scoped_vars.get(&path.value.ident) {
                self.resolver.errors.push(ResolveError::WrongKind {
                    path,
                    expected: ItemKind::Type.into(),
                    found: ItemKind::Var.into(),
                    defined_at: Some(index.span),
                });
                return LookupResult::Failure;
            }
        }

        self.lookup_type_global(path)
    }

    fn lookup_fn_scoped(&mut self, path: Spanned<Path>) -> LookupResult<FnIndex> {
        if path.value.parent_type.is_none() {
            if let Some(index) = self.scoped_vars.get(&path.value.ident) {
                self.resolver.errors.push(ResolveError::WrongKind {
                    path,
                    expected: ItemKind::Fn.into(),
                    found: ItemKind::Var.into(),
                    defined_at: Some(index.span),
                });
                return LookupResult::Failure;
            }
        }

        self.lookup_fn_global(path)
    }

    fn lookup_var_scoped(&mut self, path: Spanned<Path>) -> LookupResult<VarIndex> {
        if path.value.parent_type.is_none() {
            if let Some(index) = self.scoped_vars.get(&path.value.ident) {
                return LookupResult::Found {
                    path,
                    defined_at: Some(index.span),
                    index: Some(index.value.into()),
                };
            }
        }

        self.lookup_var_global(path)
    }

    fn lookup_out_param_var_scoped(&mut self, path: Spanned<Path>) -> LookupResult<ParamVarIndex> {
        if path.value.parent_type.is_none() {
            if let Some(index) = self.scoped_vars.get(&path.value.ident) {
                return match index.value {
                    ScopedVarIndex::ParamVar(param_var_index) => LookupResult::Found {
                        path,
                        defined_at: Some(index.span),
                        index: Some(param_var_index),
                    },
                    ScopedVarIndex::LocalVar(_) => {
                        self.resolver.errors.push(ResolveError::WriteToReadOnlyVar {
                            path,
                            defined_at: Some(index.span),
                        });
                        LookupResult::Failure
                    }
                };
            }
        }

        match self.lookup_var_global(path) {
            LookupResult::Found {
                path, defined_at, ..
            } => {
                self.errors
                    .push(ResolveError::WriteToReadOnlyVar { path, defined_at });
            }
            _ => (),
        }

        LookupResult::Failure
    }
}

fn collect_eager<T, U: FromIterator<T>>(mut iter: impl Iterator<Item = T>) -> U {
    let result = U::from_iter(&mut iter);
    for _ in iter {}
    result
}

fn map_spanned<T, U>(x: Spanned<T>, f: impl FnOnce(T) -> Option<U>) -> Option<Spanned<U>> {
    match f(x.value) {
        None => None,
        Some(y) => Some(Spanned::new(x.span, y)),
    }
}
