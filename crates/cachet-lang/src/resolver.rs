// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;
mod registry;

use std::borrow::Cow;
use std::mem;
use std::ops::{Deref, DerefMut};

use derive_more::{From, Into};
use enumset::EnumSet;
use typed_index_collections::TiVec;

use crate::ast::{Ident, Path, Spanned};
use crate::parser;
use crate::util::{collect_eager, deref_from, map_spanned};
use crate::FrontendError;

pub use crate::resolver::ast::*;
pub use crate::resolver::error::*;
use crate::resolver::registry::{LookupError, LookupResult, Registrable, Registry};

pub fn resolve(items: Vec<Spanned<parser::Item>>) -> Result<Env, ResolveErrors> {
    let mut item_catalog = ItemCatalog::new();
    item_catalog.catalog_items(items);

    let mut resolver = Resolver::new(
        item_catalog.errors,
        item_catalog.global_registry,
        item_catalog.impl_item_parents,
    );

    let enum_items = item_catalog.enum_items;

    let struct_items: Option<_> = collect_eager(
        item_catalog
            .struct_items
            .iter()
            .map(|struct_item| resolver.resolve_struct_item(struct_item)),
    );

    let ir_items: Option<_> = collect_eager(
        item_catalog
            .ir_items
            .iter()
            .map(|ir_item| resolver.resolve_ir_item(&ir_item.item)),
    );

    let global_var_items: Option<_> = collect_eager(
        item_catalog
            .global_var_items
            .iter()
            .map(|global_var_item| resolver.resolve_global_var_item(global_var_item.as_ref())),
    );

    let fn_items: Option<_> = collect_eager(
        item_catalog
            .fn_items
            .iter()
            .map(|fn_item| resolver.resolve_fn_item(fn_item.as_ref())),
    );

    let op_items: Option<_> = collect_eager(
        item_catalog
            .op_items
            .iter()
            .map(|op_item| resolver.resolve_op_item(op_item.as_ref())),
    );

    if resolver.errors.is_empty() {
        Ok(Env {
            enum_items,
            struct_items: struct_items.unwrap(),
            ir_items: ir_items.unwrap(),
            global_var_items: global_var_items.unwrap(),
            fn_items: fn_items.unwrap(),
            op_items: op_items.unwrap(),
        })
    } else {
        resolver.errors.sort_by_key(|error| error.span());
        Err(ResolveErrors(resolver.errors))
    }
}

#[derive(Clone, Copy, From)]
enum ItemIndex {
    #[from(types(BuiltInType, EnumIndex, StructIndex))]
    Type(TypeIndex),
    #[from]
    Ir(IrIndex),
    #[from(types(
        BuiltInVar,
        EnumVariantIndex,
        GlobalVarIndex,
        VarParamIndex,
        LocalVarIndex
    ))]
    Var(VarIndex),
    #[from]
    Fn(FnIndex),
    #[from]
    Op(OpIndex),
}

impl Registrable for ItemIndex {
    fn name_kind(&self) -> NameKind {
        match self {
            ItemIndex::Type(_) => NameKind::Type,
            ItemIndex::Var(_) => NameKind::Var,
            ItemIndex::Fn(_) => NameKind::Fn,
            ItemIndex::Ir(_) => NameKind::Ir,
            ItemIndex::Op(_) => NameKind::Op,
        }
    }

    fn shadows(&self) -> bool {
        false
    }
}

type GlobalRegistry = Registry<Path, ItemIndex>;

#[derive(Clone, Copy, From, Into)]
struct ImplIndex(usize);

#[derive(Clone, Copy, From)]
enum UnresolvedParentIndex {
    Impl(ImplIndex),
    Ir(IrIndex),
}

deref_from!(&ImplIndex => UnresolvedParentIndex);
deref_from!(&IrIndex => UnresolvedParentIndex);

struct GlobalItem<T> {
    parent: Option<UnresolvedParentIndex>,
    path: Spanned<Path>,
    item: T,
}

impl<T> GlobalItem<T> {
    fn as_ref(&self) -> GlobalItem<&T> {
        GlobalItem {
            parent: self.parent,
            path: self.path,
            item: &self.item,
        }
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> GlobalItem<U> {
        GlobalItem {
            parent: self.parent,
            path: self.path,
            item: f(self.item),
        }
    }
}

struct ItemCatalog {
    errors: Vec<ResolveError>,
    global_registry: GlobalRegistry,
    enum_items: TiVec<EnumIndex, parser::EnumItem>,
    struct_items: TiVec<StructIndex, parser::StructItem>,
    ir_items: TiVec<IrIndex, GlobalItem<parser::IrItem>>,
    impl_item_parents: TiVec<ImplIndex, Spanned<Path>>,
    global_var_items: TiVec<GlobalVarIndex, GlobalItem<parser::GlobalVarItem>>,
    fn_items: TiVec<FnIndex, GlobalItem<parser::FnItem>>,
    op_items: TiVec<OpIndex, GlobalItem<parser::OpItem>>,
}

impl ItemCatalog {
    fn new() -> Self {
        let mut global_registry =
            GlobalRegistry::with_capacity(BUILT_IN_TYPES.len() + BUILT_IN_VARS.len());

        for built_in_type in BUILT_IN_TYPES {
            global_registry
                .register(
                    Path::from(built_in_type.ident().to_owned()).into(),
                    built_in_type.into(),
                )
                .expect("built-in types shouldn't shadow other built-ins");
        }

        for built_in_var in BUILT_IN_VARS {
            global_registry
                .register(
                    Path::from(built_in_var.ident().to_owned()).into(),
                    built_in_var.into(),
                )
                .expect("built-in variables shouldn't shadow other built-ins");
        }

        ItemCatalog {
            errors: Vec::new(),
            global_registry,
            enum_items: TiVec::new(),
            struct_items: TiVec::new(),
            ir_items: TiVec::new(),
            impl_item_parents: TiVec::new(),
            fn_items: TiVec::new(),
            global_var_items: TiVec::new(),
            op_items: TiVec::new(),
        }
    }

    fn catalog_items(&mut self, items: Vec<Spanned<parser::Item>>) {
        for item in items {
            self.catalog_item(None, item);
        }
    }

    fn catalog_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        item: Spanned<parser::Item>,
    ) {
        match item.value {
            parser::Item::Enum(enum_item) => {
                self.catalog_enum_item(parent_index, Spanned::new(item.span, enum_item))
            }
            parser::Item::Struct(struct_item) => {
                self.catalog_struct_item(parent_index, Spanned::new(item.span, struct_item))
            }
            parser::Item::Ir(ir_item) => {
                self.catalog_ir_item(parent_index, Spanned::new(item.span, ir_item))
            }
            parser::Item::Impl(impl_item) => {
                self.catalog_impl_item(parent_index, Spanned::new(item.span, impl_item))
            }
            parser::Item::GlobalVar(global_var_item) => {
                self.catalog_global_var_item(parent_index, global_var_item)
            }
            parser::Item::Fn(fn_item) => self.catalog_fn_item(parent_index, fn_item),
            parser::Item::Op(op_item) => {
                self.catalog_op_item(parent_index, Spanned::new(item.span, op_item))
            }
        }
    }

    fn catalog_enum_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        enum_item: Spanned<parser::EnumItem>,
    ) {
        let parent_path = parent_index.map(|parent_index| {
            let parent_path = self.get_parent_path(parent_index);
            self.errors.push(
                NonMemberItemError {
                    span: enum_item.span,
                    parent: parent_path,
                }
                .into(),
            );
            parent_path.value
        });
        let enum_path = enum_item
            .value
            .ident
            .map(|enum_ident| Path::new(parent_path, enum_ident));
        let enum_index = self.enum_items.next_key();

        for (variant_index, variant) in enum_item.value.variants.iter_enumerated() {
            let enum_variant_index = EnumVariantIndex {
                enum_index,
                variant_index,
            };
            self.register_global(
                (*variant).map(|variant| enum_path.value.member(variant)),
                enum_variant_index.into(),
            );
        }

        let actual_enum_index = self.enum_items.push_and_get_key(enum_item.value);
        debug_assert_eq!(enum_index, actual_enum_index);
        self.register_global(enum_path, enum_index.into());
    }

    fn catalog_struct_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        struct_item: Spanned<parser::StructItem>,
    ) {
        let parent_path = parent_index.map(|parent_index| {
            let parent_path = self.get_parent_path(parent_index);
            self.errors.push(
                NonMemberItemError {
                    span: struct_item.span,
                    parent: parent_path,
                }
                .into(),
            );
            parent_path.value
        });
        let struct_path = struct_item
            .value
            .ident
            .map(|struct_ident| Path::new(parent_path, struct_ident));
        let struct_index = self.struct_items.push_and_get_key(struct_item.value);
        self.register_global(struct_path, struct_index.into());
    }

    fn catalog_ir_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        mut ir_item: Spanned<parser::IrItem>,
    ) {
        let parent_path = parent_index.map(|parent_index| {
            let parent_path = self.get_parent_path(parent_index);
            self.errors.push(
                NonMemberItemError {
                    span: ir_item.span,
                    parent: parent_path,
                }
                .into(),
            );
            parent_path.value
        });
        let ir_path = ir_item
            .value
            .ident
            .map(|ir_ident| Path::new(parent_path, ir_ident));
        let ir_sub_items = mem::replace(&mut ir_item.value.items, Vec::new());
        let ir_index = self.ir_items.push_and_get_key(GlobalItem {
            parent: parent_index,
            path: ir_path,
            item: ir_item.value,
        });
        self.register_global(ir_path, ir_index.into());

        for item in ir_sub_items {
            self.catalog_item(Some(ir_index.into()), item);
        }
    }

    fn catalog_impl_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        impl_item: Spanned<parser::ImplItem>,
    ) {
        if let Some(parent_index) = parent_index {
            self.errors.push(
                NonMemberItemError {
                    span: impl_item.span,
                    parent: self.get_parent_path(parent_index),
                }
                .into(),
            );
        }

        let impl_index = self
            .impl_item_parents
            .push_and_get_key(impl_item.value.parent);

        for item in impl_item.value.items {
            self.catalog_item(Some(impl_index.into()), item);
        }
    }

    fn catalog_global_var_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        global_var_item: parser::GlobalVarItem,
    ) {
        let parent_path =
            parent_index.map(|parent_index| self.get_parent_path(parent_index).value);
        let global_var_path = global_var_item
            .ident
            .map(|global_var_ident| Path::new(parent_path, global_var_ident));
        let global_var_index = self.global_var_items.push_and_get_key(GlobalItem {
            parent: parent_index,
            path: global_var_path,
            item: global_var_item,
        });
        self.register_global(global_var_path, global_var_index.into());
    }

    fn catalog_fn_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        fn_item: parser::FnItem,
    ) {
        let parent_path =
            parent_index.map(|parent_index| self.get_parent_path(parent_index).value);
        let fn_path = fn_item
            .ident
            .map(|fn_ident| Path::new(parent_path, fn_ident));
        let fn_index = self.fn_items.push_and_get_key(GlobalItem {
            parent: parent_index,
            path: fn_path,
            item: fn_item,
        });
        self.register_global(fn_path, fn_index.into());
    }

    fn catalog_op_item(
        &mut self,
        parent_index: Option<UnresolvedParentIndex>,
        op_item: Spanned<parser::OpItem>,
    ) {
        let parent_path =
            parent_index.map(|parent_index| self.get_parent_path(parent_index).value);
        let op_path = op_item
            .value
            .ident
            .map(|op_ident| Path::new(parent_path, op_ident));
        let op_index = self.op_items.push_and_get_key(GlobalItem {
            parent: parent_index,
            path: op_path,
            item: op_item.value,
        });
        self.register_global(op_path, op_index.into());
    }

    fn register_global(&mut self, path: Spanned<Path>, item_index: ItemIndex) {
        if let Err(error) = self.global_registry.register(path.into(), item_index) {
            self.errors.push(error.into());
        }
    }

    fn get_parent_path(&self, parent_index: UnresolvedParentIndex) -> Spanned<Path> {
        match parent_index {
            UnresolvedParentIndex::Impl(impl_index) => self.impl_item_parents[impl_index],
            UnresolvedParentIndex::Ir(ir_index) => self.ir_items[ir_index].path,
        }
    }
}

trait LookupResultExt<T> {
    fn found(self, errors: &mut Vec<ResolveError>) -> Option<T>;

    fn map_found<U>(self, f: impl FnOnce(T) -> U) -> LookupResult<U>;
}

impl<T> LookupResultExt<T> for LookupResult<T> {
    fn found(self, errors: &mut Vec<ResolveError>) -> Option<T> {
        match self {
            Ok((value, _)) => value,
            Err(error) => {
                errors.push(error.into());
                None
            }
        }
    }

    fn map_found<U>(self, f: impl FnOnce(T) -> U) -> LookupResult<U> {
        match self {
            Ok((value, defined_at)) => Ok((value.map(f), defined_at)),
            Err(error) => Err(error),
        }
    }
}

struct Resolver {
    errors: Vec<ResolveError>,
    global_registry: GlobalRegistry,
    impl_item_parents: TiVec<ImplIndex, Option<TypeIndex>>,
}

impl Resolver {
    fn new(
        errors: Vec<ResolveError>,
        global_registry: GlobalRegistry,
        impl_item_parents: TiVec<ImplIndex, Spanned<Path>>,
    ) -> Self {
        let mut resolver = Resolver {
            errors,
            global_registry,
            impl_item_parents: TiVec::with_capacity(impl_item_parents.len()),
        };

        for parent_index in impl_item_parents {
            let parent_index = resolver.lookup_type_global(parent_index);
            resolver
                .impl_item_parents
                .push(parent_index.found(&mut resolver.errors));
        }

        resolver
    }

    fn resolve_struct_item(&mut self, struct_item: &parser::StructItem) -> Option<StructItem> {
        let supertype = match struct_item.supertype {
            None => Some(None),
            Some(supertype) => self
                .lookup_type_global(supertype)
                .found(&mut self.errors)
                .map(Some),
        };

        Some(StructItem {
            ident: struct_item.ident,
            supertype: supertype?,
        })
    }

    fn resolve_ir_item(&mut self, ir_item: &parser::IrItem) -> Option<IrItem> {
        let emits = match ir_item.emits {
            None => Some(None),
            Some(emits) => self
                .lookup_ir_global(emits)
                .found(&mut self.errors)
                .map(Some),
        };

        Some(IrItem {
            ident: ir_item.ident,
            emits: emits?,
        })
    }

    fn resolve_global_var_item(
        &mut self,
        global_var_item: GlobalItem<&parser::GlobalVarItem>,
    ) -> Option<GlobalVarItem> {
        let parent_index = match global_var_item.parent {
            Some(parent_index) => self.resolve_parent_index(parent_index).map(Some),
            None => Some(None),
        };

        let type_ = self
            .lookup_type_global(global_var_item.item.type_)
            .found(&mut self.errors);

        Some(GlobalVarItem {
            path: global_var_item.path,
            parent: parent_index?,
            is_mut: global_var_item.item.is_mut,
            type_: type_?,
        })
    }

    fn resolve_fn_item(&mut self, fn_item: GlobalItem<&parser::FnItem>) -> Option<FnItem> {
        let parent_index = match fn_item.parent {
            Some(parent_index) => self.resolve_parent_index(parent_index).map(Some),
            None => Some(None),
        };

        let (params, param_order, body) = self.resolve_params_and_body(
            &fn_item.item.params,
            fn_item.item.body.as_ref().map(Option::as_ref),
        );

        let ret = match fn_item.item.ret {
            Some(ret) => map_spanned(ret, |ret| {
                self.lookup_type_global(ret).found(&mut self.errors)
            })
            .map(Some),
            None => Some(None),
        };

        Some(FnItem {
            path: fn_item.path,
            parent: parent_index?,
            is_unsafe: fn_item.item.is_unsafe,
            params,
            param_order,
            ret: ret?,
            body: body?,
        })
    }

    fn resolve_op_item(&mut self, op_item: GlobalItem<&parser::OpItem>) -> Option<OpItem> {
        self.resolve_fn_item(op_item.map(parser::OpItem::as_ref))
            .map(OpItem::from)
    }

    fn resolve_params_and_body(
        &mut self,
        params: &[parser::Param],
        body: Spanned<Option<&parser::Block>>,
    ) -> (Params, Vec<ParamIndex>, Option<Spanned<Option<Body>>>) {
        let mut resolved_params = Params::default();
        let mut local_vars = TiVec::new();
        let mut local_labels = TiVec::new();

        let mut scoped_resolver = ScopedResolver::new(
            self,
            &mut resolved_params,
            &mut local_vars,
            &mut local_labels,
        );

        let param_order = scoped_resolver.bind_params(params);

        let body = match body.value {
            Some(block) => scoped_resolver
                .resolve_top_level_block(block)
                .map(move |block| {
                    Spanned::new(
                        body.span,
                        Some(
                            Body {
                                local_vars,
                                local_labels,
                                block,
                            }
                            .into(),
                        ),
                    )
                }),
            None => Some(Spanned::new(body.span, None)),
        };

        (resolved_params, param_order, body)
    }

    fn resolve_parent_index(&self, parent_index: UnresolvedParentIndex) -> Option<ParentIndex> {
        match parent_index {
            UnresolvedParentIndex::Impl(impl_index) => {
                self.impl_item_parents[impl_index].map(ParentIndex::from)
            }
            UnresolvedParentIndex::Ir(ir_index) => Some(ir_index.into()),
        }
    }

    fn lookup_type_global(&mut self, path: Spanned<Path>) -> LookupResult<TypeIndex> {
        self.lookup_global(path, NameKind::Type.into())
            .map_found(|item_index| match item_index {
                ItemIndex::Type(type_index) => type_index,
                _ => unreachable!(),
            })
    }

    fn lookup_ir_global(&mut self, path: Spanned<Path>) -> LookupResult<IrIndex> {
        self.lookup_global(path, NameKind::Ir.into())
            .map_found(|item_index| match item_index {
                ItemIndex::Ir(ir_index) => ir_index,
                _ => unreachable!(),
            })
    }

    fn lookup_op_global(&mut self, path: Spanned<Path>) -> LookupResult<OpIndex> {
        self.lookup_global(path, NameKind::Op.into())
            .map_found(|item_index| match item_index {
                ItemIndex::Op(op_index) => op_index,
                _ => unreachable!(),
            })
    }

    fn lookup_var_global(&mut self, path: Spanned<Path>) -> LookupResult<VarIndex> {
        self.lookup_global(path, NameKind::Var.into())
            .map_found(|item_index| match item_index {
                ItemIndex::Var(var_index) => var_index,
                _ => unreachable!(),
            })
    }

    fn lookup_fn_global(&mut self, path: Spanned<Path>) -> LookupResult<FnIndex> {
        self.lookup_global(path, NameKind::Fn.into())
            .map_found(|item_index| match item_index {
                ItemIndex::Fn(fn_index) => fn_index,
                _ => unreachable!(),
            })
    }

    fn lookup_global(
        &mut self,
        path: Spanned<Path>,
        expected: EnumSet<NameKind>,
    ) -> LookupResult<ItemIndex> {
        self.global_registry
            .lookup(path, expected)
            .map_found(|item_index| *item_index)
    }
}

#[derive(Clone, Copy, From)]
enum ScopedIndex {
    #[from(types(VarParamIndex, OutVarParamIndex, LocalVarIndex))]
    Var(VarIndex),
    #[from(types(LabelParamIndex, LocalLabelIndex))]
    Label(LabelIndex),
}

impl Registrable for ScopedIndex {
    fn name_kind(&self) -> NameKind {
        match self {
            ScopedIndex::Var(_) => NameKind::Var,
            ScopedIndex::Label(_) => NameKind::Label,
        }
    }

    fn shadows(&self) -> bool {
        match self {
            ScopedIndex::Var(VarIndex::Local(_)) => true,
            ScopedIndex::Label(LabelIndex::Local(_)) => true,
            _ => false,
        }
    }
}

type ScopedRegistry = Registry<Ident, ScopedIndex>;

struct ScopedResolver<'a> {
    resolver: &'a mut Resolver,
    scoped_registry: Cow<'a, ScopedRegistry>,
    params: &'a mut Params,
    local_vars: &'a mut TiVec<LocalVarIndex, LocalVar>,
    local_labels: &'a mut TiVec<LocalLabelIndex, Spanned<Ident>>,
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
    fn new(
        resolver: &'a mut Resolver,
        params: &'a mut Params,
        local_vars: &'a mut TiVec<LocalVarIndex, LocalVar>,
        local_labels: &'a mut TiVec<LocalLabelIndex, Spanned<Ident>>,
    ) -> Self {
        ScopedResolver {
            resolver,
            scoped_registry: Cow::Owned(ScopedRegistry::new()),
            params,
            local_vars,
            local_labels,
        }
    }

    fn recurse(&mut self) -> ScopedResolver<'_> {
        ScopedResolver {
            resolver: self.resolver,
            scoped_registry: Cow::Borrowed(&self.scoped_registry),
            params: self.params,
            local_vars: self.local_vars,
            local_labels: self.local_labels,
        }
    }

    fn bind_params(&mut self, params: &[parser::Param]) -> Vec<ParamIndex> {
        params.iter().map(|param| self.bind_param(param)).collect()
    }

    fn bind_param(&mut self, param: &parser::Param) -> ParamIndex {
        match param {
            parser::Param::Var(var_param) => self.bind_var_param(var_param).into(),
            parser::Param::OutVar(out_var_param) => self.bind_out_var_param(out_var_param).into(),
            parser::Param::Label(label) => self.bind_label_param(*label).into(),
        }
    }

    fn bind_var_param(&mut self, var_param: &parser::VarParam) -> VarParamIndex {
        let type_ = self
            .lookup_type_global(var_param.type_)
            .found(&mut self.errors)
            .unwrap_or_else(|| BuiltInType::Unit.into());

        let var_param_index = self.params.var_params.push_and_get_key(VarParam {
            ident: var_param.ident,
            is_mut: var_param.is_mut,
            type_,
        });
        self.register_scoped(var_param.ident, var_param_index.into());
        var_param_index
    }

    fn bind_out_var_param(&mut self, out_var_param: &parser::OutVarParam) -> OutVarParamIndex {
        let type_ = self
            .lookup_type_global(out_var_param.type_)
            .found(&mut self.errors)
            .unwrap_or_else(|| BuiltInType::Unit.into());

        let out_var_param_index = self.params.out_var_params.push_and_get_key(OutVarParam {
            ident: out_var_param.ident,
            type_,
        });
        self.register_scoped(out_var_param.ident, out_var_param_index.into());
        out_var_param_index
    }

    fn bind_label_param(&mut self, label: Spanned<Ident>) -> LabelParamIndex {
        let label_param_index = self.params.label_params.push_and_get_key(label);
        self.register_scoped(label, label_param_index.into());
        label_param_index
    }

    fn resolve_top_level_block(&mut self, block: &parser::Block) -> Option<Block> {
        self.resolve_block_impl(block)
    }

    fn resolve_nested_block(&mut self, block: &parser::Block) -> Option<Block> {
        self.recurse().resolve_block_impl(block)
    }

    // This is deliberately *not* called `resolve_block`, to force explicit
    // disambiguation between calls to `resolve_top_level_block` and
    // `resolve_nested_block`. `resolve_block_impl` should not be called
    // directly.
    fn resolve_block_impl(&mut self, block: &parser::Block) -> Option<Block> {
        let stmts: Option<_> =
            collect_eager(block.stmts.iter().map(|stmt| self.resolve_stmt(stmt)));

        let value = match &block.value {
            None => Some(None),
            Some(value) => self.resolve_expr(value).map(Some),
        };

        Some(Block {
            stmts: stmts?,
            value: value?,
        })
    }

    fn resolve_stmt(&mut self, stmt: &parser::Stmt) -> Option<Stmt> {
        match stmt {
            parser::Stmt::Let(let_stmt) => self.resolve_let_stmt(let_stmt).map(Stmt::from),
            parser::Stmt::If(if_stmt) => self.resolve_if_stmt(if_stmt).map(Stmt::from),
            parser::Stmt::Check(check_stmt) => self.resolve_check_stmt(check_stmt).map(Stmt::from),
            parser::Stmt::Goto(goto_stmt) => self.resolve_goto_stmt(goto_stmt).map(Stmt::from),
            parser::Stmt::Emit(emit_stmt) => self.resolve_emit_stmt(emit_stmt).map(Stmt::from),
            parser::Stmt::Expr(expr) => self.resolve_expr(expr).map(Stmt::from),
        }
    }

    fn resolve_let_stmt(&mut self, let_stmt: &parser::LetStmt) -> Option<LetStmt> {
        let rhs = map_spanned(let_stmt.rhs.as_ref(), |rhs| self.resolve_expr(rhs.value));

        let lhs = self.bind_local_var(&let_stmt.lhs, |scoped_resolver, ident, local_var_index| {
            scoped_resolver.register_scoped(ident, local_var_index.into());
        });

        Some(LetStmt {
            lhs: lhs,
            rhs: rhs?,
        })
    }

    fn bind_local_var(
        &mut self,
        local_var: &parser::LocalVar,
        mut register_local_var: impl FnMut(&mut Self, Spanned<Ident>, LocalVarIndex),
    ) -> LocalVarIndex {
        let type_ = local_var.type_.map(|type_| {
            self.lookup_type_scoped(type_)
                .found(&mut self.errors)
                .unwrap_or_else(|| BuiltInType::Unit.into())
        });

        let local_var_index = self.local_vars.push_and_get_key(LocalVar {
            ident: local_var.ident,
            is_mut: local_var.is_mut,
            type_,
        });
        register_local_var(self, local_var.ident, local_var_index);
        local_var_index
    }

    fn resolve_if_stmt(&mut self, if_stmt: &parser::IfStmt) -> Option<IfStmt> {
        let cond = map_spanned(if_stmt.cond.as_ref(), |cond| self.resolve_expr(cond.value));

        let then = self.resolve_nested_block(&if_stmt.then);

        let else_ = match &if_stmt.else_ {
            None => Some(None),
            Some(else_) => self.resolve_nested_block(else_).map(Some),
        };

        Some(IfStmt {
            cond: cond?,
            then: then?,
            else_: else_?,
        })
    }

    fn resolve_check_stmt(&mut self, check_stmt: &parser::CheckStmt) -> Option<CheckStmt> {
        let cond = map_spanned(check_stmt.cond.as_ref(), |cond| {
            self.resolve_expr(cond.value)
        });

        Some(CheckStmt {
            kind: check_stmt.kind,
            cond: cond?,
        })
    }

    fn resolve_goto_stmt(&mut self, goto_stmt: &parser::GotoStmt) -> Option<GotoStmt> {
        let label = self
            .lookup_label_scoped(goto_stmt.label)
            .found(&mut self.errors);

        Some(GotoStmt { label: label? })
    }

    fn resolve_emit_stmt(&mut self, emit_stmt: &parser::EmitStmt) -> Option<EmitStmt> {
        let target = map_spanned(emit_stmt.target, |target| {
            self.lookup_op_scoped(target).found(&mut self.errors)
        });

        let args = map_spanned(emit_stmt.args.as_ref(), |args| {
            self.resolve_args(args.value)
        });

        Some(EmitStmt {
            target: target?,
            args: args?,
        })
    }

    fn resolve_expr(&mut self, expr: &parser::Expr) -> Option<Expr> {
        match expr {
            parser::Expr::Block(block_expr) => self.resolve_block_expr(block_expr).map(Expr::from),
            parser::Expr::Var(path) => self.resolve_var_expr(*path).map(Expr::from),
            parser::Expr::Call(call_expr) => self.resolve_call_expr(call_expr).map(Expr::from),
            parser::Expr::Negate(negate_expr) => {
                self.resolve_negate_expr(negate_expr).map(Expr::from)
            }
            parser::Expr::Cast(cast_expr) => self.resolve_cast_expr(cast_expr).map(Expr::from),
            parser::Expr::Compare(compare_expr) => {
                self.resolve_compare_expr(compare_expr).map(Expr::from)
            }
            parser::Expr::Assign(assign_expr) => {
                self.resolve_assign_expr(assign_expr).map(Expr::from)
            }
        }
    }

    fn resolve_var_expr(&mut self, path: Spanned<Path>) -> Option<Spanned<VarIndex>> {
        map_spanned(path, |path| {
            self.lookup_var_scoped(path).found(&mut self.errors)
        })
    }

    fn resolve_block_expr(&mut self, block_expr: &parser::BlockExpr) -> Option<BlockExpr> {
        let block = self.resolve_nested_block(&block_expr.block);

        Some(BlockExpr {
            kind: block_expr.kind,
            block: block?,
        })
    }

    fn resolve_call_expr(&mut self, call_expr: &parser::CallExpr) -> Option<CallExpr> {
        let target = map_spanned(call_expr.target, |target| {
            self.lookup_fn_scoped(target).found(&mut self.errors)
        });

        let args = map_spanned(call_expr.args.as_ref(), |args| {
            self.resolve_args(args.value)
        });

        Some(CallExpr {
            target: target?,
            args: args?,
        })
    }

    fn resolve_args(&mut self, args: &[Spanned<parser::Arg>]) -> Option<Vec<Spanned<Arg>>> {
        let mut deferred_local_var_registrations = Vec::new();

        let args = collect_eager(args.iter().map(|arg| {
            map_spanned(arg.as_ref(), |arg| {
                self.resolve_arg(arg.value, &mut deferred_local_var_registrations)
            })
        }));

        for (ident, local_var_index) in deferred_local_var_registrations {
            self.register_scoped(ident, local_var_index.into());
        }

        args
    }

    fn resolve_arg(
        &mut self,
        arg: &parser::Arg,
        deferred_local_var_registrations: &mut Vec<(Spanned<Ident>, LocalVarIndex)>,
    ) -> Option<Arg> {
        match arg {
            parser::Arg::VarExprOrLabel(path) => self
                .lookup_var_or_label_scoped(*path)
                .found(&mut self.errors)
                .map(|scoped_index| match scoped_index {
                    ScopedIndex::Var(var_index) => {
                        Expr::from(Spanned::new(path.span, var_index)).into()
                    }
                    ScopedIndex::Label(label_index) => label_index.into(),
                }),
            parser::Arg::Expr(expr) => self.resolve_expr(expr).map(Arg::from),
            parser::Arg::OutVar(out_var) => match out_var {
                parser::OutVar::Out(out_var_path) => map_spanned(*out_var_path, |out_var_path| {
                    self.lookup_var_scoped(out_var_path).found(&mut self.errors)
                })
                .map(OutVar::Out),
                parser::OutVar::OutLet(out_let_var) => Some(OutVar::OutLet(self.bind_local_var(
                    out_let_var,
                    |_, ident, out_let_var_index| {
                        // An `out let` variable shouldn't come into scope until
                        // after the call expression finishes executing.
                        // Otherwise, another argument in the call expression
                        // could refer to this newly-introduced variable, before
                        // it has actually been populated with a value by the
                        // call.
                        deferred_local_var_registrations.push((ident, out_let_var_index));
                    },
                ))),
            }
            .map(Arg::from),
        }
    }

    fn resolve_negate_expr(&mut self, negate_expr: &parser::NegateExpr) -> Option<NegateExpr> {
        let expr = map_spanned(negate_expr.expr.as_ref(), |expr| {
            self.resolve_expr(expr.value)
        });

        Some(NegateExpr {
            kind: negate_expr.kind,
            expr: expr?,
        })
    }

    fn resolve_cast_expr(&mut self, cast_expr: &parser::CastExpr) -> Option<CastExpr> {
        let expr = map_spanned(cast_expr.expr.as_ref(), |expr| {
            self.resolve_expr(expr.value)
        });

        let type_ = map_spanned(cast_expr.type_, |type_| {
            self.lookup_type_scoped(type_).found(&mut self.errors)
        });

        Some(CastExpr {
            expr: expr?,
            type_: type_?,
        })
    }

    fn resolve_compare_expr(&mut self, compare_expr: &parser::CompareExpr) -> Option<CompareExpr> {
        let lhs = map_spanned(compare_expr.lhs.as_ref(), |lhs| {
            self.resolve_expr(lhs.value)
        });

        let rhs = map_spanned(compare_expr.rhs.as_ref(), |rhs| {
            self.resolve_expr(rhs.value)
        });

        Some(CompareExpr {
            kind: compare_expr.kind,
            lhs: lhs?,
            rhs: rhs?,
        })
    }

    fn resolve_assign_expr(&mut self, assign_expr: &parser::AssignExpr) -> Option<AssignExpr> {
        let lhs = map_spanned(assign_expr.lhs, |lhs| {
            self.lookup_var_scoped(lhs).found(&mut self.errors)
        });

        let rhs = map_spanned(assign_expr.rhs.as_ref(), |rhs| self.resolve_expr(rhs.value));

        Some(AssignExpr {
            lhs: lhs?,
            rhs: rhs?,
        })
    }

    fn register_scoped(&mut self, ident: Spanned<Ident>, scoped_index: ScopedIndex) {
        if let Err(error) = self
            .scoped_registry
            .to_mut()
            .register(ident.into(), scoped_index)
        {
            self.errors.push(error.into());
        }
    }

    fn lookup_type_scoped(&mut self, path: Spanned<Path>) -> LookupResult<TypeIndex> {
        let path = match self.lookup_scoped_only(path, NameKind::Type.into()) {
            Ok(_) => unreachable!(),
            Err(LookupError::Undefined(error)) => error.path,
            Err(error @ LookupError::WrongKind(_)) => return Err(error),
        };

        self.lookup_type_global(path)
    }

    fn lookup_op_scoped(&mut self, path: Spanned<Path>) -> LookupResult<OpIndex> {
        let path = match self.lookup_scoped_only(path, NameKind::Op.into()) {
            Ok(_) => unreachable!(),
            Err(LookupError::Undefined(error)) => error.path,
            Err(error @ LookupError::WrongKind(_)) => return Err(error),
        };

        self.lookup_op_global(path)
    }

    fn lookup_var_scoped(&mut self, path: Spanned<Path>) -> LookupResult<VarIndex> {
        let path = match self
            .lookup_scoped_only(path, NameKind::Var.into())
            .map_found(|scoped_index| match scoped_index {
                ScopedIndex::Var(var_index) => var_index,
                _ => unreachable!(),
            }) {
            Ok(found) => return Ok(found),
            Err(LookupError::Undefined(error)) => error.path,
            Err(error @ LookupError::WrongKind(_)) => return Err(error),
        };

        self.lookup_var_global(path)
    }

    fn lookup_var_or_label_scoped(&mut self, path: Spanned<Path>) -> LookupResult<ScopedIndex> {
        let path = match self.lookup_scoped_only(path, NameKind::Var | NameKind::Label) {
            Ok(found) => return Ok(found),
            Err(LookupError::Undefined(error)) => error.path,
            Err(error @ LookupError::WrongKind(_)) => return Err(error),
        };

        self.lookup_global(path, NameKind::Var | NameKind::Label)
            .map_found(|item_index| match item_index {
                ItemIndex::Var(var_index) => ScopedIndex::Var(var_index),
                _ => unreachable!(),
            })
    }

    fn lookup_fn_scoped(&mut self, path: Spanned<Path>) -> LookupResult<FnIndex> {
        let path = match self.lookup_scoped_only(path, NameKind::Fn.into()) {
            Ok(_) => unreachable!(),
            Err(LookupError::Undefined(error)) => error.path,
            Err(error @ LookupError::WrongKind(_)) => return Err(error),
        };

        self.lookup_fn_global(path)
    }

    fn lookup_label_scoped(&mut self, path: Spanned<Path>) -> LookupResult<LabelIndex> {
        let path = match self
            .lookup_scoped_only(path, NameKind::Label.into())
            .map_found(|scoped_index| match scoped_index {
                ScopedIndex::Label(label_index) => label_index,
                _ => unreachable!(),
            }) {
            Ok(found) => return Ok(found),
            Err(LookupError::Undefined(error)) => error.path,
            Err(error @ LookupError::WrongKind(_)) => return Err(error),
        };

        match self.lookup_global(path, NameKind::Label.into()) {
            Ok(_) => unreachable!(),
            Err(error) => Err(error),
        }
    }

    fn lookup_scoped_only(
        &mut self,
        path: Spanned<Path>,
        expected: EnumSet<NameKind>,
    ) -> LookupResult<ScopedIndex> {
        if path.value.has_parent() {
            Err(LookupError::Undefined(UndefinedError { path, expected }))
        } else {
            self.scoped_registry
                .lookup(path.map(|path| path.ident()), expected)
                .map_found(|scoped_index| *scoped_index)
        }
    }
}
