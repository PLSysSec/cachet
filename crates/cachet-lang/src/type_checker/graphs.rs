// vim: set tw=99 ts=4 sts=4 sw=4 et:

use petgraph::algo::tarjan_scc;
use petgraph::graph::{DiGraph, NodeIndex};
use typed_index_collections::TiSlice;

use crate::ast::{Span, Spanned};
use crate::built_in::BuiltInType;

use crate::type_checker::ast::{
    CallableIndex, EnumIndex, EnumItem, FnIndex, OpIndex, StructIndex, StructItem, TypeIndex,
};

pub struct TypeGraph {
    inner: DiGraph<(), ()>,
    num_enum_items: usize,
}

impl TypeGraph {
    pub fn new(
        enum_items: &TiSlice<EnumIndex, EnumItem>,
        struct_items: &TiSlice<StructIndex, StructItem>,
    ) -> Self {
        let num_enum_items = enum_items.len();
        let num_struct_items = struct_items.len();
        let num_types = BuiltInType::COUNT + num_enum_items + num_struct_items;

        let mut inner = DiGraph::with_capacity(num_types, 0);

        for _ in 0..num_types {
            inner.add_node(());
        }

        let get_built_in_type_node_index =
            |built_in_type: BuiltInType| NodeIndex::new(built_in_type.index());
        let get_enum_node_index =
            |enum_index: EnumIndex| NodeIndex::new(BuiltInType::COUNT + usize::from(enum_index));
        let get_struct_node_index = |struct_index: StructIndex| {
            NodeIndex::new(BuiltInType::COUNT + num_enum_items + usize::from(struct_index))
        };
        let get_type_node_index = |type_index| match type_index {
            TypeIndex::BuiltIn(built_in_type) => get_built_in_type_node_index(built_in_type),
            TypeIndex::Enum(enum_index) => get_enum_node_index(enum_index),
            TypeIndex::Struct(struct_index) => get_struct_node_index(struct_index),
        };

        for built_in_type in BuiltInType::ALL {
            if let Some(supertype) = built_in_type.supertype() {
                inner.add_edge(
                    get_built_in_type_node_index(supertype),
                    get_built_in_type_node_index(built_in_type),
                    (),
                );
            }
        }

        for (struct_index, struct_item) in struct_items.iter_enumerated() {
            if let Some(supertype) = struct_item.supertype {
                inner.add_edge(
                    get_type_node_index(supertype),
                    get_struct_node_index(struct_index),
                    (),
                );
            }
        }

        let never_idx = get_built_in_type_node_index(BuiltInType::Never);
        // Never is a subtype of all types!
        for other_idx in inner.node_indices() {
            if other_idx != never_idx {
                inner.add_edge(other_idx, never_idx, ());
            }
        }

        TypeGraph {
            inner,
            num_enum_items,
        }
    }

    pub fn sccs(&self) -> TypeSccs<'_> {
        TypeSccs {
            inner: GraphSccs::new(&self.inner),
            num_enum_items: self.num_enum_items,
        }
    }
}

pub struct TypeSccs<'a> {
    inner: GraphSccs<'a, (), ()>,
    num_enum_items: usize,
}

impl<'a> TypeSccs<'a> {
    pub fn iter_cycles(&self) -> impl '_ + Iterator<Item = impl '_ + Iterator<Item = TypeIndex>> {
        self.inner.iter_cycles().map(move |cycle_node_indexes| {
            cycle_node_indexes
                .iter()
                .map(|node_index| self.get_type_index(*node_index))
        })
    }

    pub fn iter_post_order(&self) -> impl '_ + Iterator<Item = TypeIndex> {
        self.inner
            .iter_post_order()
            .map(|node_index| self.get_type_index(node_index))
    }

    fn get_type_index(&self, node_index: NodeIndex) -> TypeIndex {
        let mut node_index = node_index.index();

        if let Some(built_in) = BuiltInType::from_index(node_index) {
            return built_in.into();
        }
        node_index -= BuiltInType::COUNT;

        if node_index < self.num_enum_items {
            return EnumIndex::from(node_index).into();
        }
        node_index -= self.num_enum_items;

        StructIndex::from(node_index).into()
    }
}

pub struct CallGraph {
    inner: DiGraph<(), Span>,
    num_fn_items: usize,
}

impl CallGraph {
    pub fn new(num_fn_items: usize, num_op_items: usize) -> Self {
        let num_callable_items = num_fn_items + num_op_items;
        let mut inner = DiGraph::with_capacity(num_callable_items, 0);
        for _ in 0..num_callable_items {
            inner.add_node(());
        }
        CallGraph {
            inner,
            num_fn_items,
        }
    }

    pub fn record_call(
        &mut self,
        source_callable_index: CallableIndex,
        target_callable_index: Spanned<CallableIndex>,
    ) {
        self.inner.add_edge(
            self.get_node_index(source_callable_index),
            self.get_node_index(target_callable_index.value),
            target_callable_index.span,
        );
    }

    pub fn sccs(&self) -> CallableSccs<'_> {
        CallableSccs {
            inner: GraphSccs::new(&self.inner),
            num_fn_items: self.num_fn_items,
        }
    }

    fn get_node_index(&self, callable_index: CallableIndex) -> NodeIndex {
        NodeIndex::new(match callable_index {
            CallableIndex::Fn(fn_index) => usize::from(fn_index),
            CallableIndex::Op(op_index) => self.num_fn_items + usize::from(op_index),
        })
    }
}

pub struct CallableSccs<'a> {
    inner: GraphSccs<'a, (), Span>,
    num_fn_items: usize,
}

impl<'a> CallableSccs<'a> {
    pub fn iter_cycles(
        &self,
    ) -> impl '_ + Iterator<Item = impl '_ + Iterator<Item = Spanned<CallableIndex>>> {
        self.inner.iter_cycles().map(move |cycle_node_indexes| {
            let mut prev_node_index = *cycle_node_indexes.last().unwrap();
            cycle_node_indexes.iter().copied().map(move |node_index| {
                let span = self.inner.graph[self
                    .inner
                    .graph
                    .find_edge(prev_node_index, node_index)
                    .unwrap()];
                prev_node_index = node_index;
                Spanned::new(span, self.get_callable_index(node_index))
            })
        })
    }

    pub fn iter_post_order(&self) -> impl '_ + Iterator<Item = CallableIndex> {
        self.inner
            .iter_post_order()
            .map(|node_index| self.get_callable_index(node_index))
    }

    fn get_callable_index(&self, node_index: NodeIndex) -> CallableIndex {
        let mut node_index = node_index.index();

        if node_index < self.num_fn_items {
            return FnIndex::from(node_index).into();
        }
        node_index -= self.num_fn_items;

        OpIndex::from(node_index).into()
    }
}

pub struct GraphSccs<'a, N, E> {
    graph: &'a DiGraph<N, E>,
    sccs: Vec<Vec<NodeIndex>>,
}

impl<'a, N, E> GraphSccs<'a, N, E> {
    pub fn new(graph: &'a DiGraph<N, E>) -> Self {
        let mut sccs = tarjan_scc(graph);
        for scc in sccs.iter_mut() {
            // Reversing the order within the SCCs causes the results to be
            // ordered such that the node defined first appears first, followed
            // by one of its immediately reachable nodes, followed by one of
            // *that* node's immediately reachable nodes, and so on. The first
            // node in the order is immediately reachable from the last node.
            scc.reverse();
        }
        GraphSccs { graph, sccs }
    }

    pub fn iter_cycles(&self) -> impl '_ + Iterator<Item = &[NodeIndex]> {
        self.sccs
            .iter()
            .filter(move |scc| scc.len() > 1 || self.graph.find_edge(scc[0], scc[0]).is_some())
            .map(Vec::as_ref)
    }

    pub fn iter_post_order(&self) -> impl '_ + Iterator<Item = NodeIndex> {
        self.sccs.iter().flat_map(|scc| scc.iter().copied())
    }
}
