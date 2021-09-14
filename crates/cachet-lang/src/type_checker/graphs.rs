// vim: set tw=99 ts=4 sts=4 sw=4 et:

use codespan::Span;
use petgraph::algo::tarjan_scc;
use petgraph::graph::{DiGraph, NodeIndex};

use crate::type_checker::ast::{
    EnumDef, FnIndex, Spanned, StructDef, TypeIndex, BUILT_IN_TYPES, NUM_BUILT_IN_TYPES,
};

pub struct TypeGraph {
    inner: DiGraph<TypeIndex, ()>,
}

impl TypeGraph {
    pub fn new(enum_defs: &[EnumDef], struct_defs: &[StructDef]) -> Self {
        let num_enums = enum_defs.len();
        let num_structs = struct_defs.len();
        let num_types = NUM_BUILT_IN_TYPES + num_enums + num_structs;

        let mut inner = DiGraph::with_capacity(num_types, 0);

        let built_in_type_node_indices =
            BUILT_IN_TYPES.map(|built_in_type| inner.add_node(built_in_type.into()));
        for built_in_type in BUILT_IN_TYPES {
            if let Some(subtype_of) = built_in_type.subtype_of() {
                inner.add_edge(
                    built_in_type_node_indices[subtype_of],
                    built_in_type_node_indices[built_in_type],
                    (),
                );
            }
        }

        let enum_node_indices: Vec<_> = (0..num_enums)
            .map(|enum_index| inner.add_node(TypeIndex::Enum(enum_index)))
            .collect();
        let struct_node_indices: Vec<_> = (0..num_structs)
            .map(|struct_index| inner.add_node(TypeIndex::Struct(struct_index)))
            .collect();

        let get_type_node_index = |type_index| match type_index {
            TypeIndex::BuiltIn(built_in_type) => built_in_type_node_indices[built_in_type],
            TypeIndex::Enum(enum_index) => enum_node_indices[enum_index],
            TypeIndex::Struct(struct_index) => struct_node_indices[struct_index],
        };

        for (struct_node_index, struct_def) in struct_node_indices.iter().zip(struct_defs.iter()) {
            if let Some(subtype_of) = struct_def.subtype_of {
                inner.add_edge(get_type_node_index(subtype_of), *struct_node_index, ());
            }
        }

        TypeGraph { inner }
    }

    pub fn sccs(&self) -> TypeSccs<'_> {
        TypeSccs {
            inner: GraphSccs::new(&self.inner),
        }
    }
}

pub struct TypeSccs<'a> {
    inner: GraphSccs<'a, TypeIndex, ()>,
}

impl<'a> TypeSccs<'a> {
    pub fn iter_cycles(&self) -> impl '_ + Iterator<Item = impl '_ + Iterator<Item = TypeIndex>> {
        self.inner.iter_cycles().map(move |cycle_node_indices| {
            cycle_node_indices
                .iter()
                .map(move |node_index| self.inner.graph[*node_index])
        })
    }

    pub fn iter_post_order(&self) -> impl '_ + Iterator<Item = TypeIndex> {
        self.inner
            .iter_post_order()
            .map(move |node_index| self.inner.graph[node_index])
    }
}

pub struct FnGraph {
    inner: DiGraph<(), Span>,
}

impl FnGraph {
    pub fn new(num_fns: usize) -> Self {
        let mut inner = DiGraph::with_capacity(num_fns, 0);
        for _ in 0..num_fns {
            inner.add_node(());
        }
        FnGraph { inner }
    }

    pub fn record_call(&mut self, source_fn: FnIndex, target_fn: Spanned<FnIndex>) {
        self.inner.add_edge(
            NodeIndex::new(source_fn),
            NodeIndex::new(target_fn.value),
            target_fn.span,
        );
    }

    pub fn sccs(&self) -> FnSccs<'_> {
        FnSccs {
            inner: GraphSccs::new(&self.inner),
        }
    }
}

pub struct FnSccs<'a> {
    inner: GraphSccs<'a, (), Span>,
}

impl<'a> FnSccs<'a> {
    pub fn iter_cycles(
        &self,
    ) -> impl '_ + Iterator<Item = impl '_ + Iterator<Item = Spanned<FnIndex>>> {
        self.inner.iter_cycles().map(move |cycle_node_indices| {
            let mut prev_node_index = *cycle_node_indices.last().unwrap();
            cycle_node_indices.iter().copied().map(move |node_index| {
                let span = self.inner.graph[self
                    .inner
                    .graph
                    .find_edge(prev_node_index, node_index)
                    .unwrap()];
                prev_node_index = node_index;
                Spanned::new(span, node_index.index())
            })
        })
    }

    pub fn iter_post_order(&self) -> impl '_ + Iterator<Item = FnIndex> {
        self.inner.iter_post_order().map(NodeIndex::index)
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
