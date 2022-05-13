// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::{HashMap, HashSet};
use std::iter;

use derive_more::From;
use typed_index_collections::{TiSlice, TiVec};
use void::unreachable;

use cachet_lang::flattener;
use cachet_util::{typed_field_index, MaybeOwned};

use crate::bpl::ast::*;

#[derive(Clone, Debug, Default)]
pub struct FlowGraph {
    pub emit_nodes: TiVec<EmitNodeIndex, EmitNode>,
    pub label_nodes: TiVec<LabelNodeIndex, LabelNode>,
}

impl FlowGraph {
    pub fn exit_emit_node_index(&self) -> EmitNodeIndex {
        let exit_emit_node_index = self.emit_nodes.first_key().expect("missing exit emit node");
        debug_assert!(
            self[exit_emit_node_index].is_exit(),
            "first emit node does not correspond to an exit point"
        );
        exit_emit_node_index
    }

    fn link_label_to_emit(
        &mut self,
        label_node_index: LabelNodeIndex,
        emit_node_index: EmitNodeIndex,
    ) {
        self[label_node_index].bound_to.insert(emit_node_index);
    }
}

typed_field_index!(FlowGraph:emit_nodes[pub EmitNodeIndex] => EmitNode);
typed_field_index!(FlowGraph:label_nodes[pub LabelNodeIndex] => LabelNode);

#[derive(Clone, Debug)]
pub struct EmitNode {
    pub label_ident: EmitLabelIdent,
    pub succs: HashSet<EmitSucc>,
    pub target: Option<flattener::CallableIndex>,
}

impl EmitNode {
    pub fn is_exit(&self) -> bool {
        self.target.is_none()
    }
}

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum EmitSucc {
    Emit(EmitNodeIndex),
    Label(LabelNodeIndex),
}

pub type LabelScope = HashMap<flattener::LabelIndex, LabelNodeIndex>;

#[derive(Clone, Debug, Default)]
pub struct LabelNode {
    pub bound_to: HashSet<EmitNodeIndex>,
}

pub fn trace_entry_point(
    env: &flattener::Env,
    top_op_item: &flattener::CallableItem,
    bottom_ir_index: flattener::IrIndex,
) -> FlowGraph {
    let mut flow_tracer = FlowTracer::new(env, bottom_ir_index);
    flow_tracer.insert_exit_emit_node();
    flow_tracer.init_top_label_params(&top_op_item.params, &top_op_item.param_order);
    flow_tracer.trace_op_item(top_op_item);
    flow_tracer.link_exit_emit_node();

    let flow_graph = flow_tracer.graph.into_owned();
    debug_assert_eq!(
        flow_graph
            .emit_nodes
            .iter()
            .filter(|emit_node| emit_node.is_exit())
            .count(),
        1,
        "wrong number of exit emit nodes"
    );
    debug_assert_eq!(
        flow_graph
            .emit_nodes
            .iter_enumerated()
            .find(|(_, emit_node)| emit_node.succs.is_empty() && !emit_node.is_exit())
            .map(|(emit_node_index, _)| emit_node_index),
        None,
        "non-exit emit node has no successors"
    );
    debug_assert_eq!(
        flow_graph
            .label_nodes
            .iter_enumerated()
            .find(|(_, label_node)| label_node.bound_to.is_empty())
            .map(|(label_node_index, _)| label_node_index),
        None,
        "unbound label node",
    );
    flow_graph
}

struct FlowTracer<'a> {
    env: &'a flattener::Env,
    bottom_ir_index: flattener::IrIndex,
    graph: MaybeOwned<'a, FlowGraph>,
    curr_emit_label_ident: &'a EmitLabelIdent,
    next_local_emit_index: MaybeOwned<'a, LocalEmitIndex>,
    pred_emits: MaybeOwned<'a, HashSet<EmitNodeIndex>>,
    label_scope: MaybeOwned<'a, LabelScope>,
    bound_labels: MaybeOwned<'a, HashSet<LabelNodeIndex>>,
}

impl<'a> FlowTracer<'a> {
    fn new(env: &'a flattener::Env, bottom_ir_index: flattener::IrIndex) -> Self {
        static ROOT_EMIT_LABEL_IDENT: EmitLabelIdent = EmitLabelIdent {
            segments: Vec::new(),
        };

        FlowTracer {
            env,
            bottom_ir_index,
            graph: MaybeOwned::Owned(FlowGraph::default()),
            curr_emit_label_ident: &ROOT_EMIT_LABEL_IDENT,
            next_local_emit_index: MaybeOwned::Owned(LocalEmitIndex::from(0)),
            pred_emits: MaybeOwned::Owned(HashSet::new()),
            label_scope: MaybeOwned::Owned(LabelScope::new()),
            bound_labels: MaybeOwned::Owned(HashSet::new()),
        }
    }

    fn init_top_label_params(
        &mut self,
        params: &flattener::Params,
        param_order: &[flattener::ParamIndex],
    ) {
        for param_index in param_order {
            let exit_emit_node_index = self.graph.exit_emit_node_index();
            if let flattener::ParamIndex::Label(label_param_index) = param_index {
                let label_param = &params[label_param_index];
                // We only need to track control-flow between labels of the
                // bottom-level IR (i.e., the one that will be interpreted in
                // the end).
                if label_param.label.ir == self.bottom_ir_index {
                    let label_node_index = self.insert_label_node(label_param_index.into());
                    self.graph
                        .link_label_to_emit(label_node_index, exit_emit_node_index);
                }
            }
        }
    }

    fn init_label_params_with_args(
        &mut self,
        params: &flattener::Params,
        param_order: &[flattener::ParamIndex],
        args: &[flattener::Arg],
        caller_label_scope: &LabelScope,
    ) {
        for item in param_order.iter().zip(args) {
            if let (
                flattener::ParamIndex::Label(label_param_index),
                flattener::Arg::Label(label_arg),
            ) = item
            {
                let label_param = &params[label_param_index];
                if label_param.label.ir == self.bottom_ir_index {
                    let arg_label_node_index = caller_label_scope[&label_arg.label];
                    self.label_scope
                        .insert(label_param_index.into(), arg_label_node_index);
                }
            }
        }
    }

    fn trace_op_item(&mut self, op_item: &flattener::CallableItem) {
        if let Some(body) = &op_item.body {
            self.trace_body(body);
        }
    }

    fn trace_body(&mut self, body: &flattener::Body) {
        self.trace_local_labels(&body.locals.local_labels);
        self.trace_stmts(&body.stmts);
    }

    fn trace_local_labels(&mut self, local_labels: &TiSlice<LocalLabelIndex, flattener::Label>) {
        for (local_label_index, local_label) in local_labels.iter_enumerated() {
            if local_label.ir == self.bottom_ir_index {
                self.insert_label_node(local_label_index.into());
            }
        }
    }

    fn trace_stmts(&mut self, stmts: &[flattener::Stmt]) -> MaybeContinue {
        for stmt in stmts {
            self.trace_stmt(stmt)?;
        }

        Ok(())
    }

    fn trace_branches<'b>(&mut self, branches: impl Iterator<Item = &'b [flattener::Stmt]>) -> MaybeContinue {
        // First, back up the predecessor emits and bound labels going into the
        // branches.
        let init_pred_emits = self.pred_emits.clone();
        let init_bound_labels = self.bound_labels.clone();
        let mut cont = Ok(());
        self.pred_emits.clear();
        self.bound_labels.clear();

        for branch in branches {
            // Trace each branch from the starting predecessor emits and bound
            // labels.
            let mut flow_tracer = FlowTracer {
                env: self.env,
                bottom_ir_index: self.bottom_ir_index,
                graph: MaybeOwned::Borrowed(&mut self.graph),
                curr_emit_label_ident: self.curr_emit_label_ident,
                next_local_emit_index: MaybeOwned::Borrowed(&mut self.next_local_emit_index),
                pred_emits: MaybeOwned::Owned(init_pred_emits.clone()),
                label_scope: MaybeOwned::Borrowed(&mut self.label_scope),
                bound_labels: MaybeOwned::Owned(init_bound_labels.clone()),
            };

            // cont will only be EarlyReturn if all branches yield early return
            cont = cont.or(flow_tracer.trace_stmts(branch));

            // Accumulate the predecessor emits and bound labels at the end of
            // each branch.
            self.pred_emits.extend(flow_tracer.pred_emits.into_owned());
            self.bound_labels
                .extend(flow_tracer.bound_labels.into_owned());
        }

        cont
    }

    fn trace_stmt(&mut self, stmt: &flattener::Stmt) -> MaybeContinue {
        match stmt {
            flattener::Stmt::Let(_)
            | flattener::Stmt::Label(_)
            | flattener::Stmt::Check(_)
            | flattener::Stmt::Assign(_) => (),
            flattener::Stmt::Return(_) => Err(EarlyReturn)?,
            flattener::Stmt::If(if_stmt) => self.trace_if_stmt(if_stmt)?,
            flattener::Stmt::Goto(goto_stmt) => self.trace_goto_stmt(goto_stmt),
            flattener::Stmt::Bind(bind_stmt) => self.trace_bind_stmt(bind_stmt),
            flattener::Stmt::Emit(emit_stmt) => self.trace_emit_stmt(emit_stmt),
            flattener::Stmt::Block(void, _) => unreachable(*void),
            flattener::Stmt::Invoke(invoke_stmt) => self.trace_invoke_stmt(invoke_stmt),
        }

        Ok(())
    }

    fn trace_if_stmt(&mut self, if_stmt: &flattener::IfStmt) -> MaybeContinue {
        static EMPTY_BRANCH: Vec<flattener::Stmt> = Vec::new();
        let mut branches = vec![];
        let mut if_option = Some(if_stmt);
        while let Some(if_) = if_option {
            branches.push(if_.then.as_slice());
            match &if_.else_ {
                Some(flattener::ElseClause::ElseIf(if_next)) => if_option = Some(&*if_next),
                Some(flattener::ElseClause::Else(b)) => {
                    branches.push(&b);
                    if_option = None;
                }
                None => {
                    branches.push(&EMPTY_BRANCH);
                    if_option = None;
                }
            }
        }
        self.trace_branches(branches.into_iter())
    }

    fn trace_goto_stmt(&mut self, goto_stmt: &flattener::GotoStmt) {
        let label_node_index = self.label_scope[&goto_stmt.label];
        self.link_pred_emits_to_succ(label_node_index.into());
    }

    fn trace_bind_stmt(&mut self, bind_stmt: &flattener::BindStmt) {
        let label_node_index = self.label_scope[&bind_stmt.label];
        self.bound_labels.insert(label_node_index);
    }

    fn trace_emit_stmt(&mut self, emit_stmt: &flattener::EmitStmt) {
        let ir_item = &self.env[emit_stmt.ir];
        let op_item = &self.env[emit_stmt.call.target];

        let new_emit_label_segment = EmitLabelSegment {
            local_emit_index: self.take_local_emit_index(),
            ir_ident: ir_item.ident.value.into(),
            op_selector: UserOpSelector::from(op_item.path.value.ident()).into(),
        };

        // Extend the emit label identifier with the emit we're currently
        // looking at.
        let new_emit_label_ident: EmitLabelIdent = self
            .curr_emit_label_ident
            .segments
            .iter()
            .copied()
            .chain(iter::once(new_emit_label_segment))
            .collect();

        // If the op we're emitting is at the interpreter level, create a
        // corresponding emit node and set it as the new predecessor on-deck.
        if ir_item.emits.is_none() {
            self.insert_and_link_emit_node(
                new_emit_label_ident.clone(),
                Some(emit_stmt.call.target),
            );
        }

        // Trace inside the emitted op. It's important that we do this *after*
        // potentially creating a new emit node, so that, e.g., any goto
        // statements inside the op link their labels as successors of the new
        // emit node.

        let mut flow_tracer = FlowTracer {
            env: self.env,
            bottom_ir_index: self.bottom_ir_index,
            graph: MaybeOwned::Borrowed(&mut self.graph),
            curr_emit_label_ident: &new_emit_label_ident,
            next_local_emit_index: MaybeOwned::Owned(LocalEmitIndex::from(0)),
            pred_emits: MaybeOwned::Borrowed(&mut self.pred_emits),
            label_scope: MaybeOwned::Owned(LabelScope::new()),
            bound_labels: MaybeOwned::Borrowed(&mut self.bound_labels),
        };
        flow_tracer.init_label_params_with_args(
            &op_item.params,
            &op_item.param_order,
            &emit_stmt.call.args,
            &self.label_scope,
        );
        flow_tracer.trace_op_item(op_item);
    }

    fn trace_invoke_stmt(&mut self, invoke_stmt: &flattener::InvokeStmt) {
        // TODO(spinda): Handle non-external functions with label
        // out-parameters.
        debug_assert!(self.env[invoke_stmt.call.target].body.is_none());

        let exit_emit_node_index = self.graph.exit_emit_node_index();
        for arg in &invoke_stmt.call.args {
            if let flattener::Arg::Label(label_arg) = arg {
                if label_arg.is_out && label_arg.ir == self.bottom_ir_index {
                    let label_node_index = self.label_scope[&label_arg.label];
                    self.graph
                        .link_label_to_emit(label_node_index, exit_emit_node_index);
                }
            }
        }
    }

    fn insert_exit_emit_node(&mut self) {
        debug_assert!(self.graph.emit_nodes.is_empty());
        debug_assert!(self.pred_emits.is_empty());
        self.insert_emit_node(EmitLabelIdent::default(), None);
    }

    fn link_exit_emit_node(&mut self) {
        self.link_emit_node(self.graph.exit_emit_node_index());
    }

    fn insert_emit_node(
        &mut self,
        label_ident: EmitLabelIdent,
        target: Option<flattener::CallableIndex>,
    ) -> EmitNodeIndex {
        self.graph.emit_nodes.push_and_get_key(EmitNode {
            label_ident,
            succs: HashSet::new(),
            target,
        })
    }

    fn insert_and_link_emit_node(
        &mut self,
        label_ident: EmitLabelIdent,
        target: Option<flattener::CallableIndex>,
    ) {
        let emit_node_index = self.insert_emit_node(label_ident, target);
        self.link_emit_node(emit_node_index);
        self.pred_emits.insert(emit_node_index);
    }

    fn link_emit_node(&mut self, emit_node_index: EmitNodeIndex) {
        self.link_pred_emits_to_succ(emit_node_index.into());
        self.link_bound_labels_to_emit(emit_node_index);
    }

    fn link_pred_emits_to_succ(&mut self, succ: EmitSucc) {
        for pred_emit in self.pred_emits.drain() {
            self.graph[pred_emit].succs.insert(succ);
        }
    }

    fn link_bound_labels_to_emit(&mut self, emit_node_index: EmitNodeIndex) {
        for bound_label_index in self.bound_labels.drain() {
            self.graph
                .link_label_to_emit(bound_label_index, emit_node_index);
        }
    }

    fn take_local_emit_index(&mut self) -> LocalEmitIndex {
        let local_emit_index = *self.next_local_emit_index;
        *self.next_local_emit_index = LocalEmitIndex::from(usize::from(local_emit_index) + 1);
        local_emit_index
    }

    fn insert_label_node(&mut self, label_index: flattener::LabelIndex) -> LabelNodeIndex {
        let label_node_index = self
            .graph
            .label_nodes
            .push_and_get_key(LabelNode::default());
        self.label_scope.insert(label_index, label_node_index);
        label_node_index
    }
}

/// We use MaybeContinue to bubble up early returns while constructing traces.
/// EarlyReturn is used to signal an unconditional early return, thus all subsequent
/// code need not be traced. 
type MaybeContinue = Result<(), EarlyReturn>;
struct EarlyReturn;