//! Tarjan's strongly connected components algorithm. Implemented as described in the wiki page.

use crate::collections::{Map, Set};

pub(crate) fn strongconnect(num_nodes: u32, graph: &Map<u32, Set<u32>>) -> Vec<Set<u32>> {
    let mut sccs: Vec<Set<u32>> = vec![];
    let mut states: Map<u32, StrongConnectState> = Default::default();
    let mut stack: Vec<u32> = vec![];

    for node_idx in 0..num_nodes {
        if !states.contains_key(&node_idx) {
            strongconnect_(graph, &mut sccs, &mut states, &mut stack, node_idx);
        }
    }

    sccs
}

/// State of a node processed by the algorithm.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StrongConnectState {
    /// SCC group of the node.
    index: u32,

    /// Smallest `index` of any node on the stack known to be reachable from the node through the
    /// node's DFS subtree.
    lowlink: u32,

    /// Whether the node is on stack.
    on_stack: bool,
}

fn strongconnect_(
    graph: &Map<u32, Set<u32>>,
    sccs: &mut Vec<Set<u32>>,
    states: &mut Map<u32, StrongConnectState>,
    stack: &mut Vec<u32>,
    node_idx: u32,
) {
    // Allocate a group for the binding.
    let index = sccs.len() as u32;
    sccs.push(Default::default());

    stack.push(node_idx);

    let old_state = states.insert(
        node_idx,
        StrongConnectState {
            index,
            lowlink: index,
            on_stack: true,
        },
    );
    assert_eq!(old_state, None);

    // Consider successors.
    for dep in graph.get(&node_idx).unwrap_or(&Default::default()) {
        match states.get(dep) {
            None => {
                // Successor has not yet been visited; recurse on it.
                strongconnect_(graph, sccs, states, stack, *dep);
                let dep_lowlink = states.get(dep).unwrap().lowlink;
                let bind_state = states.get_mut(&node_idx).unwrap();
                bind_state.lowlink = std::cmp::min(bind_state.lowlink, dep_lowlink);
            }
            Some(dep_state) => {
                if dep_state.on_stack {
                    // Successor is in stack and hence in the current SCC.
                    //
                    // If dep is not on stack, then (bind, dep) is an edge pointing to an SCC
                    // already found and must be ignored.
                    let dep_index = dep_state.index;
                    let bind_state = states.get_mut(&node_idx).unwrap();
                    bind_state.lowlink = std::cmp::min(bind_state.lowlink, dep_index);
                }
            }
        }
    }

    // If v is a root node, pop the stack and generate an SCC.
    let bind_state = states.get(&node_idx).unwrap();
    if bind_state.index == bind_state.lowlink {
        // Start a new strongly connected component.
        let mut bind_group: Set<u32> = Default::default();
        loop {
            let w = stack.pop().unwrap();
            let w_state = states.get_mut(&w).unwrap();
            w_state.on_stack = false;
            let new = bind_group.insert(w);
            assert!(new);
            if w == node_idx {
                break;
            }
        }
        sccs.push(bind_group);
    }
}
