use crate::ast;
use crate::bind_groups::{BindingGroup, FunBindingGroup, PatBinding};
use crate::collections::{Map, Set};
use crate::id::Id;
use crate::type_scheme::Scheme;

pub(crate) struct DependencyGroups<'a> {
    pub(crate) implicitly_typed: Vec<Vec<&'a BindingGroup<'a>>>,
    pub(crate) explicitly_typed: Vec<&'a BindingGroup<'a>>,
}

pub(crate) fn dependency_analysis<'a>(
    groups: &'a [BindingGroup],
    explicit_tys: &Map<Id, Scheme>,
) -> DependencyGroups<'a> {
    let bound_vars = collect_implicitly_typed_bound_vars(groups, explicit_tys);
    let dep_graph = create_dependency_graph(groups, &bound_vars);
    let sccs = strongconnect(groups, &dep_graph);
    let implicitly_typed = group_sccs(groups, &sccs);
    let explicitly_typed = collect_explicitly_typed_bindings(groups, &sccs);
    DependencyGroups {
        implicitly_typed,
        explicitly_typed,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct BindingGroupIdx(u32);

impl BindingGroupIdx {
    fn from_usize(i: usize) -> Self {
        Self(i as u32)
    }

    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

/// Maps an implicitly typed definition to its implicitly typed dependencies.
type DepGraph = Map<BindingGroupIdx, Set<BindingGroupIdx>>;

/// Map variables to their definitions in `groups`.
///
/// The returned map does not include explicitly typed bindings, as those bindings do not take part
/// in dependency analysis for type inference.
fn collect_implicitly_typed_bound_vars<A>(
    groups: &[BindingGroup],
    explicit_tys: &Map<Id, A>,
) -> Map<Id, BindingGroupIdx> {
    let mut bound_vars: Map<Id, BindingGroupIdx> = Default::default();

    for (group_idx, group) in groups.iter().enumerate() {
        let group_idx = BindingGroupIdx::from_usize(group_idx);
        match group {
            BindingGroup::Pat(PatBinding { pat, rhs: _ }) => {
                for var in pat.node.vars() {
                    if !explicit_tys.contains_key(&var) {
                        let old = bound_vars.insert(var, group_idx);
                        debug_assert_eq!(old, None);
                    }
                }
            }
            BindingGroup::Fun(FunBindingGroup { id, defs: _ }) => {
                if !explicit_tys.contains_key(id) {
                    let old = bound_vars.insert(id.clone(), group_idx);
                    debug_assert_eq!(old, None);
                }
            }
        }
    }

    bound_vars
}

fn create_dependency_graph(
    groups: &[BindingGroup],
    bound_vars: &Map<Id, BindingGroupIdx>,
) -> Map<BindingGroupIdx, Set<BindingGroupIdx>> {
    let mut dep_graph: DepGraph = Default::default();

    for (group_idx, group) in groups.iter().enumerate() {
        let group_idx = BindingGroupIdx::from_usize(group_idx);
        match group {
            BindingGroup::Pat(PatBinding { pat: _, rhs }) => {
                analyze_rhs(group_idx, bound_vars, &mut dep_graph, rhs);
            }
            BindingGroup::Fun(FunBindingGroup { id: _, defs }) => {
                for def in defs {
                    analyze_rhs(group_idx, bound_vars, &mut dep_graph, def.rhs);
                }
            }
        }
    }

    dep_graph
}

fn analyze_rhs(
    group_idx: BindingGroupIdx,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
    rhs: &ast::RenamedRhs,
) {
    match &rhs.node {
        ast::Rhs_::GuardedRhs { rhss, where_decls } => {
            analyze_guarded_rhss(group_idx, bound_vars, dep_graph, rhss);
            analyze_where_decls(group_idx, bound_vars, dep_graph, where_decls);
        }
        ast::Rhs_::Rhs { rhs, where_decls } => {
            analyze_exp(group_idx, bound_vars, dep_graph, rhs);
            analyze_where_decls(group_idx, bound_vars, dep_graph, where_decls);
        }
    }
}

fn analyze_guarded_rhss(
    use_site: BindingGroupIdx,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
    guarded_rhss: &[ast::RenamedGuardedRhs],
) {
    for ast::AstNode {
        node: ast::GuardedRhs_ { guards, rhs },
        ..
    } in guarded_rhss
    {
        analyze_stmts(use_site, bound_vars, dep_graph, guards);
        analyze_exp(use_site, bound_vars, dep_graph, rhs);
    }
}

fn analyze_where_decls(
    use_site: BindingGroupIdx,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
    where_decls: &[ast::RenamedValueDecl],
) {
    for decl in where_decls {
        match &decl.node {
            ast::ValueDecl_::TypeSig { .. } | ast::ValueDecl_::Fixity { .. } => {}
            ast::ValueDecl_::Value { lhs: _, rhs } => match &rhs.node {
                ast::Rhs_::GuardedRhs { rhss, where_decls } => {
                    analyze_guarded_rhss(use_site, bound_vars, dep_graph, rhss);
                    analyze_where_decls(use_site, bound_vars, dep_graph, where_decls);
                }
                ast::Rhs_::Rhs { rhs, where_decls } => {
                    analyze_exp(use_site, bound_vars, dep_graph, rhs);
                    analyze_where_decls(use_site, bound_vars, dep_graph, where_decls);
                }
            },
        }
    }
}

fn analyze_exp(
    use_site: BindingGroupIdx,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
    exp: &ast::RenamedExp,
) {
    match &exp.node {
        ast::Exp_::Var(var) | ast::Exp_::Con(var) => {
            add_dep(use_site, var.clone(), bound_vars, dep_graph)
        }

        ast::Exp_::Lit(_) => {}

        ast::Exp_::Lam(_, exp) => analyze_exp(use_site, bound_vars, dep_graph, exp),

        ast::Exp_::App(exp, exps) => {
            analyze_exp(use_site, bound_vars, dep_graph, exp);
            for exp in exps {
                analyze_exp(use_site, bound_vars, dep_graph, exp);
            }
        }

        ast::Exp_::Tuple(exps) | ast::Exp_::List(exps) => {
            for exp in exps {
                analyze_exp(use_site, bound_vars, dep_graph, exp);
            }
        }

        ast::Exp_::Do(stmts) => analyze_stmts(use_site, bound_vars, dep_graph, stmts),

        ast::Exp_::TypeAnnotation {
            exp,
            context: _,
            type_: _,
        } => analyze_exp(use_site, bound_vars, dep_graph, exp),

        ast::Exp_::ArithmeticSeq { exp1, exp2, exp3 } => {
            analyze_exp(use_site, bound_vars, dep_graph, exp1);
            if let Some(exp) = exp2 {
                analyze_exp(use_site, bound_vars, dep_graph, exp);
            }
            if let Some(exp) = exp3 {
                analyze_exp(use_site, bound_vars, dep_graph, exp);
            }
        }

        ast::Exp_::ListComp { exp, quals } => {
            analyze_exp(use_site, bound_vars, dep_graph, exp);
            analyze_stmts(use_site, bound_vars, dep_graph, quals);
        }

        ast::Exp_::Case(exp, alts) => {
            analyze_exp(use_site, bound_vars, dep_graph, exp);
            analyze_alts(use_site, bound_vars, dep_graph, alts);
        }

        ast::Exp_::If(e1, e2, e3) => {
            analyze_exp(use_site, bound_vars, dep_graph, e1);
            analyze_exp(use_site, bound_vars, dep_graph, e2);
            analyze_exp(use_site, bound_vars, dep_graph, e3);
        }

        ast::Exp_::Let(decls, exp) => {
            analyze_where_decls(use_site, bound_vars, dep_graph, decls);
            analyze_exp(use_site, bound_vars, dep_graph, exp);
        }

        ast::Exp_::Update { exp, updates } => {
            analyze_exp(use_site, bound_vars, dep_graph, exp);
            for (_, exp) in updates {
                analyze_exp(use_site, bound_vars, dep_graph, exp);
            }
        }

        ast::Exp_::ReAssoc(_, _, _) => panic!("ReAssoc node in dependency analysis"),
    }
}

fn analyze_stmts(
    use_site: BindingGroupIdx,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
    stmts: &[ast::RenamedStmt],
) {
    for stmt in stmts {
        match &stmt.node {
            ast::Stmt_::Exp(exp) | ast::Stmt_::Bind(_, exp) => {
                analyze_exp(use_site, bound_vars, dep_graph, exp)
            }
            ast::Stmt_::Let(decls) => {
                analyze_where_decls(use_site, bound_vars, dep_graph, decls);
            }
        }
    }
}

fn analyze_alts(
    use_site: BindingGroupIdx,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
    alts: &[ast::RenamedAlt],
) {
    for ast::AstNode {
        node:
            ast::Alt_ {
                pat: _,
                guarded_rhss,
                where_decls,
            },
        ..
    } in alts
    {
        for (stmts, exp) in guarded_rhss {
            analyze_stmts(use_site, bound_vars, dep_graph, stmts);
            analyze_exp(use_site, bound_vars, dep_graph, exp);
        }
        analyze_where_decls(use_site, bound_vars, dep_graph, where_decls);
    }
}

fn add_dep(
    use_site: BindingGroupIdx,
    var: Id,
    bound_vars: &Map<Id, BindingGroupIdx>,
    dep_graph: &mut DepGraph,
) {
    if let Some(decl_idx) = bound_vars.get(&var) {
        dep_graph.entry(use_site).or_default().insert(*decl_idx);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

// Tarjan's strongly connected components algorithm. Implemented as described in the wiki page.

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

fn strongconnect(groups: &[BindingGroup], dep_graph: &DepGraph) -> Vec<Set<BindingGroupIdx>> {
    let mut sccs: Vec<Set<BindingGroupIdx>> = vec![];
    let mut states: Map<BindingGroupIdx, StrongConnectState> = Default::default();
    let mut stack: Vec<BindingGroupIdx> = vec![];

    for group_idx in 0..groups.len() {
        let group_idx = BindingGroupIdx::from_usize(group_idx);
        if !states.contains_key(&group_idx) {
            strongconnect_(dep_graph, &mut sccs, &mut states, &mut stack, group_idx);
        }
    }

    sccs
}

fn strongconnect_(
    dep_graph: &DepGraph,
    sccs: &mut Vec<Set<BindingGroupIdx>>,
    states: &mut Map<BindingGroupIdx, StrongConnectState>,
    stack: &mut Vec<BindingGroupIdx>,
    group_idx: BindingGroupIdx,
) {
    // Allocate a group for the binding.
    let index = sccs.len() as u32;
    sccs.push(Default::default());

    stack.push(group_idx);

    let old_state = states.insert(
        group_idx,
        StrongConnectState {
            index,
            lowlink: index,
            on_stack: true,
        },
    );
    assert_eq!(old_state, None);

    // Consider successors.
    for dep in dep_graph.get(&group_idx).unwrap_or(&Default::default()) {
        match states.get(dep) {
            None => {
                // Successor has not yet been visited; recurse on it.
                strongconnect_(dep_graph, sccs, states, stack, *dep);
                let dep_lowlink = states.get(dep).unwrap().lowlink;
                let bind_state = states.get_mut(&group_idx).unwrap();
                bind_state.lowlink = std::cmp::min(bind_state.lowlink, dep_lowlink);
            }
            Some(dep_state) => {
                if dep_state.on_stack {
                    // Successor is in stack and hence in the current SCC.
                    //
                    // If dep is not on stack, then (bind, dep) is an edge pointing to an SCC
                    // already found and must be ignored.
                    let dep_index = dep_state.index;
                    let bind_state = states.get_mut(&group_idx).unwrap();
                    bind_state.lowlink = std::cmp::min(bind_state.lowlink, dep_index);
                }
            }
        }
    }

    // If v is a root node, pop the stack and generate an SCC.
    let bind_state = states.get(&group_idx).unwrap();
    if bind_state.index == bind_state.lowlink {
        // Start a new strongly connected component.
        let mut bind_group: Set<BindingGroupIdx> = Default::default();
        loop {
            let w = stack.pop().unwrap();
            let w_state = states.get_mut(&w).unwrap();
            w_state.on_stack = false;
            let new = bind_group.insert(w);
            assert!(new);
            if w == group_idx {
                break;
            }
        }
        sccs.push(bind_group);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

fn group_sccs<'a, 'b>(
    groups: &'a [BindingGroup<'b>],
    sccs: &[Set<BindingGroupIdx>],
) -> Vec<Vec<&'a BindingGroup<'b>>> {
    let mut grouped = Vec::with_capacity(sccs.len());

    for scc in sccs {
        let mut group = Vec::with_capacity(scc.len());
        for idx in scc {
            group.push(&groups[idx.as_usize()]);
        }
        grouped.push(group);
    }

    grouped
}

////////////////////////////////////////////////////////////////////////////////////////////////////

fn collect_explicitly_typed_bindings<'a, 'b, 'c>(
    groups: &'a [BindingGroup<'b>],
    implicitly_typed_sccs: &'c [Set<BindingGroupIdx>],
) -> Vec<&'a BindingGroup<'b>> {
    let mut implicits: Set<BindingGroupIdx> = Default::default();
    for scc in implicitly_typed_sccs {
        implicits.extend(scc);
    }

    let mut explicit_groups: Vec<&'a BindingGroup<'b>> = vec![];
    for (group_idx, group) in groups.iter().enumerate() {
        if !implicits.contains(&BindingGroupIdx::from_usize(group_idx)) {
            explicit_groups.push(group);
        }
    }

    explicit_groups
}
