use crate::ast;
use crate::bind_groups::{BindingGroup, FunBindingGroup, PatBinding};
use crate::collections::{Map, Set};
use crate::id::Id;
use crate::scc::strongconnect;
use crate::type_scheme::Scheme;

pub(crate) struct DependencyGroups<'a> {
    /// Strongly connected components of implicitly typed bindings, in reverse topological order,
    /// i.e. definitions come before the use sites.
    pub(crate) implicitly_typed: Vec<Vec<&'a BindingGroup<'a>>>,

    /// Explicitly typed bindings.
    pub(crate) explicitly_typed: Vec<&'a BindingGroup<'a>>,
}

pub(crate) fn dependency_analysis<'a>(
    groups: &'a [BindingGroup],
    explicit_tys: &Map<Id, Scheme>,
) -> DependencyGroups<'a> {
    let bound_vars = collect_implicitly_typed_bound_vars(groups, explicit_tys);
    let dep_graph = create_dependency_graph(groups, &bound_vars);
    let sccs = strongconnect(groups.len() as u32, &dep_graph);
    let implicitly_typed = group_sccs(groups, &sccs);
    let explicitly_typed = collect_explicitly_typed_bindings(groups, &sccs);
    DependencyGroups {
        implicitly_typed,
        explicitly_typed,
    }
}

/// Maps an implicitly typed definition to its implicitly typed dependencies.
type DepGraph = Map<u32, Set<u32>>;

/// Map variables to their definitions in `groups`.
///
/// The returned map does not include explicitly typed bindings, as those bindings do not take part
/// in dependency analysis for type inference.
fn collect_implicitly_typed_bound_vars<A>(
    groups: &[BindingGroup],
    explicit_tys: &Map<Id, A>,
) -> Map<Id, u32> {
    let mut bound_vars: Map<Id, u32> = Default::default();

    for (group_idx, group) in groups.iter().enumerate() {
        let group_idx = group_idx as u32;
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

fn create_dependency_graph(groups: &[BindingGroup], bound_vars: &Map<Id, u32>) -> DepGraph {
    let mut dep_graph: DepGraph = Default::default();

    for (group_idx, group) in groups.iter().enumerate() {
        let group_idx = group_idx as u32;
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
    group_idx: u32,
    bound_vars: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
    rhs: &ast::Rhs,
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
    use_site: u32,
    bound_vars: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
    guarded_rhss: &[ast::GuardedRhs],
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
    use_site: u32,
    bound_vars: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
    where_decls: &[ast::ValueDecl],
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

fn analyze_exp(use_site: u32, bound_vars: &Map<Id, u32>, dep_graph: &mut DepGraph, exp: &ast::Exp) {
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
    use_site: u32,
    bound_vars: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
    stmts: &[ast::Stmt],
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
    use_site: u32,
    bound_vars: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
    alts: &[ast::Alt],
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

fn add_dep(use_site: u32, var: Id, bound_vars: &Map<Id, u32>, dep_graph: &mut DepGraph) {
    if let Some(decl_idx) = bound_vars.get(&var) {
        dep_graph.entry(use_site).or_default().insert(*decl_idx);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

fn group_sccs<'a, 'b>(
    groups: &'a [BindingGroup<'b>],
    sccs: &[Set<u32>],
) -> Vec<Vec<&'a BindingGroup<'b>>> {
    let mut grouped = Vec::with_capacity(sccs.len());

    for scc in sccs {
        let mut group = Vec::with_capacity(scc.len());
        for idx in scc {
            group.push(&groups[*idx as usize]);
        }
        grouped.push(group);
    }

    grouped
}

////////////////////////////////////////////////////////////////////////////////////////////////////

fn collect_explicitly_typed_bindings<'a, 'b, 'c>(
    groups: &'a [BindingGroup<'b>],
    implicitly_typed_sccs: &'c [Set<u32>],
) -> Vec<&'a BindingGroup<'b>> {
    let mut implicits: Set<u32> = Default::default();
    for scc in implicitly_typed_sccs {
        implicits.extend(scc);
    }

    let mut explicit_groups: Vec<&'a BindingGroup<'b>> = vec![];
    for (group_idx, group) in groups.iter().enumerate() {
        if !implicits.contains(&(group_idx as u32)) {
            explicit_groups.push(group);
        }
    }

    explicit_groups
}
