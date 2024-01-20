use crate::ast;
use crate::collections::{Map, Set};
use crate::id::Id;
use crate::scc::strongconnect;

#[allow(unused)]
pub(super) fn dependency_analysis(decls: &[ast::RenamedTopDecl]) -> Vec<Set<u32>> {
    let explicitly_kinded_tys = collect_explicitly_kinded_tys(decls);
    let defs = collect_implicitly_kinded_types(decls, &explicitly_kinded_tys);
    let dep_graph = create_dependency_graph(decls, &defs);
    strongconnect(decls.len() as u32, &dep_graph)
}

fn collect_explicitly_kinded_tys(decls: &[ast::RenamedTopDecl]) -> Set<Id> {
    let mut ids: Set<Id> = Default::default();
    for decl in decls {
        if let ast::TopDeclKind_::KindSig(sig) = &decl.kind {
            ids.insert(sig.node.ty.clone());
        }
    }
    ids
}

fn collect_implicitly_kinded_types(
    decls: &[ast::RenamedTopDecl],
    explicitly_kinded_tys: &Set<Id>,
) -> Map<Id, u32> {
    let mut tys: Map<Id, u32> = Default::default();

    for (decl_idx, decl) in decls.iter().enumerate() {
        let decl_idx = decl_idx as u32;

        let id = match &decl.kind {
            ast::TopDeclKind_::Type(type_decl) => &type_decl.node.ty,

            ast::TopDeclKind_::Data(data_decl) => &data_decl.node.ty_con,

            ast::TopDeclKind_::Newtype(newtype_decl) => &newtype_decl.node.ty_con,

            ast::TopDeclKind_::Class(class_decl) => &class_decl.node.ty_con,

            ast::TopDeclKind_::Default(_)
            | ast::TopDeclKind_::Instance(_)
            | ast::TopDeclKind_::KindSig(_)
            | ast::TopDeclKind_::Value(_)
            | ast::TopDeclKind_::Unparsed => {
                continue;
            }
        };

        if !explicitly_kinded_tys.contains(id) {
            tys.insert(id.clone(), decl_idx);
        }
    }

    tys
}

/// Maps an implicitly kinded definition to its implicitly kinded dependencies.
type DepGraph = Map<u32, Set<u32>>;

fn create_dependency_graph(decls: &[ast::RenamedTopDecl], defs: &Map<Id, u32>) -> DepGraph {
    let mut dep_graph: DepGraph = Default::default();

    for (decl_idx, decl) in decls.iter().enumerate() {
        let decl_idx = decl_idx as u32;
        analyze_decl(decl_idx, decl, defs, &mut dep_graph);
    }

    dep_graph
}

fn analyze_decl(
    decl_idx: u32,
    decl: &ast::RenamedTopDecl,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    match &decl.kind {
        ast::TopDeclKind_::Type(type_decl) => {
            analyze_type_decl(decl_idx, type_decl, defs, dep_graph)
        }

        ast::TopDeclKind_::Data(data_decl) => {
            analyze_data_decl(decl_idx, data_decl, defs, dep_graph)
        }

        ast::TopDeclKind_::Newtype(newtype_decl) => {
            analyze_newtype_decl(decl_idx, newtype_decl, defs, dep_graph)
        }

        ast::TopDeclKind_::Class(class_decl) => {
            analyze_class_decl(decl_idx, class_decl, defs, dep_graph)
        }

        ast::TopDeclKind_::Default(_)
        | ast::TopDeclKind_::Instance(_)
        | ast::TopDeclKind_::KindSig(_)
        | ast::TopDeclKind_::Value(_)
        | ast::TopDeclKind_::Unparsed => {}
    }
}

fn analyze_type_decl(
    decl_idx: u32,
    decl: &ast::RenamedTypeDecl,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    analyze_type(decl_idx, &decl.node.rhs, defs, dep_graph)
}

fn analyze_data_decl(
    decl_idx: u32,
    decl: &ast::RenamedDataDecl,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    let ast::DataDecl_ { context, cons, .. } = &decl.node;

    if !context.is_empty() {
        todo!("Data declaration with context");
    }

    for con in cons {
        analyze_con(decl_idx, con, defs, dep_graph);
    }
}

fn analyze_con(
    decl_idx: u32,
    con: &ast::RenamedCon,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    for field in &con.node.fields {
        analyze_field(decl_idx, field, defs, dep_graph);
    }
}

fn analyze_field(
    decl_idx: u32,
    field: &ast::RenamedFieldDecl,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    analyze_type(decl_idx, &field.node.ty, defs, dep_graph);
}

fn analyze_newtype_decl(
    decl_idx: u32,
    decl: &ast::RenamedNewtypeDecl,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    analyze_con(decl_idx, &decl.node.con, defs, dep_graph);
}

fn analyze_class_decl(
    decl_idx: u32,
    decl: &ast::RenamedClassDecl,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    for pred in &decl.node.context {
        analyze_type(decl_idx, pred, defs, dep_graph);
    }

    for value_decl in &decl.node.decls {
        if let ast::ValueDecl_::TypeSig {
            vars: _,
            foralls: _,
            context,
            ty,
        } = &value_decl.node
        {
            for pred in context {
                analyze_type(decl_idx, pred, defs, dep_graph);
            }
            analyze_type(decl_idx, ty, defs, dep_graph);
        }
    }
}

fn analyze_type(
    decl_idx: u32,
    ty: &ast::RenamedType,
    defs: &Map<Id, u32>,
    dep_graph: &mut DepGraph,
) {
    match &ty.node {
        ast::Type_::Tuple(tys) => tys
            .iter()
            .for_each(|ty| analyze_type(decl_idx, ty, defs, dep_graph)),

        ast::Type_::List(ty) => analyze_type(decl_idx, ty, defs, dep_graph),

        ast::Type_::Arrow(ty1, ty2) => {
            analyze_type(decl_idx, ty1, defs, dep_graph);
            analyze_type(decl_idx, ty2, defs, dep_graph);
        }

        ast::Type_::App(con, args) => {
            analyze_type(decl_idx, con, defs, dep_graph);
            for arg in args {
                analyze_type(decl_idx, arg, defs, dep_graph);
            }
        }

        ast::Type_::Con(con) => match &con.node {
            ast::TyCon_::Id(con_id) => {
                if let Some(con_def_idx) = defs.get(con_id) {
                    dep_graph.entry(decl_idx).or_default().insert(*con_def_idx);
                }
            }
            ast::TyCon_::Tuple(_) | ast::TyCon_::Arrow | ast::TyCon_::List => {}
        },

        ast::Type_::Var(_) => {}
    }
}
