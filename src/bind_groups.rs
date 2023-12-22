use crate::ast;
use crate::collections::Set;
use crate::id::Id;

/// A single definition. Functions definitions split into multiple definitions are commoned here as
/// `FunBindingGroup`.
#[derive(Debug)]
pub(crate) enum BindingGroup<'a> {
    Pat(PatBinding<'a>),
    Fun(FunBindingGroup<'a>),
}

impl<'a> BindingGroup<'a> {
    // TOOD: By the time we call this we should've checked that an id is defined once, so the
    // collection type could be `Vec<Id>`.
    pub(crate) fn collect_defined_ids(&self, ids: &mut Set<Id>) {
        match self {
            BindingGroup::Pat(PatBinding { pat, .. }) => {
                pat.node.vars_(ids);
            }
            BindingGroup::Fun(FunBindingGroup { id, .. }) => {
                ids.insert(id.clone());
            }
        }
    }
}

/// A pattern binding. For example:
///
/// ```ignore
/// [x, y, z] = map f list
/// ```
///
/// pat = `[x, y, z]`, rhs = `map f list`.
#[derive(Debug)]
pub(crate) struct PatBinding<'a> {
    pub(crate) pat: &'a ast::RenamedPat,
    pub(crate) rhs: &'a ast::RenamedRhs,
}

/// A function definition, split into multiple definitions. For example:
///
/// ```ignore
/// map f []     = []
/// map f (x:xs) = f x : map f xs
/// ```
///
/// Here `id` is `map`. `defs` will be a vector of two:
///
/// 0. pat = [`f`, `[]`], rhs = `[]`.
/// 1. pat = [`f`, `[(x : xs)], rhs = `f x : map f xs`.
#[derive(Debug)]
pub(crate) struct FunBindingGroup<'a> {
    pub(crate) id: Id,
    pub(crate) defs: Vec<FunDef<'a>>,
}

/// A function definition, without the function name.
///
/// See `FunBindingGroup` documentation for an example.
#[derive(Debug)]
pub(crate) struct FunDef<'a> {
    /// Arguments of the function.
    pub(crate) args: &'a [ast::RenamedPat],

    /// Body of the function.
    pub(crate) rhs: &'a ast::RenamedRhs,
}

pub(crate) fn group_top_binds(decls: &[ast::RenamedDecl]) -> Vec<BindingGroup> {
    group_binds_(decls, extract_top_bind_lhs_rhs)
}

pub(crate) fn group_binds(decls: &[ast::RenamedValueDecl]) -> Vec<BindingGroup> {
    group_binds_(decls, extract_bind_lhs_rhs)
}

fn group_binds_<'a, Decl, F>(decls: &'a [Decl], extract_lhs_rhs: F) -> Vec<BindingGroup>
where
    F: Fn(&'a Decl) -> Option<(&'a ast::RenamedLhs, &'a ast::RenamedRhs)>,
{
    let mut groups: Vec<BindingGroup> = Vec::with_capacity(decls.len());

    // Set of ids we've finished defining. If we see a definition of any of these that's an error.
    let mut defined_ids: Set<Id> = Default::default();

    let mut current_group: Option<FunBindingGroup<'a>> = None;

    for decl in decls {
        let (lhs, rhs) = match extract_lhs_rhs(decl) {
            Some(lhs_rhs) => lhs_rhs,
            None => continue,
        };

        match &lhs.node {
            ast::Lhs_::Pat(pat) => {
                if let Some(group) = current_group.take() {
                    let new = defined_ids.insert(group.id.clone());
                    assert!(new);

                    groups.push(BindingGroup::Fun(group));
                }

                let pat_vars = pat.node.vars();
                for var in &pat_vars {
                    if defined_ids.contains(var) {
                        panic!("Multiple declarations of {}", var);
                    }
                }

                defined_ids.extend(pat_vars);

                groups.push(BindingGroup::Pat(PatBinding { pat, rhs }));
            }

            ast::Lhs_::Fun { var, pats } => {
                let def = FunDef { args: pats, rhs };

                if let Some(group) = &mut current_group {
                    if &group.id == var {
                        group.defs.push(def);
                        continue;
                    } else {
                        groups.push(BindingGroup::Fun(current_group.take().unwrap()));
                    }
                }

                current_group = Some(FunBindingGroup {
                    id: var.clone(),
                    defs: vec![def],
                });
            }
        }
    }

    if let Some(group) = current_group.take() {
        groups.push(BindingGroup::Fun(group));
    }

    groups
}

fn extract_top_bind_lhs_rhs(
    decl: &ast::RenamedDecl,
) -> Option<(&ast::RenamedLhs, &ast::RenamedRhs)> {
    if let ast::Decl_::Value(value_decl) = &decl.node {
        extract_bind_lhs_rhs(value_decl)
    } else {
        None
    }
}

fn extract_bind_lhs_rhs(
    decl: &ast::RenamedValueDecl,
) -> Option<(&ast::RenamedLhs, &ast::RenamedRhs)> {
    if let ast::ValueDecl_::Value { lhs, rhs } = &decl.node {
        Some((lhs, rhs))
    } else {
        None
    }
}
