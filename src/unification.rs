use crate::collections::{Map, Set};
use crate::id::Id;
use crate::type_inference::TypeSynonym;
use crate::typing::{Ty, TyRef, TyVarRef};

use std::ops::Deref;

pub(crate) fn unify(
    ty_syns: &Map<Id, TypeSynonym>,
    ty1: &TyRef,
    ty2: &TyRef,
) -> Result<(), String> {
    let ty1: TyRef = deref_syn(ty_syns, &ty1.normalize());
    let ty1_: Ty = ty1.deref().clone();

    let ty2: TyRef = deref_syn(ty_syns, &ty2.normalize());
    let ty2_: Ty = ty2.deref().clone();

    // println!("unify {} ~ {}", ty1, ty2);

    match (ty1_, ty2_) {
        (Ty::App(l1, r1), Ty::App(l2, r2)) => {
            unify(ty_syns, &l1, &l2)?;
            unify(ty_syns, &r1, &r2)?;
            Ok(())
        }

        (Ty::Var(v1), Ty::Var(v2)) => {
            if v1 == v2 {
                return Ok(());
            }

            let v1_level = v1.level();
            let v2_level = v2.level();

            // We've normalized the types, so the links must be followed to the end.
            debug_assert_eq!(v1.link(), None);
            debug_assert_eq!(v2.link(), None);

            // Links must increase in level so that we can follow them to find the actual level.
            if v1_level < v2_level {
                link_var(&v1, &ty2)?;
            } else {
                link_var(&v2, &ty1)?;
            }

            Ok(())
        }

        (Ty::Var(v), _) => {
            debug_assert_eq!(v.link(), None);
            link_var(&v, &ty2)
        }

        (_, Ty::Var(v)) => {
            debug_assert_eq!(v.link(), None);
            link_var(&v, &ty1)
        }

        (Ty::Con(c1), Ty::Con(c2)) => {
            if c1 == c2 {
                Ok(())
            } else {
                Err(format!("Cannot unify constructors {:?} and {:?}", c1, c2))
            }
        }

        (ty1_, ty2_) => Err(format!("Cannot unify types {:?} and {:?}", ty1_, ty2_)),
    }
}

/// One-way unification: unify variables in `ty1`. Does not update `ty2`.
pub(crate) fn unify_left(
    ty_syns: &Map<Id, TypeSynonym>,
    ty1: &TyRef,
    ty2: &TyRef,
) -> Result<(), String> {
    let fixed_vars = ty2.vars();
    unify_left_(ty_syns, &fixed_vars, ty1, ty2)
}

fn unify_left_(
    ty_syns: &Map<Id, TypeSynonym>,
    fixed_vars: &Set<TyVarRef>,
    ty1: &TyRef,
    ty2: &TyRef,
) -> Result<(), String> {
    let ty1: TyRef = deref_syn(ty_syns, &ty1.normalize());
    let ty1_: Ty = ty1.deref().clone();

    let ty2: TyRef = deref_syn(ty_syns, &ty2.normalize());
    let ty2_: Ty = ty2.deref().clone();

    match (ty1_, ty2_) {
        (Ty::App(l1, r1), Ty::App(l2, r2)) => {
            unify_left_(ty_syns, fixed_vars, &l1, &l2)?;
            unify_left_(ty_syns, fixed_vars, &r1, &r2)?;
            Ok(())
        }

        (Ty::Var(v1), Ty::Var(v2)) => {
            if v1 == v2 {
                return Ok(());
            }

            let v1_level = v1.level();
            let v2_level = v2.level();

            // We've normalized the types, so the links must be followed to the end.
            debug_assert_eq!(v1.link(), None);
            debug_assert_eq!(v2.link(), None);

            if v1_level > v2_level {
                return Err(format!(
                    "Unable to unify left variable {:?} (level = {}) with {:?} (level = {})",
                    v1, v1_level, v2, v2_level
                ));
            }

            if fixed_vars.contains(&v1) {
                return Err(format!("Left variable is fixed: {}", v1));
            }

            link_var(&v1, &ty2)?;

            Ok(())
        }

        (Ty::Var(v), _) => {
            debug_assert_eq!(v.link(), None);
            if fixed_vars.contains(&v) {
                return Err(format!("Left variable is fixed: {}", v));
            }
            link_var(&v, &ty2)
        }

        (Ty::Con(c1), Ty::Con(c2)) => {
            if c1 == c2 {
                Ok(())
            } else {
                Err(format!("Cannot unify constructors {:?} and {:?}", c1, c2))
            }
        }

        (ty1_, ty2_) => Err(format!("Cannot one-way unify {:?} with {:?}", ty1_, ty2_)),
    }
}

fn link_var(var: &TyVarRef, ty: &TyRef) -> Result<(), String> {
    // println!("  link var {} -> {}", var, ty);
    if occurs(var, ty) {
        return Err(format!("link_var: {:?} occurs in {:?}", var, ty));
    }
    prune_level(var.level(), ty);
    var.set_link(ty.clone());
    // println!("    = {}", var);
    Ok(())
}

/// Returns whether `var` occurs in `ty`.
fn occurs(var: &TyVarRef, ty: &TyRef) -> bool {
    match ty.deref().clone() {
        Ty::Var(var_) => *var == var_,

        Ty::Con(_) => false,

        Ty::App(ty1, ty2) => occurs(var, &ty1) || occurs(var, &ty2),

        Ty::Gen(_) => panic!("Quantified type variable in occurs check"),
    }
}

fn prune_level(max_level: u32, ty: &TyRef) {
    let ty = ty.normalize();
    let ty_ = ty.deref().clone();
    match ty_ {
        Ty::Var(var) => {
            assert_eq!(var.link(), None);
            var.prune_level(max_level);
        }

        Ty::Con(_) => {}

        Ty::App(ty1, ty2) => {
            prune_level(max_level, &ty1);
            prune_level(max_level, &ty2);
        }

        Ty::Gen(_) => panic!("Quantified type variable in prune_level"),
    }
}

fn deref_syn(ty_syns: &Map<Id, TypeSynonym>, ty: &TyRef) -> TyRef {
    let mut args: Vec<TyRef> = vec![];
    let mut ty_ = ty;
    while let Ty::App(t1, t2) = ty_.deref() {
        args.push(t2.clone());
        ty_ = t1;
    }
    args.reverse();

    if let Ty::Con(con) = ty_.deref() {
        if let Some(TypeSynonym {
            args: syn_args,
            rhs: syn_rhs,
        }) = ty_syns.get(con)
        {
            if syn_args.len() != args.len() {
                panic!(
                    "Partial or over application of type synonym: syn args = {}, applied = {}",
                    syn_args.len(),
                    args.len()
                );
            }
            return subst(syn_rhs, &args);
        }
    }

    ty.clone()
}

fn subst(ty: &TyRef, substs: &[TyRef]) -> TyRef {
    match ty.deref() {
        Ty::Var(var) => panic!("Type variable in type synonym RHS: {}", var),
        Ty::Con(_) => ty.clone(),
        Ty::Gen(idx) => substs[*idx as usize].clone(),
        Ty::App(ty1, ty2) => TyRef::new_app(subst(ty1, substs), subst(ty2, substs)),
    }
}
