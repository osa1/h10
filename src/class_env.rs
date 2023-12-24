use crate::collections::Map;
use crate::id::Id;
use crate::type_inference::TypeSynonym;
use crate::type_scheme::{instantiate_scheme, Scheme};
use crate::typing::{Kind, Ty, TyRef, TyVarRef};
use crate::unification::unify_left;

use std::ops::Deref;

/// A class environment.
// TODO: This is for the whole program, or just of a module?
#[derive(Debug, Clone)]
pub struct ClassEnv {
    /// Maps class `Id`s to details.
    pub classes: Map<Id, Class>,

    /// Default declarations, e.g. `Integer` and `Double` in `default (Integer, Double)`.
    // TODO: defaults are per-module, so the `ClassEnv` type should be per-module as well, or this
    // should be moved somewhere else.
    pub defaults: Vec<TyRef>,
}

#[derive(Debug, Clone)]
pub struct Class {
    /// The class, e.g. `Functor` in `class Functor f where ...`.
    pub class: Id,

    /// The type argument in class head, e.g. `f` in `class Functor f where ...`.
    pub arg: Id,

    /// Kind of the instances. For example:
    ///
    /// - `Functor f`: kind of `f` is `* -> *`.
    /// - `Show a`: kind of `a` is `*`.
    pub arg_kind: Kind,

    /// Superclasses of the class. For example: `class Functor f => Applicative f where ...`
    /// superclasses are `[Functor]`.
    pub supers: Vec<Id>,

    /// A map of typeclass methods and their types.
    ///
    /// In the schemes, `Gen 0` is the class type argument.
    pub methods: Map<Id, Scheme>,

    /// Instances of the class.
    pub instances: Vec<Instance>,
}

/// Example: `instance (Show a, Show b) => Show (a, b) where ...`.
#[derive(Debug, Clone)]
pub struct Instance {
    /// `Show`
    pub class: Id,

    /// `[*, *]`
    pub kinds: Vec<Kind>,

    /// `[Show (Gen 0), Show (Gen 1)]`
    pub context: Vec<Pred>,

    /// `(Gen 0, Gen 1)`
    pub head: TyRef,
}

/// A predicate of form: `ty` is an instance of `class`.
///
/// May or may not be in head-normal form.
///
/// Head-normal form is one of:
///
/// - `C t`
/// - `C (t T1 ... TN)`
///
/// where `C` is the class, `t` is a type variable, `T1`...`TN` are types.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pred {
    pub class: Id,
    pub ty: TyRef,
}

fn is_hnf(ty: &TyRef) -> bool {
    match ty.deref() {
        Ty::Var(_) => true,
        Ty::Con(_) => false,
        Ty::App(ty, _) => is_hnf(ty),
        Ty::Gen(_) => panic!("Quantified variable in is_hnf: {}", ty),
    }
}

fn hnf_ty_var(ty: &TyRef) -> Option<TyVarRef> {
    match ty.deref() {
        Ty::Var(var) => Some(var.clone()),
        Ty::Con(_) => None,
        Ty::App(ty, _) => hnf_ty_var(ty),
        Ty::Gen(_) => panic!("Quantified variable in hnf_ty_var: {}", ty),
    }
}

impl Pred {
    /// If the predicate is in head-normal form return the variable at the head.
    ///
    /// E.g. `t` in `C (t T1 ... TN)`.
    pub fn head_var(&self) -> Option<TyVarRef> {
        let mut ty: Ty = self.ty.deref().clone();
        loop {
            match ty {
                Ty::Var(var) => return Some(var),
                Ty::Con(_) => return None,
                Ty::App(ty1, _) => {
                    ty = ty1.deref().clone();
                }
                Ty::Gen(_) => return None, // or panic?
            }
        }
    }

    /// Normalize type of a predicate.
    ///
    /// Normalization dereferences the unification variables.
    pub fn normalize(&self) -> Pred {
        Pred {
            class: self.class.clone(),
            ty: self.ty.normalize(),
        }
    }

    pub fn hnf_ty_var(&self) -> TyVarRef {
        hnf_ty_var(&self.ty).unwrap_or_else(|| panic!("Predicate is not in HNF: {:?}", self))
    }
}

impl ClassEnv {
    /// Get super classes of a class. Example:
    /// ```ignore
    /// class Eq a => Ord a where ...
    /// ```
    /// Super classes of `Ord` is `[Eq]`.
    fn super_classes(&self, id: &Id) -> &[Id] {
        &self
            .classes
            .get(id)
            .unwrap_or_else(|| panic!("Class not in class env: {:?}", id))
            .supers
    }

    /// Get instances of a class.
    fn instances(&self, id: &Id) -> &[Instance] {
        &self
            .classes
            .get(id)
            .unwrap_or_else(|| panic!("Class not in class env: {:?}", id))
            .instances
    }

    /// Given a predicate `C T`, for each super class of `C` (`S1`, `S2`, ...), return the
    /// predicates `S1 T`, `S2 T`, ...
    ///
    /// The idea is that if `C T` holds, then the predicates returned by this function (`S1 T`,
    /// `S2 T`, ...) will also hold.
    fn super_preds<'a>(&'a self, pred: &'a Pred) -> impl Iterator<Item = Pred> + 'a {
        SuperClassIter::new(self, &pred.class).map(move |supercls| Pred {
            class: supercls,
            ty: pred.ty.clone(),
        })
    }

    /// Try to find an instance head matching the given `pred`, and return the instance's context.
    ///
    /// For example, for the `pred = Show (t1, t2)`, this will find the instance
    ///
    /// ```ignore
    /// instance (Show a, Show b) => Show (a, b) where ...
    /// ```
    ///
    /// and return `[Show t1, Show t2]`.
    ///
    /// Without overlapping instances there will be at most one match, so this returns an `Option`.
    fn instance_subgoals(&self, ty_syns: &Map<Id, TypeSynonym>, pred: &Pred) -> Option<Vec<Pred>> {
        let Pred { ty: pred_ty, class } = pred;

        for Instance {
            class: _, // same as `class`
            kinds,
            context,
            head,
        } in self.instances(class)
        {
            let (head_preds, head) = instantiate_scheme(kinds, context, head, 0);
            if unify_left(ty_syns, &head, pred_ty).is_ok() {
                return Some(head_preds);
            }
        }

        None
    }

    /// Given a list of predicates that hold, returns whether the goal holds.
    fn entails(&self, ty_syns: &Map<Id, TypeSynonym>, preds: &[Pred], goal: &Pred) -> bool {
        if preds.iter().any(|p| p == goal) {
            return true;
        }

        for p in preds {
            for super_pred in self.super_preds(p) {
                if &super_pred == goal {
                    return true;
                }
            }
        }

        match self.instance_subgoals(ty_syns, goal) {
            Some(subgoals) => subgoals
                .iter()
                .all(|subgoal| self.entails(ty_syns, preds, subgoal)),
            None => false,
        }
    }

    /// Given a set of predicates, remove the ones that are entailed by the others.
    fn simplify(&self, ty_syns: &Map<Id, TypeSynonym>, preds: &[Pred]) -> Vec<Pred> {
        let preds_normalized: Vec<Pred> = preds.iter().map(|p| p.normalize()).collect();

        let mut new_preds: Vec<Pred> = Vec::with_capacity(preds.len());

        for (p_idx, p) in preds_normalized.iter().enumerate() {
            let mut other_preds = new_preds.clone();
            other_preds.extend(preds[p_idx + 1..].iter().cloned());
            if !self.entails(ty_syns, &other_preds, p) {
                new_preds.push(p.clone());
            }
        }

        new_preds
    }

    fn to_hnf(&self, ty_syns: &Map<Id, TypeSynonym>, pred: &Pred) -> Vec<Pred> {
        if is_hnf(&pred.ty) {
            return vec![pred.clone()];
        }

        let mut preds: Vec<Pred> = vec![];

        match self.instance_subgoals(ty_syns, pred) {
            Some(subgoals) => {
                for pred in subgoals {
                    preds.extend(self.to_hnf(ty_syns, &pred).into_iter());
                }
            }
            None => panic!("Context reduction fails"),
        }

        preds
    }

    /// Context reduction: reduce a list of predicates to an equivalent but simpler list.
    ///
    /// The resulting list will have predicates in form:
    ///
    /// - `C t` where `C` is a class and `t` is a type variable.
    /// - `C (t T1 ... Tn)` where `C` is a class, `t` is a type variable, `T1`...`Tn` are types.
    ///
    /// The idea is that inferred predicates should not be more expressive than user-written
    /// predicates, which need to have the simple syntax described above. Context reduction
    /// converts more complicated predicates to predicates in user syntax.
    ///
    /// Relevant H 2010 section: 4.5.3 Context Reduction Errors.
    pub fn reduce_context(&self, ty_syns: &Map<Id, TypeSynonym>, context: &[Pred]) -> Vec<Pred> {
        let mut hnf_preds = Vec::with_capacity(context.len());
        for p in context {
            hnf_preds.extend(self.to_hnf(ty_syns, p));
        }
        self.simplify(ty_syns, &hnf_preds)
    }

    pub fn get_method_sig(&self, class: &Id, method: &str) -> Option<&Scheme> {
        let class: &Class = self.classes.get(class)?;
        for (method_id, method_scheme) in &class.methods {
            if method_id.name().unwrap() == method {
                return Some(method_scheme);
            }
        }
        None
    }
}

struct SuperClassIter<'a> {
    env: &'a ClassEnv,
    stack: Vec<Id>,
}

impl<'a> SuperClassIter<'a> {
    fn new(env: &'a ClassEnv, class: &Id) -> Self {
        let mut stack: Vec<Id> = Vec::with_capacity(100);
        stack.push(class.clone());
        SuperClassIter { env, stack }
    }
}

impl<'a> Iterator for SuperClassIter<'a> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;
        let supers = self.env.super_classes(&current);
        self.stack.extend(supers.iter().cloned());
        Some(current)
    }
}
