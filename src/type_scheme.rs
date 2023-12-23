use crate::ast::{self, Span};
use crate::ast_to_ty::convert_ast_ty;
use crate::class_env::Pred;
use crate::collections::{Map, TrieMap};
use crate::id::Id;
use crate::kind_inference::infer_fv_kinds;
use crate::typing::{Kind, Ty, TyRef};

use std::cmp::Ordering;
use std::fmt;
use std::ops::Deref;

/// A type scheme.
///
/// Running example: `forall f a b . Functor f => (a -> b) -> f a -> f b`.
#[derive(Debug, Clone)]
pub struct Scheme {
    /// Kinds of type variables.
    ///
    /// Example: `[* -> *, *, *]`.
    pub kinds: Vec<Kind>,

    /// Predicates.
    ///
    /// Example: `[Functor (Gen 0)]`.
    pub preds: Vec<Pred>,

    /// The type.
    ///
    /// Example: `((Gen 1) -> (Gen 2)) -> (Gen 0) (Gen 1) -> (Gen 0) (Gen 2)`.
    pub ty: TyRef,

    /// Location of the type scheme. Only available for explicit (i.e. user written) types.
    pub span: Option<Span>,
}

impl Scheme {
    pub fn monomorphic(ty: TyRef) -> Self {
        Scheme {
            kinds: vec![],
            preds: vec![],
            ty,
            span: None,
        }
    }

    pub fn is_monomorphic(&self) -> bool {
        self.kinds.is_empty()
    }

    /// Convert a type signature AST to a type scheme.
    ///
    /// - `ty_kinds`: Maps type constructors in scope to their kinds.
    /// - `bound_tys`: Maps bound type variables in scope to their `TyRef`s.
    pub fn from_type_sig(
        span: Span,
        ty_kinds: &Map<Id, Kind>,
        bound_tys: &TrieMap<Id, TyRef>,
        context: &[ast::RenamedType],
        ty: &ast::RenamedType,
    ) -> Self {
        let fv_kinds: Vec<(Id, Kind)> = infer_fv_kinds(ty_kinds, context, ty, &Kind::Star)
            .into_iter()
            .collect();

        let fv_gens: Map<Id, u32> = fv_kinds
            .iter()
            .enumerate()
            .map(|(id_idx, (id, _kind))| (id.clone(), id_idx as u32))
            .collect();

        let kinds: Vec<Kind> = fv_kinds.into_iter().map(|(_id, k)| k).collect();

        fn to_pred(ty: &TyRef) -> Pred {
            match ty.deref().clone() {
                Ty::App(ty1, ty2) => match ty1.deref().clone() {
                    Ty::Con(id) => Pred { class: id, ty: ty2 },
                    other => panic!("Invalid predicate: {:?}", other),
                },
                other => panic!("Invalid predicate: {:?}", other),
            }
        }

        let preds: Vec<Pred> = context
            .iter()
            .map(|pred| {
                let pred_ty = convert_ast_ty(bound_tys, &fv_gens, pred);
                to_pred(&pred_ty)
            })
            .collect();

        let ty = convert_ast_ty(bound_tys, &fv_gens, ty);

        Scheme {
            kinds,
            preds,
            ty,
            span: Some(span),
        }
    }

    /// Instantiate a type scheme at the given level.
    pub fn instantiate(&self, level: u32) -> (Vec<Pred>, TyRef) {
        instantiate_scheme(&self.kinds, &self.preds, &self.ty, level)
    }

    /// Instantiate a monomorphic type scheme. Panics if the type scheme is not monomorphic.
    pub fn instantiate_monomorphic(&self) -> (Vec<Pred>, TyRef) {
        assert!(
            self.kinds.is_empty(),
            "Type scheme is not monomorphic: {}",
            self
        );
        (self.preds.clone(), self.ty.clone())
    }

    /// Substitute `ty` for the given quantified variable (`Ty::Gen`).
    ///
    /// `ty` can't be a quantified variable`.
    ///
    /// Does not check kinds, the removed gen and `ty` should have the same kind, otherwise the
    /// resulting scheme will have a bad kind.
    pub fn subst_gen(&self, gen: u32, ty: &TyRef) -> Self {
        let num_gens = self.kinds.len();
        assert!((gen as usize) < num_gens);

        let mut gen_map: Vec<TyRef> = Vec::with_capacity(num_gens);
        let mut new_kinds: Vec<Kind> = Vec::with_capacity(num_gens - 1);
        for gen_ in 0..num_gens as u32 {
            let new_gen = match gen_.cmp(&gen) {
                Ordering::Less => {
                    new_kinds.push(self.kinds[gen_ as usize].clone());
                    TyRef::new_gen(gen_)
                }
                Ordering::Equal => ty.clone(),
                Ordering::Greater => {
                    new_kinds.push(self.kinds[gen_ as usize].clone());
                    TyRef::new_gen(gen_ - 1)
                }
            };
            gen_map.push(new_gen);
        }

        Scheme {
            kinds: new_kinds,

            preds: self
                .preds
                .iter()
                .map(|pred| Pred {
                    class: pred.class.clone(),
                    ty: pred.ty.subst_gens(&gen_map),
                })
                .collect(),

            ty: self.ty.subst_gens(&gen_map),

            span: None,
        }
    }
}

/// Same as `TypeScheme::instantiate`, but takes kinds and predicates as arguments.
pub fn instantiate_scheme(
    kinds: &[Kind],
    preds: &[Pred],
    ty: &TyRef,
    level: u32,
) -> (Vec<Pred>, TyRef) {
    let vars: Vec<TyRef> = kinds
        .iter()
        .map(|kind| TyRef::new_var(kind.clone(), level))
        .collect();

    fn instantiate_ty(ty: &TyRef, vars: &[TyRef]) -> TyRef {
        match ty.deref().clone() {
            Ty::Var(_) | Ty::Con(_) => ty.clone(),

            Ty::App(ty1, ty2) => {
                let ty1_ = instantiate_ty(&ty1, vars);
                let ty2_ = instantiate_ty(&ty2, vars);
                if ty1_ == ty1 && ty2_ == ty2 {
                    ty.clone()
                } else {
                    TyRef::new_app(ty1_, ty2_)
                }
            }

            Ty::Gen(idx) => vars[idx as usize].clone(),
        }
    }

    let preds: Vec<Pred> = preds
        .iter()
        .map(|Pred { class, ty }| Pred {
            class: class.clone(),
            ty: instantiate_ty(ty, &vars),
        })
        .collect();

    let ty = instantiate_ty(ty, &vars);

    (preds, ty)
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Scheme {
            kinds,
            preds,
            ty,
            span: _,
        } = self;
        if !kinds.is_empty() {
            write!(f, "∀ ")?;
            for (kind_idx, kind) in kinds.iter().enumerate() {
                write!(f, "(${} : {:?}) ", kind_idx, kind)?;
            }
            write!(f, ". ")?;
        }

        if !preds.is_empty() {
            let num_preds = preds.len();
            let parens = num_preds > 1;
            if parens {
                write!(f, "(")?;
            }

            for (pred_idx, pred) in preds.iter().enumerate() {
                write!(f, "{} {}", pred.class, pred.ty)?;
                if pred_idx != num_preds - 1 {
                    write!(f, ", ")?;
                }
            }

            if parens {
                write!(f, ")")?;
            }

            write!(f, " => ")?;
        }

        write!(f, "{}", ty)
    }
}
