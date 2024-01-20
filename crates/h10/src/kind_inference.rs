/*
Notes:

- Kinds are STLC types:
  - Arrows:    k ::= k -> k
  - Constants: k ::= Type

- No principal types (kinds):

  - `data K a = K ()`: Kind of `K` can be `Type -> Type`, `(Type -> Type) -> Type`, ...

  - `data Tree a = Leaf | Fork (Tree a) (Tree a)`: Same as above, `a` is not restricted to a kind.

- H98 (and I hope H10 too) says kinds default to `Type` when cannot be inferred.

- Some tricky cases from "Kind Inference for Datatypes" by Xie et. al.:

  - Mutual recursion:

       data P1 a = MkP1 P2
       data P2   = MkP2 (P1 Maybe)

       -- a  : Type -> Type
       -- P1 : (Type -> Type) -> Type
       -- P2 : Type

  - Without mutual recursion defaulting kicks in:

       data P1 a = MkP1
       data P2   = MkP2 (P1 Maybe)

       -- a  : Type (defaulting)
       -- P1 : Type -> Type
       -- P2 : rejected

Kind Inference for Datatypes by Xie el. al. has a formal description of H98 kind inference and all
the other extensions. In this implementation we do unification + defaulting, which should be the
same algorithm for H98 described in the paper.

Type signatures (or inferred types of terms) are not considered when inferring kinds of data type
arguments.

Type signatures of typeclass methods *are* considered when inferring kinds of typeclass type
arguments. This is because without the type signatures there will be no constraints on the type
arguments of classes in some cases, which will result in ill-typed method types.

For example:

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

Without considering `f a` we can't infer that `f :: Type -> Type`.

Note: In this implementation we avoid dependency analysis and analyze a whole module with all the
types, but we don't consider type signatures (except in typeclass method signatures) during
inference, as mentioned above.

This allows more programs without rejecting H10 programs, but it's not modular: a use of a type in
another module influences kind of the type.
*/

mod dependency_analysis;

#[cfg(test)]
mod tests;

use crate::ast;
use crate::collections::{Map, Set};
use crate::id::{self, type_ty_tyref, Id};
use crate::type_inference::make_fun_ty;
use crate::typing::{Ty, TyRef};
use crate::unification::unify;

use std::ops::Deref;

/// Infers kinds of type constructors.
pub(crate) fn infer_type_kinds(decls: &[ast::RenamedTopDecl]) -> Map<Id, TyRef> {
    let types: Vec<TypeDecl> = collect_types(decls);

    // `ty_arg_kinds[i]` maps type arguments of `types[i]` to their kinds.
    let mut ty_arg_kinds: Vec<Map<Id, TyRef>> = Vec::with_capacity(types.len());

    // Maps type constructors to their kinds. The result of kind inference.
    let mut con_kinds: Map<Id, TyRef> = Default::default();

    // Builtins.
    con_kinds.insert(id::char_ty_id(), type_type());
    con_kinds.insert(id::int_ty_id(), type_type());
    con_kinds.insert(id::integer_ty_id(), type_type());
    con_kinds.insert(id::type_ty_id(), type_type());

    // Initially types get kinds based on number of type parameters. N parameters means N arrows:
    // `k1 -> ... -> k{N} -> Type`, where each `k` is a distinct unification variable.
    for ty in &types {
        // TODO: No need to collect the arg kinds in a `Vec`.
        let arg_kinds: Vec<(Id, TyRef)> = ty
            .args
            .iter()
            .map(|id| (id.clone(), new_unification_var()))
            .collect();

        let con_kind: TyRef = make_fun_ty(
            arg_kinds.iter().map(|(_, kind)| kind.clone()).collect(),
            type_type(),
        );

        let arg_kinds: Map<Id, TyRef> = arg_kinds.into_iter().collect();

        let old_kind: Option<TyRef> = con_kinds.insert(ty.con.clone(), con_kind);
        assert_eq!(
            old_kind, None,
            "Type kind added multiple times: {:?}",
            ty.con
        );

        ty_arg_kinds.push(arg_kinds);
    }

    for (ty, arg_kinds) in types.iter().zip(ty_arg_kinds.iter()) {
        let bound_kinds: Map<Id, TyRef> = ty
            .bounds
            .iter()
            .map(|id| (id.clone(), TyRef::new_var(type_ty_tyref(), 0)))
            .collect();

        for field in &ty.stars {
            let field_ty_kind = ty_kind(&con_kinds, arg_kinds, &bound_kinds, field);
            unify(&Default::default(), &field_ty_kind, &type_type()).unwrap();
        }
    }

    normalize_kinds(&con_kinds)
}

/// Given a list of predicates and a type (e.g. to infer kinds of free variables in `preds => ty`)
/// and the mapping from type constructors to kinds, infers kinds of free varibles in the
/// predicates and type.
///
/// `expected_kind` is the expected kind of `ty`.
pub(crate) fn infer_fv_kinds(
    con_kinds: &Map<Id, TyRef>,
    preds: &[ast::RenamedType],
    ast_ty: &ast::RenamedType,
    expected_kind: &TyRef,
) -> Map<Id, TyRef> {
    // The process is similar to kind inference for type constructors. Free variables are
    // initialized with unification variables. Types of terms have kind `Type`. Unification
    // constrains kinds of type variables. Unconstrained type variables are defaulted as `Type`.

    let mut vars: Map<Id, TyRef> = Default::default();
    for pred in preds {
        collect_vars(pred, &mut vars);
    }
    collect_vars(ast_ty, &mut vars);

    for pred_ast_ty in preds {
        let pred_kind = ty_kind(con_kinds, &vars, &Default::default(), pred_ast_ty);
        unify(&Default::default(), &pred_kind, &type_type()).unwrap();
    }

    let ty_kind = ty_kind(con_kinds, &vars, &Default::default(), ast_ty);
    unify(&Default::default(), &ty_kind, expected_kind).unwrap();

    normalize_kinds(&vars)
}

/// Create a new unification variable with kind `Type` and level 0.
fn new_unification_var() -> TyRef {
    TyRef::new_var(type_ty_tyref(), 0)
}

/// The [`TyRef`] for `Type`.
fn type_type() -> TyRef {
    type_ty_tyref()
}

/// Infer kind of `ty`.
///
/// Operates on ast type representation to be able to show locations in error messages.
//
// TODO: Maybe consider assining locations to type inference types?
fn ty_kind(
    con_kinds: &Map<Id, TyRef>,
    arg_kinds: &Map<Id, TyRef>,
    bound_kinds: &Map<Id, TyRef>,
    ty: &ast::RenamedType,
) -> TyRef {
    match &ty.node {
        ast::Type_::Tuple(args) => {
            for arg in args {
                let arg_kind = ty_kind(con_kinds, arg_kinds, bound_kinds, arg);
                unify(&Default::default(), &arg_kind, &type_type()).unwrap();
            }
            type_type()
        }

        ast::Type_::List(arg) => {
            let arg_kind = ty_kind(con_kinds, arg_kinds, bound_kinds, arg);
            unify(&Default::default(), &arg_kind, &type_type()).unwrap();
            type_type()
        }

        ast::Type_::Arrow(arg1, arg2) => {
            let arg1_kind = ty_kind(con_kinds, arg_kinds, bound_kinds, arg1);
            unify(&Default::default(), &arg1_kind, &type_type()).unwrap();

            let arg2_kind = ty_kind(con_kinds, arg_kinds, bound_kinds, arg2);
            unify(&Default::default(), &arg2_kind, &type_type()).unwrap();

            type_type()
        }

        ast::Type_::App(ty, tys) => {
            let result_kind = new_unification_var();

            let mut expected_fun_kind = result_kind.clone();
            for arg_ty in tys.iter().rev() {
                let arg_ty_kind = ty_kind(con_kinds, arg_kinds, bound_kinds, arg_ty);
                expected_fun_kind = make_fun_ty(vec![arg_ty_kind], expected_fun_kind);
            }
            let expected_fun_kind = expected_fun_kind;

            let actual_fun_kind = ty_kind(con_kinds, arg_kinds, bound_kinds, ty);

            unify(&Default::default(), &expected_fun_kind, &actual_fun_kind).unwrap();

            result_kind
        }

        ast::Type_::Con(con) => match &con.node {
            ast::TyCon_::Id(con) => con_kinds.get(con).unwrap().clone(),

            ast::TyCon_::Tuple(arity) => make_fun_ty(
                std::iter::repeat_with(type_type)
                    .take(*arity as usize)
                    .collect(),
                type_type(),
            ),

            ast::TyCon_::Arrow => make_fun_ty(vec![type_type(), type_type()], type_type()),

            ast::TyCon_::List => make_fun_ty(vec![type_type()], type_type()),
        },

        ast::Type_::Var(var) => arg_kinds
            .get(var)
            .or_else(|| bound_kinds.get(var))
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Argument kind unknown: {:?} ({}), env: {:?}",
                    var, ty.span, arg_kinds
                )
            }),
    }
}

/// Collect type variables in [`ty`] in [`fvs`], with a fresh unification variable for each
/// variable.
fn collect_vars(ty: &ast::RenamedType, fvs: &mut Map<Id, TyRef>) {
    match &ty.node {
        ast::Type_::Tuple(tys) => tys.iter().for_each(|ty| collect_vars(ty, fvs)),

        ast::Type_::List(ty) => collect_vars(ty, fvs),

        ast::Type_::Arrow(ty1, ty2) => {
            collect_vars(ty1, fvs);
            collect_vars(ty2, fvs);
        }

        ast::Type_::App(ty, tys) => {
            collect_vars(ty, fvs);
            tys.iter().for_each(|ty| collect_vars(ty, fvs));
        }

        ast::Type_::Con(_) => {}

        ast::Type_::Var(id) => {
            fvs.insert(id.clone(), new_unification_var());
        }
    }
}

/// A type declaration, abstracted for the purposes of kind inference.
#[derive(Debug, Clone)]
struct TypeDecl {
    /// Type constructor.
    con: Id,

    /// Type constructor arguments with inferred kinds.
    ///
    /// These get assigned fresh unification variables. At the end of the inference, variables that
    /// are not constrained are defaulted as `*`.
    args: Vec<Id>,

    /// Other bound type variables that are *not* a part of the constructor type signature.
    ///
    /// This comes into play in typeclasses like:
    ///
    /// ```ignore
    /// class Functor f where
    ///   fmap :: (a -> b) -> f a -> f b
    /// ```
    ///
    /// Here `a` and `b` are not a type argument to `Functor`, but they need to be inferred to be
    /// able to infer kind of `f`.
    bounds: Set<Id>,

    /// Types expected to have kind `*`.
    ///
    /// These are fields in `data`, predicates in contexts (in `data`, `class`, typeclass method
    /// type signatures), and typeclass type signatures.
    stars: Vec<ast::RenamedType>,
}

fn collect_types(decls: &[ast::RenamedTopDecl]) -> Vec<TypeDecl> {
    fn data_to_type_decl(decl: &ast::RenamedDataDecl) -> TypeDecl {
        TypeDecl {
            con: decl.node.ty_con.clone(),
            args: decl.node.ty_args.clone(),
            bounds: Default::default(),
            stars: decl
                .node
                .cons
                .iter()
                .flat_map(|con| con.node.fields.iter().map(|field| field.node.ty.clone()))
                .chain(decl.node.context.iter().cloned())
                .collect(),
        }
    }

    fn newtype_to_type_decl(decl: &ast::RenamedNewtypeDecl) -> TypeDecl {
        TypeDecl {
            con: decl.node.ty_con.clone(),
            args: decl.node.ty_args.clone(),
            bounds: Default::default(),
            stars: decl
                .node
                .con
                .node
                .fields
                .iter()
                .map(|field| field.node.ty.clone())
                .collect(),
        }
    }

    fn type_to_type_decl(decl: &ast::RenamedTypeDecl) -> TypeDecl {
        TypeDecl {
            con: decl.node.ty.clone(),
            args: decl.node.vars.clone(),
            bounds: Default::default(),
            stars: vec![],
        }
    }

    /// Typeclass method type signatures can be considered as data constructor fields for the
    /// purposes of kind inference. This generates those "fields".
    fn typeclass_to_type_decl(decl: &ast::RenamedClassDecl) -> TypeDecl {
        let mut args: Set<Id> = Default::default();
        let mut bounds: Set<Id> = Default::default();
        let mut fields: Vec<ast::RenamedType> = vec![];

        args.insert(decl.node.ty_arg.clone());

        // Predicates in class context need to have kind `*`.
        fields.extend(decl.node.context.iter().cloned());

        for decl in &decl.node.decls {
            if let ast::ValueDecl_::TypeSig {
                vars: _,
                foralls: _,
                context,
                ty,
            } = &decl.node
            {
                for ty in context {
                    ty.node.vars_(&mut bounds);
                }
                ty.node.vars_(&mut bounds);

                // E.g. `compare :: a -> a -> Ordering`.
                fields.extend(context.iter().cloned());

                // Add `(->)` arguments as fields.
                let mut ty = ty.clone();
                loop {
                    match ty {
                        ast::AstNode {
                            node: ast::Type_::Arrow(lhs, rhs),
                            ..
                        } => {
                            fields.push(*lhs);
                            ty = *rhs;
                        }
                        _ => {
                            fields.push(ty);
                            break;
                        }
                    }
                }
            }
        }

        TypeDecl {
            con: decl.node.ty_con.clone(),
            args: args.into_iter().collect(),
            bounds,
            stars: fields,
        }
    }

    decls
        .iter()
        .filter_map(|decl| match &decl.kind {
            ast::TopDeclKind_::Data(d) => Some(data_to_type_decl(d)),
            ast::TopDeclKind_::Newtype(d) => Some(newtype_to_type_decl(d)),
            ast::TopDeclKind_::Class(d) => Some(typeclass_to_type_decl(d)),
            ast::TopDeclKind_::Type(d) => Some(type_to_type_decl(d)),
            _ => None,
        })
        .collect()
}

/// Normalize all kinds in `kinds`.
fn normalize_kinds(kinds: &Map<Id, TyRef>) -> Map<Id, TyRef> {
    kinds
        .iter()
        .map(|(id, kind)| (id.clone(), normalize_kind(kind)))
        .collect()
}

/// Replace unification variables in `kind` with their values. Variables without a value are
/// defaulted as `*`.
fn normalize_kind(kind: &TyRef) -> TyRef {
    match kind.deref() {
        Ty::Var(var) => match var.link() {
            Some(link) => {
                let normalized = normalize_kind(&link);
                var.set_link(normalized.clone());
                normalized
            }
            None => {
                // Defaulting.
                type_type()
            }
        },

        Ty::App(ty1, ty2) => TyRef::new_app(normalize_kind(ty1), normalize_kind(ty2)),

        Ty::Con(_) => kind.clone(),

        Ty::Gen(_) => panic!("Gen in normalize_kind"),
    }
}
