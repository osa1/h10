/*
Notes:

- Kinds are STLC types:
  - Arrows:    k ::= k -> k
  - Constants: k ::= *

- No principal types (kinds):

  - `data K a = K ()`: Kind of `K` can be `* -> *`, `(* -> *) -> *`, ...

  - `data Tree a = Leaf | Fork (Tree a) (Tree a)`: Same as above, `a` is not restricted to a kind.

- H98 (and I hope H10 too) says kinds default to `*` when cannot be inferred.

- Some tricky cases from "Kind Inference for Datatypes" by Xie et. al.:

  - Mutual recursion:

       data P1 a = MkP1 P2
       data P2   = MkP2 (P1 Maybe)

       -- a  : * -> *
       -- P1 : (* -> *) -> *
       -- P2 : *

  - Without mutual recursion defaulting kicks in:

       data P1 a = MkP1
       data P2   = MkP2 (P1 Maybe)

       -- a  : * (defaulting)
       -- P1 : * -> *
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

Without considering `f a` we can't infer that `f :: * -> *`.

Note: In this implementation we avoid dependency analysis and analyze a whole module with all the
types, but we don't consider type signatures (except in typeclass method signatures) during
inference, as mentioned above.

This allows more programs without rejecting H10 programs, but it's not modular: a use of a type in
another module influences kind of the type.
*/

#[cfg(test)]
mod tests;

use crate::ast;
use crate::collections::{Map, Set};
use crate::id::{self, Id};
use crate::typing;

/// Infers kinds of type constructors.
pub(crate) fn infer_type_kinds(decls: &[ast::RenamedDecl]) -> Map<Id, typing::Kind> {
    let types: Vec<TypeDecl> = collect_types(decls);

    // `ty_arg_kinds[i]` maps type arguments of `types[i]` to their kinds.
    let mut ty_arg_kinds: Vec<Map<Id, Kind>> = Vec::with_capacity(types.len());

    let mut con_kinds: Map<Id, Kind> = Default::default();

    // Builtins.
    con_kinds.insert(id::char_ty_id(), Kind::Star);
    con_kinds.insert(id::int_ty_id(), Kind::Star);
    con_kinds.insert(id::integer_ty_id(), Kind::Star);

    let mut state = KindInferState::new();

    // Initially types get kinds based on number of type parameters. N parameters means N arrows:
    // `k1 -> ... -> k{N} -> *`, where each `k` is a distinct unification variable.
    for ty in &types {
        let arg_kinds: Vec<(Id, Kind)> = ty
            .args
            .iter()
            .map(|id| (id.clone(), Kind::Var(state.new_var())))
            .collect();

        let con_kind: Kind = state.new_kind_fun(
            arg_kinds.iter().map(|(_, kind)| kind.clone()).collect(),
            Kind::Star,
        );

        let arg_kinds: Map<Id, Kind> = arg_kinds.into_iter().collect();

        let old_kind: Option<Kind> = con_kinds.insert(ty.con.clone(), con_kind);
        assert_eq!(
            old_kind, None,
            "Type kind added multiple times: {:?}",
            ty.con
        );

        ty_arg_kinds.push(arg_kinds);
    }

    for (ty, arg_kinds) in types.iter().zip(ty_arg_kinds.iter()) {
        let bound_kinds: Map<Id, Kind> = ty
            .bounds
            .iter()
            .map(|id| (id.clone(), Kind::Var(state.new_var())))
            .collect();
        for field in &ty.stars {
            unify_ty_kind(
                &mut state,
                &con_kinds,
                arg_kinds,
                &bound_kinds,
                field,
                &Kind::Star,
            );
        }
    }

    normalize_kinds(&con_kinds, &state.unification_vars)
}

/// Given a list of predicates and a type (e.g. to infer kinds of free variables in `preds => ty`)
/// and the mapping from type constructors to kinds, infers kinds of free varibles in the
/// predicates and type.
///
/// `expected_kind` is the expected kind of `ty`.
pub(crate) fn infer_fv_kinds(
    con_kinds: &Map<Id, typing::Kind>,
    preds: &[ast::RenamedType],
    ty: &ast::RenamedType,
    expected_kind: &typing::Kind,
) -> Map<Id, typing::Kind> {
    // The process is similar to kind inference for type constructors. Free variables are
    // initialized with unification variables. Types of terms have kind `*`. Unification constrains
    // kinds of type variables. Unconstrained type variables are defaulted as `*`.

    let mut state = KindInferState::new();

    let mut fvs: Map<Id, Kind> = Default::default();
    for pred in preds {
        collect_fvs(&mut state, pred, &mut fvs);
    }
    collect_fvs(&mut state, ty, &mut fvs);

    let con_kinds: Map<Id, Kind> = con_kinds
        .iter()
        .map(|(con, kind)| (con.clone(), Kind::from(kind)))
        .collect();

    for pred in preds {
        unify_ty_kind(
            &mut state,
            &con_kinds,
            &Default::default(),
            &fvs,
            pred,
            &Kind::Star,
        );
    }

    unify_ty_kind(
        &mut state,
        &con_kinds,
        &Default::default(),
        &fvs,
        ty,
        &Kind::from(expected_kind),
    );

    normalize_kinds(&fvs, &state.unification_vars)
}

fn collect_fvs(state: &mut KindInferState, ty: &ast::RenamedType, fvs: &mut Map<Id, Kind>) {
    match &ty.node {
        ast::Type_::Tuple(tys) => tys.iter().for_each(|ty| collect_fvs(state, ty, fvs)),

        ast::Type_::List(ty) => collect_fvs(state, ty, fvs),

        ast::Type_::Arrow(ty1, ty2) => {
            collect_fvs(state, ty1, fvs);
            collect_fvs(state, ty2, fvs);
        }

        ast::Type_::App(ty, tys) => {
            collect_fvs(state, ty, fvs);
            tys.iter().for_each(|ty| collect_fvs(state, ty, fvs));
        }

        ast::Type_::Con(_) => {}

        ast::Type_::Var(id) => {
            fvs.insert(id.clone(), Kind::Var(state.new_var()));
        }
    }
}

/// Kinds with unification variables.
#[derive(Clone, PartialEq, Eq)]
enum Kind {
    Star,
    Fun(Box<Kind>, Box<Kind>),
    Var(KindVar),
}

impl std::fmt::Debug for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Fun(k1, k2) => match &**k1 {
                Kind::Star | Kind::Var(_) => write!(f, "{:?} -> {:?}", k1, k2),
                Kind::Fun(_, _) => write!(f, "({:?}) -> {:?}", k1, k2),
            },
            Kind::Var(var) => write!(f, "_{}", var.0),
        }
    }
}

impl From<&typing::Kind> for Kind {
    fn from(kind: &typing::Kind) -> Self {
        match kind {
            typing::Kind::Star => Kind::Star,
            typing::Kind::Fun(k1, k2) => Kind::Fun(
                Box::new(Kind::from(k1.as_ref())),
                Box::new(Kind::from(k2.as_ref())),
            ),
        }
    }
}

/// A unification variable representing a kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct KindVar(u32);

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

fn collect_types(decls: &[ast::RenamedDecl]) -> Vec<TypeDecl> {
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
        .filter_map(|decl| match &decl.node {
            ast::Decl_::Data(d) => Some(data_to_type_decl(d)),
            ast::Decl_::Newtype(d) => Some(newtype_to_type_decl(d)),
            ast::Decl_::Class(d) => Some(typeclass_to_type_decl(d)),
            ast::Decl_::Type(d) => Some(type_to_type_decl(d)),
            _ => None,
        })
        .collect()
}

#[derive(Debug)]
struct KindInferState {
    /// Next fresh unification varible.
    next_var: KindVar,

    /// Maps unification variables to their current values.
    unification_vars: Map<KindVar, Kind>,
}

impl KindInferState {
    fn new() -> Self {
        Self {
            next_var: KindVar(0),
            unification_vars: Default::default(),
        }
    }

    fn new_var(&mut self) -> KindVar {
        let var = self.next_var;
        self.next_var = KindVar(var.0 + 1);
        var
    }

    /// `k1 -> ... -> k{arity - 1} -> *`.
    fn new_kind(&mut self, arity: usize) -> Kind {
        self.new_kind_w_result_kind(Kind::Star, arity)
    }

    /// Similar to `new_kind`, but the given result kind.
    fn new_kind_w_result_kind(&mut self, mut kind: Kind, mut arity: usize) -> Kind {
        while arity != 0 {
            kind = Kind::Fun(Box::new(Kind::Var(self.new_var())), Box::new(kind));
            arity -= 1;
        }
        kind
    }

    fn new_kind_fun(&mut self, args: Vec<Kind>, mut ret: Kind) -> Kind {
        for arg in args.into_iter().rev() {
            ret = Kind::Fun(Box::new(arg), Box::new(ret));
        }
        ret
    }
}

/// Unify kind of `ty` with `expected_kind`. Type variables in `ty` must be bound in `arg_kinds` to
/// their kinds.
fn unify_ty_kind(
    state: &mut KindInferState,
    con_kinds: &Map<Id, Kind>,
    arg_kinds: &Map<Id, Kind>,
    bound_kinds: &Map<Id, Kind>,
    ty: &ast::RenamedType,
    expected_kind: &Kind,
) {
    let kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, ty);
    unify(state, kind, expected_kind.clone());
}

fn unify(state: &mut KindInferState, mut k1: Kind, mut k2: Kind) {
    loop {
        if let Kind::Var(var) = k1 {
            if let Some(k) = state.unification_vars.get(&var) {
                k1 = k.clone();
                continue;
            }
        }
        break;
    }

    loop {
        if let Kind::Var(var) = k2 {
            if let Some(k) = state.unification_vars.get(&var) {
                k2 = k.clone();
                continue;
            }
        }
        break;
    }

    match (k1, k2) {
        (Kind::Var(var), k) | (k, Kind::Var(var)) => {
            if occurs(&var, &k) {
                panic!("Occurs check fail");
            }
            let old = state.unification_vars.insert(var, k);
            assert_eq!(old, None);
        }

        (Kind::Star, Kind::Star) => {}

        (Kind::Fun(k1_1, k1_2), Kind::Fun(k2_1, k2_2)) => {
            unify(state, *k1_1, *k2_1);
            unify(state, *k1_2, *k2_2);
        }

        (k1, k2) => panic!("Kinds do not unify: {:?} ~ {:?}", k1, k2),
    }
}

/// Returns whether `var` occurs in `kind`.
fn occurs(var: &KindVar, kind: &Kind) -> bool {
    match kind {
        Kind::Star => false,
        Kind::Fun(k1, k2) => occurs(var, k1) || occurs(var, k2),
        Kind::Var(var2) => var == var2,
    }
}

/// Infer kind of `ty`.
fn ty_kind(
    state: &mut KindInferState,
    con_kinds: &Map<Id, Kind>,
    arg_kinds: &Map<Id, Kind>,
    bound_kinds: &Map<Id, Kind>,
    ty: &ast::RenamedType,
) -> Kind {
    match &ty.node {
        ast::Type_::Tuple(args) => {
            for arg in args {
                let arg_kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, arg);
                unify(state, arg_kind, Kind::Star);
            }
            Kind::Star
        }

        ast::Type_::List(arg) => {
            let arg_kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, arg);
            unify(state, arg_kind, Kind::Star);
            Kind::Star
        }

        ast::Type_::Arrow(arg1, arg2) => {
            let arg1_kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, arg1);
            unify(state, arg1_kind, Kind::Star);

            let arg2_kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, arg2);
            unify(state, arg2_kind, Kind::Star);

            Kind::Star
        }

        ast::Type_::App(ty, tys) => {
            let result_kind = Kind::Var(state.new_var());

            let mut expected_fun_kind = result_kind.clone();
            for arg_ty in tys.iter().rev() {
                let arg_ty_kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, arg_ty);
                expected_fun_kind = Kind::Fun(Box::new(arg_ty_kind), Box::new(expected_fun_kind));
            }
            let expected_fun_kind = expected_fun_kind;

            let actual_fun_kind = ty_kind(state, con_kinds, arg_kinds, bound_kinds, ty);

            unify(state, expected_fun_kind, actual_fun_kind);

            result_kind
        }

        ast::Type_::Con(con) => match &con.node {
            ast::TyCon_::Id(con) => con_kinds.get(con).unwrap().clone(),

            ast::TyCon_::Tuple(arity) => state.new_kind(*arity as usize),

            ast::TyCon_::Arrow => state.new_kind(2),

            ast::TyCon_::List => state.new_kind(1),
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

/// Normalize all kinds in `kinds`.
fn normalize_kinds(kinds: &Map<Id, Kind>, vars: &Map<KindVar, Kind>) -> Map<Id, typing::Kind> {
    kinds
        .iter()
        .map(|(id, kind)| (id.clone(), normalize_kind(kind, vars)))
        .collect()
}

/// Replace unification variables in `kind` with their values. Variables without a value are
/// defaulted as `*`.
fn normalize_kind(kind: &Kind, vars: &Map<KindVar, Kind>) -> typing::Kind {
    match kind {
        Kind::Star => typing::Kind::Star,

        Kind::Fun(k1, k2) => typing::Kind::Fun(
            Box::new(normalize_kind(k1, vars)),
            Box::new(normalize_kind(k2, vars)),
        ),

        Kind::Var(var) => match vars.get(var) {
            Some(kind) => normalize_kind(kind, vars),
            None => typing::Kind::Star, // defaulting
        },
    }
}
