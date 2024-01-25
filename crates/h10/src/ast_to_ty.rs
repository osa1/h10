//! Conversion of AST types to type checking types.

use crate::ast;
use crate::collections::Map;
use crate::id::{self, Id};
use crate::typing::TyRef;

/// Convert a renamed AST type (`ast::ParsedType`) to type checking type (`TyRef`).
///
/// - `bound_vars`: Maps bound type variables in scope to their `TyRef`s.
/// - `free_vars`: Maps free variables in the type being converted to their `Gen` indices.
pub fn convert_ast_ty<GetBoundVar>(
    get_bound_var: &GetBoundVar,
    free_vars: &Map<Id, u32>,
    ty: &ast::ParsedType,
) -> TyRef
where
    GetBoundVar: Fn(&Id) -> Option<TyRef>,
{
    match &ty.node {
        ast::Type_::Var(id) => match (get_bound_var(id), free_vars.get(id)) {
            (Some(var_ref), None) => var_ref.clone(),
            (None, Some(idx)) => TyRef::new_gen(*idx), // TODO: cache gen tys
            (None, None) => panic!("Unbound type variable: {}", id),
            (Some(_), Some(_)) => panic!("Type variable {} is both bound and free", id),
        },

        ast::Type_::Con(con) => match &con.node {
            ast::TyCon_::Id(id) => TyRef::new_con(id.clone()),

            ast::TyCon_::Tuple(arity) => TyRef::new_con(id::tuple_ty_id(*arity)),

            ast::TyCon_::Arrow => TyRef::new_con(id::arrow_ty_id()),

            ast::TyCon_::List => TyRef::new_con(id::list_ty_id()),
        },

        ast::Type_::Tuple(elems) => {
            let elems: Vec<TyRef> = elems
                .iter()
                .map(|elem| convert_ast_ty(get_bound_var, free_vars, elem))
                .collect();

            crate::type_inference::make_tuple_ty(&elems)
        }

        ast::Type_::List(elem) => {
            crate::type_inference::make_list_ty(convert_ast_ty(get_bound_var, free_vars, elem))
        }

        ast::Type_::Arrow(ty1, ty2) => {
            let ty1 = convert_ast_ty(get_bound_var, free_vars, ty1);
            let ty2 = convert_ast_ty(get_bound_var, free_vars, ty2);
            crate::type_inference::make_fun_ty(vec![ty1], ty2)
        }

        ast::Type_::App(ty1, args) => {
            let ty1 = convert_ast_ty(get_bound_var, free_vars, ty1);
            args.iter().fold(ty1, |ty1, ty2| {
                let ty2 = convert_ast_ty(get_bound_var, free_vars, ty2);
                TyRef::new_app(ty1, ty2)
            })
        }
    }
}
