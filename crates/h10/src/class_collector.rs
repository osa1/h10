use crate::ast;
use crate::ast_to_ty::convert_ast_ty;
use crate::class_env::{Class, ClassEnv, Instance, Pred};
use crate::collections::Map;
use crate::id::Id;
use crate::kind_inference::infer_fv_kinds;
use crate::type_scheme::Scheme;
use crate::typing::TyRef;

/// Creates the class environment for a module. The returned environment is not type checked, use
/// `TI::ti_module` to type check it.
pub(crate) fn module_class_env(module: &[ast::TopDecl], kinds: &Map<Id, TyRef>) -> ClassEnv {
    let mut class_env: Map<Id, Class> = Default::default();

    // Collect classes.
    for decl in module {
        if let ast::TopDeclKind::Class(ast::AstNode {
            node:
                ast::ClassDecl_ {
                    context: _, // handled in the next pass
                    ty_con,
                    ty_arg,
                    decls,
                },
            span,
        }) = &decl.kind
        {
            let class_kind: &TyRef = kinds.get(ty_con).unwrap();

            println!("Class {} kind = {}", ty_con, class_kind);

            let (mut arg_kinds, _ret_kind) = class_kind.split_fun_ty();
            assert_eq!(arg_kinds.len(), 1); // no multi-param typeclasses yet

            let arg_kind = arg_kinds.pop().unwrap();

            let mut methods: Map<Id, Scheme> = Default::default();
            for decl in decls {
                if let ast::ValueDecl_::TypeSig {
                    vars,
                    foralls: _,
                    context,
                    ty,
                } = &decl.node
                {
                    let mut method_context: Vec<ast::Type> = Vec::with_capacity(context.len() + 1);

                    // Add `C t` as the first predicate to the context. `t` will be seen by the
                    // converter first and `t` will be the first quantified variable (`Gen 0`) as
                    // expected.
                    // TODO: It would be good to avoid creating an `AstNode` as ther's no source
                    // code (span) for this predicate.
                    method_context.push(ast::AstNode::new(
                        span.clone(),
                        ast::Type_::App(
                            // C
                            Box::new(ast::AstNode::new(
                                span.clone(),
                                ast::Type_::Con(ast::AstNode::new(
                                    span.clone(),
                                    ast::TyCon_::Id(ty_con.clone()),
                                )),
                            )),
                            // t
                            vec![ast::AstNode::new(
                                span.clone(),
                                ast::Type_::Var(ty_arg.clone()),
                            )],
                        ),
                    ));

                    method_context.extend(context.iter().cloned());

                    for var in vars {
                        let scheme = Scheme::from_type_sig(
                            decl.span.clone(),
                            kinds,
                            &Default::default(),
                            &method_context,
                            ty,
                        );
                        let old = methods.insert(var.clone(), scheme);
                        assert!(old.is_none(), "{:?}", old);
                    }
                }
            }

            let old = class_env.insert(
                ty_con.clone(),
                Class {
                    class: ty_con.clone(),
                    arg: ty_arg.clone(),
                    arg_kind,
                    supers: vec![], // added in the next pass
                    methods,
                    instances: vec![], // added in the next pass
                },
            );

            if old.is_some() {
                panic!("Class {:?} defined multiple times", ty_con);
            }
        }
    }

    // Add superclasses.
    for decl in module {
        if let ast::TopDeclKind::Class(ast::AstNode {
            node:
                ast::ClassDecl_ {
                    context,
                    ty_con,
                    ty_arg,
                    decls: _,
                },
            ..
        }) = &decl.kind
        {
            for pred in context {
                // pred must be `S t` where `t` is ty_arg
                let super_con = match split_pred(pred) {
                    Some((
                        con,
                        ast::AstNode {
                            node: ast::Type_::Var(arg),
                            ..
                        },
                    )) if arg == ty_arg => con,
                    _ => panic!("Superclass should be in form `C {:?}`: {:?}", ty_arg, pred),
                };

                class_env.get_mut(ty_con).unwrap().supers.push(super_con);
            }
        }
    }

    // Add instances.
    for decl in module {
        if let ast::TopDeclKind::Instance(ast::AstNode {
            node:
                ast::InstanceDecl_ {
                    context,
                    ty_con,
                    ty,
                    decls: _,
                },
            ..
        }) = &decl.kind
        {
            // Argument kiund of `ty_con`, expected kind of `ty`.
            let ty_con_arg_kind: TyRef = kinds.get(ty_con).unwrap().get_kind_arrow_star_kind();

            let fv_kinds: Vec<(Id, TyRef)> = infer_fv_kinds(kinds, context, ty, &ty_con_arg_kind);

            let fv_gens: Map<Id, u32> = fv_kinds
                .iter()
                .enumerate()
                .map(|(id_idx, (id, _kind))| (id.clone(), id_idx as u32))
                .collect();

            let kinds: Vec<TyRef> = fv_kinds.into_iter().map(|(_, kind)| kind).collect();

            // TODO: Any restrictions on predicates here that we need to check?
            let mut preds: Vec<Pred> = Default::default();
            for pred in context {
                let (pred_con, pred_arg) = match split_pred(pred) {
                    Some(pred) => pred,
                    _ => panic!("Invalid instance context"),
                };

                // TODO: Shouldn't constructors be bound here?
                let pred_ty = convert_ast_ty(&|_id| None, &fv_gens, pred_arg);

                preds.push(Pred {
                    class: pred_con,
                    ty: pred_ty,
                });
            }

            // TODO: Same as above, consturctors should be bound.
            let head = convert_ast_ty(&|_id| None, &fv_gens, ty);

            class_env.get_mut(ty_con).unwrap().instances.push(Instance {
                class: ty_con.clone(),
                kinds,
                context: preds,
                head,
            });
        }
    }

    ClassEnv {
        classes: class_env,
        defaults: Default::default(),
    }
}

fn split_pred(pred: &ast::Type) -> Option<(Id, &ast::Type)> {
    match &pred.node {
        ast::Type_::App(con, args) => match &con.node {
            ast::Type_::Con(ast::AstNode {
                node: ast::TyCon_::Id(id),
                ..
            }) if args.len() == 1 => Some((id.clone(), &args[0])),
            _ => None,
        },
        _ => None,
    }
}
