//! The type checking and inference routines.

#[cfg(test)]
mod tests;

use crate::ast;
use crate::ast_to_ty::convert_ast_ty;
use crate::bind_groups::{
    group_binds, group_top_binds, BindingGroup, FunBindingGroup, FunDef, PatBinding,
};
use crate::class_collector::module_class_env;
use crate::class_env::{ClassEnv, Pred};
use crate::collections::{Map, Set, TrieMap};
use crate::dependency_analysis::{dependency_analysis, DependencyGroups};
use crate::id::{self, Id};
use crate::kind_inference::{infer_fv_kinds, infer_type_kinds};
use crate::token::Literal;
use crate::type_scheme::Scheme;
use crate::typing::{Kind, Ty, TyRef, TyVarRef};
use crate::unification::{unify, unify_left};

use std::collections::hash_map::Entry;
use std::ops::Deref;

/// Type check and infer a renamed module.
///
/// Note: for now we don't allow imports and type check a whole program as just one module. The
/// only built-ins the type checked module gets is the Haskell 2010 built-ins that can't be
/// implemented in Haskell 2010, such as the list type, and `Int` and `Float` types.
pub fn ti_module(module: &[ast::RenamedDecl]) -> (Map<Id, Kind>, TrieMap<Id, Scheme>) {
    let ty_kinds: Map<Id, Kind> = infer_type_kinds(module);
    let class_env: ClassEnv = module_class_env(module, &ty_kinds);
    let con_tys: Map<Id, Scheme> = collect_con_tys(module, &ty_kinds);
    let ty_syns: Map<Id, TypeSynonym> = collect_type_synonyms(module, &ty_kinds);
    let mut ti = TI::new(class_env, con_tys, ty_kinds, ty_syns);
    let bind_tys = ti.ti_module(module).unwrap();
    (ti.ty_kinds, bind_tys)
}

fn collect_con_tys(decls: &[ast::RenamedDecl], ty_kinds: &Map<Id, Kind>) -> Map<Id, Scheme> {
    // Collect data con types, to be used in field types.
    let mut con_ty_refs: TrieMap<Id, TyRef> = Default::default();

    for decl in decls {
        match &decl.node {
            ast::Decl_::Data(decl) => {
                let ty_con = TyRef::new_con(decl.node.ty_con.clone());
                for con in &decl.node.cons {
                    con_ty_refs.insert_mut(con.node.con.clone(), ty_con.clone());
                }
            }

            ast::Decl_::Newtype(decl) => {
                let ty_con = TyRef::new_con(decl.node.ty_con.clone());
                con_ty_refs.insert_mut(decl.node.con.node.con.clone(), ty_con);
            }

            ast::Decl_::Type(_)
            | ast::Decl_::Value(_)
            | ast::Decl_::Class(_)
            | ast::Decl_::Instance(_)
            | ast::Decl_::Default(_) => {}
        }
    }

    // Generate data con details.
    let mut con_tys: Map<Id, Scheme> = Default::default();

    for decl in decls {
        match &decl.node {
            ast::Decl_::Data(decl) => {
                let ty_con_kind: &Kind = ty_kinds.get(&decl.node.ty_con).unwrap();
                let ty_con_args: &[Id] = &decl.node.ty_args;

                // Map args to their kinds, to be used to generate `Gen`s in the scheme.
                let (ty_con_arg_kinds, ty_con_ret_kind) = ty_con_kind.split_fun_kind();
                assert_eq!(ty_con_ret_kind, Kind::Star);
                assert_eq!(ty_con_arg_kinds.len(), ty_con_args.len());

                let ty_arg_gens: Map<Id, u32> = ty_con_args
                    .iter()
                    .enumerate()
                    .map(|(idx, id)| (id.clone(), idx as u32))
                    .collect();

                for con in &decl.node.cons {
                    let mut field_tys: Vec<TyRef> = Vec::with_capacity(con.node.fields.len());
                    for field in &con.node.fields {
                        if !field.node.vars.is_empty() {
                            todo!("Record constructors");
                        }
                        field_tys.push(convert_ast_ty(&con_ty_refs, &ty_arg_gens, &field.node.ty));
                    }
                    let con_scheme = Scheme {
                        kinds: ty_con_arg_kinds.clone(),
                        preds: vec![],
                        ty: make_fun_ty(
                            field_tys,
                            (0..ty_con_args.len()).fold(
                                con_ty_refs
                                    .get(&con.node.con)
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "Unknown data con={:?}, data cons={:?}",
                                            decl.node.ty_con,
                                            con_ty_refs.iter().collect::<Vec<_>>()
                                        )
                                    })
                                    .clone(),
                                |ty, arg_idx| TyRef::new_app(ty, TyRef::new_gen(arg_idx as u32)),
                            ),
                        ),
                        span: Some(decl.span.clone()),
                    };
                    con_tys.insert(con.node.con.clone(), con_scheme);
                }
            }

            ast::Decl_::Newtype(_decl) => {
                todo!("Newtype in collect_con_tys")
            }

            ast::Decl_::Type(_)
            | ast::Decl_::Value(_)
            | ast::Decl_::Class(_)
            | ast::Decl_::Instance(_)
            | ast::Decl_::Default(_) => {}
        }
    }

    con_tys
}

#[derive(Debug)]
pub(crate) struct TypeSynonym {
    pub(crate) args: Vec<Kind>,
    pub(crate) rhs: TyRef,
}

fn collect_type_synonyms(
    decls: &[ast::RenamedDecl],
    ty_kinds: &Map<Id, Kind>,
) -> Map<Id, TypeSynonym> {
    let mut type_synonyms: Map<Id, TypeSynonym> = Default::default();

    for decl in decls {
        if let ast::Decl_::Type(ast::AstNode {
            node: ast::TypeDecl_ { ty, vars, rhs },
            ..
        }) = &decl.node
        {
            let ty_con_kind: &Kind = ty_kinds.get(ty).unwrap();
            let ty_con_args = vars.clone();

            let (ty_con_arg_kinds, ty_con_ret_kind) = ty_con_kind.split_fun_kind();
            assert_eq!(ty_con_ret_kind, Kind::Star);
            assert_eq!(ty_con_arg_kinds.len(), ty_con_args.len());

            let var_gens: Map<Id, u32> = vars
                .iter()
                .enumerate()
                .map(|(idx, var)| (var.clone(), idx as u32))
                .collect();

            let rhs = convert_ast_ty(&Default::default(), &var_gens, rhs);

            type_synonyms.insert(
                ty.clone(),
                TypeSynonym {
                    args: ty_con_arg_kinds,
                    rhs,
                },
            );
        }
    }

    type_synonyms
}

/// The type inference state.
#[derive(Debug)]
struct TI {
    /// The class environment.
    class_env: ClassEnv,

    /// Maps constructors to their types.
    con_tys: Map<Id, Scheme>,

    /// Maps type constructors to their kinds.
    ty_kinds: Map<Id, Kind>,

    /// Type synonyms.
    ty_syns: Map<Id, TypeSynonym>,

    /// Id of the `Num` type constructor.
    num_ty_id: Option<Id>,

    /// Id of the `Fractional` type constructor.
    fractional_ty_id: Option<Id>,

    /// Id of the `String` type constructor.
    string_ty_id: Option<Id>,
}

impl TI {
    fn new(
        class_env: ClassEnv,
        mut con_tys: Map<Id, Scheme>,
        ty_kinds: Map<Id, Kind>,
        ty_synonyms: Map<Id, TypeSynonym>,
    ) -> Self {
        // Add prelude constructors that can't be implemented in Haskell.
        let list_arg_ty = TyRef::new_gen(0);
        let list_return_ty = TyRef::new_app(TyRef::new_con(id::list_ty_id()), list_arg_ty.clone());

        // []
        con_tys.insert(
            id::nil_id(),
            Scheme {
                span: None,
                kinds: vec![Kind::Star],
                preds: vec![],
                ty: list_return_ty.clone(),
            },
        );

        // (:)
        con_tys.insert(
            id::cons_id(),
            Scheme {
                span: None,
                kinds: vec![Kind::Star],
                preds: vec![],
                ty: make_fun_ty(vec![list_arg_ty, list_return_ty.clone()], list_return_ty),
            },
        );

        Self {
            class_env,
            con_tys,
            ty_kinds,
            ty_syns: ty_synonyms,
            num_ty_id: None,
            fractional_ty_id: None,
            string_ty_id: None,
        }
    }

    fn num_ty_id(&mut self) -> Id {
        if let Some(num_ty_id) = &self.num_ty_id {
            return num_ty_id.clone();
        }

        let num_ty_id = self
            .class_env
            .classes
            .values()
            .find(|cls| cls.class.name() == Some("Num"))
            .unwrap_or_else(|| panic!("Num is not in class environment"))
            .class
            .clone();

        self.num_ty_id = Some(num_ty_id.clone());

        num_ty_id
    }

    fn fractional_ty_id(&mut self) -> Id {
        if let Some(fractional_ty_id) = &self.fractional_ty_id {
            return fractional_ty_id.clone();
        }

        let fractional_ty_id = self
            .class_env
            .classes
            .values()
            .find(|cls| cls.class.name() == Some("Fractional"))
            .unwrap_or_else(|| panic!("Fractional is not in class environment"))
            .class
            .clone();

        self.fractional_ty_id = Some(fractional_ty_id.clone());

        fractional_ty_id
    }

    fn string_ty_id(&mut self) -> Id {
        if let Some(string_ty_id) = &self.string_ty_id {
            return string_ty_id.clone();
        }

        let string_ty_id = self
            .ty_syns
            .keys()
            .find(|syn_id| syn_id.name() == Some("String"))
            .unwrap_or_else(|| panic!("String is not in type synonyms"))
            .clone();

        self.string_ty_id = Some(string_ty_id.clone());

        string_ty_id
    }

    /// Entry point for type checking a module: type checks a whole module.
    fn ti_module(&mut self, module: &[ast::RenamedDecl]) -> Result<TrieMap<Id, Scheme>, String> {
        let mut module_assumps: TrieMap<Id, Scheme> = Default::default();

        // Add data constructors to assumptions.
        for (con_id, con_scheme) in &self.con_tys {
            module_assumps.insert_mut(con_id.clone(), con_scheme.clone());
        }

        // Add typeclass methods to assumptions.
        for class in self.class_env.classes.values() {
            for (method_id, method_scheme) in &class.methods {
                module_assumps.insert_mut(method_id.clone(), method_scheme.clone());
            }
        }

        println!("--------------------------------------------------");
        println!("Assumptions before type inference:");
        for (value, scheme) in &module_assumps {
            println!("{} : {}", value, scheme);
        }
        println!("--------------------------------------------------");

        println!("Class env before type inference:");
        for cls in self.class_env.classes.values() {
            println!("{}", cls.class);
        }
        println!("--------------------------------------------------");

        let sigs: Map<Id, Scheme> = collect_top_sigs(module, &self.ty_kinds);
        self.ti_top_level_binds(module, &mut module_assumps, &sigs)?;
        self.ti_classes(module, &mut module_assumps, &sigs)?;
        self.ti_instances(module, &mut module_assumps, sigs)?;

        Ok(module_assumps)
    }

    /// Type check everything except classes and instances.
    fn ti_top_level_binds(
        &mut self,
        module: &[ast::RenamedDecl],
        assumps: &mut TrieMap<Id, Scheme>,
        sigs: &Map<Id, Scheme>,
    ) -> Result<(), String> {
        let groups: Vec<BindingGroup> = group_top_binds(module);
        self.ti_groups(
            0,                   // level
            &Default::default(), // type variables
            assumps,
            sigs,
            &groups,
        )
    }

    /// Type check class method implementations.
    fn ti_classes(
        &mut self,
        module: &[ast::RenamedDecl],
        assumps: &mut TrieMap<Id, Scheme>,
        sigs: &Map<Id, Scheme>,
    ) -> Result<(), String> {
        for decl in module {
            if let ast::Decl_::Class(class_decl) = &decl.node {
                let groups = group_binds(&class_decl.node.decls);
                for method in groups {
                    let binds = vec![method];
                    self.ti_groups(
                        0,                   // level
                        &Default::default(), // type variables
                        assumps,
                        sigs,
                        &binds,
                    )?;
                }
            }
        }
        Ok(())
    }

    /// Type check instances.
    fn ti_instances(
        &mut self,
        module: &[ast::RenamedDecl],
        assumps: &mut TrieMap<Id, Scheme>,
        mut sigs: Map<Id, Scheme>,
    ) -> Result<(), String> {
        for decl in module {
            if let ast::Decl_::Instance(instance_decl) = &decl.node {
                self.ti_instance(instance_decl, assumps, &mut sigs)?;
            }
        }
        Ok(())
    }

    fn ti_instance(
        &mut self,
        instance: &ast::RenamedInstanceDecl,
        assumps: &mut TrieMap<Id, Scheme>,
        sigs: &mut Map<Id, Scheme>,
    ) -> Result<(), String> {
        let ast::InstanceDecl_ {
            context: instance_context,
            ty_con: instance_ty_con,
            ty: instance_ty,
            decls: instance_decls,
        } = &instance.node;

        // TODO: Add instance context to the class environment.

        // Kind of the typeclass type constructor argument.
        // E.g. in `Functor f`, `* -> *`.
        let ty_con_arg_kind = self
            .ty_kinds
            .get(instance_ty_con)
            .unwrap()
            .get_kind_arrow_star_kind();

        // Kinds of free varibles in instance type argument.
        // E.g. in `Functor (Either a)`, `{a = *}`.
        let fv_kinds: Map<Id, Kind> = infer_fv_kinds(
            &self.ty_kinds,
            instance_context,
            instance_ty,
            ty_con_arg_kind,
        );

        // TODO: Shouldn't we sort the kinds based on fv id?
        let fv_kinds_vec: Vec<Kind> = fv_kinds.values().cloned().collect();

        // Maps free variables in `fv_kinds` to `Gen` indices.
        let fv_gens: Map<Id, u32> = fv_kinds
            .iter()
            .enumerate()
            .map(|(id_idx, (id, _kind))| (id.clone(), id_idx as u32))
            .collect();

        // Type of the instance type, e.g. in `Functor (Either a)`, `Either a`.
        let instance_ty: TyRef = convert_ast_ty(
            &Default::default(), // bound vars
            &fv_gens,            // free vars
            instance_ty,
        );

        let groups = group_binds(instance_decls);
        for method in groups {
            // Get the method's type from the class environment, substitute the instance's
            // type for the class type argument.
            let instance_method_id = match &method {
                BindingGroup::Pat(pat) => match pat.pat.simple_pat_var() {
                    Some(var) => var.clone(),
                    None => panic!("Pattern binding in instance declaration"),
                },
                BindingGroup::Fun(fun_binding) => fun_binding.id.clone(),
            };

            let instance_method_str: &str = instance_method_id.name().unwrap();

            // TODO: Kind check class type arg vs. instance type arg.
            let class_method_scheme: &Scheme = self
                .class_env
                .get_method_sig(instance_ty_con, instance_method_str)
                .unwrap();

            let instance_method_scheme: Scheme =
                class_method_scheme.subst_gen_0(&fv_kinds_vec, &instance_ty);

            // Add the instance method type to the assumptions so that it will be checked
            // against the expected type.
            //
            // Note: The class method and the instance method have different ids.
            let old = sigs.insert(instance_method_id.clone(), instance_method_scheme);
            assert!(old.is_none());

            let binds = vec![method];
            self.ti_groups(
                0,                   // level
                &Default::default(), // type variables
                assumps,
                sigs,
                &binds,
            )?;

            let old = sigs.remove(&instance_method_id);
            assert!(old.is_some());
        }

        Ok(())
    }

    /// Type infer binding groups.
    ///
    /// `sigs` is the type signatures of the bindings in the groups. These types should not be
    /// added to `assumps` at this point.
    fn ti_groups(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &mut TrieMap<Id, Scheme>,
        sigs: &Map<Id, Scheme>,
        groups: &[BindingGroup],
    ) -> Result<(), String> {
        let DependencyGroups {
            implicitly_typed,
            explicitly_typed,
        } = dependency_analysis(groups, sigs);

        // TODO: We don't need explicit and implicit distinction here. Treating all the same makes
        // the rest of the process simpler. Refactor dependency analysis to return one list.
        let mut all_groups: Vec<Vec<&BindingGroup>> = implicitly_typed;
        for explicit_binding in explicitly_typed {
            all_groups.push(vec![explicit_binding]);
        }

        for group in &all_groups {
            self.ti_recursive_group(level, ty_vars, assumps, sigs, group)?;
        }

        Ok(())
    }

    fn ti_recursive_group(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &mut TrieMap<Id, Scheme>,
        sigs: &Map<Id, Scheme>,
        group: &[&BindingGroup],
    ) -> Result<(), String> {
        let mut group_ids: Set<Id> = Default::default();
        for binding in group {
            binding.collect_defined_ids(&mut group_ids);
        }

        // Add types of bindings in the group to the assumptions.
        let mut group_id_type_schemes: Map<Id, Scheme> = Default::default();
        for id in &group_ids {
            let id_ty = match sigs.get(id) {
                Some(explicit_ty) => explicit_ty.clone(),
                None => Scheme::monomorphic(TyRef::new_var(Kind::Star, level)),
            };
            let old = group_id_type_schemes.insert(id.clone(), id_ty.clone());
            debug_assert!(old.is_none(), "{} : {:?}", id, old);

            assumps.insert_mut(id.clone(), id_ty);
        }

        // Predicates inferred for the recursive group.
        let mut inferred_preds: Vec<Pred> = vec![];

        // Type check bindings.
        let mut inferred_tys: Vec<TyRef> = Vec::with_capacity(group.len());
        for binding in group {
            let inferred_ty =
                self.ti_binding_group(level, ty_vars, assumps, sigs, binding, &mut inferred_preds)?;
            inferred_tys.push(inferred_ty);
        }

        // Simplify inferred preds.
        let inferred_preds: Vec<Pred> = self
            .class_env
            .reduce_context(&self.ty_syns, &inferred_preds);

        // Add predicates to inferred types.
        let mut inferred_ty_preds: Vec<(Vec<Pred>, TyRef)> = Vec::with_capacity(inferred_tys.len());

        for inferred_ty in inferred_tys {
            let mut preds: Vec<Pred> = Vec::new();
            let inferred_ty: TyRef = inferred_ty.normalize();

            for inferred_pred in &inferred_preds {
                let pred_var = inferred_pred.hnf_ty_var();
                if inferred_ty.contains_var(&pred_var) {
                    preds.push(inferred_pred.clone());
                }
            }

            inferred_ty_preds.push((preds, inferred_ty));
        }

        // Unify inferred types of bindings with the type schemes we've added before type
        // inference.
        //
        // This is to check that (1) use sites (2) explicit types (3) inferred types are
        // all compatible.
        for (binding, (binding_inferred_preds, binding_inferred_ty)) in
            group.iter().zip(inferred_ty_preds.iter())
        {
            match binding {
                BindingGroup::Fun(FunBindingGroup { id, .. })
                | BindingGroup::Pat(PatBinding {
                    pat:
                        ast::AstNode {
                            node: ast::Pat_::Var(id),
                            ..
                        },
                    ..
                }) => {
                    match sigs.get(id) {
                        Some(sig) => {
                            // Explicitly typed binding: compare inferred and given schemes.

                            // Instantiate the types at the current level, check that the
                            // inferred type is at least as general as the explicit type.
                            let (explicit_preds, explicit_ty) = sig.instantiate(u32::MAX);

                            // Example: explicit = `(a, b)`, inferred = `(int, bool)`.
                            if unify_left(&self.ty_syns, binding_inferred_ty, &explicit_ty).is_err()
                            {
                                panic!(
                                    "Type error: explicit type `{}` is more general than the inferred type `{}`",
                                    explicit_ty.normalize(), binding_inferred_ty.normalize()
                                );
                            }

                            // The unification above can allow simplification of predicates,
                            // for example `Functor a` can become `Functor []`, which can be
                            // eliminated.
                            let inferred_preds: Vec<Pred> = self
                                .class_env
                                .reduce_context(&self.ty_syns, &inferred_preds);

                            // Predicates in the inferred type must be in the explicit type as well. Explicit
                            // type can have more predicates.
                            'outer: for Pred { class, ty } in &inferred_preds {
                                for exp_pred in &explicit_preds {
                                    if class == &exp_pred.class {
                                        // TODO: Syntactic equality OK?
                                        if ty.deep_normalize() == exp_pred.ty {
                                            continue 'outer;
                                        }
                                    }
                                }
                                panic!(
                                    "Type error: inferred predicate `{} {}` not in explicit type {:?} . {} ({})",
                                    class, ty, explicit_preds, explicit_ty, match &sig.span {
                                        Some(span) => format!("{}", span),
                                        None => "?".to_string(),
                                    }
                                );
                            }
                        }
                        None => {
                            // Implicitly typed binding: unify the fresh type we've created for
                            // the binding in `group_id_type_schemes`.
                            let (fun_fresh_preds, fun_fresh_ty) = group_id_type_schemes
                                .get(id)
                                .unwrap()
                                .instantiate_monomorphic();

                            // TODO: Monomorphic types can't have preds, move this assertion to
                            // `instantiate_monomorphic`.
                            assert!(fun_fresh_preds.is_empty());

                            unify(&self.ty_syns, &fun_fresh_ty, binding_inferred_ty)?;

                            // Generalize the inferred type and update the assumption.
                            let scheme =
                                generalize(level, binding_inferred_preds, binding_inferred_ty);
                            assumps.insert_mut(id.clone(), scheme);
                        }
                    }
                }

                BindingGroup::Pat(PatBinding { .. }) => {
                    // TODO: Do we need anything to do here?
                }
            }
        }

        Ok(())
    }

    // NB. `assumps` should've been updated with the types of the binders.
    fn ti_binding_group(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &TrieMap<Id, Scheme>,
        explicit_tys: &Map<Id, Scheme>,
        group: &BindingGroup,
        preds: &mut Vec<Pred>,
    ) -> Result<TyRef, String> {
        let mut group_assumps = assumps.clone();

        match group {
            BindingGroup::Pat(PatBinding { pat, rhs }) => {
                if pat.simple_pat_var().is_some() {
                    self.ti_rhs(level, ty_vars, &group_assumps, rhs, preds)
                } else {
                    let pat_ty = self.ti_pat(
                        level,
                        pat,
                        &mut group_assumps,
                        &explicit_tys.keys().cloned().chain(pat.vars()).collect(),
                        preds,
                    )?;
                    let rhs_ty = self.ti_rhs(level, ty_vars, &group_assumps, rhs, preds)?;
                    unify(&self.ty_syns, &pat_ty, &rhs_ty)?;
                    Ok(rhs_ty)
                }
            }

            BindingGroup::Fun(FunBindingGroup { id: _, defs }) => {
                let mut fun_arg_tys: Option<Vec<TyRef>> = None;
                let mut fun_rhs_ty: Option<TyRef> = None;

                for FunDef { args, rhs } in defs {
                    let mut def_arg_tys: Vec<TyRef> = Vec::with_capacity(args.len());
                    for arg in args.iter() {
                        def_arg_tys.push(self.ti_pat(
                            level,
                            arg,
                            &mut group_assumps,
                            &Default::default(),
                            preds,
                        )?);
                    }
                    let def_arg_tys = def_arg_tys;

                    match &fun_arg_tys {
                        None => {
                            fun_arg_tys = Some(def_arg_tys.clone());
                        }
                        Some(fun_arg_tys) => {
                            if def_arg_tys.len() != fun_arg_tys.len() {
                                return Err(
                                    "Function definition with different number of args".to_owned()
                                );
                            }
                            for (fun_arg, def_arg) in fun_arg_tys.iter().zip(def_arg_tys.iter()) {
                                unify(&self.ty_syns, fun_arg, def_arg)?;
                            }
                        }
                    }

                    let rhs_ty = self.ti_rhs(level, ty_vars, &group_assumps, rhs, preds)?;

                    match &fun_rhs_ty {
                        Some(fun_rhs_ty) => {
                            unify(&self.ty_syns, fun_rhs_ty, &rhs_ty)?;
                        }
                        None => {
                            fun_rhs_ty = Some(rhs_ty);
                        }
                    }
                }

                Ok(make_fun_ty(fun_arg_tys.unwrap(), fun_rhs_ty.unwrap()))
            }
        }
    }

    /// Infer type of a pattern. Variables in `fixed_vars` must already be in `assumps`. Rest of
    /// the variables bound by the pattern will be added to `assumps`.
    ///
    /// `fixed_vars` is used to give type to patterns with explicitly typed binders, e.g.
    ///
    /// ```ignore
    /// a :: Int
    /// b :: Bool
    /// (a, b, c) = f x
    /// ```
    //
    // TODO: Check that a variable does not appear multiple times.
    fn ti_pat(
        &mut self,
        level: u32,
        pat: &ast::RenamedPat,
        assumps: &mut TrieMap<Id, Scheme>,
        fixed_vars: &Set<Id>,
        preds: &mut Vec<Pred>,
    ) -> Result<TyRef, String> {
        match &pat.node {
            ast::Pat_::Var(var) => {
                if fixed_vars.contains(var) {
                    let ty = assumps.get(var).unwrap();
                    assert!(
                        ty.is_monomorphic(),
                        "Type of the pattern var \"{}\" is not monomorphic: {}",
                        var,
                        ty
                    );
                    let (preds, instantiated) = ty.instantiate(level);
                    assert!(preds.is_empty());
                    Ok(instantiated)
                } else {
                    let var_ty = TyRef::new_var(Kind::Star, level);
                    assumps.insert_mut(var.clone(), Scheme::monomorphic(var_ty.clone()));
                    Ok(var_ty)
                }
            }

            ast::Pat_::Wildcard => Ok(TyRef::new_var(Kind::Star, level)),

            ast::Pat_::As(var, pat) => {
                let pat_ty = self.ti_pat(level, pat, assumps, fixed_vars, preds)?;
                assumps.insert_mut(var.clone(), Scheme::monomorphic(pat_ty.clone()));
                Ok(pat_ty)
            }

            ast::Pat_::Lit(lit) => Ok(self.ti_lit(level, lit, preds)),

            ast::Pat_::Tuple(pats) => {
                let mut pat_tys = Vec::with_capacity(pats.len());
                for pat in pats {
                    pat_tys.push(self.ti_pat(level, pat, assumps, fixed_vars, preds)?);
                }
                Ok(make_tuple_ty(&pat_tys))
            }

            ast::Pat_::List(pats) => {
                let elem_ty = if pats.is_empty() {
                    TyRef::new_var(Kind::Star, level)
                } else {
                    let mut pat_tys = Vec::with_capacity(pats.len());

                    for pat in pats {
                        pat_tys.push(self.ti_pat(level, pat, assumps, fixed_vars, preds)?);
                    }

                    for pat_idx in 0..pats.len() - 1 {
                        unify(&self.ty_syns, &pat_tys[pat_idx], &pat_tys[pat_idx + 1])?;
                    }

                    pat_tys.into_iter().next().unwrap()
                };

                let list_con = TyRef::new_con(id::list_ty_id());
                Ok(TyRef::new_app(list_con, elem_ty))
            }

            ast::Pat_::Irrefutable(pat) => self.ti_pat(level, pat, assumps, fixed_vars, preds),

            ast::Pat_::Con(con, args) => {
                let data_con_scheme: &Scheme = match &con.node {
                    ast::GCon_::Tuple(_) => todo!(),
                    ast::GCon_::EmptyList => todo!(),
                    ast::GCon_::QCon(id) => self
                        .con_tys
                        .get(id)
                        .unwrap_or_else(|| panic!("Unbound QCon: {:?}", id)),
                };

                let (data_con_preds, data_con_ty) = data_con_scheme.instantiate(level);
                assert_eq!(data_con_preds, vec![]);

                let mut con_arg_tys: Vec<TyRef> = Vec::with_capacity(args.len());
                for arg in args {
                    con_arg_tys.push(self.ti_pat(level, arg, assumps, fixed_vars, preds)?);
                }

                let ret_ty = TyRef::new_var(Kind::Star, level);
                let inferred_ty = make_fun_ty(con_arg_tys, ret_ty.clone());

                unify(&self.ty_syns, &inferred_ty, &data_con_ty)?;

                Ok(ret_ty)
            }
        }
    }

    fn ti_lit(&mut self, level: u32, lit: &Literal, preds: &mut Vec<Pred>) -> TyRef {
        match lit {
            Literal::Int => {
                let ty = TyRef::new_var(Kind::Star, level);
                preds.push(Pred {
                    class: self.num_ty_id(),
                    ty: ty.clone(),
                });
                ty
            }

            Literal::Float => {
                let ty = TyRef::new_var(Kind::Star, level);
                preds.push(Pred {
                    class: self.fractional_ty_id(),
                    ty: ty.clone(),
                });
                ty
            }

            Literal::Char => TyRef::new_con(id::char_ty_id()),

            Literal::String => TyRef::new_con(self.string_ty_id()),
        }
    }

    fn ti_rhs(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &TrieMap<Id, Scheme>,
        rhs: &ast::RenamedRhs,
        preds: &mut Vec<Pred>,
    ) -> Result<TyRef, String> {
        let mut assumps = assumps.clone();

        let where_decls: &[ast::RenamedValueDecl] = rhs.node.where_decls();
        let groups: Vec<BindingGroup> = group_binds(where_decls);
        let sigs: Map<Id, Scheme> = collect_sigs(where_decls, &self.ty_kinds);
        self.ti_groups(level, ty_vars, &mut assumps, &sigs, &groups)?;

        match &rhs.node {
            ast::Rhs_::GuardedRhs {
                rhss,
                where_decls: _, // handled above
            } => self.ti_guarded_rhss(level, ty_vars, &assumps, rhss, preds),

            ast::Rhs_::Rhs {
                rhs,
                where_decls: _, // handled above
            } => self.ti_exp(level, ty_vars, &assumps, rhs, preds),
        }
    }

    fn ti_exp(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &TrieMap<Id, Scheme>,
        exp: &ast::RenamedExp,
        preds: &mut Vec<Pred>,
    ) -> Result<TyRef, String> {
        match &exp.node {
            ast::Exp_::Var(var) | ast::Exp_::Con(var) => {
                let scheme = assumps
                    .get(var)
                    .unwrap_or_else(|| panic!("Unbound variable: {:?}", var));
                let (var_preds, var_ty) = scheme.instantiate(level);
                preds.extend(var_preds);
                Ok(var_ty)
            }

            ast::Exp_::Lit(lit) => Ok(self.ti_lit(level, lit, preds)),

            ast::Exp_::Lam(args, body) => {
                let mut body_assumps = assumps.clone();

                let arg_tys: Vec<TyRef> = args
                    .iter()
                    .flat_map(|arg| {
                        self.ti_pat(
                            level + 1,
                            arg,
                            &mut body_assumps,
                            &Default::default(),
                            preds,
                        )
                    })
                    .collect();

                let ret_ty = self.ti_exp(level + 1, ty_vars, &body_assumps, body, preds)?;

                Ok(make_fun_ty(arg_tys, ret_ty))
            }

            ast::Exp_::App(f, args) => {
                let f_ty = self.ti_exp(level, ty_vars, assumps, f, preds)?;

                let arg_tys: Vec<TyRef> = args
                    .iter()
                    .flat_map(|arg| self.ti_exp(level, ty_vars, assumps, arg, preds))
                    .collect();

                let ret_ty = TyRef::new_var(Kind::Star, level);

                let expected_f_ty = make_fun_ty(arg_tys, ret_ty.clone());

                unify(&self.ty_syns, &f_ty, &expected_f_ty)?;

                Ok(ret_ty)
            }

            ast::Exp_::Tuple(elems) => {
                let mut elem_tys: Vec<TyRef> = Vec::with_capacity(elems.len());
                for elem in elems {
                    elem_tys.push(self.ti_exp(level, ty_vars, assumps, elem, preds)?);
                }
                Ok(make_tuple_ty(&elem_tys))
            }

            ast::Exp_::List(elems) => {
                let elem_tys: Vec<TyRef> = elems
                    .iter()
                    .flat_map(|elem| self.ti_exp(level, ty_vars, assumps, elem, preds))
                    .collect();

                let elem_ty = if elems.is_empty() {
                    TyRef::new_var(Kind::Star, level)
                } else {
                    for elem_idx in 0..elems.len() - 1 {
                        unify(&self.ty_syns, &elem_tys[elem_idx], &elem_tys[elem_idx + 1])?;
                    }
                    elem_tys.into_iter().next().unwrap()
                };

                Ok(make_list_ty(elem_ty))
            }

            ast::Exp_::Do(_) => todo!("Do expression at {}", exp.span),

            ast::Exp_::TypeAnnotation {
                exp: _,
                context: _,
                type_: _,
            } => todo!("Type annotation exprsesion at {}", exp.span),

            ast::Exp_::ArithmeticSeq { exp1, exp2, exp3 } => {
                let exp1_ty = self.ti_exp(level, ty_vars, assumps, exp1, preds)?;
                if let Some(exp2) = exp2 {
                    let exp2_ty = self.ti_exp(level, ty_vars, assumps, exp2, preds)?;
                    unify(&self.ty_syns, &exp1_ty, &exp2_ty)?;
                }
                if let Some(exp3) = exp3 {
                    let exp3_ty = self.ti_exp(level, ty_vars, assumps, exp3, preds)?;
                    unify(&self.ty_syns, &exp1_ty, &exp3_ty)?;
                }
                Ok(make_list_ty(exp1_ty))
            }

            ast::Exp_::ListComp { exp: _, quals: _ } => {
                todo!("List comprehension expression at {}", exp.span)
            }

            ast::Exp_::Case(scrut, alts) => {
                let scrut_ty = self.ti_exp(level, ty_vars, assumps, scrut, preds)?;
                let mut rhs_tys: Vec<TyRef> = Vec::with_capacity(alts.len());

                for alt in alts {
                    let ast::AstNode {
                        node:
                            ast::Alt_ {
                                pat,
                                guarded_rhss,
                                where_decls,
                            },
                        ..
                    } = alt;

                    // Note: `where` binds are in scope of the guards and can refer to pat vars.
                    let mut alt_assumps = assumps.clone();
                    let pat_ty =
                        self.ti_pat(level, pat, &mut alt_assumps, &Default::default(), preds)?;
                    unify(&self.ty_syns, &pat_ty, &scrut_ty)?;

                    let groups: Vec<BindingGroup> = group_binds(where_decls);
                    let sigs: Map<Id, Scheme> = collect_sigs(where_decls, &self.ty_kinds);
                    self.ti_groups(level, ty_vars, &mut alt_assumps, &sigs, &groups)?;

                    for (guards, rhs) in guarded_rhss {
                        self.ti_guard_stmts(level, ty_vars, &mut alt_assumps, guards, preds)?;
                        let rhs_ty = self.ti_exp(level, ty_vars, &alt_assumps, rhs, preds)?;
                        rhs_tys.push(rhs_ty);
                    }
                }

                // TODO: Does it matter which pairs of RHSs we unify?
                for i in 0..rhs_tys.len() - 1 {
                    unify(&self.ty_syns, &rhs_tys[i], &rhs_tys[i + 1])?;
                }

                Ok(rhs_tys.pop().unwrap())
            }

            ast::Exp_::If(e1, e2, e3) => {
                let e1_ty = self.ti_exp(level, ty_vars, assumps, e1, preds)?;
                unify(&self.ty_syns, &e1_ty, &bool())?;

                let e2_ty = self.ti_exp(level, ty_vars, assumps, e2, preds)?;
                let e3_ty = self.ti_exp(level, ty_vars, assumps, e3, preds)?;
                unify(&self.ty_syns, &e2_ty, &e3_ty)?;

                Ok(e2_ty)
            }

            ast::Exp_::Let(decls, body) => {
                let mut body_assumps = assumps.clone();
                let groups: Vec<BindingGroup> = group_binds(decls);
                let sigs: Map<Id, Scheme> = collect_sigs(decls, &self.ty_kinds);
                self.ti_groups(level, ty_vars, &mut body_assumps, &sigs, &groups)?;
                self.ti_exp(level + 1, ty_vars, &body_assumps, body, preds)
            }

            ast::Exp_::Update { exp: _, updates: _ } => {
                todo!("Record update expression at {}", exp.span)
            }

            ast::Exp_::ReAssoc(_, _, _) => {
                // Binary associations should've been handled before type checking.
                panic!("ReAssoc node in type checker")
            }
        }
    }

    fn ti_guarded_rhss(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &TrieMap<Id, Scheme>,
        rhss: &[ast::RenamedGuardedRhs],
        preds: &mut Vec<Pred>,
    ) -> Result<TyRef, String> {
        assert!(!rhss.is_empty());

        // TODO: Instead checking all rhss first we could unify first ty with the second, then
        // third etc. It shouldn't affect the final type but would it make type errors better?
        let mut rhs_tys: Vec<TyRef> = Vec::with_capacity(rhss.len());

        for ast::AstNode {
            node: ast::GuardedRhs_ { guards, rhs },
            ..
        } in rhss
        {
            let mut assumps = assumps.clone();
            self.ti_guard_stmts(level, ty_vars, &mut assumps, guards, preds)?;
            rhs_tys.push(self.ti_exp(level, ty_vars, &assumps, rhs, preds)?);
        }

        for i in 0..rhs_tys.len() - 1 {
            unify(&self.ty_syns, &rhs_tys[i], &rhs_tys[i + 1])?;
        }

        Ok(rhs_tys.pop().unwrap())
    }

    fn ti_guard_stmts(
        &mut self,
        level: u32,
        ty_vars: &TrieMap<Id, TyRef>,
        assumps: &mut TrieMap<Id, Scheme>,
        stmts: &[ast::RenamedStmt],
        preds: &mut Vec<Pred>,
    ) -> Result<(), String> {
        for stmt in stmts {
            match &stmt.node {
                ast::Stmt_::Exp(exp) => {
                    let exp_ty = self.ti_exp(level, ty_vars, assumps, exp, preds)?;
                    unify(&self.ty_syns, &exp_ty, &bool())?;
                }

                ast::Stmt_::Bind(pat, exp) => {
                    // H10 left-hand sides can't contain type annotations, so variables in the
                    // pattern will have fresh types.
                    let pat_ty = self.ti_pat(level, pat, assumps, &Default::default(), preds)?;
                    let exp_ty = self.ti_exp(level, ty_vars, assumps, exp, preds)?;
                    unify(&self.ty_syns, &pat_ty, &exp_ty)?;
                }

                ast::Stmt_::Let(decls) => {
                    let groups: Vec<BindingGroup> = group_binds(decls);
                    let sigs: Map<Id, Scheme> = collect_sigs(decls, &self.ty_kinds);
                    self.ti_groups(level, ty_vars, assumps, &sigs, &groups)?;
                }
            }
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Generalization
//
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Default)]
struct GeneralizeState {
    kinds: Vec<Kind>,
    tys: Map<TyVarRef, TyRef>,
}

impl GeneralizeState {
    fn generalize(&mut self, level: u32, preds: &[Pred], ty: &TyRef) -> (Vec<Pred>, TyRef) {
        let mut preds_ = Vec::with_capacity(preds.len());
        for pred in preds {
            preds_.push(Pred {
                class: pred.class.clone(),
                ty: self.generalize_ty(level, &pred.ty),
            });
        }

        let ty_ = self.generalize_ty(level, ty);

        (preds_, ty_)
    }

    fn generalize_ty(&mut self, level: u32, ty: &TyRef) -> TyRef {
        let ty = ty.normalize();
        match ty.deref() {
            Ty::Var(var_ref) => {
                let kinds = &mut self.kinds;
                let tys = &mut self.tys;
                if var_ref.level() >= level {
                    match tys.entry(var_ref.clone()) {
                        Entry::Occupied(entry) => entry.get().clone(),
                        Entry::Vacant(entry) => {
                            let idx = kinds.len() as u32;
                            kinds.push(var_ref.kind().clone());
                            entry.insert(TyRef::new_gen(idx)).clone()
                        }
                    }
                } else {
                    ty.clone()
                }
            }

            Ty::Con(_) => ty.clone(),

            Ty::App(ty1, ty2) => {
                let ty1_ = self.generalize_ty(level, ty1);
                let ty2_ = self.generalize_ty(level, ty2);
                if ty1 == &ty1_ && ty2 == &ty2_ {
                    ty.clone()
                } else {
                    TyRef::new_app(ty1_, ty2_)
                }
            }

            Ty::Gen(_) => panic!("Gen in generalize_ty"),
        }
    }
}

fn generalize(level: u32, preds: &[Pred], ty: &TyRef) -> Scheme {
    let mut state = GeneralizeState::default();
    let (preds, ty) = state.generalize(level, preds, ty);
    Scheme {
        kinds: state.kinds,
        preds,
        ty,
        span: None,
    }
}

fn collect_top_sigs(decls: &[ast::RenamedDecl], ty_kinds: &Map<Id, Kind>) -> Map<Id, Scheme> {
    let mut sigs: Map<Id, Scheme> = Default::default();
    for decl in decls {
        if let ast::Decl_::Value(value_decl) = &decl.node {
            collect_sig(value_decl, ty_kinds, &mut sigs);
        }
    }
    sigs
}

fn collect_sigs(decls: &[ast::RenamedValueDecl], ty_kinds: &Map<Id, Kind>) -> Map<Id, Scheme> {
    let mut sigs: Map<Id, Scheme> = Default::default();
    for decl in decls {
        collect_sig(decl, ty_kinds, &mut sigs);
    }
    sigs
}

fn collect_sig(decl: &ast::RenamedValueDecl, ty_kinds: &Map<Id, Kind>, sigs: &mut Map<Id, Scheme>) {
    match &decl.node {
        ast::ValueDecl_::TypeSig { vars, context, ty } => {
            for var in vars {
                let scheme = Scheme::from_type_sig(
                    decl.span.clone(),
                    ty_kinds,
                    &Default::default(),
                    context,
                    ty,
                );
                let old_sig = sigs.insert(var.clone(), scheme);
                if old_sig.is_some() {
                    panic!("{} defined more than once at {}", var, decl.span);
                }
            }
        }

        ast::ValueDecl_::Fixity { .. } | ast::ValueDecl_::Value { .. } => {}
    }
}

// TODO: Use a shared (->) type
pub fn make_fun_ty(args: Vec<TyRef>, mut ret: TyRef) -> TyRef {
    let arrow_ty_con = TyRef::new_con(id::arrow_ty_id());
    for arg in args.into_iter().rev() {
        let app_arg = TyRef::new_app(arrow_ty_con.clone(), arg);
        ret = TyRef::new_app(app_arg, ret);
    }
    ret
}

// TODO: Use a shared [] type
pub fn make_list_ty(arg: TyRef) -> TyRef {
    let list_ty_con = TyRef::new_con(id::list_ty_id());
    TyRef::new_app(list_ty_con, arg)
}

// TODO: Use a shared type
fn bool() -> TyRef {
    TyRef::new_con(id::bool_ty_id())
}

pub fn make_tuple_ty(args: &[TyRef]) -> TyRef {
    let con = id::tuple_ty_id(args.len() as u32);
    args.iter()
        .fold(TyRef::new_con(con), |a, b| TyRef::new_app(a, b.clone()))
}
