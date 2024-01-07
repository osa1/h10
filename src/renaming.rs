use crate::ast;
use crate::collections::Map;
use crate::id::{self, Id, IdKind};

use std::borrow::Borrow;
use std::hash::Hash;

/// Rename a single module.
///
/// Note: for now we don't allow imports and rename a whole program as just one module. The only
/// built-ins the renamed module gets is the Haskell 2010 built-ins that can't be implemented in
/// Haskell 2010, such as the list type, and `Int` and `Float` types.
pub fn rename_module(module: &[ast::ParsedDecl]) -> Vec<ast::RenamedDecl> {
    let mut renamer = Renamer::new();
    renamer.rename_module(module)
}

#[derive(Debug)]
pub(crate) struct Renamer {
    values: Scope<String, Id>,
    tys: Scope<String, Id>,

    /// Scopes with `Id`s defined in them, in the order of definition.
    ///
    /// When we see a value definition for the first time (in the "bind" pass) we will either have
    /// an `Id` for it in `values` (when we see a type signature for it before the definition) or
    /// we'll create a new `Id`. In both cases, the `Id` for the definition is pushed to the
    /// current scope's vector. Adding more definition to the current scope's last `Id` is allowed.
    /// If we see a definition of an `Id` in the last scope but the `Id` is not the last definition
    /// in the scope, it's a re-definition, which is an error.
    defined_values: Vec<Vec<Id>>,
}

impl Renamer {
    pub(crate) fn new() -> Self {
        let mut renamer = Renamer {
            values: Default::default(),
            tys: Default::default(),
            defined_values: vec![vec![]],
        };

        renamer.bind_builtin_type("Int", id::int_ty_id());
        renamer.bind_builtin_type("Char", id::char_ty_id());
        renamer.bind_builtin_type("Bool", id::bool_ty_id());
        renamer.bind_builtin_type("Integer", id::integer_ty_id());
        renamer.bind_builtin_type("[]", id::list_ty_id());
        renamer.bind_builtin_type("Type", id::type_ty_id());

        renamer.bind_builtin_value(":", id::cons_id());
        renamer.bind_builtin_value("[]", id::nil_id());

        renamer
    }

    fn new_value_scope(&mut self) {
        debug_assert_eq!(self.values.len_scopes(), self.defined_values.len());
        self.values.enter();
        self.defined_values.push(vec![]);
    }

    fn exit_value_scope(&mut self) {
        debug_assert_eq!(self.values.len_scopes(), self.defined_values.len());
        self.values.exit();
        self.defined_values.pop().unwrap();
    }

    fn bind_builtin_type(&mut self, ty: &str, id: Id) {
        self.tys.bind(ty.to_owned(), id);
    }

    fn bind_builtin_value(&mut self, ty: &str, id: Id) {
        self.values.bind(ty.to_owned(), id);
    }

    pub(crate) fn rename_module(&mut self, module: &[ast::ParsedDecl]) -> Vec<ast::RenamedDecl> {
        // TODO: Bind imported stuff here
        for decl in module {
            self.bind_top_decl(decl);
        }

        module.iter().map(|decl| self.rename_decl(decl)).collect()
    }

    fn bind_fresh_type_var(&mut self, var: &str) -> Id {
        let id = self.fresh_id(var, IdKind::TyVar);
        self.tys.bind(var.to_owned(), id.clone());
        id
    }

    fn bind_fresh_var(&mut self, var: &str) -> Id {
        if let Some(id) = self.values.get_current_scope(var) {
            // If we've seen a definition of `var`, it should be the last definition we've seen.
            let mut last = true;
            for defined_id in self.defined_values.last().unwrap().iter().rev() {
                if defined_id == id {
                    if last {
                        return id.clone();
                    } else {
                        panic!("{} defined multiple times", var);
                    }
                }
                last = false;
            }
            self.defined_values.last_mut().unwrap().push(id.clone());
            return id.clone();
        }

        let id = self.fresh_id(var, IdKind::Term);
        self.values.bind(var.to_owned(), id.clone());
        self.defined_values.last_mut().unwrap().push(id.clone());
        id
    }

    fn fresh_id(&mut self, name: &str, kind: IdKind) -> Id {
        Id::new(Some(name.to_owned()), kind)
    }

    /// Bind names defined in top-level declarations.
    fn bind_top_decl(&mut self, decl: &ast::ParsedDecl) {
        match &decl.node {
            ast::Decl_::Value(decl) => self.bind_value_decl(decl),
            ast::Decl_::Type(decl) => self.bind_type_decl(decl),
            ast::Decl_::KindSig(decl) => self.bind_kind_sig_decl(decl),
            ast::Decl_::Data(decl) => self.bind_data_decl(decl),
            ast::Decl_::Newtype(decl) => self.bind_newtype_decl(decl),
            ast::Decl_::Class(decl) => self.bind_class_decl(decl),
            ast::Decl_::Instance(_) => {}
            ast::Decl_::Default(_) => {}
        }
    }

    /// Bind names defined in a top-level value declaration.
    fn bind_value_decl(&mut self, decl: &ast::ParsedValueDecl) {
        match &decl.node {
            ast::ValueDecl_::Value { lhs, .. } => self.bind_lhs(lhs),

            ast::ValueDecl_::TypeSig { vars, .. } => {
                // TODO: Check that we haven't seen signature for the same var in the same scope.
                for var in vars {
                    if self.values.get_current_scope(var).is_none() {
                        let id = self.fresh_id(var, IdKind::Term);
                        self.values.bind(var.to_owned(), id.clone());
                    }
                }
            }

            ast::ValueDecl_::Fixity { ops, .. } => {
                for op in ops {
                    self.bind_op(op);
                }
            }
        }
    }

    fn bind_lhs(&mut self, lhs: &ast::ParsedLhs) {
        match &lhs.node {
            ast::Lhs_::Pat(pat) => self.bind_pat(pat),
            ast::Lhs_::Fun { var, .. } => {
                self.bind_fresh_var(var);
            }
        }
    }

    fn bind_pat(&mut self, pat: &ast::ParsedPat) {
        match &pat.node {
            ast::Pat_::Var(var) => {
                self.bind_fresh_var(var);
            }

            ast::Pat_::As(var, pat) => {
                self.bind_fresh_var(var);
                self.bind_pat(pat);
            }

            ast::Pat_::Lit(_) | ast::Pat_::Wildcard => {}

            ast::Pat_::Tuple(pats) | ast::Pat_::Con(_, pats) | ast::Pat_::List(pats) => {
                for pat in pats {
                    self.bind_pat(pat);
                }
            }

            ast::Pat_::Irrefutable(pat) => self.bind_pat(pat),
        }
    }

    fn bind_op(&mut self, op: &ast::ParsedOp) {
        match &op.node {
            ast::Op_::Con(var) | ast::Op_::Var(var) => {
                self.bind_fresh_var(var);
            }
        }
    }

    fn bind_type(&mut self, ty: &str) {
        if self.tys.get(ty).is_none() {
            let id = self.fresh_id(ty, IdKind::TyVar);
            self.tys.bind(ty.to_owned(), id);
        }
    }

    fn bind_kind_sig_decl(&mut self, decl: &ast::ParsedKindSigDecl) {
        self.bind_type(&decl.node.ty);
    }

    fn bind_type_decl(&mut self, decl: &ast::ParsedTypeDecl) {
        self.bind_type(&decl.node.ty);
    }

    fn bind_data_decl(&mut self, decl: &ast::ParsedDataDecl) {
        self.bind_type(&decl.node.ty_con);
        for con in &decl.node.cons {
            self.bind_con(con);
        }
    }

    fn bind_newtype_decl(&mut self, decl: &ast::ParsedNewtypeDecl) {
        self.bind_type(&decl.node.ty_con);
        self.bind_con(&decl.node.con);
    }

    fn bind_con(&mut self, con: &ast::ParsedCon) {
        self.bind_fresh_var(&con.node.con);
        for field in &con.node.fields {
            for var in &field.node.vars {
                self.bind_fresh_var(var);
            }
        }
    }

    fn bind_class_decl(&mut self, decl: &ast::ParsedClassDecl) {
        self.bind_type(&decl.node.ty_con);
        for decl in &decl.node.decls {
            self.bind_value_decl(decl);
        }
    }

    fn rename_decl(&mut self, decl: &ast::ParsedDecl) -> ast::RenamedDecl {
        decl.map(|decl| match decl {
            ast::Decl_::Value(decl) => ast::Decl_::Value(self.rename_value_decl(decl)),
            ast::Decl_::Type(decl) => ast::Decl_::Type(self.rename_type_decl(decl)),
            ast::Decl_::KindSig(decl) => ast::Decl_::KindSig(self.rename_kind_sig_decl(decl)),
            ast::Decl_::Data(decl) => ast::Decl_::Data(self.rename_data_decl(decl)),
            ast::Decl_::Newtype(decl) => ast::Decl_::Newtype(self.rename_newtype_decl(decl)),
            ast::Decl_::Class(decl) => ast::Decl_::Class(self.rename_class_decl(decl)),
            ast::Decl_::Instance(decl) => ast::Decl_::Instance(self.rename_instance_decl(decl)),
            ast::Decl_::Default(decl) => ast::Decl_::Default(self.rename_default_decl(decl)),
        })
    }

    fn rename_value_decl(&mut self, decl: &ast::ParsedValueDecl) -> ast::RenamedValueDecl {
        match &decl.node {
            ast::ValueDecl_::TypeSig {
                vars,
                foralls,
                context,
                ty,
            } => {
                let vars: Vec<Id> = vars.iter().map(|var| self.rename_var(var)).collect();

                // `context` and `ty` may use variables before declaring
                self.tys.enter();

                let foralls: Vec<Id> = foralls.iter().map(|var| self.bind_type_var(var)).collect();

                // When the type has a scoped type variables all type variables should be listed.
                let allow_binding = foralls.is_empty();

                let context: Vec<ast::RenamedType> = context
                    .iter()
                    .map(|ty| self.rename_type(ty, allow_binding))
                    .collect();

                let ty = self.rename_type(ty, allow_binding);

                self.tys.exit();

                decl.with_node(ast::ValueDecl_::TypeSig {
                    vars,
                    foralls,
                    context,
                    ty,
                })
            }

            ast::ValueDecl_::Fixity { fixity, prec, ops } => {
                decl.with_node(ast::ValueDecl_::Fixity {
                    fixity: *fixity,
                    prec: *prec,
                    ops: ops.iter().map(|op| self.rename_op(op)).collect(),
                })
            }

            ast::ValueDecl_::Value { lhs, rhs } => {
                self.new_value_scope();
                let lhs = self.rename_lhs(lhs);
                let rhs = self.rename_rhs(rhs);
                self.exit_value_scope();
                decl.with_node(ast::ValueDecl_::Value { lhs, rhs })
            }
        }
    }

    fn rename_kind_sig_decl(&mut self, decl: &ast::ParsedKindSigDecl) -> ast::RenamedKindSigDecl {
        let ast::KindSigDecl_ { ty, foralls, sig } = &decl.node;
        let ty = self.rename_type_var(ty);
        self.tys.enter();
        let foralls = foralls
            .iter()
            .map(|id| self.bind_fresh_type_var(id))
            .collect();
        let sig = self.rename_type(sig, true);
        self.tys.exit();
        decl.with_node(ast::KindSigDecl_ { ty, foralls, sig })
    }

    fn rename_type_decl(&mut self, decl: &ast::ParsedTypeDecl) -> ast::RenamedTypeDecl {
        let ast::TypeDecl_ { ty, vars, rhs } = &decl.node;
        let ty = self.rename_type_var(ty);
        self.tys.enter();
        let vars = vars
            .iter()
            .map(|var| self.bind_fresh_type_var(var))
            .collect();
        let rhs = self.rename_type(rhs, true);
        self.tys.exit();
        decl.with_node(ast::TypeDecl_ { ty, vars, rhs })
    }

    fn rename_data_decl(&mut self, decl: &ast::ParsedDataDecl) -> ast::RenamedDataDecl {
        let ast::DataDecl_ {
            context,
            ty_con,
            ty_args,
            cons,
            deriving,
        } = &decl.node;
        let ty_con = self.rename_type_var(ty_con);
        self.tys.enter();
        let context = context
            .iter()
            .map(|ctx| self.rename_type(ctx, true))
            .collect();
        let ty_args = ty_args
            .iter()
            .map(|var| self.bind_fresh_type_var(var))
            .collect();
        let cons = cons.iter().map(|con| self.rename_con(con)).collect();
        self.tys.exit();
        let deriving = deriving.iter().map(|id| self.rename_type_var(id)).collect();
        decl.with_node(ast::DataDecl_ {
            context,
            ty_con,
            ty_args,
            cons,
            deriving,
        })
    }

    fn rename_con(&mut self, con: &ast::ParsedCon) -> ast::RenamedCon {
        let ast::Con_ { con: con_, fields } = &con.node;
        let con_ = self.rename_var(con_);
        let fields = fields
            .iter()
            .map(|field| self.rename_field(field))
            .collect();
        con.with_node(ast::Con_ { con: con_, fields })
    }

    fn rename_field(&mut self, field: &ast::ParsedFieldDecl) -> ast::RenamedFieldDecl {
        let ast::FieldDecl_ { vars, ty } = &field.node;
        let vars = vars.iter().map(|var| self.rename_var(var)).collect();
        let ty = self.rename_type(ty, true);
        field.with_node(ast::FieldDecl_ { vars, ty })
    }

    fn rename_newtype_decl(&mut self, decl: &ast::ParsedNewtypeDecl) -> ast::RenamedNewtypeDecl {
        let ast::NewtypeDecl_ {
            context,
            ty_con,
            ty_args,
            con,
        } = &decl.node;
        let ty_con = self.rename_type_var(ty_con);
        self.tys.enter();
        let context = context
            .iter()
            .map(|ctx| self.rename_type(ctx, true))
            .collect();
        for ty_arg in ty_args {
            self.bind_fresh_type_var(ty_arg);
        }
        let ty_args = ty_args
            .iter()
            .map(|arg| self.rename_type_var(arg))
            .collect();
        let con = self.rename_con(con);
        self.tys.exit();
        decl.with_node(ast::NewtypeDecl_ {
            context,
            ty_con,
            ty_args,
            con,
        })
    }

    fn rename_class_decl(&mut self, decl: &ast::ParsedClassDecl) -> ast::RenamedClassDecl {
        let ast::ClassDecl_ {
            context,
            ty_con,
            ty_arg,
            decls,
        } = &decl.node;
        let ty_con = self.rename_type_var(ty_con);
        self.tys.enter();
        // Context will refer to the type variable, so bind the type variable first.
        self.bind_fresh_type_var(ty_arg);
        let context = context
            .iter()
            .map(|ty| self.rename_type(ty, true))
            .collect();
        let ty_arg = self.rename_type_var(ty_arg);
        let decls = decls
            .iter()
            .map(|decl| self.rename_value_decl(decl))
            .collect();
        self.tys.exit();
        decl.with_node(ast::ClassDecl_ {
            context,
            ty_con,
            ty_arg,
            decls,
        })
    }

    fn rename_instance_decl(&mut self, decl: &ast::ParsedInstanceDecl) -> ast::RenamedInstanceDecl {
        let ast::InstanceDecl_ {
            context,
            ty_con,
            ty,
            decls,
        } = &decl.node;
        let ty_con = self.rename_type_var(ty_con);
        self.tys.enter();
        let context = context
            .iter()
            .map(|ty| self.rename_type(ty, true))
            .collect();
        let ty = self.rename_type(ty, true);

        // Instance methods are top-level, but we create a new scope to be able to able to override
        // class method ids while renaming instance declarations.
        self.new_value_scope();
        for decl in decls {
            self.bind_value_decl(decl);
        }

        let decls = decls
            .iter()
            .map(|decl| self.rename_value_decl(decl))
            .collect();

        self.exit_value_scope();

        self.tys.exit();
        decl.with_node(ast::InstanceDecl_ {
            context,
            ty_con,
            ty,
            decls,
        })
    }

    fn rename_default_decl(&mut self, decl: &ast::ParsedDefaultDecl) -> ast::RenamedDefaultDecl {
        let ast::DefaultDecl_ { tys } = &decl.node;
        decl.with_node(ast::DefaultDecl_ {
            tys: tys.iter().map(|ty| self.rename_type(ty, true)).collect(),
        })
    }

    fn rename_lhs(&mut self, lhs: &ast::ParsedLhs) -> ast::RenamedLhs {
        match &lhs.node {
            ast::Lhs_::Pat(pat) => {
                // pat variables already bound
                lhs.with_node(ast::Lhs_::Pat(self.rename_pat(pat)))
            }

            ast::Lhs_::Fun { var, pats } => {
                let var = self.rename_var(var);
                for pat in pats {
                    self.bind_pat(pat);
                }
                let pats = pats.iter().map(|pat| self.rename_pat(pat)).collect();
                lhs.with_node(ast::Lhs_::Fun { var, pats })
            }
        }
    }

    fn rename_pat(&mut self, pat: &ast::ParsedPat) -> ast::RenamedPat {
        pat.map(|pat| match pat {
            ast::Pat_::Var(var) => ast::Pat_::Var(self.rename_var(var)),

            ast::Pat_::As(var, pat) => {
                ast::Pat_::As(self.rename_var(var), Box::new(self.rename_pat(pat)))
            }

            ast::Pat_::Lit(lit) => ast::Pat_::Lit(lit.clone()),

            ast::Pat_::Wildcard => ast::Pat_::Wildcard,

            ast::Pat_::Tuple(pats) => {
                ast::Pat_::Tuple(pats.iter().map(|pat| self.rename_pat(pat)).collect())
            }

            ast::Pat_::List(pats) => {
                ast::Pat_::List(pats.iter().map(|pat| self.rename_pat(pat)).collect())
            }

            ast::Pat_::Irrefutable(pat) => ast::Pat_::Irrefutable(Box::new(self.rename_pat(pat))),

            ast::Pat_::Con(con, pats) => ast::Pat_::Con(
                self.rename_gcon(con),
                pats.iter().map(|pat| self.rename_pat(pat)).collect(),
            ),
        })
    }

    fn rename_gcon(&mut self, gcon: &ast::ParsedGCon) -> ast::RenamedGCon {
        gcon.map(|gcon| match gcon {
            ast::GCon_::Tuple(arity) => ast::GCon_::Tuple(*arity),
            ast::GCon_::EmptyList => ast::GCon_::EmptyList,
            ast::GCon_::QCon(var) => ast::GCon_::QCon(self.rename_var(var)),
        })
    }

    fn rename_rhs(&mut self, rhs: &ast::ParsedRhs) -> ast::RenamedRhs {
        rhs.map(|rhs| match rhs {
            ast::Rhs_::GuardedRhs { rhss, where_decls } => {
                self.new_value_scope();

                for decl in where_decls {
                    self.bind_value_decl(decl);
                }

                let where_decls = where_decls
                    .iter()
                    .map(|decl| self.rename_value_decl(decl))
                    .collect();

                let rhss = rhss
                    .iter()
                    .map(|rhs| self.rename_guarded_rhs(rhs))
                    .collect();

                self.exit_value_scope();

                ast::Rhs_::GuardedRhs { rhss, where_decls }
            }

            ast::Rhs_::Rhs { rhs, where_decls } => {
                self.new_value_scope();

                for decl in where_decls {
                    self.bind_value_decl(decl);
                }

                let where_decls = where_decls
                    .iter()
                    .map(|decl| self.rename_value_decl(decl))
                    .collect();

                let rhs = self.rename_exp(rhs);

                self.exit_value_scope();

                ast::Rhs_::Rhs { rhs, where_decls }
            }
        })
    }

    fn rename_guarded_rhs(
        &mut self,
        guarded_rhs: &ast::ParsedGuardedRhs,
    ) -> ast::RenamedGuardedRhs {
        guarded_rhs.map(|ast::GuardedRhs_ { guards, rhs }| {
            self.new_value_scope();
            let guards = self.bind_and_rename_stmts(guards);
            let rhs = self.rename_exp(rhs);
            self.exit_value_scope();
            ast::GuardedRhs_ { guards, rhs }
        })
    }

    fn rename_exp(&mut self, exp: &ast::ParsedExp) -> ast::RenamedExp {
        exp.map(|exp| {
            match exp {
                ast::Exp_::Var(var) => ast::Exp_::Var(self.rename_var(var)),

                ast::Exp_::Con(var) => ast::Exp_::Con(self.rename_var(var)),

                ast::Exp_::Lit(lit) => ast::Exp_::Lit(lit.clone()),

                ast::Exp_::Lam(pats, exp) => {
                    self.new_value_scope();
                    for pat in pats {
                        self.bind_pat(pat);
                    }
                    let pats = pats.iter().map(|pat| self.rename_pat(pat)).collect();
                    let exp = self.rename_exp(exp);
                    self.exit_value_scope();
                    ast::Exp_::Lam(pats, Box::new(exp))
                }

                ast::Exp_::App(e1, es) => ast::Exp_::App(
                    Box::new(self.rename_exp(e1)),
                    es.iter().map(|exp| self.rename_exp(exp)).collect(),
                ),

                ast::Exp_::Tuple(es) => {
                    ast::Exp_::Tuple(es.iter().map(|e| self.rename_exp(e)).collect())
                }

                ast::Exp_::List(es) => {
                    ast::Exp_::List(es.iter().map(|e| self.rename_exp(e)).collect())
                }

                ast::Exp_::Do(stmts) => {
                    self.new_value_scope();
                    let stmts = self.bind_and_rename_stmts(stmts);
                    self.exit_value_scope();
                    ast::Exp_::Do(stmts)
                }

                // TODO: context can introduce variables, type cannot
                ast::Exp_::TypeAnnotation {
                    exp,
                    context,
                    type_,
                } => ast::Exp_::TypeAnnotation {
                    exp: Box::new(self.rename_exp(exp)),
                    context: context
                        .iter()
                        .map(|ty| self.rename_type(ty, true))
                        .collect(),
                    type_: self.rename_type(type_, true),
                },

                ast::Exp_::ArithmeticSeq { exp1, exp2, exp3 } => ast::Exp_::ArithmeticSeq {
                    exp1: Box::new(self.rename_exp(exp1)),
                    exp2: exp2.as_ref().map(|exp| Box::new(self.rename_exp(exp))),
                    exp3: exp3.as_ref().map(|exp| Box::new(self.rename_exp(exp))),
                },

                ast::Exp_::ListComp { exp, quals } => {
                    self.new_value_scope();
                    let quals = self.bind_and_rename_stmts(quals);
                    let exp = self.rename_exp(exp);
                    self.exit_value_scope();
                    ast::Exp_::ListComp {
                        exp: Box::new(exp),
                        quals,
                    }
                }

                ast::Exp_::Case(exp, alts) => ast::Exp_::Case(
                    Box::new(self.rename_exp(exp)),
                    alts.iter().map(|alt| self.rename_alt(alt)).collect(),
                ),

                ast::Exp_::If(e1, e2, e3) => ast::Exp_::If(
                    Box::new(self.rename_exp(e1)),
                    Box::new(self.rename_exp(e2)),
                    Box::new(self.rename_exp(e3)),
                ),

                ast::Exp_::Let(decls, exp) => {
                    self.new_value_scope();
                    for decl in decls {
                        self.bind_value_decl(decl);
                    }
                    let decls = decls
                        .iter()
                        .map(|decl| self.rename_value_decl(decl))
                        .collect();
                    let exp = self.rename_exp(exp);
                    self.exit_value_scope();
                    ast::Exp_::Let(decls, Box::new(exp))
                }

                ast::Exp_::Update { exp, updates } => ast::Exp_::Update {
                    exp: Box::new(self.rename_exp(exp)),
                    updates: updates
                        .iter()
                        .map(|(id, exp)| (self.rename_var(id), self.rename_exp(exp)))
                        .collect(),
                },

                ast::Exp_::ReAssoc(_, _, _) => todo!("ReAssoc renaming"),
            }
        })
    }

    fn rename_alt(&mut self, alt: &ast::ParsedAlt) -> ast::RenamedAlt {
        alt.map(
            |ast::Alt_ {
                 pat,
                 guarded_rhss,
                 where_decls,
             }| {
                self.new_value_scope();

                self.bind_pat(pat);

                let pat = self.rename_pat(pat);
                for decl in where_decls {
                    self.bind_value_decl(decl);
                }

                let where_decls = where_decls
                    .iter()
                    .map(|decl| self.rename_value_decl(decl))
                    .collect();

                let guarded_rhss = guarded_rhss
                    .iter()
                    .map(|(stmts, exp)| {
                        self.new_value_scope();
                        let stmts = stmts
                            .iter()
                            .map(|stmt| self.bind_and_rename_stmt(stmt))
                            .collect();
                        let exp = self.rename_exp(exp);
                        (stmts, exp)
                    })
                    .collect();

                self.values.exit();
                ast::Alt_ {
                    pat,
                    guarded_rhss,
                    where_decls,
                }
            },
        )
    }

    fn bind_and_rename_stmts(&mut self, stmts: &[ast::ParsedStmt]) -> Vec<ast::RenamedStmt> {
        stmts
            .iter()
            .map(|stmt| self.bind_and_rename_stmt(stmt))
            .collect()
    }

    fn bind_and_rename_stmt(&mut self, stmt: &ast::ParsedStmt) -> ast::RenamedStmt {
        stmt.map(|stmt| match stmt {
            ast::Stmt_::Exp(exp) => ast::Stmt_::Exp(self.rename_exp(exp)),

            ast::Stmt_::Bind(pat, exp) => {
                self.bind_pat(pat);
                ast::Stmt_::Bind(self.rename_pat(pat), self.rename_exp(exp))
            }

            ast::Stmt_::Let(decls) => {
                for decl in decls {
                    self.bind_value_decl(decl);
                }
                ast::Stmt_::Let(
                    decls
                        .iter()
                        .map(|decl| self.rename_value_decl(decl))
                        .collect(),
                )
            }
        })
    }

    // NB. Types can use varibles without declaring them first
    pub(crate) fn rename_type(
        &mut self,
        ty: &ast::ParsedType,
        allow_binding: bool,
    ) -> ast::RenamedType {
        ty.map(|ty| match ty {
            ast::Type_::Tuple(tys) => ast::Type_::Tuple(
                tys.iter()
                    .map(|ty| self.rename_type(ty, allow_binding))
                    .collect(),
            ),

            ast::Type_::List(ty) => ast::Type_::List(Box::new(self.rename_type(ty, allow_binding))),

            ast::Type_::Arrow(ty1, ty2) => ast::Type_::Arrow(
                Box::new(self.rename_type(ty1, allow_binding)),
                Box::new(self.rename_type(ty2, allow_binding)),
            ),

            ast::Type_::App(ty, tys) => ast::Type_::App(
                Box::new(self.rename_type(ty, allow_binding)),
                tys.iter()
                    .map(|ty| self.rename_type(ty, allow_binding))
                    .collect(),
            ),

            ast::Type_::Con(con) => ast::Type_::Con(self.rename_tycon(con)),

            ast::Type_::Var(var) => {
                ast::Type_::Var(self.rename_or_bind_type_var(var, allow_binding))
            }
        })
    }

    fn rename_tycon(&mut self, tycon: &ast::ParsedTyCon) -> ast::RenamedTyCon {
        tycon.map(|tycon| match tycon {
            ast::TyCon_::Id(var) => match self.tys.get(var) {
                Some(id) => ast::TyCon_::Id(id.clone()),
                None => panic!("Unbound type constructor: {:?}", var),
            },
            ast::TyCon_::Tuple(arity) => ast::TyCon_::Tuple(*arity),
            ast::TyCon_::Arrow => ast::TyCon_::Arrow,
            ast::TyCon_::List => ast::TyCon_::List,
        })
    }

    fn rename_op(&mut self, op: &ast::ParsedOp) -> ast::RenamedOp {
        op.map(|op| match op {
            ast::Op_::Con(var) => ast::Op_::Con(self.rename_var(var)),
            ast::Op_::Var(var) => ast::Op_::Var(self.rename_var(var)),
        })
    }

    fn rename_var(&mut self, var: &str) -> Id {
        self.values
            .get(var)
            .unwrap_or_else(|| panic!("Unbound variable: {}", var))
            .clone()
    }

    fn rename_or_bind_type_var(&mut self, var: &str, allow_binding: bool) -> Id {
        match self.tys.get(var) {
            Some(id) => id.clone(),
            None => {
                if allow_binding {
                    self.bind_type_var(var)
                } else {
                    panic!("Unbound type varible: {}", var)
                }
            }
        }
    }

    fn bind_type_var(&mut self, var: &str) -> Id {
        let id = self.fresh_id(var, IdKind::TyVar);
        self.tys.bind(var.to_owned(), id.clone());
        id
    }

    fn rename_type_var(&mut self, var: &str) -> Id {
        self.tys
            .get(var)
            .unwrap_or_else(|| panic!("Unbound type variable: {}, env: {:?}", var, self.tys))
            .clone()
    }
}

#[derive(Debug)]
struct Scope<K, V>(Vec<Map<K, V>>);

impl<K, V> Default for Scope<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> Scope<K, V> {
    fn new() -> Self {
        Scope(vec![Default::default()])
    }

    fn len_scopes(&self) -> usize {
        self.0.len()
    }

    fn exit(&mut self) {
        debug_assert!(!self.0.is_empty());
        self.0.pop();
    }

    fn enter(&mut self) {
        self.0.push(Default::default());
    }
}

impl<K: Hash + Eq, V> Scope<K, V> {
    /// Bind at the current scope. If the mapped thing is already mapped in the *current scope*
    /// (not in a parent scope!), returns the old value for the thing. The return value can be used
    /// to check duplicate definitions.
    fn bind(&mut self, k: K, v: V) -> Option<V> {
        self.0.last_mut().unwrap().insert(k, v)
    }

    fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        for map in self.0.iter().rev() {
            if let Some(val) = map.get(k) {
                return Some(val);
            }
        }
        None
    }

    fn get_current_scope<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.last().unwrap().get(k)
    }
}

#[test]
fn simple_1() {
    let pgm = r#"
data Bool = False | True

toggle True = False
toggle False = True
"#;

    let parsed = crate::parser::parse_module(pgm).unwrap();
    let renamed = rename_module(&parsed);

    let value1_id = renamed[1].value().value().0.fun().0;
    let value2_id = renamed[2].value().value().0.fun().0;
    assert_eq!(value1_id, value2_id);
}

#[test]
fn simple_2() {
    let pgm = r#"
data Bool = False | True

toggle :: Bool -> Bool
toggle True = False
toggle False = True
"#;

    let parsed = crate::parser::parse_module(pgm).unwrap();
    let renamed = rename_module(&parsed);

    let ty_sig_id = &renamed[1].value().type_sig().0[0];
    let value1_id = renamed[2].value().value().0.fun().0;
    let value2_id = renamed[3].value().value().0.fun().0;
    assert_eq!(ty_sig_id, value1_id);
    assert_eq!(value1_id, value2_id);
}

#[test]
fn simple_3() {
    let pgm = r#"
data Bool = False | True

toggle True = False
toggle False = True

toggle :: Bool -> Bool
"#;

    let parsed = crate::parser::parse_module(pgm).unwrap();
    let renamed = rename_module(&parsed);

    let ty_sig_id = &renamed[3].value().type_sig().0[0];
    let value1_id = renamed[1].value().value().0.fun().0;
    let value2_id = renamed[2].value().value().0.fun().0;
    assert_eq!(ty_sig_id, value1_id);
    assert_eq!(value1_id, value2_id);
}

#[test]
fn instance_method() {
    // Check that class and instance methods get different ids, use sites use the id of the class
    // method.

    let pgm = r#"
class Toggle a where
  toggle :: a -> a

data Bool = True | False

instance Toggle Bool where
  toggle True = False
  toggle False = True

f = toggle
"#;

    let parsed = crate::parser::parse_module(pgm).unwrap();
    let renamed = rename_module(&parsed);

    let class_method_id = renamed[0].class().node.decls[0].type_sig().0[0].clone();

    let instance_decl = renamed[2].instance();

    fn get_fun_lhs_var(decl: &ast::RenamedValueDecl) -> Id {
        decl.value().0.fun().0.clone()
    }

    let instance_method_id1 = get_fun_lhs_var(&instance_decl.node.decls[0]);
    let instance_method_id2 = get_fun_lhs_var(&instance_decl.node.decls[1]);
    assert_eq!(instance_method_id1, instance_method_id2);
    assert_ne!(instance_method_id1, class_method_id);

    let use_var = renamed[3].value().value().1.rhs().0.var();
    assert_eq!(use_var, &class_method_id);
}

#[test]
fn kind_sig_1() {
    let pgm = r#"
kind T :: (Type -> Type) -> Type
type T f = f Int
"#;

    let parsed = crate::parser::parse_module(pgm).unwrap();
    let renamed = rename_module(&parsed);

    let sig = renamed[0].kind_sig();
    let sig_id = sig.node.ty.clone();

    let syn = renamed[1].type_syn();
    let syn_id = syn.node.ty.clone();

    assert_eq!(sig_id, syn_id);
}
