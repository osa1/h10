//! Implements geneartion of defined and used values and types of top-level bindings.

#[cfg(test)]
mod tests;

use crate::ast;
use crate::collections::Set;
use crate::id::Id;
use crate::scope_map::ScopeSet;

/// Defined and used values and types of a declaration.
#[allow(unused)]
#[derive(Debug, Clone)]
pub struct DeclInfo {
    defines: Defs,
    uses: Uses,
}

/// Types and values defined by a declaration.
#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Defs {
    /// Type defined by the declaration, if any.
    ///
    /// A declaration can define at most one type, hence [`Option`].
    tys: Set<String>,

    /// Values defined by the declaration.
    ///
    /// A declaration can define multiple values. Examples:
    ///
    /// ```ignore
    /// data X = A | B      -- defines type `X`, values `A` and `B`
    ///
    /// (a, b) = f 1 2 3    -- defines values `a` and `b`
    /// ```
    values: Set<String>,
}

/// Types and values defined by a declaration.
#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Uses {
    tys: Set<String>,
    values: Set<String>,
}

impl DeclInfo {
    pub fn new_from_top_decl(top_decl: &ast::TopDeclKind) -> Self {
        let mut info = DeclInfo::new_empty();

        match top_decl {
            ast::TopDeclKind::Value(value) => analyze_value_decl(
                value,
                &mut info,
                &mut Default::default(),
                &mut Default::default(),
                true,
            ),
            ast::TopDeclKind::Type(ty) => analyze_ty_syn(ty, &mut info),
            ast::TopDeclKind::KindSig(kind_sig) => analyze_kind_sig(kind_sig, &mut info),
            ast::TopDeclKind::Data(data) => analyze_data(data, &mut info),
            ast::TopDeclKind::Newtype(newty) => analyze_newtype(newty, &mut info),
            ast::TopDeclKind::Class(class) => analyze_class(class, &mut info),
            ast::TopDeclKind::Instance(instance) => analyze_instance(instance, &mut info),
            ast::TopDeclKind::Default(default) => analyze_default(default, &mut info),
        }

        info
    }

    #[allow(unused)]
    pub fn new_from_value_decl(value_decl: &ast::ValueDecl) -> Self {
        let mut info = DeclInfo::new_empty();
        analyze_value_decl(
            value_decl,
            &mut info,
            &mut Default::default(),
            &mut Default::default(),
            true,
        );
        info
    }

    fn new_empty() -> Self {
        DeclInfo {
            defines: Defs::new(),
            uses: Uses::new(),
        }
    }
}

impl Defs {
    fn new() -> Self {
        Defs {
            tys: Default::default(),
            values: Default::default(),
        }
    }
}

impl Uses {
    fn new() -> Self {
        Self {
            tys: Default::default(),
            values: Default::default(),
        }
    }
}

/*
fn analyze_value_decls<'a>(
    values: &'a [ast::ValueDecl],
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
    top_level: bool,
) {
    collect_local_binders(values, info, local_bound_vars);
    for value_decl in values {
        analyze_value_decl(value_decl, info, bound_ty_vars, local_bound_vars, top_level);
    }
}
*/

/// Analyze a single value declaration.
///
/// Make sure to bind values defined by the decl list (in a `let` or `where`) before calling this.
fn analyze_value_decl<'a>(
    value: &'a ast::ValueDecl,
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
    top_level: bool,
) {
    match &value.node {
        ast::ValueDecl_::TypeSig {
            vars,
            foralls,
            context,
            ty,
        } => {
            // TODO: When `foralls` is not given we should collect type variables in the entire
            // signature first as bound.

            bound_ty_vars.enter();

            for ast::TypeBinder_ { id, ty: _ } in foralls.iter().map(|binder| &binder.node) {
                bound_ty_vars.bind(id);
            }

            for ast::TypeBinder_ { id: _, ty } in foralls.iter().map(|binder| &binder.node) {
                if let Some(ty) = ty {
                    analyze_ty(ty, info, bound_ty_vars)
                }
            }

            info.defines.values.extend(vars.iter().cloned());

            for ty in context {
                analyze_ty(ty, info, bound_ty_vars);
            }

            analyze_ty(ty, info, bound_ty_vars);

            bound_ty_vars.exit();
        }

        ast::ValueDecl_::Fixity {
            fixity: _,
            prec: _,
            ops,
        } => {
            info.defines
                .values
                .extend(ops.iter().map(|op| op.id().clone()));
        }

        ast::ValueDecl_::Value { lhs, rhs } => {
            analyze_lhs(lhs, info, local_bound_vars, top_level);
            analyze_rhs(rhs, info, bound_ty_vars, local_bound_vars);
        }
    }
}

fn analyze_ty(ty: &ast::Type, info: &mut DeclInfo, bound_ty_vars: &ScopeSet<&Id>) {
    match &ty.node {
        ast::Type_::Tuple(tys) => tys
            .iter()
            .for_each(|ty| analyze_ty(ty, info, bound_ty_vars)),

        ast::Type_::List(ty) => analyze_ty(ty, info, bound_ty_vars),

        ast::Type_::Arrow(ty1, ty2) => {
            analyze_ty(ty1, info, bound_ty_vars);
            analyze_ty(ty2, info, bound_ty_vars);
        }

        ast::Type_::App(ty, tys) => {
            analyze_ty(ty, info, bound_ty_vars);
            tys.iter()
                .for_each(|ty| analyze_ty(ty, info, bound_ty_vars));
        }

        ast::Type_::Con(ty_con) => analyze_ty_con(ty_con, info, bound_ty_vars),

        ast::Type_::Var(var) => {
            if !bound_ty_vars.is_bound(var) {
                info.uses.tys.insert(var.clone());
            }
        }
    }
}

fn analyze_ty_con(ty_con: &ast::TyCon, info: &mut DeclInfo, bound_ty_vars: &ScopeSet<&Id>) {
    match &ty_con.node {
        ast::TyCon_::Id(id) => {
            if !bound_ty_vars.is_bound(id) {
                info.uses.tys.insert(id.clone());
            }
        }

        ast::TyCon_::Tuple(_) | ast::TyCon_::Arrow | ast::TyCon_::List => {}
    }
}

fn analyze_lhs<'a>(
    lhs: &'a ast::Lhs,
    info: &mut DeclInfo,
    local_bound_vars: &mut ScopeSet<&'a Id>,
    top_level: bool,
) {
    match &lhs.node {
        ast::Lhs_::Pat(pat) => analyze_pat(pat, info, local_bound_vars),

        ast::Lhs_::Fun { var, pats } => {
            local_bound_vars.bind(var);
            pats.iter()
                .for_each(|pat| analyze_pat(pat, info, local_bound_vars));

            if top_level {
                info.defines.values.insert(var.clone());
            }
        }
    }
}

/// Add variables bound by the pattern to [`local_bound_vars`] and constructors used as uses in
/// [`info`].
fn analyze_pat<'a>(
    pat: &'a ast::Pat,
    info: &mut DeclInfo,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    match &pat.node {
        ast::Pat_::Var(var) => local_bound_vars.bind(var),

        ast::Pat_::As(var, pat) => {
            local_bound_vars.bind(var);
            analyze_pat(pat, info, local_bound_vars);
        }

        ast::Pat_::Lit(_) | ast::Pat_::Wildcard => {}

        ast::Pat_::Tuple(pats) | ast::Pat_::List(pats) => pats
            .iter()
            .for_each(|pat| analyze_pat(pat, info, local_bound_vars)),

        ast::Pat_::Irrefutable(pat) => analyze_pat(pat, info, local_bound_vars),

        ast::Pat_::Con(con, pats) => {
            // Pattern data constructor is a value dependency.
            match &con.node {
                ast::GCon_::Tuple(_) | ast::GCon_::EmptyList => {}
                ast::GCon_::QCon(con) => {
                    info.uses.values.insert(con.clone());
                }
            }

            pats.iter()
                .for_each(|pat| analyze_pat(pat, info, local_bound_vars));
        }
    }
}

fn analyze_rhs<'a>(
    rhs: &'a ast::Rhs,
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    local_bound_vars.enter();

    let where_decls = rhs.node.where_decls();
    collect_local_binders(where_decls, info, local_bound_vars);

    match &rhs.node {
        ast::Rhs_::GuardedRhs {
            rhss,
            where_decls: _,
        } => {
            for rhs in rhss {
                analyze_guarded_rhs(rhs, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Rhs_::Rhs {
            rhs,
            where_decls: _,
        } => analyze_exp(rhs, info, bound_ty_vars, local_bound_vars),
    }

    for value_decl in where_decls {
        analyze_value_decl(value_decl, info, bound_ty_vars, local_bound_vars, false);
    }

    local_bound_vars.exit();
}

// TODO: With scoped type variables we should pass bound type vars here.
fn analyze_exp<'a>(
    exp: &'a ast::Exp,
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    match &exp.node {
        ast::Exp_::Var(var) | ast::Exp_::Con(var) => {
            if !local_bound_vars.is_bound(var) {
                info.uses.values.insert(var.clone());
            }
        }

        ast::Exp_::Lit(_) => {}

        ast::Exp_::Lam(args, body) => {
            local_bound_vars.enter();
            for arg in args {
                analyze_pat(arg, info, local_bound_vars);
            }
            analyze_exp(body, info, bound_ty_vars, local_bound_vars);
            local_bound_vars.exit();
        }

        ast::Exp_::App(e1, es) => {
            analyze_exp(e1, info, bound_ty_vars, local_bound_vars);
            for e in es {
                analyze_exp(e, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Exp_::Tuple(es) | ast::Exp_::List(es) => {
            for e in es {
                analyze_exp(e, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Exp_::Do(stmts) => {
            for stmt in stmts {
                analyze_stmt(stmt, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Exp_::TypeAnnotation {
            exp,
            context,
            type_,
        } => {
            analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
            let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();

            for ty in context {
                for var in ty.node.vars_borrowed() {
                    bound_ty_vars.bind(var);
                }
            }

            for ty in context {
                analyze_ty(ty, info, &bound_ty_vars);
            }

            analyze_ty(type_, info, &bound_ty_vars);
        }

        ast::Exp_::ArithmeticSeq { exp1, exp2, exp3 } => {
            analyze_exp(exp1, info, bound_ty_vars, local_bound_vars);
            if let Some(exp2) = exp2 {
                analyze_exp(exp2, info, bound_ty_vars, local_bound_vars);
            }
            if let Some(exp3) = exp3 {
                analyze_exp(exp3, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Exp_::ListComp { exp, quals } => {
            local_bound_vars.enter();
            for stmt in quals {
                analyze_stmt(stmt, info, bound_ty_vars, local_bound_vars);
            }
            analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
            local_bound_vars.exit();
        }

        ast::Exp_::Case(scrut, alts) => {
            analyze_exp(scrut, info, bound_ty_vars, local_bound_vars);
            for alt in alts {
                analyze_alt(alt, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Exp_::If(e1, e2, e3) => {
            analyze_exp(e1, info, bound_ty_vars, local_bound_vars);
            analyze_exp(e2, info, bound_ty_vars, local_bound_vars);
            analyze_exp(e3, info, bound_ty_vars, local_bound_vars);
        }

        ast::Exp_::Let(value_decls, exp) => {
            local_bound_vars.enter();
            collect_local_binders(value_decls, info, local_bound_vars);
            for value_decl in value_decls {
                analyze_value_decl(value_decl, info, bound_ty_vars, local_bound_vars, false);
            }
            analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
            local_bound_vars.exit();
        }

        ast::Exp_::Update { exp, updates } => {
            analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
            for (field, exp) in updates {
                // Fields have to be defined at the top-level.
                info.uses.values.insert(field.clone());
                analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
            }
        }

        ast::Exp_::ReAssoc(_, _, _) => todo!(),
    }
}

fn analyze_alt<'a>(
    alt: &'a ast::Alt,
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    let ast::Alt_ {
        pat,
        guarded_rhss,
        where_decls,
    } = &alt.node;

    local_bound_vars.enter();

    // `where` decls are bound in `pat`.
    collect_local_binders(where_decls, info, local_bound_vars);

    analyze_pat(pat, info, local_bound_vars);

    for (stmts, exp) in guarded_rhss {
        for stmt in stmts {
            analyze_stmt(stmt, info, bound_ty_vars, local_bound_vars);
        }
        analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
    }

    for value_decl in where_decls {
        analyze_value_decl(value_decl, info, bound_ty_vars, local_bound_vars, false);
    }

    local_bound_vars.exit();
}

fn analyze_guarded_rhs<'a>(
    rhs: &'a ast::GuardedRhs,
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    let ast::GuardedRhs_ { guards, rhs } = &rhs.node;

    for guard in guards {
        // NB. Guards can bind variables.
        analyze_stmt(guard, info, bound_ty_vars, local_bound_vars);
    }

    analyze_exp(rhs, info, bound_ty_vars, local_bound_vars);
}

fn analyze_stmt<'a>(
    stmt: &'a ast::Stmt,
    info: &mut DeclInfo,
    bound_ty_vars: &mut ScopeSet<&'a Id>,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    match &stmt.node {
        ast::Stmt_::Exp(exp) => analyze_exp(exp, info, bound_ty_vars, local_bound_vars),

        ast::Stmt_::Bind(pat, exp) => {
            analyze_pat(pat, info, local_bound_vars);
            analyze_exp(exp, info, bound_ty_vars, local_bound_vars);
        }

        ast::Stmt_::Let(decls) => {
            collect_local_binders(decls, info, local_bound_vars);
            for decl in decls {
                analyze_value_decl(decl, info, &mut Default::default(), local_bound_vars, false);
            }
        }
    }
}

/// Bind left-hand side variables as locally bound (in [`local_bound_vars`]) and used types (in
/// signatures) constructors (in patterns) as used (in [`info`]).
fn collect_local_binders<'a>(
    binders: &'a [ast::ValueDecl],
    info: &mut DeclInfo,
    local_bound_vars: &mut ScopeSet<&'a Id>,
) {
    for binder in binders {
        match &binder.node {
            // TODO: With scoped type variables, type vars in `foralls` should be defined in
            // `vars`.
            ast::ValueDecl_::TypeSig {
                vars: _,
                foralls: _,
                context: _,
                ty: _,
            } => {}

            ast::ValueDecl_::Fixity {
                fixity: _,
                prec: _,
                ops,
            } => {
                ops.iter().for_each(|op| local_bound_vars.bind(op.id()));
            }

            ast::ValueDecl_::Value { lhs, rhs: _ } => match &lhs.node {
                ast::Lhs_::Pat(pat) => analyze_pat(pat, info, local_bound_vars),
                ast::Lhs_::Fun { var, pats: _ } => {
                    // NB. `pats` should only be bound in the RHS of the function.
                    local_bound_vars.bind(var);
                }
            },
        }
    }
}

fn analyze_ty_syn<'a, 'b>(ty_decl: &'a ast::TypeDecl, info: &'b mut DeclInfo) {
    let ast::TypeDecl_ { ty, vars, rhs } = &ty_decl.node;
    info.defines.tys.insert(ty.clone());
    let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();
    for var in vars {
        bound_ty_vars.bind(var);
    }
    analyze_ty(rhs, info, &bound_ty_vars);
}

fn analyze_kind_sig<'a, 'b>(kind_sig: &'a ast::KindSigDecl, info: &'b mut DeclInfo) {
    let ast::KindSigDecl_ { ty, foralls, sig } = &kind_sig.node;
    info.defines.tys.insert(ty.clone());

    let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();
    for ast::AstNode {
        node: ast::TypeBinder_ { id, ty },
        ..
    } in foralls
    {
        bound_ty_vars.bind(id);

        // NB. Type of a binder can refer to previous binders.
        if let Some(ty) = ty {
            analyze_ty(ty, info, &bound_ty_vars);
        }
    }

    analyze_ty(sig, info, &bound_ty_vars);
}

fn analyze_data<'a, 'b>(data: &'a ast::DataDecl, info: &'b mut DeclInfo) {
    let ast::DataDecl_ {
        context,
        ty_con,
        ty_args,
        cons,
        deriving: _,
    } = &data.node;

    assert!(context.is_empty()); // TODO: Handle context

    info.defines.tys.insert(ty_con.clone());

    let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();
    for ty_arg in ty_args {
        bound_ty_vars.bind(ty_arg);
    }

    cons.iter()
        .for_each(|con| analyze_con(con, info, &bound_ty_vars));
}

fn analyze_con(con: &ast::Con, info: &mut DeclInfo, bound_ty_vars: &ScopeSet<&Id>) {
    let ast::Con_ { con, fields } = &con.node;
    info.defines.values.insert(con.clone());
    fields
        .iter()
        .for_each(|field| analyze_field(field, info, bound_ty_vars));
}

fn analyze_field(field: &ast::FieldDecl, info: &mut DeclInfo, bound_ty_vars: &ScopeSet<&Id>) {
    let ast::FieldDecl_ { vars, ty } = &field.node;
    vars.iter().for_each(|field_var| {
        info.defines.values.insert(field_var.clone());
    });
    analyze_ty(ty, info, bound_ty_vars);
}

fn analyze_newtype<'a, 'b>(newty: &'a ast::NewtypeDecl, info: &'b mut DeclInfo) {
    let ast::NewtypeDecl_ {
        context,
        ty_con,
        ty_args,
        con,
    } = &newty.node;

    assert!(context.is_empty()); // TODO: Handle context

    info.defines.tys.insert(ty_con.clone());

    let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();
    for ty_arg in ty_args {
        bound_ty_vars.bind(ty_arg);
    }

    analyze_con(con, info, &bound_ty_vars);
}

fn analyze_class<'a, 'b>(class: &'a ast::ClassDecl, info: &'b mut DeclInfo) {
    let ast::ClassDecl_ {
        context,
        ty_con,
        ty_arg,
        decls,
    } = &class.node;

    info.defines.tys.insert(ty_con.clone());

    let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();
    bound_ty_vars.bind(ty_con);
    bound_ty_vars.bind(ty_arg);

    // Bind type variables in context.
    for ty in context {
        for var in ty.node.vars_borrowed() {
            bound_ty_vars.bind(var);
        }
    }

    // Add other types in context as dependencies.
    for ty in context {
        analyze_ty(ty, info, &bound_ty_vars);
    }

    for decl in decls {
        analyze_value_decl(
            decl,
            info,
            &mut bound_ty_vars,
            &mut Default::default(),
            true,
        );
    }
}

fn analyze_instance<'a, 'b>(instance: &'a ast::InstanceDecl, info: &'b mut DeclInfo) {
    let ast::InstanceDecl_ {
        context,
        ty_con,
        ty,
        decls,
    } = &instance.node;

    let mut bound_ty_vars: ScopeSet<&'a Id> = Default::default();

    // Bind type variables in context.
    for ty in context {
        for var in ty.node.vars_borrowed() {
            bound_ty_vars.bind(var);
        }
    }

    // Add other types in context as dependencies.
    for ty in context {
        analyze_ty(ty, info, &bound_ty_vars);
    }

    info.uses.tys.insert(ty_con.clone());

    analyze_ty(ty, info, &bound_ty_vars);

    for decl in decls {
        analyze_value_decl(
            decl,
            info,
            &mut bound_ty_vars,
            &mut Default::default(),
            true,
        );
    }
}

fn analyze_default(_value: &ast::DefaultDecl, _info: &mut DeclInfo) {
    todo!()
}
