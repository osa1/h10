use crate::id::{self, Id, IdKind};

use std::cell::{Cell, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

/// Shared (reference counted) reference to a type (`Ty`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyRef(Rc<Ty>);

impl Deref for TyRef {
    type Target = Ty;

    fn deref(&self) -> &Ty {
        self.0.deref()
    }
}

/// Shared (reference counted) reference to a unification variable (`TyVar`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVarRef(Rc<TyVar>);

/// A kind.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Star,
    Fun(Box<Kind>, Box<Kind>),
}

impl Kind {
    pub fn split_fun_kind(&self) -> (Vec<Kind>, Kind) {
        let mut args: Vec<Kind> = vec![];
        let mut ret = self;

        loop {
            match ret {
                Kind::Star => return (args, Kind::Star),
                Kind::Fun(arg, ret_) => {
                    args.push(arg.deref().clone());
                    ret = ret_;
                }
            }
        }
    }
}

/// A type.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ty {
    /// A unification variable.
    Var(TyVarRef),

    /// A type constructor.
    Con(Id),

    /// A type application.
    App(TyRef, TyRef),

    /// A generic (aka. quantified) type variable.
    Gen(u32),
}

/// A unification variable.
#[derive(Clone, PartialOrd, Ord)]
struct TyVar {
    /// The identity of the variable. Unification variables currently don't have names, this is
    /// only used to compare variables.
    id: Id,

    /// Kind of the variable.
    kind: Kind,

    /// The binding depth of the variable. All unification variables in an equivalence class must
    /// have the same level.
    ///
    /// This is used in generalization: a binding at level N can only generalize variables at level
    /// N or higher.
    //
    // TODO: Shouldn't every binding generalize only its own variables? Example where a nested
    // scope's variable is generalized safely?
    level: Cell<u32>,

    /// Link to the canonical representative of the variable's equivalence class. When not avaiable
    /// it means the variable itself is the representative.
    ///
    /// Unifying a variable with a type makes the type the representative. When unifying two
    /// variables, the one with the higher level becomes the representative.
    ///
    /// (TODO: Does it matter which var points to which?)
    ///
    /// All unification variables in an equivalence class need to have the same level. See
    /// `prune_level`.
    link: RefCell<Option<TyRef>>,
}

impl PartialEq for TyVar {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TyVar {}

impl Hash for TyVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl TyRef {
    pub fn new(ty: Ty) -> TyRef {
        TyRef(Rc::new(ty))
    }

    pub fn new_var(kind: Kind, level: u32) -> TyRef {
        TyRef::new(Ty::Var(TyVarRef::new(kind, level)))
    }

    pub fn new_con(con: Id) -> TyRef {
        TyRef::new(Ty::Con(con))
    }

    pub fn new_gen(i: u32) -> TyRef {
        TyRef::new(Ty::Gen(i))
    }

    pub fn new_app(ty1: TyRef, ty2: TyRef) -> TyRef {
        TyRef::new(Ty::App(ty1, ty2))
    }

    pub fn ty(&self) -> Ty {
        self.deref().clone()
    }

    pub fn contains_var(&self, var: &TyVarRef) -> bool {
        match &*self.normalize() {
            Ty::Var(var_) => var == var_,
            Ty::Con(_) => false,
            Ty::App(ty1, ty2) => ty1.contains_var(var) || ty2.contains_var(var),
            Ty::Gen(_) => false, // TODO: or report bug?
        }
    }

    pub fn normalize(&self) -> TyRef {
        match self.deref().clone() {
            Ty::Var(var) => match var.link() {
                Some(link) => {
                    let normalized = link.normalize();
                    var.set_link(normalized.clone());
                    normalized
                }
                None => self.clone(),
            },

            _ => self.clone(),
        }
    }

    pub fn split_fun_ty(&self) -> (Vec<TyRef>, TyRef) {
        let mut ty: TyRef = self.clone();

        let mut args: Vec<TyRef> = vec![];

        let ret: TyRef = loop {
            match ty.deref() {
                Ty::App(ty1, ty2) => {
                    if let Ty::App(ty1_, ty2_) = ty1.deref() {
                        if let Ty::Con(con) = ty1_.deref() {
                            if con == &id::arrow_ty_id() {
                                args.push(ty2_.clone());
                                ty = ty2.clone();
                                continue;
                            }
                        }
                    }
                    break ty;
                }

                Ty::Var(_) | Ty::Con(_) | Ty::Gen(_) => break ty,
            }
        };

        (args, ret)
    }

    pub(crate) fn subst_gens(&self, gens: &[TyRef]) -> TyRef {
        match self.deref() {
            Ty::Var(_) | Ty::Con(_) => self.clone(),
            Ty::App(ty1, ty2) => TyRef::new_app(ty1.subst_gens(gens), ty2.subst_gens(gens)),
            Ty::Gen(gen) => gens[*gen as usize].clone(),
        }
    }
}

impl TyVarRef {
    fn new(kind: Kind, level: u32) -> TyVarRef {
        TyVarRef(Rc::new(TyVar::new(kind, level)))
    }
}

impl TyVar {
    fn new(kind: Kind, level: u32) -> TyVar {
        TyVar {
            id: Id::new(None, IdKind::TyVar),
            kind,
            level: Cell::new(level),
            link: RefCell::new(None),
        }
    }

    fn id(&self) -> Id {
        self.id.clone()
    }

    fn kind(&self) -> &Kind {
        &self.kind
    }

    fn link(&self) -> Option<TyRef> {
        self.link.borrow().clone()
    }

    fn set_link(&self, link: TyRef) {
        *self.link.borrow_mut() = Some(link);
    }

    fn level(&self) -> u32 {
        self.level.get()
    }

    fn set_level(&self, level: u32) {
        self.level.set(level)
    }

    fn prune_level(&self, level: u32) {
        self.set_level(std::cmp::min(self.level(), level))
    }
}

impl TyVarRef {
    pub fn id(&self) -> Id {
        self.0.id()
    }

    pub fn kind(&self) -> &Kind {
        self.0.kind()
    }

    pub fn link(&self) -> Option<TyRef> {
        self.0.link()
    }

    pub fn set_link(&self, link: TyRef) {
        self.0.set_link(link);
    }

    pub fn level(&self) -> u32 {
        self.0.level()
    }

    pub fn set_level(&self, level: u32) {
        self.0.set_level(level)
    }

    pub fn prune_level(&self, level: u32) {
        self.0.prune_level(level)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Debugging
//
////////////////////////////////////////////////////////////////////////////////////////////////////

impl fmt::Debug for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Fun(k1, k2) => match &**k1 {
                Kind::Star => write!(f, "* -> {:?}", k2),
                Kind::Fun(_, _) => write!(f, "({:?}) -> {:?}", k1, k2),
            },
        }
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(var_ref) => var_ref.0.deref().fmt(f),

            Self::Con(con) => con.fmt(f),

            Self::App(ty1, ty2) => f
                .debug_tuple("App")
                .field(ty1.deref())
                .field(ty2.deref())
                .finish(),

            Self::Gen(idx) => write!(f, "${}", idx),
        }
    }
}

impl fmt::Debug for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TyVar")
            .field("id", &self.id)
            .field("kind", &self.kind)
            .field("level", &self.level.get())
            .field("link", &LinkCellDebug(&self.link))
            .finish()
    }
}

struct LinkCellDebug<'a>(&'a RefCell<Option<TyRef>>);

impl<'a> fmt::Debug for LinkCellDebug<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.try_borrow() {
            Ok(link) => match &*link {
                Some(ty_ref) => ty_ref.deref().fmt(f),
                None => write!(f, "•"),
            },
            Err(_) => write!(f, "<link cell borrowed>"),
        }
    }
}

fn ty_ref_needs_parens(ty: &TyRef) -> bool {
    match ty.deref() {
        Ty::Var(_) | Ty::Con(_) | Ty::Gen(_) => false,
        Ty::App(_, _) => true,
    }
}

impl fmt::Display for TyRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (fun_args, fun_ret) = self.split_fun_ty();
        for arg in fun_args {
            if ty_ref_needs_parens(&arg) {
                write!(f, "({}) -> ", arg)?;
            } else {
                write!(f, "{} -> ", arg)?;
            }
        }
        fmt::Display::fmt(fun_ret.deref(), f)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Var(var_ref) => fmt::Display::fmt(var_ref, f),

            Ty::Con(id) => fmt::Display::fmt(id, f),

            Ty::App(ty1, ty2) => {
                let ty1_parens = ty_ref_needs_parens(ty1);
                let ty2_parens = ty_ref_needs_parens(ty2);
                if ty1_parens {
                    write!(f, "(")?;
                }
                fmt::Display::fmt(ty1, f)?;
                if ty1_parens {
                    write!(f, ")")?;
                }
                write!(f, " ")?;
                if ty2_parens {
                    write!(f, "(")?;
                }
                fmt::Display::fmt(ty2, f)?;
                if ty2_parens {
                    write!(f, ")")?;
                }
                Ok(())
            }

            Ty::Gen(idx) => write!(f, "${}", idx),
        }
    }
}

impl fmt::Display for TyVarRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <TyVar as fmt::Display>::fmt(&*self.0, f)
    }
}

impl fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.link() {
            Some(link) => fmt::Debug::fmt(&link, f),
            None => fmt::Debug::fmt(&self.id, f),
        }
    }
}
