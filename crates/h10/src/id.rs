pub type Id = String;

/*
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(RcId<String>);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{{{}}}", self.name(), self.0.id_base64())
    }
}

impl Id {
    pub fn new(name: String) -> Id {
        Id(RcId::new(name))
    }

    pub fn name(&self) -> &str {
        &self.0
    }
}
*/

thread_local!(static ARROW_TY_ID: Id = "(->)".to_owned());

pub fn arrow_ty_id() -> Id {
    ARROW_TY_ID.with(|id| id.clone())
}

thread_local!(static CHAR_TY_ID: Id = "Char".to_owned());

pub fn char_ty_id() -> Id {
    CHAR_TY_ID.with(|id| id.clone())
}

thread_local!(static LIST_TY_ID: Id = "[]".to_owned());

pub fn list_ty_id() -> Id {
    LIST_TY_ID.with(|id| id.clone())
}

thread_local!(static BOOL_TY_ID: Id = "Bool".to_owned());

pub fn bool_ty_id() -> Id {
    BOOL_TY_ID.with(|id| id.clone())
}

thread_local!(static INT_TY_ID: Id = "Int".to_owned());

pub fn int_ty_id() -> Id {
    INT_TY_ID.with(|id| id.clone())
}

thread_local!(static CONS_ID: Id = ":".to_owned());

pub fn cons_id() -> Id {
    CONS_ID.with(|id| id.clone())
}

thread_local!(static NIL_ID: Id = "[]".to_owned());

pub fn nil_id() -> Id {
    NIL_ID.with(|id| id.clone())
}

thread_local!(static INTEGER_TY_ID: Id = "Integer".to_owned());

pub fn integer_ty_id() -> Id {
    INTEGER_TY_ID.with(|id| id.clone())
}

thread_local!(static UNIT_TY_ID: Id = "()".to_owned());

pub fn unit_ty_id() -> Id {
    UNIT_TY_ID.with(|id| id.clone())
}

thread_local!(static TUPLE_TY_IDS: [Id; 5] = [
    "(,)".to_owned(),
    "(,,)".to_owned(),
    "(,,,)".to_owned(),
    "(,,,,)".to_owned(),
    "(,,,,,)".to_owned(),
]);

pub fn tuple_ty_id(arity: u32) -> Id {
    if arity == 0 {
        return unit_ty_id();
    }
    assert!(arity >= 2 && arity <= 6);
    TUPLE_TY_IDS.with(|tys| tys[arity as usize - 2].clone())
}

thread_local!(static TYPE_TY_ID: Id = "Type".to_owned());

pub fn type_ty_id() -> Id {
    TYPE_TY_ID.with(|id| id.clone())
}

thread_local!(static TYPE_TY_TYREF: crate::typing::TyRef = crate::typing::TyRef::new_con(type_ty_id()));

/// A [`TyRef`] for `Type`.
// TODO: Maybe make this a [`TyRef`] method.
pub fn type_ty_tyref() -> crate::typing::TyRef {
    TYPE_TY_TYREF.with(|ty| ty.clone())
}
