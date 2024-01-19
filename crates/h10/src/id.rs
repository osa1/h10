use rc_id::RcId;

use std::fmt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(RcId<IdDetails>);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0.name {
            Some(name) => write!(f, "{}", name),
            None => write!(f, "_{{{}}}", self.0.id_base64()),
        }
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0.name {
            Some(name) => write!(f, "{}{{{}}}", name, self.0.id_base64()),
            None => write!(f, "_{{{}}}", self.0.id_base64()),
        }
    }
}

impl Id {
    pub fn new(name: Option<String>, kind: IdKind) -> Id {
        Id(RcId::new(IdDetails { name, kind }))
    }

    pub fn name(&self) -> Option<&str> {
        self.details().name.as_deref()
    }

    pub fn details(&self) -> &IdDetails {
        &self.0
    }
}

#[derive(Debug)]
pub struct IdDetails {
    name: Option<String>,

    #[allow(unused)]
    kind: IdKind,
}

#[derive(Debug, Clone, Copy)]
pub enum IdKind {
    DataCon,
    Term,
    TyCon,
    TyVar,
}

thread_local!(static ARROW_TY_ID: Id = Id::new(Some("(->)".to_owned()), IdKind::TyCon));

pub fn arrow_ty_id() -> Id {
    ARROW_TY_ID.with(|id| id.clone())
}

thread_local!(static CHAR_TY_ID: Id = Id::new(Some("Char".to_owned()), IdKind::TyCon));

pub fn char_ty_id() -> Id {
    CHAR_TY_ID.with(|id| id.clone())
}

thread_local!(static LIST_TY_ID: Id = Id::new(Some("[]".to_owned()), IdKind::TyCon));

pub fn list_ty_id() -> Id {
    LIST_TY_ID.with(|id| id.clone())
}

thread_local!(static BOOL_TY_ID: Id = Id::new(Some("Bool".to_owned()), IdKind::TyCon));

pub fn bool_ty_id() -> Id {
    BOOL_TY_ID.with(|id| id.clone())
}

thread_local!(static INT_TY_ID: Id = Id::new(Some("Int".to_owned()), IdKind::TyCon));

pub fn int_ty_id() -> Id {
    INT_TY_ID.with(|id| id.clone())
}

thread_local!(static CONS_ID: Id = Id::new(Some(":".to_owned()), IdKind::Term));

pub fn cons_id() -> Id {
    CONS_ID.with(|id| id.clone())
}

thread_local!(static NIL_ID: Id = Id::new(Some("[]".to_owned()), IdKind::Term));

pub fn nil_id() -> Id {
    NIL_ID.with(|id| id.clone())
}

thread_local!(static INTEGER_TY_ID: Id = Id::new(Some("Integer".to_owned()), IdKind::TyCon));

pub fn integer_ty_id() -> Id {
    INTEGER_TY_ID.with(|id| id.clone())
}

thread_local!(static UNIT_TY_ID: Id = Id::new(Some("()".to_owned()), IdKind::TyCon));

pub fn unit_ty_id() -> Id {
    UNIT_TY_ID.with(|id| id.clone())
}

thread_local!(static TUPLE_TY_IDS: [Id; 5] = [
    Id::new(Some("(,)".to_owned()), IdKind::TyCon),
    Id::new(Some("(,,)".to_owned()), IdKind::TyCon),
    Id::new(Some("(,,,)".to_owned()), IdKind::TyCon),
    Id::new(Some("(,,,,)".to_owned()), IdKind::TyCon),
    Id::new(Some("(,,,,,)".to_owned()), IdKind::TyCon),
]);

pub fn tuple_ty_id(arity: u32) -> Id {
    if arity == 0 {
        return unit_ty_id();
    }
    assert!(arity >= 2 && arity <= 6);
    TUPLE_TY_IDS.with(|tys| tys[arity as usize - 2].clone())
}

thread_local!(static TYPE_TY_ID: Id = Id::new(Some("Type".to_owned()), IdKind::TyCon));

pub fn type_ty_id() -> Id {
    TYPE_TY_ID.with(|id| id.clone())
}

thread_local!(static TYPE_TY_TYREF: crate::typing::TyRef = crate::typing::TyRef::new_con(type_ty_id()));

/// A [`TyRef`] for `Type`.
// TODO: Maybe make this a [`TyRef`] method.
pub fn type_ty_tyref() -> crate::typing::TyRef {
    TYPE_TY_TYREF.with(|ty| ty.clone())
}
