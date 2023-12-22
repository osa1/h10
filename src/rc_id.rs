use crate::base64::base64_encode;

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// A wrapper around `Rc` that implements `Eq`, `Ord`, and `Hash` based on `Rc` identity.
#[derive(Debug)]
pub struct RcId<A>(Rc<A>);

impl<A> Clone for RcId<A> {
    fn clone(&self) -> Self {
        RcId(self.0.clone())
    }
}

impl<A> PartialEq for RcId<A> {
    fn eq(&self, other: &Self) -> bool {
        // Using `Rc::as_ptr` here instead of `Rc::ptr_eq` to make it explicit that the
        // implementation agrees with `Ord` and `Hash` implementations below
        self.id() == other.id()
    }
}

impl<A> Eq for RcId<A> {}

impl<A> PartialOrd for RcId<A> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<A> Ord for RcId<A> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id().cmp(&other.id())
    }
}

impl<A> Hash for RcId<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl<A> RcId<A> {
    pub fn new(a: A) -> RcId<A> {
        Self::from_rc(Rc::new(a))
    }

    pub fn from_rc(inner: Rc<A>) -> RcId<A> {
        RcId(inner)
    }

    pub fn id(&self) -> usize {
        Rc::as_ptr(&self.0) as usize
    }

    pub fn id_base64(&self) -> String {
        base64_encode(self.id())
    }
}

impl<A> AsRef<A> for RcId<A> {
    fn as_ref(&self) -> &A {
        &self.0
    }
}

impl<A> std::borrow::Borrow<A> for RcId<A> {
    fn borrow(&self) -> &A {
        &self.0
    }
}

impl<A> std::ops::Deref for RcId<A> {
    type Target = A;

    #[inline(always)]
    fn deref(&self) -> &A {
        &self.0
    }
}
