use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// A wrapper around `Arc` that implements `Eq`, `Ord`, and `Hash` based on `Arc` identity.
#[derive(Debug)]
pub struct ArcId<A>(Arc<A>);

impl<A> Clone for ArcId<A> {
    fn clone(&self) -> Self {
        ArcId(self.0.clone())
    }
}

impl<A> PartialEq for ArcId<A> {
    fn eq(&self, other: &Self) -> bool {
        // Using `Arc::as_ptr` here instead of `Arc::ptr_eq` to make it explicit that the
        // implementation agrees with `Ord` and `Hash` implementations below
        self.id() == other.id()
    }
}

impl<A> Eq for ArcId<A> {}

impl<A> PartialOrd for ArcId<A> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<A> Ord for ArcId<A> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id().cmp(&other.id())
    }
}

impl<A> Hash for ArcId<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl<A> ArcId<A> {
    pub fn new(a: A) -> ArcId<A> {
        Self::from_rc(Arc::new(a))
    }

    pub fn from_rc(inner: Arc<A>) -> ArcId<A> {
        ArcId(inner)
    }

    pub fn id(&self) -> usize {
        Arc::as_ptr(&self.0) as usize
    }

    pub fn id_base64(&self) -> String {
        base64_encode(self.id())
    }
}

impl<A> AsRef<A> for ArcId<A> {
    fn as_ref(&self) -> &A {
        &self.0
    }
}

impl<A> std::borrow::Borrow<A> for ArcId<A> {
    fn borrow(&self) -> &A {
        &self.0
    }
}

impl<A> std::ops::Deref for ArcId<A> {
    type Target = A;

    #[inline(always)]
    fn deref(&self) -> &A {
        &self.0
    }
}

const ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const LOW_SIX_BITS_MASK: usize = 0b0011_1111;

fn base64_encode(mut i: usize) -> String {
    let alphabet = ALPHABET.as_bytes();

    // A character in base64 encodes 6 bits.
    let encoding_size = (std::mem::size_of::<usize>() * 8).div_ceil(6);

    let mut vec = vec![0; encoding_size];

    for byte in &mut vec {
        let idx = i & LOW_SIX_BITS_MASK;
        *byte = alphabet[idx];
        i >>= 6;
    }

    String::from_utf8(vec).unwrap()
}
