use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Arena<T> {
    allocs: Vec<Allocation<T>>,

    /// A free slot in the arena, if available.
    ///
    /// When this is not available we grow the vector.
    free: Option<Idx<T>>,
}

pub struct Idx<T> {
    idx: u32,
    phantom: PhantomData<T>,
}

const _: () = assert!(std::mem::size_of::<Idx<u64>>() == std::mem::size_of::<u32>());

impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Idx").field(&self.idx).finish()
    }
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T> Eq for Idx<T> {}

impl<T> Hash for Idx<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl<T> Idx<T> {
    fn from_usize(idx: usize) -> Self {
        Self {
            idx: idx as u32,
            phantom: Default::default(),
        }
    }

    fn as_usize(&self) -> usize {
        self.idx as usize
    }
}

#[derive(Debug)]
enum Allocation<T> {
    Free { next_free_slot: Option<Idx<T>> },
    Used { elem: T },
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            allocs: vec![],
            free: None,
        }
    }

    pub fn allocate(&mut self, elem: T) -> Idx<T> {
        match self.free.take() {
            Some(idx) => {
                let free_decl =
                    std::mem::replace(&mut self.allocs[idx.as_usize()], Allocation::Used { elem });
                match free_decl {
                    Allocation::Free { next_free_slot } => {
                        self.free = next_free_slot;
                    }
                    Allocation::Used { elem: _ } => {
                        panic!("Free slot in `self.free` was not really free");
                    }
                }
                idx
            }
            None => {
                let idx = Idx::from_usize(self.allocs.len());
                self.allocs.push(Allocation::Used { elem });
                idx
            }
        }
    }

    pub fn free(&mut self, idx: Idx<T>) {
        self.allocs[idx.as_usize()] = Allocation::Free {
            next_free_slot: self.free,
        };
        self.free = Some(idx);
    }

    pub fn get(&self, idx: Idx<T>) -> &T {
        match &self.allocs[idx.as_usize()] {
            Allocation::Free { .. } => panic!("Declaration index is not in use"),
            Allocation::Used { elem } => elem,
        }
    }

    pub fn get_mut(&mut self, idx: Idx<T>) -> &mut T {
        match &mut self.allocs[idx.as_usize()] {
            Allocation::Free { .. } => panic!("Declaration index is not in use"),
            Allocation::Used { elem } => elem,
        }
    }
}
