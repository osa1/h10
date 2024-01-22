#![allow(unused)]

use crate::ast;

#[derive(Debug)]
pub struct DeclArena {
    /// Allocated declarations, indexed by [`DeclIdx`].
    decls: Vec<DeclAllocation>,

    /// A free slot in the arena, if available.
    ///
    /// When this is not available we grow the vector.
    free: Option<DeclIdx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclIdx {
    idx: u32,
}

impl DeclIdx {
    fn from_usize(idx: usize) -> Self {
        Self { idx: idx as u32 }
    }

    fn as_usize(&self) -> usize {
        self.idx as usize
    }
}

#[derive(Debug)]
enum DeclAllocation {
    Free { next_free_slot: Option<DeclIdx> },
    Used { decl: ast::ParsedTopDecl },
}

impl DeclArena {
    pub fn new() -> Self {
        Self {
            decls: vec![],
            free: None,
        }
    }

    pub fn allocate(&mut self, decl: ast::ParsedTopDecl) -> DeclIdx {
        match self.free.take() {
            Some(idx) => {
                let free_decl = std::mem::replace(
                    &mut self.decls[idx.as_usize()],
                    DeclAllocation::Used { decl },
                );
                match free_decl {
                    DeclAllocation::Free { next_free_slot } => {
                        self.free = next_free_slot;
                    }
                    DeclAllocation::Used { decl: _ } => {
                        panic!("Free slot in `self.free` was not really free");
                    }
                }
                idx
            }
            None => {
                let idx = DeclIdx::from_usize(self.decls.len());
                self.decls.push(DeclAllocation::Used { decl });
                idx
            }
        }
    }

    pub fn free(&mut self, idx: DeclIdx) {
        self.decls[idx.as_usize()] = DeclAllocation::Free {
            next_free_slot: self.free,
        };
        self.free = Some(idx);
    }

    pub fn get(&self, idx: DeclIdx) -> &ast::ParsedTopDecl {
        match &self.decls[idx.as_usize()] {
            DeclAllocation::Free { .. } => panic!("Declaration index is not in use"),
            DeclAllocation::Used { decl } => decl,
        }
    }

    pub fn get_mut(&mut self, idx: DeclIdx) -> &mut ast::ParsedTopDecl {
        match &mut self.decls[idx.as_usize()] {
            DeclAllocation::Free { .. } => panic!("Declaration index is not in use"),
            DeclAllocation::Used { decl } => decl,
        }
    }
}
