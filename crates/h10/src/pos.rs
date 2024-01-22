use std::cmp::Ordering;

use lexgen_util::Loc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos {
    pub line: u32,
    pub char: u32,
}

impl Pos {
    pub fn new(line: u32, char: u32) -> Self {
        Self { line, char }
    }

    pub fn from_loc(loc: &Loc) -> Self {
        Self {
            line: loc.line,
            char: loc.col,
        }
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.char.cmp(&other.char),
            cmp => cmp,
        }
    }
}
