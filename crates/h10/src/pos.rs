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

    pub fn adjust_for_insertion(&self, insertion_point: Pos, insertion_end: Pos) -> Pos {
        if *self < insertion_point {
            *self
        } else if self.line == insertion_point.line {
            Pos {
                line: insertion_end.line,
                char: insertion_end.char + self.char - insertion_point.char,
            }
        } else {
            Pos {
                line: self.line + insertion_end.line - insertion_point.line,
                char: self.char,
            }
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

#[test]
fn adjust_for_insertion_1() {
    // Position is before the insertion point.
    let pos = Pos { line: 1, char: 5 };
    let insertion_point = Pos { line: 1, char: 10 };
    let insertion_end = Pos { line: 1, char: 15 };
    assert_eq!(
        pos.adjust_for_insertion(insertion_point, insertion_end),
        pos
    );
}

#[test]
fn adjust_for_insertion_2() {
    // Position is after the insertion point, on the same line as insertion.
    let pos = Pos { line: 1, char: 5 };
    let insertion_point = Pos { line: 1, char: 5 };
    let insertion_end = Pos { line: 1, char: 15 };
    assert_eq!(
        pos.adjust_for_insertion(insertion_point, insertion_end),
        Pos { line: 1, char: 15 }
    );
}

#[test]
fn adjust_for_insertion_3() {
    // Position is after the insertion point, on a different line.
    let pos = Pos { line: 3, char: 10 };
    let insertion_point = Pos { line: 1, char: 5 };
    let insertion_end = Pos { line: 1, char: 15 };
    assert_eq!(
        pos.adjust_for_insertion(insertion_point, insertion_end),
        Pos { line: 3, char: 10 }
    );
}
