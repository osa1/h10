use std::cmp::Ordering;
use std::fmt;

use lexgen_util::Loc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos {
    pub line: u32,
    pub char: u32,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
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

    pub fn adjust_for_deletion(&self, deletion_start: Pos, deletion_end: Pos) -> Pos {
        if *self < deletion_start {
            *self
        } else if *self < deletion_end {
            deletion_start
        } else {
            let n_deleted_lines = deletion_end.line - deletion_start.line;
            Pos {
                line: self.line - n_deleted_lines,
                char: if self.line == deletion_end.line {
                    let n_deleted_chars = if deletion_start.line == deletion_end.line {
                        deletion_end.char - deletion_start.char
                    } else {
                        deletion_end.char
                    };
                    self.char - n_deleted_chars
                } else {
                    self.char
                },
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
fn ord_1() {
    assert!(Pos { line: 0, char: 5 } < Pos { line: 1, char: 0 });
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

#[test]
fn adjust_for_deletion_1() {
    // Position is before the deletion point.
    let pos = Pos { line: 1, char: 5 };
    let deletion_start = Pos { line: 1, char: 10 };
    let deletion_end = Pos { line: 1, char: 15 };
    assert_eq!(pos.adjust_for_deletion(deletion_start, deletion_end), pos);
}

#[test]
fn adjust_for_deletion_2() {
    // Position is after the deletion point, on the same line as insertion.
    let pos = Pos { line: 1, char: 20 };
    let deletion_start = Pos { line: 1, char: 5 };
    let deletion_end = Pos { line: 1, char: 15 };
    assert_eq!(
        pos.adjust_for_deletion(deletion_start, deletion_end),
        Pos { line: 1, char: 10 }
    );
}

#[test]
fn adjust_for_deletion_3() {
    // Position is after the deletion point, on another line.
    let pos = Pos { line: 2, char: 5 };
    let deletion_start = Pos { line: 1, char: 5 };
    let deletion_end = Pos { line: 1, char: 15 };
    assert_eq!(
        pos.adjust_for_deletion(deletion_start, deletion_end),
        Pos { line: 2, char: 5 }
    );
}

#[test]
fn adjust_for_deletion_4() {
    // Position is after the deletion point, on the same line.
    let pos = Pos { line: 2, char: 5 };
    let deletion_start = Pos { line: 1, char: 5 };
    let deletion_end = Pos { line: 2, char: 0 };
    assert_eq!(
        pos.adjust_for_deletion(deletion_start, deletion_end),
        Pos { line: 1, char: 5 }
    );
}
