use crate::buffer::Line;
use crate::rc_id::RcId;

use std::mem::{replace, take};

use tower_lsp::lsp_types::Position;

pub struct IndentationGroups<A> {
    /// Non-overlapping indentation groups sorted by `start_line_idx`.
    groups: Vec<IndentationGroup<A>>,
}

#[derive(Debug)]
struct IndentationGroup<A> {
    /// Line position of the first line in the group.
    first_line: RcId<Line>,

    /// Number of lines in the group, including the trailing whitespace lines.
    num_lines: u32,

    /// The data associated with the group (e.g. defs and uses).
    data: Option<A>,
}

impl<A> Default for IndentationGroup<A> {
    fn default() -> Self {
        Self {
            start_line_idx: 0,
            num_lines: 0,
            data: None,
        }
    }
}
