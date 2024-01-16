#![allow(unused)]

mod line;

#[cfg(test)]
mod tests;

use line::Line;

use std::iter::Rev;
use std::str::Chars;

use tower_lsp::lsp_types::Position;

#[derive(Debug)]
pub struct Buffer {
    lines: Vec<Line>,
}

// -----------------------------------------------------------------------------
// Creating a new buffer.
// -----------------------------------------------------------------------------

impl Buffer {
    pub fn new() -> Self {
        Self {
            lines: vec![Line::new(String::new())],
        }
    }

    pub fn with_contents(contents: &str) -> Self {
        let mut buffer = Self::new();
        buffer.insert(
            Position {
                line: 0,
                character: 0,
            },
            contents,
        );
        buffer
    }
}

// -----------------------------------------------------------------------------
// Manipulating buffers
// -----------------------------------------------------------------------------

impl Buffer {
    pub fn insert(&mut self, pos: Position, str: &str) {
        let line = pos.line;
        let col = pos.character;

        // Delete rest of the current line, insert it after `str`.
        let rest = self.lines[line as usize].remove_rest(col);

        // Number of lines inserted so far.
        let mut lines_inserted = 0;

        // Number of characters in the first inserted line.
        let mut first_line_len_chars: u32 = 0;

        // Number of characters in the last inserted line. Only set when inserting more than one
        // line (i.e. when there's at least one '\n' in `str`).
        let mut last_line_len_chars: u32 = 0;

        let mut lines = str.split('\n').peekable();
        while let Some(str_line) = lines.next() {
            if lines_inserted == 0 {
                self.lines[line as usize].append(str_line);
                first_line_len_chars = str_line.chars().count() as u32;
            } else {
                if lines.peek().is_none() {
                    last_line_len_chars = str_line.chars().count() as u32;
                }
                self.lines.insert(
                    (line + lines_inserted) as usize,
                    Line::new(str_line.to_owned()),
                );
            }
            lines_inserted += 1;
        }

        // N '\n' chars = N+1 lines.
        lines_inserted -= 1;

        self.lines[(line + lines_inserted) as usize].append(&rest);
    }

    /// Remove the range between two cursors (inclusive).
    pub fn remove(&mut self, removed_text: &mut String, start: Position, end: Position) {
        debug_assert!(removed_text.is_empty());

        debug_assert!(start <= end, "start={:?}, end={:?}", start, end);

        if start.line == end.line {
            self.remove_single_line(removed_text, start.line, start.character, end.character);
        } else {
            self.remove_multiple_lines(
                removed_text,
                start.line,
                start.character,
                end.line,
                end.character,
            );
        }
    }

    fn remove_single_line(
        &mut self,
        removed_text: &mut String,
        line: u32,
        col_start: u32,
        col_end: u32,
    ) {
        debug_assert!(
            col_start <= col_end,
            "col_start: {col_start}, col_end: {col_end}"
        );

        let line_len_chars = self.line_len_chars(line);
        debug_assert!(col_end <= line_len_chars);

        let remove_newline = col_end == line_len_chars;

        if remove_newline {
            if col_start != col_end {
                // col_end is virtual newline.
                self.remove_char_range_in_line(removed_text, line, col_start, col_end - 1);
            }
            self.join(line);
            removed_text.push('\n');
        } else {
            self.remove_char_range_in_line(removed_text, line, col_start, col_end);
        }
    }

    fn remove_multiple_lines(
        &mut self,
        removed_text: &mut String,
        line_start: u32,
        col_start: u32,
        line_end: u32,
        col_end: u32,
    ) {
        // Remove deleted part of the first line.
        let first_line_len_chars = self.line_len_chars(line_start);
        if col_start != first_line_len_chars {
            self.remove_char_range_in_line(
                removed_text,
                line_start,
                col_start,
                first_line_len_chars - 1,
            );
        }

        removed_text.push('\n');

        // Remove lines in the middle.
        for removed_line in self.lines.drain(line_start as usize + 1..line_end as usize) {
            removed_text.push_str(removed_line.text());
            removed_text.push('\n');
        }

        let n_removed_lines = line_end - line_start - 1;

        // Remove deleted part of the last line. Join the next line if virtual end-of-line is
        // included in selection.
        let last_line_idx = line_end - n_removed_lines;
        let _joined = if col_end == self.line_len_chars(last_line_idx) {
            if col_end == 0 {
                removed_text.push_str(self.remove_line(last_line_idx).text());
            } else {
                // col_end is virtual newline.
                self.remove_char_range_in_line(removed_text, last_line_idx, 0, col_end - 1);
                self.join(last_line_idx);
            }
            removed_text.push('\n');
            true
        } else {
            self.remove_char_range_in_line(removed_text, last_line_idx, 0, col_end);
            false
        };

        // Join first and last lines.
        let _first_line_chars = self.line_len_chars(line_start);
        let last_line = self.remove_line(last_line_idx);
        self.lines[line_start as usize].insert_str(col_start, last_line.text());
    }

    fn remove_line(&mut self, line: u32) -> Line {
        self.lines.remove(line as usize)
    }

    /// Join the given line with the next.
    fn join(&mut self, line: u32) {
        if line == self.len_lines() - 1 {
            return;
        }

        let next = self.remove_line(line + 1);
        self.lines[line as usize].append(next.text());
    }

    /// INVARIANT: Line is not empty. Range is inclusive.
    fn remove_char_range_in_line(
        &mut self,
        removed_text: &mut String,
        line: u32,
        range_start: u32, // inclusive
        range_end: u32,   // inclusive
    ) {
        self.lines[line as usize].remove_char_range(removed_text, range_start, range_end);
    }
}

// -----------------------------------------------------------------------------
// Query methods, stuff for rendering
// -----------------------------------------------------------------------------

impl Buffer {
    pub fn line(&self, line: u32) -> &Line {
        &self.lines[line as usize]
    }

    pub fn line_text(&self, line: u32) -> &str {
        self.lines[line as usize].text()
    }

    /// Get character at given line and column. When the column is at the end of the line this
    /// function returns `'\n`.
    pub fn get_char(&self, pos: Position) -> char {
        self.lines[pos.line as usize].nth_char(pos.character)
    }

    pub fn lines(&self) -> &[Line] {
        &self.lines
    }

    pub fn len_lines(&self) -> u32 {
        self.lines.len() as u32
    }

    /// Number of characters in a line. Does not include implicit newlines.
    pub fn line_len_chars(&self, line: u32) -> u32 {
        self.lines[line as usize].len_chars()
    }

    fn line_len_bytes(&self, line: u32) -> usize {
        self.lines[line as usize].len_bytes()
    }
}

// -----------------------------------------------------------------------------
// Iterating characters from given line and column
// -----------------------------------------------------------------------------

/// Character iterator
#[derive(Debug, Clone)]
pub struct CharIter<'a> {
    lines: &'a [Line],
    line_idx: u32,
    iter: Chars<'a>,
}

impl<'a> Iterator for CharIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self.iter.next() {
            Some(char) => Some(char),
            None => {
                if self.line_idx as usize >= self.lines.len() - 1 {
                    return None;
                }

                self.line_idx += 1;
                let line = &self.lines[self.line_idx as usize];

                self.iter = line.text().chars();
                Some('\n')
            }
        }
    }
}

/// Reverse character iterator
#[derive(Debug, Clone)]
pub struct CharRevIter<'a> {
    lines: &'a [Line],
    line_idx: u32,
    iter: Rev<std::str::Chars<'a>>,
}

impl<'a> Iterator for CharRevIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self.iter.next() {
            Some(char) => Some(char),
            None => {
                if self.line_idx == 0 {
                    return None;
                }

                self.line_idx -= 1;
                let line = &self.lines[self.line_idx as usize];

                self.iter = line.text().chars().rev();
                Some('\n')
            }
        }
    }
}

impl Buffer {
    /// Iterate characters in the buffer, starting from given position. Also yields newline
    /// characters.
    pub fn iter_from(&self, line: u32, col: u32) -> CharIter {
        assert!(!self.lines.is_empty());

        let mut iter = self.lines[line as usize].text().chars();
        for _ in 0..col {
            // TODO: Is there a shorthand for this?
            iter.next();
        }

        CharIter {
            lines: &self.lines,
            line_idx: line,
            iter,
        }
    }

    /// Iterate characters in the buffer in reverse, starting from given line and column
    #[allow(unused)]
    fn iter_rev_from(&self, line: u32, col: u32) -> CharRevIter {
        assert!(!self.lines.is_empty());

        let mut byte_iter = self.lines[line as usize].text().char_indices();
        if col > 0 {
            for _ in 0..col {
                byte_iter.next();
            }
        }

        let byte_idx = byte_iter
            .next()
            .map(|(idx, _)| idx)
            .unwrap_or_else(|| self.line_len_bytes(line));

        CharRevIter {
            lines: &self.lines,
            line_idx: line,
            iter: self.lines[line as usize].text()[..byte_idx].chars().rev(),
        }
    }

    /// Same as `iter_rev_from`, but column is the last column of the line.
    #[allow(unused)]
    pub fn iter_rev_from_line_end(&self, line: u32) -> CharRevIter {
        assert!(!self.lines.is_empty());

        CharRevIter {
            lines: &self.lines,
            line_idx: line,
            iter: self.lines[line as usize].text().chars().rev(),
        }
    }
}
