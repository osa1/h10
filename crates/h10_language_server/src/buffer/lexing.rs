//! Functions related to full and incremental lexing of a buffer or change

#[cfg(test)]
mod tests;

mod token_split_iterator;

use crate::buffer::line::Token;
use crate::buffer::Buffer;
use crate::token::TokenKind;
use token_split_iterator::TokenSplitIterator;

use std::time::Instant;

impl Buffer {
    #[allow(unused)]
    pub fn lex_full(&mut self) {
        let t_start = Instant::now();

        let mut line_tokens: Vec<Vec<Token>> = vec![vec![]; self.len_lines() as usize];

        for token in TokenSplitIterator::new(self, 0, 0) {
            match token {
                Ok((token_line, token)) => {
                    line_tokens[token_line as usize].push(token);
                }
                Err(_) => break, // TODO
            }
        }

        for (line_idx, tokens) in line_tokens.into_iter().enumerate() {
            let line = &mut self.lines[line_idx];
            line.set_tokens(tokens);
        }

        let dt = t_start.elapsed();
        println!("Buffer::lex_full took {}ms", dt.as_millis());
    }

    // Incremental lex plan:
    //
    // - Only re-lex whole lines to keep things simple.
    // - Scan backwards from `line` to find a line that starts with whitespace.
    // - Scan until the line `line + n_lines_modified` is covered.
    // - Stop when we scan a line with identical tokens as before.
    // - Update tokens of scanned lines.

    /// Re-lex the buffer after a change in given `line` (zero based) and `col` (zero based).
    pub fn lex_incremental(&mut self, line: u32, col: u32, n_lines_modified: u32) {
        let t_start = Instant::now();

        let start_line = self.find_incremental_lex_start(line, col);
        debug_assert!(start_line <= line, "start_line={start_line}, line={line}");

        // Initial capacity somewhat random.
        let mut line_tokens: Vec<Vec<Token>> = Vec::with_capacity(n_lines_modified as usize * 2);

        let mut last_line = start_line;

        for token in TokenSplitIterator::new(self, start_line, 0) {
            match token {
                Ok((token_line, token)) => {
                    let vec_idx = (token_line - start_line) as usize;

                    if token_line != last_line {
                        if token_line > line + n_lines_modified
                            && line_tokens[vec_idx - 1]
                                == self.lines[token_line as usize - 1].tokens()
                        {
                            break;
                        } else {
                            last_line = token_line;
                        }
                    }

                    if line_tokens.len() <= vec_idx {
                        line_tokens.resize(vec_idx + 1, vec![]);
                    }

                    line_tokens[vec_idx].push(token);
                }
                Err(_) => break, // TODO
            }
        }

        for (line_idx, line_tokens) in line_tokens.into_iter().enumerate() {
            self.lines[line_idx + start_line as usize].set_tokens(line_tokens);
        }

        let _dt = t_start.elapsed();
    }

    /// Given a modification point (insertion or deletion), find the line and column of where to
    /// start re-lexing.
    ///
    /// Current implementation: scan back starting with the token at given position until finding a
    /// whitespace or reaching to the beginning of the document.
    fn find_incremental_lex_start(&self, mod_line: u32, mod_col: u32) -> u32 {
        for (line, token) in self.token_iter_rev(mod_line, mod_col) {
            if token.kind == TokenKind::Whitespace {
                if token.col == 0 {
                    return line;
                } else if line < mod_line
                    && token.col + token.length_cols == self.line_len_chars(line)
                {
                    return line + 1;
                }
            }
        }

        0
    }

    /// Reverse iterate buffer tokens starting from the token at the given position.
    fn token_iter_rev(&self, mut line: u32, mut col: u32) -> BufferTokenIter<'_> {
        let mut line_tokens = self.line(line).tokens();

        while line_tokens.is_empty() && line != 0 {
            line -= 1;
            line_tokens = self.line(line).tokens();
            col = u32::MAX;
        }

        let line_token_idx = match line_tokens.binary_search_by_key(&col, |token| token.col) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        BufferTokenIter {
            buffer: self,
            line,
            line_token_idx,
            done: line_tokens.is_empty(),
        }
    }
}

struct BufferTokenIter<'buffer> {
    buffer: &'buffer Buffer,
    line: u32,
    line_token_idx: usize,
    done: bool,
}

impl<'buffer> Iterator for BufferTokenIter<'buffer> {
    type Item = (u32, Token);

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let token = self.buffer.line(self.line).tokens()[self.line_token_idx];

        if self.line_token_idx == 0 {
            if self.line == 0 {
                self.done = true;
            } else {
                self.line -= 1;
                while self.line != 0 && self.buffer.line(self.line).tokens().is_empty() {
                    self.line -= 1;
                }
                if self.buffer.line(self.line).tokens().is_empty() {
                    self.done = true;
                    return None;
                }
                self.line_token_idx = self.buffer.line(self.line).tokens().len() - 1;
            }
        } else {
            self.line_token_idx -= 1;
        }

        Some((self.line, token))
    }
}
