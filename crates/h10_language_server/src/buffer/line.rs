use crate::token::TokenKind;

// TODO: Add a field for the indentation, drop leading whitespace from `text`.
#[derive(Debug)]
pub struct Line {
    /// The line contents. Does not include the newline at the end.
    text: String,

    /// Whether the line consist of single-byte characters.
    single_byte_chars: bool,

    /// Length of the string, in characters. When `single_byte_chars` is true, this is the same as
    /// `text.len()`.
    len_chars: u32,

    /// Tokens in the line.
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,

    /// Start column of the token in the line.
    pub col: u32,

    /// Length of the token, in columns.
    pub length_cols: u32,
}

const _: () = assert!(std::mem::size_of::<Token>() == 12);

// -----------------------------------------------------------------------------
// Creating a new line
// -----------------------------------------------------------------------------

impl Line {
    pub(super) fn new(text: String) -> Line {
        let (n_chars, single_byte) = len_chars(&text);
        let len_chars = n_chars;
        let single_byte_chars = single_byte;

        Line {
            text,
            single_byte_chars,
            len_chars,
            tokens: vec![],
        }
    }
}

// -----------------------------------------------------------------------------
// Manipulating lines
// -----------------------------------------------------------------------------

impl Line {
    /// Insert a string to the given position in the line.
    ///
    /// The inserted string (`str`) should not have line breaks.
    pub(super) fn insert_str(&mut self, char_idx: u32, str: &str) {
        debug_assert!(!str.chars().any(|c| c == '\n'));

        let byte_idx = self.char_byte_idx(char_idx);
        self.text.insert_str(byte_idx, str);

        let (n_chars, single_byte) = len_chars(str);
        let new_len_chars = self.len_chars + n_chars;
        self.len_chars = new_len_chars;
        self.single_byte_chars &= single_byte;
    }

    /// Append the current line with the given string.
    ///
    /// The string (`str`) should not have line breaks.
    ///
    /// This is a slightly more efficient version of `line.insert_str(line.len_chars(), ...)` as
    /// you don't have to calculate number of characters when appending.
    pub(super) fn append(&mut self, str: &str) {
        debug_assert!(!str.chars().any(|c| c == '\n'));

        if str.is_empty() {
            return;
        }

        self.text.push_str(str);

        let (n_chars, single_byte) = len_chars(str);
        let new_len_chars = self.len_chars + n_chars;
        self.len_chars = new_len_chars;
        self.single_byte_chars &= single_byte;
    }

    /// Removes and returns text starting from given character index. Useful for implementing
    /// inserting new lines.
    pub(super) fn remove_rest(&mut self, char_idx: u32) -> String {
        if char_idx == self.len_chars() {
            return String::new();
        }

        let byte_idx = self.char_byte_idx(char_idx);
        let rest = self.text[byte_idx..].to_owned();

        self.text.replace_range(byte_idx.., "");

        let (n_chars, single_byte) = len_chars(&self.text);
        self.len_chars = n_chars;
        self.single_byte_chars = single_byte;

        rest
    }

    /// Remove characters from `range_start` (zero-based, inclusive) to `range_end` (zero-based,
    /// exclusive).
    pub(super) fn remove_char_range(
        &mut self,
        removed_text: &mut String,
        range_start: u32, // inclusive
        range_end: u32,   // exclusive
    ) {
        if range_start == range_end {
            return;
        }

        let start_char_byte_idx = self
            .text
            .char_indices()
            .nth(range_start as usize)
            .map(|(idx, _)| idx)
            .unwrap_or_else(|| self.text.len());

        let end_char_byte_idx = self
            .text
            .char_indices()
            .nth(range_end as usize)
            .map(|(idx, _)| idx)
            .unwrap_or_else(|| self.text.len());

        removed_text.push_str(&self.text[start_char_byte_idx..end_char_byte_idx]);

        self.text
            .replace_range(start_char_byte_idx..end_char_byte_idx, "");

        let (n_chars, single_byte) = len_chars(&self.text);
        self.len_chars = n_chars;
        self.single_byte_chars = single_byte;
    }
}

/// Returns number of characters, and whether all characters are single byte.
///
/// We don't store the newline character in lines so this doesn't count it.
fn len_chars(s: &str) -> (u32, bool) {
    let mut n_chars = 0;
    let mut all_single_char = true;

    for char in s.chars() {
        n_chars += 1;
        if all_single_char && char.len_utf8() != 1 {
            all_single_char = false;
        }
    }

    (n_chars, all_single_char)
}

// -----------------------------------------------------------------------------
// Queries
// -----------------------------------------------------------------------------

impl Line {
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Get the token at column.
    pub fn token_at_col(&self, col: u32) -> Option<Token> {
        for token in &self.tokens {
            if token.col <= col && col < token.col + token.length_cols {
                return Some(*token);
            }
        }
        None
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn len_chars(&self) -> u32 {
        if cfg!(debug_assertions) {
            let (len, single_byte) = len_chars(&self.text);
            debug_assert_eq!(len, self.len_chars);
            debug_assert_eq!(single_byte, self.single_byte_chars);
        }

        self.len_chars
    }

    pub fn len_bytes(&self) -> usize {
        self.text.len()
    }

    /// Find byte index of Nth character in this line. `O(n)` operation when the string has
    /// multi-byte characters.
    pub fn char_byte_idx(&self, n: u32) -> usize {
        if self.single_byte_chars {
            debug_assert!(self.text.chars().all(|c| c.len_utf8() == 1));
            n as usize
        } else {
            match self.text.char_indices().nth(n as usize) {
                Some((byte_idx, _)) => byte_idx,
                None => {
                    debug_assert_eq!(n, self.len_chars());
                    self.text.len()
                }
            }
        }
    }

    pub fn nth_char(&self, n: u32) -> char {
        debug_assert!(n <= self.len_chars);
        if n == self.len_chars {
            '\n'
        } else if self.single_byte_chars {
            char::from(self.text.as_bytes()[n as usize])
        } else {
            self.text.chars().nth(n as usize).unwrap_or('\n')
        }
    }

    pub fn byte_idx_to_char_idx(&self, byte_idx: usize) -> u32 {
        for (char_idx, (char_byte_idx, _)) in self.text.char_indices().enumerate() {
            if char_byte_idx == byte_idx {
                return char_idx as u32;
            }
        }
        panic!(
            "byte_idx_to_char: no char at byte index (text={:?}, byte_idx={})",
            self.text, byte_idx
        )
    }

    pub fn token_text(&self, token: &Token) -> &str {
        let token_col = token.col;
        let token_n_chars = token.length_cols;
        let token_start = self.char_byte_idx(token_col);
        let token_end = self.char_byte_idx(token_col + token_n_chars);

        // TODO HACK FIXME: It looks like `TokenSplitIterator` includes the virtual newline
        // character in tokens sometimes, ignore it.
        let token_end = std::cmp::min(token_end, self.text.len());
        &self.text[token_start..token_end]
    }

    pub(super) fn set_tokens(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
    }
}
