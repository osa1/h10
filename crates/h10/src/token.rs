#[cfg(test)]
mod tests;

use crate::ast::Span;
use crate::collections::Set;
use crate::decl_arena::DeclIdx;

use h10_lexer::token::Token as LexerToken;
use rc_id::RcId;

use lexgen_util::Loc;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::ops::Deref;

/// Wraps lexer tokens in a shared reference to allow various information and linking to other
/// tokens.
///
/// Equality and hash are implemented based on reference equality.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenRef {
    node: RcId<Token>,
}

impl Deref for TokenRef {
    type Target = Token;

    #[inline(always)]
    fn deref(&self) -> &Token {
        &self.node
    }
}

#[derive(Debug)]
pub struct Token {
    /// The token kind.
    token: LexerToken,

    /// Span of the token.
    ///
    /// When the token is attached to an AST node (`ast_node`), line number is relative to the AST
    /// node.
    ///
    /// Column number is also relative, but it's also absolute as top-level groups always start at
    /// column 0.
    pub span: RefCell<Span>,

    /// The previous token.
    prev: RefCell<Option<TokenRef>>,

    /// The next token.
    next: RefCell<Option<TokenRef>>,

    /// When this token is a part of an AST node, the reference to the node.
    ///
    /// Currently only update top-level declarations when re-parsing, so this holds an index to a
    /// top-level declaration. In the future we may want to hold references to arbitrary AST nodes
    /// to allow more fine-grained incremental updates.
    ast_node: RefCell<Option<DeclIdx>>,

    /// The token text.
    pub text: RefCell<String>,
}

impl TokenRef {
    pub fn new(token: LexerToken, span: Span, text: String) -> Self {
        Self {
            node: RcId::new(Token::new(token, span, text)),
        }
    }

    pub fn from_lexer_token(
        source: &str,
        (start, t, end): (Loc, LexerToken, Loc),
        lexer_input: &str,
    ) -> Self {
        Self::new(
            t,
            Span {
                source: source.into(),
                start,
                end,
            },
            lexer_input[start.byte_idx..end.byte_idx].to_owned(),
        )
    }

    pub fn token(&self) -> LexerToken {
        self.node.token
    }

    pub fn span(&self) -> Span {
        self.node.span.borrow().clone()
    }

    pub fn set_next(&self, next: Option<TokenRef>) {
        *self.node.next.borrow_mut() = next.clone();
        if let Some(next) = next {
            *next.node.prev.borrow_mut() = Some(self.clone());
        }
    }

    pub fn next(&self) -> Option<TokenRef> {
        self.node.next.borrow().clone()
    }

    pub fn prev(&self) -> Option<TokenRef> {
        self.node.prev.borrow().clone()
    }

    /// Returns an iterator that iterates all tokens starting from `self` until the end of the
    /// list.
    pub fn iter(&self) -> impl Iterator<Item = TokenRef> {
        TokenIterator {
            current: Some(self.clone()),
        }
    }

    /// Returns an iterator that iterates all tokens starting from `sel`, until `last`, including
    /// `last`.
    pub fn iter_until(&self, last: &TokenRef) -> impl Iterator<Item = TokenRef> {
        TokenUntilIterator {
            current: Some(self.clone()),
            last: last.clone(),
        }
    }

    pub fn clear_links(&self) {
        *self.node.next.borrow_mut() = None;
        *self.node.prev.borrow_mut() = None;
    }

    pub fn set_ast_node(&self, node_idx: DeclIdx) {
        *self.ast_node.borrow_mut() = Some(node_idx);
    }

    pub fn ast_node(&self) -> Option<DeclIdx> {
        *self.ast_node.borrow()
    }

    /// Whether the token contains the given location.
    ///
    /// Does not search next or previous tokens.
    pub fn contains_location(&self, line: u32, char: u32) -> bool {
        let pos = Pos::new(line, char);
        self.contains_location_(pos)
    }

    fn contains_location_(&self, pos: Pos) -> bool {
        let span = self.span();
        let start = Pos::from_loc(&span.start);
        let end = Pos::from_loc(&span.end);

        pos >= start && pos < end
    }

    /// Starting from `self` find the token that contains the character at position
    /// `(range_start_line, range_start_char)`, then remove token contents and whole tokens until
    /// `(range_end_line, `range_end_char`).
    ///
    /// Returns the new head of the list. Returns `None` when the whole token list is removed.
    //
    // TODO: Update spans.
    pub fn remove_range(
        &self,
        range_start_line: u32,
        range_start_char: u32,
        range_end_line: u32,
        range_end_char: u32,
    ) -> Option<TokenRef> {
        let range_start = Pos::new(range_start_line, range_start_char);
        let range_end = Pos::new(range_end_line, range_end_char);

        // Find the start token.
        let mut start_token = self.clone();
        while !start_token.contains_location_(range_start) {
            match start_token.next() {
                Some(next_token) => start_token = next_token,
                None => panic!(
                    "Location ({}, {}) are not reachable from the token at {}:{}",
                    range_start_line,
                    range_start_char,
                    self.span().start.line,
                    self.span().start.col,
                ),
            }
        }

        // Find the end token.
        let mut end_token = start_token.clone();
        while !end_token.contains_location_(range_end) {
            match end_token.next() {
                Some(next_token) => end_token = next_token,
                None => panic!(
                    "Location ({}, {}) are not reachable from the token at {}:{}",
                    range_end_line,
                    range_end_char,
                    self.span().start.line,
                    self.span().start.col,
                ),
            }
        }

        // Remove tokens and text in the range: some part of the first token, all of the tokens in
        // between, and some part of the last token.
        start_token.remove_range_self(range_start, range_end);

        if start_token == end_token {
            if start_token.text.borrow().is_empty() {
                if let Some(prev) = start_token.prev() {
                    prev.set_next(start_token.next());
                }
                let new_head = start_token.next();
                start_token.clear_links();
                new_head
            } else {
                Some(start_token)
            }
        } else {
            // Remove tokens in between.
            let mut token = start_token.next().unwrap();
            start_token.set_next(Some(end_token.clone()));

            while token != end_token {
                let next = token.next();
                token.clear_links();
                match next {
                    Some(next) => token = next,
                    None => break,
                }
            }

            // Remove part of the last token.
            end_token.remove_range_self(range_start, range_end);

            if end_token.text.borrow().is_empty() {
                start_token.set_next(end_token.next());
                end_token.clear_links();
            }

            Some(start_token)
        }
    }

    /// Remove the given range from the current token only. Does not update prev/next tokens.
    fn remove_range_self(&self, range_start: Pos, range_end: Pos) {
        let start = std::cmp::max(range_start, Pos::from_loc(&self.span().start));
        let end = std::cmp::min(range_end, Pos::from_loc(&self.span().end));

        let mut pos = Pos::from_loc(&self.span().start);
        let text = self.text.borrow();
        let mut iter = text.char_indices();
        let mut byte_idx = 0;

        while pos != start {
            let (byte_idx_, char) = iter.next().unwrap();
            byte_idx = byte_idx_;

            if char == '\n' {
                pos.line += 1;
                pos.char = 0;
            } else {
                pos.char += 1;
            }
        }

        let start_byte_idx = byte_idx;

        while pos != end {
            let (byte_idx_, char) = iter.next().unwrap();
            byte_idx = byte_idx_;

            if char == '\n' {
                pos.line += 1;
                pos.char = 0;
            } else {
                pos.char += 1;
            }
        }

        let end_byte_idx = byte_idx;

        drop(text);

        self.text.borrow_mut().drain(start_byte_idx..end_byte_idx);
    }

    /// Starting from the current token find the token with the given position and insert the text.
    ///
    /// Updates the span of the updated token and spans of the rest of the tokens.
    //
    // TODO: Update spans.
    pub fn insert(&self, line: u32, char: u32, text: &str) {
        let insert_pos = Pos::new(line, char);

        let mut token = self.clone();
        while !token.contains_location_(insert_pos) {
            match token.next() {
                Some(next_token) => token = next_token,
                None => panic!(
                    "Location ({}, {}) are not reachable from the token at {}:{}",
                    line,
                    char,
                    self.span().start.line,
                    self.span().start.col,
                ),
            }
        }

        let mut token_pos = Pos::from_loc(&token.span().start);
        let token_text = token.text.borrow();
        let mut iter = token_text.char_indices();

        while token_pos != insert_pos {
            let (_, char) = iter.next().unwrap();

            if char == '\n' {
                token_pos.line += 1;
                token_pos.char = 0;
            } else {
                token_pos.char += 1;
            }
        }

        let byte_idx = iter.next().unwrap().0;

        drop(token_text);

        token.text.borrow_mut().insert_str(byte_idx, text);
    }

    pub fn check_loops(&self) {
        let mut tokens: Set<TokenRef> = Default::default();

        let mut token: TokenRef = self.clone();
        loop {
            let new = tokens.insert(token.clone());
            if !new {
                if &token == self {
                    panic!("Token {:?} linked to itself", token.token());
                }

                // We've seen `token` twice, add tokens from `self` to the first occurrence of
                // `token`.
                let mut token_list: Vec<LexerToken> = Vec::with_capacity(tokens.len());
                let mut debug_list_token = self.clone();
                while debug_list_token != token {
                    token_list.push(debug_list_token.token());
                    debug_list_token = debug_list_token.next().unwrap();
                }

                // Add the loop.
                token_list.push(debug_list_token.token());
                debug_list_token = debug_list_token.next().unwrap();
                while debug_list_token != token {
                    token_list.push(debug_list_token.token());
                    debug_list_token = debug_list_token.next().unwrap();
                }

                panic!(
                    "Token loop: {}",
                    token_list
                        .into_iter()
                        .map(|t| format!("{:?}", t))
                        .collect::<Vec<String>>()
                        .join(", ")
                );
            }
            match token.next() {
                Some(next) => token = next,
                None => break,
            }
        }
    }

    /// For testing. Checks that the texts of tokens starting with `self` make the expected string
    /// `text`. This cna be used to make sure lexing does not miss any characters.
    pub fn check_token_str(&self, expected: &str) {
        // Check that the token texts make the original program.
        let mut token_text = String::with_capacity(expected.len());
        let mut token: Option<TokenRef> = Some(self.clone());
        while let Some(token_) = token {
            token_text.extend(token_.deref().text.borrow().chars());
            token = token_.next();
        }
        assert_eq!(token_text, expected);
    }
}

impl Token {
    fn new(token: LexerToken, span: Span, text: String) -> Self {
        /*
        TODO: Figure out why this is failing.
        if cfg!(debug_assertions) {
            let mut pos = Pos::from_loc(&span.start);
            for char in text.chars() {
                if char == '\n' {
                    pos.line += 1;
                    pos.char = 0;
                } else {
                    pos.char += 1;
                }
            }
            assert_eq!(pos, Pos::from_loc(&span.end));
        }
        */

        Self {
            token,
            span: RefCell::new(span),
            prev: RefCell::new(None),
            next: RefCell::new(None),
            ast_node: RefCell::new(None),
            text: RefCell::new(text),
        }
    }
}

struct TokenIterator {
    current: Option<TokenRef>,
}

impl Iterator for TokenIterator {
    type Item = TokenRef;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.current.take();
        if let Some(item) = &item {
            self.current = item.next();
        }
        item
    }
}

struct TokenUntilIterator {
    current: Option<TokenRef>,
    last: TokenRef,
}

impl Iterator for TokenUntilIterator {
    type Item = TokenRef;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.current.take();
        if let Some(item) = &item {
            if item != &self.last {
                self.current = item.next();
            }
        }
        item
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Pos {
    line: u32,
    char: u32,
}

impl Pos {
    fn new(line: u32, char: u32) -> Self {
        Self { line, char }
    }

    fn from_loc(loc: &Loc) -> Self {
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
