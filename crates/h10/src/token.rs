#[cfg(test)]
mod tests;

use crate::arena::{Arena, Idx};
use crate::ast::Span;
use crate::collections::Set;
use crate::indentation_groups::IndentationGroup;
use crate::pos::Pos;

use arc_id::ArcId;
use h10_lexer::Token as LexerToken;
use h10_lexer::TokenKind as LexerTokenKind;

use std::fmt;
use std::ops::Deref;
use std::sync::Mutex;

use lexgen_util::Loc;
use smol_str::SmolStr;

/// Wraps lexer tokens in a shared reference to allow various information and linking to other
/// tokens.
///
/// Equality and hash are implemented based on reference equality.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenRef {
    node: ArcId<Token>,
}

impl Deref for TokenRef {
    type Target = Token;

    #[inline(always)]
    fn deref(&self) -> &Token {
        &self.node
    }
}

pub struct Token {
    /// The token kind.
    kind: LexerTokenKind,

    /// Span of the token.
    ///
    /// When the token is attached to an AST node (`ast_node`), line number is relative to the AST
    /// node.
    ///
    /// This is to avoid having to update line numbers of every token after inserting a new line.
    ///
    /// Column number is always absolute, so it can be obtained without a [`Arena<IndentationGroup>`], see
    /// [`Token::indentation`].
    span: Mutex<Span>,

    /// The previous token.
    prev: Mutex<Option<TokenRef>>,

    /// The next token.
    next: Mutex<Option<TokenRef>>,

    /// When this token is a part of an AST node, the reference to the node.
    ///
    /// Currently only update top-level declarations when re-parsing, so this holds an index to a
    /// top-level declaration. In the future we may want to hold references to arbitrary AST nodes
    /// to allow more fine-grained incremental updates.
    ast_node: Mutex<Option<Idx<IndentationGroup>>>,

    /// The token text.
    text: SmolStr,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("span", &self.span)
            // .field("prev", &self.prev)
            // .field("next", &self.next)
            .field("ast_node", &self.ast_node)
            .field("text", &self.text)
            .finish()
    }
}

impl TokenRef {
    pub fn new(token: LexerToken, span: Span) -> Self {
        Self {
            node: ArcId::new(Token::new(token, span)),
        }
    }

    pub fn from_lexer_token((start, t, end): (Loc, LexerToken, Loc)) -> Self {
        Self::new(
            t,
            Span {
                start: Pos::from_loc(&start),
                end: Pos::from_loc(&end),
            },
        )
    }

    pub fn kind(&self) -> LexerTokenKind {
        self.node.kind
    }

    pub fn text(&self) -> &str {
        self.text.as_str()
    }

    /// Get the span of the token, with line number relative to it's ast node.
    ///
    /// This span is useful to check if a token can be reused, during incremental lexing. When an
    /// indentation group is moved up or down, spans of the tokens do not change and they can be
    /// reused.
    pub fn span(&self) -> Span {
        self.node.span.lock().unwrap().clone()
    }

    /// Similar to [`span`], but the returned span will have absolute line numbers even when the
    /// token is a part of an AST.
    pub fn absolute_span(&self, arena: &Arena<IndentationGroup>) -> Span {
        let mut span = self.span();
        if let Some(ast_idx) = self.ast_node() {
            let ast = arena.get(ast_idx);
            span.start.line += ast.line_number(arena);
            span.end.line += ast.line_number(arena);
        }
        span
    }

    pub fn indentation(&self) -> u32 {
        self.span().start.char
    }

    /// Whether the token is the first token of a top-level declaration.
    pub fn is_first_token(&self, arena: &Arena<IndentationGroup>) -> bool {
        match self.ast_node() {
            Some(decl_idx) => &arena.get(decl_idx).first_token == self,
            None => false,
        }
    }

    /// Whether the token is the last token of a top-level declaration.
    pub fn is_last_token(&self, arena: &Arena<IndentationGroup>) -> bool {
        match self.ast_node() {
            Some(decl_idx) => &arena.get(decl_idx).last_token == self,
            None => false,
        }
    }

    pub fn set_next(&self, next: Option<TokenRef>) {
        *self.node.next.lock().unwrap() = next.clone();
        if let Some(next) = next {
            *next.node.prev.lock().unwrap() = Some(self.clone());
        }
    }

    pub fn next(&self) -> Option<TokenRef> {
        self.node.next.lock().unwrap().clone()
    }

    pub fn prev(&self) -> Option<TokenRef> {
        self.node.prev.lock().unwrap().clone()
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

    /// Returns an iterator that starting from the current token yields characters in the texts of
    /// tokens in the list.
    pub fn iter_chars(&self) -> impl Iterator<Item = char> + Clone {
        TokenCharIterator {
            token: Some(self.clone()),
            byte_idx: 0,
        }
    }

    pub fn clear_links(&self) {
        *self.node.next.lock().unwrap() = None;
        *self.node.prev.lock().unwrap() = None;
    }

    /// Set the AST node of the token and update span line numbers to make them relative to the AST
    /// node.
    pub fn set_ast_node(&self, node_idx: Idx<IndentationGroup>, arena: &Arena<IndentationGroup>) {
        let mut new_span = self.absolute_span(arena);
        *self.ast_node.lock().unwrap() = Some(node_idx);
        let ast_node_line = arena.get(node_idx).line_number(arena);
        new_span.start.line -= ast_node_line;
        new_span.end.line -= ast_node_line;
        *self.span.lock().unwrap() = new_span;
    }

    pub fn ast_node(&self) -> Option<Idx<IndentationGroup>> {
        *self.ast_node.lock().unwrap()
    }

    /// Whether the token contains the given location in the document.
    ///
    /// Note: [`pos`] is a position in the document, not in the token's AST node!
    ///
    /// Does not search next or previous tokens.
    pub fn contains_location(&self, pos: Pos, arena: &Arena<IndentationGroup>) -> bool {
        let span = self.absolute_span(arena);
        pos >= span.start && pos < span.end
    }

    pub fn check_loops(&self) {
        let mut tokens: Set<TokenRef> = Default::default();

        let mut token: TokenRef = self.clone();
        loop {
            let new = tokens.insert(token.clone());
            if !new {
                if &token == self {
                    panic!("Token {:?} linked to itself", token.kind());
                }

                // We've seen `token` twice, add tokens from `self` to the first occurrence of
                // `token`.
                let mut token_list: Vec<LexerTokenKind> = Vec::with_capacity(tokens.len());
                let mut debug_list_token = self.clone();
                while debug_list_token != token {
                    token_list.push(debug_list_token.kind());
                    debug_list_token = debug_list_token.next().unwrap();
                }

                // Add the loop.
                token_list.push(debug_list_token.kind());
                debug_list_token = debug_list_token.next().unwrap();
                while debug_list_token != token {
                    token_list.push(debug_list_token.kind());
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
        for token in self.iter() {
            token_text.push_str(token.text());
        }
        assert_eq!(token_text, expected);

        // Also check the char iterator.
        token_text.clear();
        token_text.extend(self.iter_chars());
        assert_eq!(token_text, expected);
    }
}

impl Token {
    fn new(token: LexerToken, span: Span) -> Self {
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
            kind: token.kind,
            span: Mutex::new(span),
            prev: Mutex::new(None),
            next: Mutex::new(None),
            ast_node: Mutex::new(None),
            text: token.text.clone(),
        }
    }
}

/// An iterator that yields tokens until the end of a list.
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

/// An iterator that yields tokens until a given token (including the given token).
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

/// Yields characters in a list of tokens.
//
// TODO: This could be made faster if we could store `Chars` of the `text`, but it's currently
// behind a `RefCell`.
#[derive(Clone)]
struct TokenCharIterator {
    /// The current token.
    token: Option<TokenRef>,

    /// The byte index in the current token's `text`.
    byte_idx: usize,
}

impl Iterator for TokenCharIterator {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let token = match &self.token {
            Some(token) => token,
            None => return None,
        };

        let text = token.text();

        if text.len() == self.byte_idx {
            self.token = token.next();
            self.byte_idx = 0;
            return self.next();
        }

        let char = text.get(self.byte_idx..).unwrap().chars().next().unwrap();

        self.byte_idx += char.len_utf8();

        Some(char)
    }
}
