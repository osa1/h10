#[cfg(test)]
mod tests;

use crate::ast::Span;
use crate::collections::Set;
use crate::decl_arena::DeclIdx;
use crate::pos::Pos;

use h10_lexer::token::Token as LexerToken;
use rc_id::RcId;

use lexgen_util::Loc;

use std::cell::RefCell;
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
    pub fn contains_location(&self, pos: Pos) -> bool {
        let span = self.span();
        let start = Pos::from_loc(&span.start);
        let end = Pos::from_loc(&span.end);

        pos >= start && pos < end
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
