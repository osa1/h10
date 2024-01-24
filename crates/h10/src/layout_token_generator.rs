#![allow(unused)]

#[cfg(test)]
mod tests;

use crate::ast::Span;
use crate::token::TokenRef;
use h10_lexer::{ReservedId, Special, Token, TokenKind};

use std::cmp::Ordering;

use lexgen_util::Loc;
use smol_str::SmolStr;

/// A [`TokenRef`] iterator that yields tokens in the argument, with extra layout tokens.
///
/// The arugment [`TokenRef`] list is not modified; layout tokens are not added to the list.
#[derive(Clone)]
pub struct LayoutTokenGenerator {
    /// The next token in the token list.
    token: Option<TokenRef>,

    /// Line number of the last non-whitespace token yielded.
    last_token_line: Option<u32>,

    /// The layout context.
    context: Vec<Context>,

    /// In `do_layout` mode we're expecting to see a `{` next. If we see anything else, depending
    /// on the context and the indentation of the next token we insert `{`, or `{` and a `}`.
    do_layout: bool,

    /// If we're inserting a `{}` in `do_layout` and returned the `{` part, this will be true and
    /// we'll return `}` in the next interation.
    override_next: Option<TokenRef>,
}

#[derive(Debug, Clone)]
pub struct LayoutError {
    pub loc: Loc,
    pub error: &'static str,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
enum Context {
    Explicit,
    Implicit(u32),
}

impl LayoutTokenGenerator {
    pub fn new_top_level(token: TokenRef) -> Self {
        LayoutTokenGenerator {
            token: Some(token),
            last_token_line: None,
            context: vec![Context::Implicit(0)],
            do_layout: false,
            override_next: Some(lbrace(Loc {
                line: 0,
                col: 0,
                byte_idx: 0,
            })),
        }
    }

    pub fn new(token: TokenRef) -> Self {
        LayoutTokenGenerator {
            token: Some(token),
            last_token_line: None,
            context: vec![],
            do_layout: false,
            override_next: None,
        }
    }

    pub fn pop_layout(&mut self) -> bool {
        match self.context.pop() {
            Some(Context::Explicit) | None => false,
            Some(Context::Implicit(_)) => true,
        }
    }

    pub fn in_explicit_layout(&self) -> bool {
        matches!(self.context.last(), Some(Context::Explicit))
    }
}

impl Iterator for LayoutTokenGenerator {
    type Item = Result<TokenRef, LayoutError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.override_next.take() {
            assert!(!self.do_layout);
            return Some(Ok(next));
        }

        let token = match &self.token {
            Some(token) => {
                if matches!(
                    token.token(),
                    TokenKind::Whitespace | TokenKind::Comment { .. },
                ) {
                    self.token = token.next();
                    return self.next();
                }
                token
            }
            None => {
                return if self.do_layout {
                    self.do_layout = false;
                    // End-of-input after a `do`, `of`, `where`, or `let`: insert `{}`.
                    let last_line = match self.last_token_line {
                        Some(last_line) => last_line,
                        None => return None, // empty file
                    };
                    let loc = Loc {
                        line: last_line + 1,
                        col: 0,
                        byte_idx: 0,
                    };
                    self.context.push(Context::Explicit);
                    Some(Ok(lbrace(loc)))
                } else {
                    // TODO: This should only terminate implicit layouts?
                    match self.context.pop() {
                        Some(_) => {
                            let last_line = self.last_token_line.unwrap();
                            let loc = Loc {
                                line: last_line + 1,
                                col: 0,
                                byte_idx: 0,
                            };
                            Some(Ok(rbrace(loc)))
                        }
                        None => None,
                    }
                };
            }
        };

        if matches!(token.token(), TokenKind::Special(Special::LBrace)) {
            // Explicit layout.
            self.do_layout = false;
            self.context.push(Context::Explicit);
            let token = token.clone();
            self.token = token.next();
            return Some(Ok(token));
        }

        let start = token.span().start;

        if self.do_layout {
            self.do_layout = false;

            // Implicit layout. The rules are:
            //
            // 1. L ({n} : ts) (m : ms) = '{' : L ts (n : m : ms)   -- if n > m
            //
            //    If the next token is indented more than the current layout, generate '{'.
            //
            // 2. L ({n} : ts) []       = '{' : L ts [n]
            //
            //    If the current context is empty, generate '{'.
            //
            // 3. L ({n} : ts) ms       = '{' : '}' : L (<n> : ts) ms
            //
            //    Otherwise create an empty context.

            // (1)
            let next_token_indent = start.col;
            if let Some(current_context) = self.context.last() {
                if Context::Implicit(next_token_indent) > *current_context {
                    self.context.push(Context::Implicit(next_token_indent));
                    self.last_token_line = None; // don't generate ';' immediately after
                    return Some(Ok(lbrace(start)));
                }
            }

            // (2)
            if self.context.is_empty() {
                self.context.push(Context::Implicit(next_token_indent));
                self.last_token_line = None; // don't generate ';' immediately after
                return Some(Ok(lbrace(start)));
            }

            // (3)
            self.override_next = Some(rbrace(start));
            return Some(Ok(lbrace(start)));
        }

        if matches!(token.token(), TokenKind::Special(Special::RBrace),) {
            // Pop explicit layout.
            match self.context.pop() {
                Some(Context::Explicit) => {}
                Some(Context::Implicit(_)) => {
                    return Some(Err(LayoutError {
                        loc: start,
                        error: "Right brace in implicit layout",
                    }))
                }
                None => {
                    return Some(Err(LayoutError {
                        loc: start,
                        error: "Right brace without a left brace",
                    }))
                }
            }
        }

        let beginning_of_line = self
            .last_token_line
            .map(|last_token_line| last_token_line != start.line)
            .unwrap_or(false);

        if beginning_of_line {
            // Moved to a new line.
            match self.context.last() {
                Some(Context::Explicit) => {
                    // No layout, don't generate layout tokens.
                    self.last_token_line = None;
                }
                Some(Context::Implicit(n)) => {
                    match start.col.cmp(n) {
                        Ordering::Less => {
                            // Next token indented less, insert `}`, close layout.
                            self.context.pop();
                            return Some(Ok(rbrace(start)));
                        }
                        Ordering::Equal => {
                            // Next token at the same indentation with layout, insert `;`.
                            self.last_token_line = None;
                            return Some(Ok(semic(start)));
                        }
                        Ordering::Greater => {
                            // Next token indented more.
                            self.last_token_line = None;
                        }
                    }
                }
                None => {
                    // Must be the first token?
                    self.last_token_line = None;
                }
            }
        }

        self.last_token_line = Some(start.line);
        let ret = token.clone();
        self.token = ret.next();

        self.do_layout = matches!(
            ret.token(),
            TokenKind::ReservedId(
                ReservedId::Do | ReservedId::Of | ReservedId::Where | ReservedId::Let
            ),
        );

        Some(Ok(ret))
    }
}

const LBRACE: Token = Token {
    kind: TokenKind::Special(Special::LBrace),
    text: SmolStr::new_inline("{"),
};

const RBRACE: Token = Token {
    kind: TokenKind::Special(Special::RBrace),
    text: SmolStr::new_inline("}"),
};

const SEMIC: Token = Token {
    kind: TokenKind::Special(Special::Semi),
    text: SmolStr::new_inline(";"),
};

fn lbrace(loc: Loc) -> TokenRef {
    TokenRef::new(
        LBRACE,
        Span {
            start: loc,
            end: virtual_token_end(&loc),
        },
    )
}

fn rbrace(loc: Loc) -> TokenRef {
    TokenRef::new(
        RBRACE,
        Span {
            start: loc,
            end: virtual_token_end(&loc),
        },
    )
}

fn semic(loc: Loc) -> TokenRef {
    TokenRef::new(
        SEMIC,
        Span {
            start: loc,
            end: virtual_token_end(&loc),
        },
    )
}

fn virtual_token_end(start: &Loc) -> Loc {
    Loc {
        line: start.line,
        col: start.col + 1,
        byte_idx: start.byte_idx, // not in input
    }
}
