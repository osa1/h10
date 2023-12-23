#[cfg(test)]
mod tests;

use crate::lexer::Lexer;
use crate::token::{ReservedId, Special, Token};

use std::cmp::Ordering;
use std::convert::Infallible;
use std::iter::Peekable;

use lexgen_util::{LexerError, Loc};

/// A layout lexer is a lexer with two extra methods to allow handling implicit layouts when
/// parsing.
///
/// When parsing expression and types in tests (instead of whole modules), use `Lexer`, which
/// implements `LayoutLexer_` without actually handling implicit layouts.
pub trait LayoutLexer_:
    Iterator<Item = Result<(Loc, Token, Loc), LexerError<Infallible>>> + Clone
{
    fn pop_layout(&mut self) -> bool;
    fn in_explicit_layout(&self) -> bool;
}

impl<'input, I: Clone + Iterator<Item = char>> LayoutLexer_ for Lexer<'input, I> {
    fn pop_layout(&mut self) -> bool {
        false
    }

    fn in_explicit_layout(&self) -> bool {
        true
    }
}

impl<'input> LayoutLexer_ for LayoutLexer<'input, std::str::Chars<'input>> {
    fn pop_layout(&mut self) -> bool {
        self.pop_layout_()
    }

    fn in_explicit_layout(&self) -> bool {
        self.in_explicit_layout_()
    }
}

/// A wrapper on `Lexer` for handling layout.
#[derive(Clone)]
pub struct LayoutLexer<'input, I: Clone + Iterator<Item = char>> {
    lexer: Peekable<Lexer<'input, I>>,

    last_token_line: Option<u32>,

    context: Vec<i32>,

    /// In `do_layout` mode we're expecting to see a `{` next. If we see anything else, depending
    /// on the context and the indentation of the next token we insert `{`, or `{` and a `}`.
    do_layout: bool,

    /// If we're inserting a `{}` in `do_layout` and returned the `{` part, this will be true and
    /// we'll return `}` in the next interation.
    override_next: Option<(Loc, Token, Loc)>,
}

const LBRACE: Token = Token::Special(Special::LBrace);
const RBRACE: Token = Token::Special(Special::RBrace);
const SEMIC: Token = Token::Special(Special::Semi);

impl<'input> LayoutLexer<'input, std::str::Chars<'input>> {
    pub fn new(input: &'input str) -> Self {
        let mut lexer = Self {
            lexer: Lexer::new(input).peekable(),
            last_token_line: None,
            context: vec![],
            do_layout: false,
            override_next: None,
        };
        lexer.initialize();
        lexer
    }

    fn pop_layout_(&mut self) -> bool {
        match self.context.pop() {
            Some(-1) | None => false,
            Some(_) => true,
        }
    }

    fn in_explicit_layout_(&self) -> bool {
        self.context.last().copied() == Some(-1)
    }

    /// Initialize the context if the first token is not `module` or `{`.
    ///
    /// This implements the Haskell 2010 rule in section 10.3:
    ///
    /// > If the first lexeme of a module is not `{` or `module`, then it is preceded by `{n}`
    /// > where n is the indentation of the lexeme.
    fn initialize(&mut self) {
        match self.lexer.peek() {
            Some(Ok((
                _,
                Token::Special(Special::LBrace) | Token::ReservedId(ReservedId::Module),
                _,
            ))) => {}

            Some(Ok((start, _, _))) => {
                self.context.push(start.col as i32);
                self.override_next = Some((*start, LBRACE, virtual_token_end(start)));
            }

            _ => {}
        }
    }
}

impl<'input, I: Clone + Iterator<Item = char>> Iterator for LayoutLexer<'input, I> {
    type Item = Result<(Loc, Token, Loc), LexerError<Infallible>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.override_next.take() {
            assert!(!self.do_layout);
            return Some(Ok(next));
        }

        let (start, token, end) = match self.lexer.peek() {
            Some(Ok(tok)) => tok,
            Some(Err(_)) => return self.lexer.next(),
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
                    self.context.push(-1);
                    Some(Ok((loc, LBRACE, virtual_token_end(&loc))))
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
                            Some(Ok((loc, RBRACE, virtual_token_end(&loc))))
                        }
                        None => None,
                    }
                };
            }
        };

        if self.do_layout {
            self.do_layout = false;

            if matches!(token, Token::Special(Special::LBrace)) {
                // Explicit layout.
                self.context.push(-1);
                return self.lexer.next(); // consume '{'
            }

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
            let next_token_indent = start.col as i32;
            if let Some(current_context) = self.context.last() {
                if next_token_indent > *current_context {
                    self.context.push(next_token_indent);
                    self.last_token_line = None; // don't generate ';' immediately after
                    return Some(Ok((*start, LBRACE, virtual_token_end(start))));
                }
            }

            // (2)
            if self.context.is_empty() {
                self.context.push(next_token_indent);
                self.last_token_line = None; // don't generate ';' immediately after
                return Some(Ok((*start, LBRACE, virtual_token_end(start))));
            }

            // (3)
            self.override_next = Some((*start, RBRACE, virtual_token_end(start)));
            return Some(Ok((*start, LBRACE, virtual_token_end(start))));
        }

        let beginning_of_line = self
            .last_token_line
            .map(|last_token_line| last_token_line != start.line)
            .unwrap_or(false);

        if beginning_of_line {
            // Moved to a new line.
            match self.context.last() {
                Some(-1) => {
                    // No layout, don't generate layout tokens.
                    self.last_token_line = None;
                }
                Some(n) => {
                    match (start.col as i32).cmp(n) {
                        Ordering::Less => {
                            // Next token indented less, insert `}`, close layout.
                            self.context.pop();
                            return Some(Ok((*start, RBRACE, virtual_token_end(start))));
                        }
                        Ordering::Equal => {
                            // Next token at the same indentation with layout, insert `;`.
                            self.last_token_line = None;
                            return Some(Ok((*start, SEMIC, virtual_token_end(start))));
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
        let ret = (*start, token.clone(), *end);
        self.lexer.next(); // consume token

        self.do_layout = matches!(
            ret.1,
            Token::ReservedId(
                ReservedId::Do | ReservedId::Of | ReservedId::Where | ReservedId::Let
            ),
        );

        Some(Ok(ret))
    }
}

fn virtual_token_end(start: &Loc) -> Loc {
    Loc {
        line: start.line,
        col: start.col + 1,
        byte_idx: start.byte_idx, // not in input
    }
}
