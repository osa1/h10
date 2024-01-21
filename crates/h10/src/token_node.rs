#![allow(unused)]

use crate::ast::Span;
use crate::collections::Set;
use crate::layout_lexer::LayoutLexer_;

use h10_lexer::token::Token;
use rc_id::RcId;

use lexgen_util::Loc;

use std::cell::RefCell;
use std::ops::Deref;

/// Wraps lexer tokens in a shared reference to allow attaching them to AST nodes.
///
/// Equality and hash are implemented based on reference equality.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenNodeRef {
    node: RcId<TokenNode>,
}

impl Deref for TokenNodeRef {
    type Target = TokenNode;

    #[inline(always)]
    fn deref(&self) -> &TokenNode {
        &self.node
    }
}

#[derive(Debug)]
pub struct TokenNode {
    /// The token kind.
    token: Token,

    /// Span of the token.
    span: RefCell<Span>,

    /// The token after this one in the input.
    // TODO: Do we need the previous token?
    next: RefCell<Option<TokenNodeRef>>,

    /// The token text.
    pub text: RefCell<String>,
}

impl TokenNodeRef {
    pub fn new(token: Token, span: Span, text: String) -> Self {
        Self {
            node: RcId::new(TokenNode::new(token, span, text)),
        }
    }

    pub fn from_lexer_token(
        source: &str,
        (start, t, end): (Loc, Token, Loc),
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

    pub fn token(&self) -> Token {
        self.node.token
    }

    pub fn span(&self) -> Span {
        self.node.span.borrow().clone()
    }

    pub fn set_next(&self, next: TokenNodeRef) {
        assert_ne!(&next, self);
        *self.node.next.borrow_mut() = Some(next);
    }

    pub fn next(&self) -> Option<TokenNodeRef> {
        self.node.next.borrow().clone()
    }

    pub fn check_loops(&self) {
        let mut tokens: Set<TokenNodeRef> = Default::default();

        let mut token: TokenNodeRef = self.clone();
        loop {
            let new = tokens.insert(token.clone());
            if !new {
                if &token == self {
                    panic!("Token {:?} linked to itself", token.token());
                }

                // We've seen `token` twice, add tokens from `self` to the first occurrence of
                // `token`.
                let mut token_list: Vec<Token> = Vec::with_capacity(tokens.len());
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
        let mut token: Option<TokenNodeRef> = Some(self.clone());
        while let Some(token_) = token {
            token_text.extend(token_.deref().text.borrow().chars());
            token = token_.next();
        }
        assert_eq!(token_text, expected);
    }
}

impl TokenNode {
    fn new(token: Token, span: Span, text: String) -> Self {
        Self {
            token,
            span: RefCell::new(span),
            next: RefCell::new(None),
            text: RefCell::new(text),
        }
    }
}
