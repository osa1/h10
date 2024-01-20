#![allow(unused)]

use crate::ast::Span;
use crate::layout_lexer::LayoutLexer_;

use h10_lexer::token::Token;
use rc_id::RcId;

use std::cell::RefCell;

/// Wraps lexer tokens in a shared reference to allow attaching them to AST nodes.
///
/// Equality is implemented based on reference equality.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenNodeRef {
    node: RcId<TokenNode>,
}

#[derive(Debug)]
pub struct TokenNode {
    /// The token kind.
    token: Token,

    /// Span of the token.
    span: RefCell<Span>,

    /// The token after this one in the input.
    next: RefCell<Option<TokenNodeRef>>,
    // TODO: Do we need the previous token?

    // TODO: Do we need a reference to the AST node? I think probably not: all AST nodes have spans
    // so we know starting from the root which declaration node needs to be marked as "dirty" by
    // just comparing the modified span with the node's span.
}

impl TokenNodeRef {
    pub fn new(token: Token, span: Span) -> Self {
        Self {
            node: RcId::new(TokenNode::new(token, span)),
        }
    }

    pub fn token(&self) -> Token {
        self.node.token
    }

    pub fn span(&self) -> Span {
        self.node.span.borrow().clone()
    }

    pub fn set_next(&self, next: TokenNodeRef) {
        *self.node.next.borrow_mut() = Some(next);
    }
}

impl TokenNode {
    fn new(token: Token, span: Span) -> Self {
        Self {
            token,
            span: RefCell::new(span),
            next: RefCell::new(None),
        }
    }
}
