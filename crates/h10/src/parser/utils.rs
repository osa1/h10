use crate::ast::{AstNode, Span};
use crate::layout_lexer::LayoutError;
use crate::parser::error::ErrorKind;
use crate::parser::{Parser, ParserResult};
use crate::token::TokenRef;
use h10_lexer::TokenKind;

use lexgen_util::{LexerError, Loc};

impl<'input> Parser<'input> {
    pub(super) fn in_explicit_layout(&self) -> bool {
        self.lexer.in_explicit_layout()
    }

    pub(super) fn pop_layout(&mut self) -> bool {
        self.lexer.pop_layout()
    }

    /// Get the next token without incrementing the lexer. This handles indentation/layout and
    /// returns virtual semicolons and braces when necessary.
    pub(super) fn peek_(&mut self) -> ParserResult<TokenRef> {
        let v: Result<TokenRef, LexerError<LayoutError>> = match &self.peeked {
            Some(v) => v.clone(),
            None => match self.lexer.next() {
                Some(Ok((l, v, r))) => {
                    let span = self.span(l, r);
                    let node = TokenRef::new(v, span);
                    self.peeked = Some(Ok(node.clone()));
                    Ok(node)
                }
                Some(Err(err)) => Err(err),
                None => return self.fail(Loc::default(), ErrorKind::UnexpectedEndOfInput),
            },
        };
        match v {
            Ok(token) => Ok(token),
            Err(lexer_error) => self.fail(lexer_error.location, ErrorKind::LexerError),
        }
    }

    /// Get the next token. This is the same as `peek`, but it increments the lexer.
    pub(super) fn next_(&mut self) -> ParserResult<TokenRef> {
        let v: Result<TokenRef, LexerError<LayoutError>> = match self.peeked.take() {
            Some(v) => v,
            None => match self.lexer.next() {
                Some(Ok((l, v, r))) => {
                    let span = self.span(l, r);
                    Ok(TokenRef::new(v, span))
                }
                Some(Err(err)) => Err(err),
                None => return self.fail(Loc::default(), ErrorKind::UnexpectedEndOfInput),
            },
        };
        match v {
            Ok(token) => {
                if let Some(last_tok) = self.last_tok.take() {
                    last_tok.set_next(Some(token.clone()));
                }

                self.last_tok = Some(token.clone());

                Ok(token)
            }
            Err(lexer_error) => self.fail(lexer_error.location, ErrorKind::LexerError),
        }
    }

    /// Get the next token without incrementing the lexer. This handles indentation/layout and
    /// returns virtual semicolons and braces when necessary.
    pub(super) fn peek(&mut self) -> ParserResult<(Loc, TokenKind, Loc)> {
        self.peek_()
            .map(|t| (t.span().start, t.token(), t.span().end))
    }

    /// Get the next token. This is the same as `peek`, but it increments the lexer.
    pub(super) fn next(&mut self) -> ParserResult<(Loc, TokenKind, Loc)> {
        self.next_()
            .map(|t| (t.span().start, t.token(), t.span().end))
    }

    pub(super) fn try_<A, F>(&mut self, f: F) -> ParserResult<A>
    where
        F: FnOnce(&mut Self) -> ParserResult<A>,
    {
        let parser = self.clone();
        match f(self) {
            Ok(val) => Ok(val),
            Err(err) => {
                *self = parser;
                Err(err)
            }
        }
    }

    pub(super) fn try_opt<A, F>(&mut self, f: F) -> Option<A>
    where
        F: FnOnce(&mut Self) -> Option<A>,
    {
        let parser = self.clone();
        match f(self) {
            Some(val) => Some(val),
            None => {
                *self = parser;
                None
            }
        }
    }

    pub(super) fn skip_token(&mut self, token: TokenKind) -> bool {
        if let Ok(t) = self.peek_() {
            if t.token() == token {
                self.skip();
                return true;
            }
        }
        false
    }

    pub(super) fn skip_token_string(&mut self, token: TokenKind) -> Option<String> {
        if let Ok(t) = self.peek_() {
            if t.token() == token {
                self.skip();
                return Some(t.text.borrow().to_owned());
            }
        }
        None
    }

    pub(super) fn skip_token_pred<F>(&mut self, token: TokenKind, pred: F) -> bool
    where
        F: Fn(&str) -> bool,
    {
        if let Ok(t) = self.peek_() {
            if t.token() == token && pred(&t.text.borrow()) {
                self.skip();
                return true;
            }
        }
        false
    }

    pub(super) fn skip_all(&mut self, token: TokenKind) {
        while self.skip_token(token) {}
    }

    pub(super) fn expect_token(&mut self, token: TokenKind) -> ParserResult<TokenRef> {
        self.expect_token_pred(|token_| token_ == token)
    }

    pub(super) fn expect_token_pred<F>(&mut self, pred: F) -> ParserResult<TokenRef>
    where
        F: Fn(TokenKind) -> bool,
    {
        let t = self.next_()?;
        if pred(t.token()) {
            Ok(t)
        } else {
            self.fail(t.span().start, ErrorKind::UnexpectedToken)
        }
    }

    pub(super) fn expect_token_string(
        &mut self,
        token: TokenKind,
    ) -> ParserResult<(Loc, String, Loc)> {
        self.expect_token(token)
            .map(|t| (t.span().start, t.text.borrow().to_owned(), t.span().end))
    }

    pub(super) fn expect_token_pred_string<F>(
        &mut self,
        pred: F,
    ) -> ParserResult<(Loc, String, Loc)>
    where
        F: Fn(TokenKind) -> bool,
    {
        self.expect_token_pred(pred)
            .map(|t| (t.span().start, t.text.borrow().to_owned(), t.span().end))
    }

    pub(super) fn skip(&mut self) {
        let ret = self.next();
        debug_assert!(ret.is_ok());
    }

    pub(super) fn last_tok_span(&self) -> (Loc, Loc) {
        let span = self.last_tok.as_ref().unwrap().span.borrow();
        (span.start, span.end)
    }
}

//
// Utilities to generate AST nodes.
//

impl<'input> Parser<'input> {
    pub(super) fn span(&self, l: Loc, r: Loc) -> Span {
        Span { start: l, end: r }
    }

    pub(super) fn spanned<T>(&self, l: Loc, r: Loc, node: T) -> AstNode<T> {
        AstNode {
            span: self.span(l, r),
            node,
        }
    }
}