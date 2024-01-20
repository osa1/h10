use crate::ast::{AstNode, Span};
use crate::layout_lexer::LayoutLexer_;
use crate::parser::error::ErrorKind;
use crate::parser::{Parser, ParserResult};
use h10_lexer::token::Token;

use lexgen_util::Loc;

impl<'input, L: LayoutLexer_> Parser<'input, L> {
    pub(super) fn in_explicit_layout(&self) -> bool {
        self.lexer.in_explicit_layout()
    }

    pub(super) fn pop_layout(&mut self) -> bool {
        self.lexer.pop_layout()
    }

    /// Get the next token without incrementing the lexer. This handles indentation/layout and
    /// returns virtual semicolons and braces when necessary.
    pub(super) fn peek(&mut self) -> ParserResult<(Loc, Token, Loc)> {
        let v = match &self.peeked {
            Some(v) => v.clone(),
            None => match self.lexer.next() {
                Some(v) => {
                    self.peeked = Some(v.clone());
                    v
                }
                None => return self.fail(Loc::default(), ErrorKind::UnexpectedEndOfInput),
            },
        };
        match v {
            Ok(token) => Ok(token),
            Err(lexer_error) => self.fail(lexer_error.location, ErrorKind::LexerError),
        }
    }

    /// Get the next token. This is the same as `peek`, but it increments the lexer.
    pub(super) fn next(&mut self) -> ParserResult<(Loc, Token, Loc)> {
        let v = match self.peeked.take() {
            Some(v) => v,
            None => match self.lexer.next() {
                Some(v) => v,
                None => return self.fail(Loc::default(), ErrorKind::UnexpectedEndOfInput),
            },
        };
        match v {
            Ok((l, v, r)) => {
                self.last_tok_span = (l, r);
                Ok((l, v, r))
            }
            Err(lexer_error) => self.fail(lexer_error.location, ErrorKind::LexerError),
        }
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

    pub(super) fn skip_token(&mut self, token: Token) -> bool {
        match self.peek() {
            Ok((_l, token_, _r)) if token_ == token => {
                self.skip();
                true
            }
            _ => false,
        }
    }

    #[allow(unused)]
    pub(super) fn skip_token_loc(&mut self, token: Token) -> Option<(Loc, Loc)> {
        match self.peek() {
            Ok((l, token_, r)) if token_ == token => {
                self.skip();
                Some((l, r))
            }
            _ => None,
        }
    }

    pub(super) fn skip_token_string(&mut self, token: Token) -> Option<String> {
        match self.peek() {
            Ok((l, token_, r)) if token_ == token => {
                self.skip();
                Some(self.string(l, r))
            }
            _ => None,
        }
    }

    pub(super) fn skip_token_pred<F>(&mut self, token: Token, pred: F) -> bool
    where
        F: Fn(&str) -> bool,
    {
        match self.peek() {
            Ok((l, token_, r)) if token_ == token && pred(self.str(l, r)) => {
                self.skip();
                true
            }
            _ => false,
        }
    }

    pub(super) fn skip_all(&mut self, token: Token) {
        while self.skip_token(token) {}
    }

    pub(super) fn expect_token(&mut self, token: Token) -> ParserResult<(Loc, Loc)> {
        self.expect_token_pred(|token_| token_ == token)
    }

    pub(super) fn expect_token_pred<F>(&mut self, pred: F) -> ParserResult<(Loc, Loc)>
    where
        F: Fn(Token) -> bool,
    {
        let (l, next_token, r) = self.next()?;
        if pred(next_token) {
            Ok((l, r))
        } else {
            self.fail(l, ErrorKind::UnexpectedToken)
        }
    }

    pub(super) fn expect_token_string(&mut self, token: Token) -> ParserResult<(Loc, String, Loc)> {
        self.expect_token(token)
            .map(|(l, r)| (l, self.string(l, r), r))
    }

    pub(super) fn expect_token_pred_string<F>(
        &mut self,
        pred: F,
    ) -> ParserResult<(Loc, String, Loc)>
    where
        F: Fn(Token) -> bool,
    {
        self.expect_token_pred(pred)
            .map(|(l, r)| (l, self.string(l, r), r))
    }

    pub(super) fn skip(&mut self) {
        let ret = self.next();
        debug_assert!(ret.is_ok());
    }

    pub(super) fn str(&mut self, l: Loc, r: Loc) -> &'input str {
        &self.input[l.byte_idx..r.byte_idx]
    }

    pub(super) fn string(&mut self, l: Loc, r: Loc) -> String {
        self.str(l, r).to_owned()
    }
}

//
// Utilities to generate AST nodes.
//

impl<'input, L: LayoutLexer_> Parser<'input, L> {
    pub(super) fn span(&self, l: Loc, r: Loc) -> Span {
        Span {
            source: self.source.clone(),
            start: l,
            end: r,
        }
    }

    pub(super) fn spanned<T>(&self, l: Loc, r: Loc, node: T) -> AstNode<T> {
        AstNode {
            span: self.span(l, r),
            node,
        }
    }
}
