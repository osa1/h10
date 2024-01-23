use crate::buffer::line::Token;
use crate::buffer::{Buffer, CharIter};
use crate::token::TokenKind;

use h10_lexer::Lexer as H10Lexer;
use h10_lexer::Literal as H10Literal;
use h10_lexer::Token as H10Token;

use lexgen_util::{LexerError, Loc};

/// An iterator that splits tokens that span multiple lines into tokens that span just one line.
pub struct TokenSplitIterator<'buffer> {
    buffer: &'buffer Buffer,

    // NB. Lexer must be initialized at the beginning of a line
    lexer: H10Lexer<'static, CharIter<'buffer>>,

    // Index of the line lexing starts from. Lexer will yield numers from 0, add this to get the
    // actual line number.
    start_line: u32,

    // NB. Byte indices won't be accurate, do not use!
    last_token: Option<MultiLineToken>,
}

#[derive(Debug)]
struct MultiLineToken {
    kind: TokenKind,
    line_start: u32,
    col_start: u32,
    line_end: u32,
    col_end: u32,
}

impl<'buffer> TokenSplitIterator<'buffer> {
    pub fn new(buffer: &'buffer Buffer, start_line: u32, start_col: u32) -> Self {
        Self {
            buffer,
            lexer: H10Lexer::new_from_iter(buffer.iter_from(start_line, start_col)),
            start_line,
            last_token: None,
        }
    }
}

impl From<(Loc, H10Token, Loc)> for MultiLineToken {
    fn from((token_start, token, token_end): (Loc, H10Token, Loc)) -> Self {
        MultiLineToken {
            kind: h10_token_kind(&token),
            line_start: token_start.line,
            col_start: token_start.col,
            line_end: token_end.line,
            col_end: token_end.col,
        }
    }
}

impl<'buffer> Iterator for TokenSplitIterator<'buffer> {
    // (line number, token)
    type Item = Result<(u32, Token), ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(last_token) = self.last_token.take() {
            if last_token.line_start == last_token.line_end {
                return Some(Ok((
                    self.start_line + last_token.line_start,
                    Token {
                        kind: last_token.kind,
                        col: last_token.col_start,
                        length_cols: last_token.col_end - last_token.col_start,
                    },
                )));
            } else {
                let token_1 = Token {
                    kind: last_token.kind,
                    col: last_token.col_start,
                    length_cols: self
                        .buffer
                        .line_len_chars(self.start_line + last_token.line_start)
                        - last_token.col_start
                        + 1, // +1 for virtual newline
                };

                // Convert exclisive end index to inclusive
                if !(last_token.line_start + 1 == last_token.line_end && last_token.col_end == 0) {
                    let token_2 = MultiLineToken {
                        kind: last_token.kind,
                        line_start: last_token.line_start + 1,
                        col_start: 0,
                        line_end: last_token.line_end,
                        col_end: last_token.col_end,
                    };

                    self.last_token = Some(token_2);
                }

                return Some(Ok((self.start_line + last_token.line_start, token_1)));
            }
        }

        match simplify_item(self.lexer.next()) {
            Some(Ok(token)) => {
                if token.line_start == token.line_end {
                    Some(Ok((
                        self.start_line + token.line_start,
                        Token {
                            kind: token.kind,
                            col: token.col_start,
                            length_cols: token.col_end - token.col_start,
                        },
                    )))
                } else {
                    let token_1 = Token {
                        kind: token.kind,
                        col: token.col_start,
                        length_cols: self
                            .buffer
                            .line_len_chars(self.start_line + token.line_start)
                            - token.col_start
                            + 1, // +1 for virtual newline
                    };

                    let token_2 = MultiLineToken {
                        kind: token.kind,
                        line_start: token.line_start + 1,
                        col_start: 0,
                        line_end: token.line_end,
                        col_end: token.col_end,
                    };

                    self.last_token = Some(token_2);
                    Some(Ok((self.start_line + token.line_start, token_1)))
                }
            }
            Some(Err(())) => Some(Err(())),
            None => None,
        }
    }
}

fn simplify_item(
    item: Option<Result<(Loc, H10Token, Loc), LexerError<std::convert::Infallible>>>,
) -> Option<Result<MultiLineToken, ()>> {
    item.map(|res| match res {
        Ok(token) => Ok(token.into()),
        Err(_) => Err(()),
    })
}

fn h10_token_kind(token: &H10Token) -> TokenKind {
    // TODO: Generate comments
    match token {
        H10Token::Whitespace => TokenKind::Whitespace,

        H10Token::Comment { documentation: _ } => TokenKind::Comment,

        H10Token::QVarId | H10Token::VarId | H10Token::VarSym => TokenKind::Variable,

        H10Token::QConId | H10Token::ConSym | H10Token::ConId => TokenKind::Type,

        H10Token::QVarSym | H10Token::QConSym => TokenKind::Operator,

        H10Token::Literal(lit) => match lit {
            H10Literal::Int | H10Literal::Float => TokenKind::Number,
            H10Literal::Char | H10Literal::String => TokenKind::String,
        },

        H10Token::Special(_) => TokenKind::Delimiter,

        H10Token::ReservedOp(_) | H10Token::ReservedId(_) => TokenKind::Keyword,
    }
}
