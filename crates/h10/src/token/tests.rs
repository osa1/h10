#![allow(unused)]

use crate::ast::Span;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::TokenKind;

fn span(line_start: u32, char_start: u32, line_end: u32, char_end: u32) -> Span {
    Span {
        start: Pos::new(line_start, char_start),
        end: Pos::new(line_end, char_end),
    }
}
