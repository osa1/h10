#![allow(unused)]

use crate::ast::Span;
use crate::token::TokenRef;
use h10_lexer::TokenKind;

use lexgen_util::Loc;

fn span(line_start: u32, char_start: u32, line_end: u32, char_end: u32) -> Span {
    Span {
        start: loc(line_start, char_start),
        end: loc(line_end, char_end),
    }
}

fn loc(line: u32, char: u32) -> Loc {
    Loc {
        line,
        col: char,
        byte_idx: 0,
    }
}
