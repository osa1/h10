use crate::ast::Span;
use crate::token_node::TokenNodeRef;
use h10_lexer::token::Token;

use lexgen_util::Loc;

fn span(line_start: u32, char_start: u32, line_end: u32, char_end: u32) -> Span {
    Span {
        source: "<test>".into(),
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

#[test]
fn simple_insertion() {
    let token = TokenNodeRef::new(Token::Whitespace, span(0, 0, 0, 2), "  ".to_owned());

    /*
    TODO: Token position update plan:

    Since a token cannot span multiple blocks, line indices in tokens can be relative to block's
    line number.

    This way when incrementing line numbers we only visit the tokens in the block.

    But how to merge blocks?

    Option 1: Merge blocks as just one token with the contents of all of the merged tokens.

    Option 2: ???
    */

    token.insert(0, 0, "a");
    assert_eq!(&*token.text.borrow(), "a  ");
    assert_eq!(token.span().start, loc(0, 0));
    // assert_eq!(token.span().end, loc(0, 3));

    // TODO: Update spans when inserting.
    // token.insert(0, 2, "a");
    // assert_eq!(&*token.text.borrow(), "a a ");
    // assert_eq!(token.span().start, loc(0, 0));
    // // assert_eq!(token.span().end, loc(0, 4));
}
