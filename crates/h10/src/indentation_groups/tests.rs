use super::*;
use crate::lexing::lex_full;
use crate::pos::Pos;

use indoc::indoc;

#[test]
fn simple_parsing_1() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let mut arena = DeclArena::new();
    let token = lex_full(pgm, Pos::ZERO);
    let groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 2);

    let group0 = arena.get(groups[0]);
    let group1 = arena.get(groups[1]);

    assert_eq!(group0.line_number, 0);
    let group0_tokens = group0.iter_tokens().collect::<Vec<_>>();
    assert_eq!(group0_tokens.len(), 4);
    for token in group0_tokens {
        assert_eq!(token.span().start.line, 0);
    }

    assert_eq!(group1.line_number, 1);
    let group1_tokens = group1.iter_tokens().collect::<Vec<_>>();
    assert_eq!(group1_tokens.len(), 4);
    for token in group1_tokens {
        assert_eq!(token.span().start.line, 0);
    }

    token.check_token_str(pgm);
}

#[test]
fn simple_parsing_2() {
    let pgm = indoc! {"
            data A

            data B
        "};
    let mut arena = DeclArena::new();
    let token = lex_full(pgm, Pos::ZERO);
    let groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 2);

    let group0 = arena.get(groups[0]);
    let group1 = arena.get(groups[1]);

    assert_eq!(group0.line_number, 0);
    let group0_tokens = group0.iter_tokens().collect::<Vec<_>>();
    assert_eq!(group0_tokens.len(), 4); // [data, ws, A, ws]
    for token in &group0_tokens {
        assert_eq!(token.span().start.line, 0);
    }
    // TODO: End location is inclusive or exclusive?
    // assert_eq!(group0_tokens.last().unwrap().span().end.line, 1);

    assert_eq!(group1.line_number, 2);
    let group1_tokens = group1.iter_tokens().collect::<Vec<_>>();
    assert_eq!(group1_tokens.len(), 4); // [data, ws, A, ws]
    for token in &group1_tokens {
        assert_eq!(token.span().start.line, 0);
    }

    token.check_token_str(pgm);
}

#[test]
fn simple_parsing_3() {
    let pgm = indoc! {"

            data A          -- 1

            data B          -- 3
              = X
              | Y

            f x y =         -- 7
              x

              where
                t = 5       -- 11
        "};
    let mut arena = DeclArena::new();
    let token = lex_full(pgm, Pos::ZERO);
    let groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 4);

    let group1 = arena.get(groups[1]);
    let group2 = arena.get(groups[2]);
    let group3 = arena.get(groups[3]);

    for token in group1.iter_tokens() {
        assert_eq!(token.span().start.line, 0);
    }

    assert_eq!(group1.line_number, 1);
    assert_eq!(group2.line_number, 3);
    assert_eq!(group3.line_number, 7);

    token.check_token_str(pgm);
}

#[test]
fn comments() {
    let pgm = indoc! {"
            --| A documentation comment.
            data A

            -- A non-documentation comment.
            data B
              = X
              | Y

            -- A trailing comment.
        "};
    let mut arena = DeclArena::new();
    let token = lex_full(pgm, Pos::ZERO);
    let groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 2);

    let group0 = arena.get(groups[0]);
    assert_eq!(group0.span_start(&arena), Pos::new(0, 0));
    assert_eq!(group0.span_end(&arena), Pos::new(4, 0));

    let group1 = arena.get(groups[1]);
    assert_eq!(group1.span_start(&arena), Pos::new(4, 0));
    assert_eq!(group1.span_end(&arena), Pos::new(9, 0));
}

#[test]
fn nested_1() {
    let pgm = indoc! {"
            class Blah a where
              x :: a -> Int
              y :: a -> String
        "};

    let mut arena = DeclArena::new();
    let token = lex_full(pgm, Pos::ZERO);
    let groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 1);

    let group0 = arena.get(groups[0]);
    assert_eq!(group0.line_number, 0); // relative to document
    assert_eq!(group0.span_start(&arena).line, 0); // absolute
    assert_eq!(group0.span_end(&arena).line, 3); // absolute

    let nested0 = arena.get(group0.nested.unwrap());
    assert_eq!(nested0.parent, Some(groups[0]));

    // Whitespace after `where` is skipped, so the first group is `x :: ...`.
    assert_eq!(nested0.line_number, 1);
    assert_eq!(nested0.span_start(&arena).line, 1);
    assert_eq!(nested0.span_end(&arena).line, 2);

    let nested1 = arena.get(nested0.next.unwrap());
    assert_eq!(nested1.span_start(&arena).line, 2);
    assert_eq!(nested1.span_end(&arena).line, 3);
}
