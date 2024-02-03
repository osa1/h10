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

fn check_group_sanity(group_idx: DeclIdx, arena: &DeclArena) {
    check_group_links(group_idx, arena);
}

fn check_group_links(mut group_idx: DeclIdx, arena: &DeclArena) {
    let mut prev: Option<DeclIdx> = None;
    loop {
        let group = arena.get(group_idx);
        assert_eq!(arena.get(group_idx).prev, prev);
        match group.next {
            Some(next_group_idx) => {
                prev = Some(group_idx);
                group_idx = next_group_idx;
            }
            None => break,
        }

        if let Some(nested_group_idx) = group.nested {
            check_group_links(nested_group_idx, arena);
        }
    }
}

#[test]
fn parse_1() {
    let pgm = "";
    let token = lex_full(pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group = arena.get(group_idx);
    assert_eq!(group.first_token, group.last_token);
    assert_eq!(group.line_number, 0);
}

#[test]
fn parse_2() {
    let pgm = "a";
    let token = lex_full(pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group = arena.get(group_idx);
    assert_eq!(group.first_token, group.last_token);
    assert_eq!(group.line_number, 0);
}

#[test]
fn parse_3() {
    #[rustfmt::skip]
    let lines = [
        "a",
        " b"
    ];
    let pgm = lines.join("\n");
    let token = lex_full(&pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group = arena.get(group_idx);
    assert_eq!(group.first_token.text(), "a");
    assert_eq!(group.last_token.text(), "b");
    assert_eq!(group.line_number, 0);
}

#[test]
fn parse_4() {
    #[rustfmt::skip]
    let lines = [
        "  ",
        "a",
        " b"
    ];
    let pgm = lines.join("\n");
    let token = lex_full(&pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group = arena.get(group_idx);
    assert_eq!(group.first_token.text(), "  \n");
    assert_eq!(group.last_token.text(), "b");
    assert_eq!(group.line_number, 0);
}

#[test]
fn parse_5() {
    #[rustfmt::skip]
    let lines = [
        "  -- a comment",
        "a",
        " b"
    ];
    let pgm = lines.join("\n");
    let token = lex_full(&pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group = arena.get(group_idx);
    assert_eq!(group.first_token.text(), "  ");
    assert_eq!(group.last_token.text(), "b");
    assert_eq!(group.line_number, 0);
}

#[test]
fn parse_6() {
    let lines = [
        "  -- a comment",
        "-- | A doc comment",
        "  -- another indented comment",
        "a",
        "-- a",
        " ",
        " b",
    ];
    let pgm = lines.join("\n");
    let token = lex_full(&pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group = arena.get(group_idx);
    assert_eq!(group.first_token.text(), "  ");
    assert_eq!(group.last_token.text(), "b");
    assert_eq!(group.line_number, 0);
}

#[test]
fn parse_7() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group0 = arena.get(group_idx);
    let group1 = arena.get(group0.next.unwrap());
    assert_eq!(group0.first_token.text(), "data");
    assert_eq!(group1.first_token.text(), "data");
}

#[test]
fn parse_8() {
    let pgm = indoc! {"

            newtype A       -- 1

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

    let group_idx = parse(token.clone(), &mut arena);
    check_group_sanity(group_idx, &arena);

    let group0 = arena.get(group_idx);
    let group1 = arena.get(group0.next.unwrap());
    let group2 = arena.get(group1.next.unwrap());
    assert!(group2.next.is_none());

    assert_eq!(group0.first_token.text(), "\n"); // initial whitespace
    assert_eq!(group0.first_token.next().unwrap().text(), "newtype");
    assert_eq!(group1.first_token.text(), "data");
    assert_eq!(group2.first_token.text(), "f");

    assert_eq!(group0.line_number, 0);
    assert_eq!(group1.line_number, 3);
    assert_eq!(group2.line_number, 7);

    token.check_token_str(pgm);
}

#[test]
fn trivia_nodes() {
    // Check that the initial trivia is assigned to the first AST node and trailing trivia are
    // assigned to the previous AST node.
    #[rustfmt::skip]
    let lines = [
        "-- Comment 1",
        "data X",
        "-- Comment 2",
        "newtype Y",
        "-- Comment 3",
    ];
    let pgm = lines.join("\n");

    let mut arena = DeclArena::new();
    let token = lex_full(&pgm, Pos::ZERO);

    let group_idx = parse(token.clone(), &mut arena);
    check_group_sanity(group_idx, &arena);

    let group0 = arena.get(group_idx);
    let group1 = arena.get(group0.next.unwrap());

    let mut group0_str = String::new();
    for token in group0.first_token.iter_until(&group0.last_token) {
        group0_str.push_str(token.text());
    }

    assert_eq!(group0_str, lines[0..3].join("\n") + "\n");

    let mut group1_str = String::new();
    for token in group1.first_token.iter_until(&group1.last_token) {
        group1_str.push_str(token.text());
    }

    assert_eq!(group1_str, lines[3..].join("\n"));
}

#[test]
fn parse_nested_1() {
    #[rustfmt::skip]
    let lines = [
        "class A b c",
        " where",
        "-- a",
        "  x",
        "   z",
        "  t",
        "b"
    ];
    let pgm = lines.join("\n");
    let token = lex_full(&pgm, Pos::ZERO);
    let mut arena = DeclArena::new();

    let group_idx = parse(token, &mut arena);
    check_group_sanity(group_idx, &arena);

    let group0 = arena.get(group_idx);
    assert_eq!(group0.first_token.text(), "class");
    assert_eq!(group0.last_token.text(), "\n");
    assert_eq!(group0.last_token.absolute_span(&arena).start.line, 5);

    let group1 = arena.get(group0.next.unwrap());
    assert_eq!(group1.first_token.text(), "b");
    assert_eq!(group1.first_token, group1.last_token);

    let nested0 = arena.get(group0.nested.unwrap());
    assert_eq!(nested0.first_token.text(), "x");

    let nested1 = arena.get(nested0.next.unwrap());
    assert_eq!(nested1.first_token.text(), "t");
}
