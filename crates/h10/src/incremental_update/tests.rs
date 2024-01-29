use super::*;
use crate::decl_arena::DeclArena;
use crate::indentation_groups::parse_indentation_groups;
use crate::lexing::lex_full;
use crate::pos::Pos;

use indoc::indoc;

#[test]
fn single_group_insert_first() {
    let pgm = "f x = x";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);

    assert_eq!(groups.len(), 1);
    let group_idx = groups[0];

    insert(&mut arena, &mut groups, Pos { line: 0, char: 0 }, "q ");

    assert_eq!(groups.len(), 1);
    let new_group_idx = groups[0];

    assert_ne!(arena.get(new_group_idx).first_token, token);

    let new_pgm: String = arena.get(group_idx).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "q f x = x");
}

#[test]
fn single_group_insert_middle() {
    let pgm = "f x = x";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);

    assert_eq!(groups.len(), 1);
    let group_idx = groups[0];

    assert_eq!(arena.get(group_idx).first_token, token);

    let group = arena.get(group_idx);
    assert_eq!(group.span_start().line, 0);
    assert_eq!(group.span_end().line, 0);

    insert(&mut arena, &mut groups, Pos { line: 0, char: 6 }, "1 + ");

    assert_eq!(groups.len(), 1);

    let new_group_idx = groups[0];
    let new_pgm: String = arena.get(new_group_idx).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "f x = 1 + x");
}

#[test]
fn single_group_insert_last() {
    let pgm = "f x = x";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);

    assert_eq!(groups.len(), 1);
    let group_idx = groups[0];

    assert_eq!(arena.get(group_idx).first_token, token);

    insert(&mut arena, &mut groups, Pos { line: 0, char: 7 }, " + 1");

    assert_eq!(groups.len(), 1);

    let new_group_idx = groups[0];
    let new_pgm: String = arena.get(new_group_idx).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "f x = x + 1");
}

#[test]
fn single_group_insert_new_group_first() {
    let pgm = "f x = x\n";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);

    assert_eq!(groups.len(), 1);
    let group_idx = groups[0];

    assert_eq!(arena.get(group_idx).first_token, token);

    insert(
        &mut arena,
        &mut groups,
        Pos { line: 0, char: 0 },
        "data X = X\n",
    );

    assert_eq!(groups.len(), 2);

    let new_pgm: String = arena.get(groups[0]).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "data X = X\nf x = x\n");
}

#[test]
fn single_group_insert_new_group_last() {
    let pgm = "f x = x\n";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);

    assert_eq!(groups.len(), 1);
    let group_idx = groups[0];

    assert_eq!(arena.get(group_idx).first_token, token);

    insert(
        &mut arena,
        &mut groups,
        Pos { line: 1, char: 0 },
        "data X = X\n",
    );

    assert_eq!(groups.len(), 2);

    let new_pgm: String = arena.get(groups[0]).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "f x = x\ndata X = X\n");
}

#[test]
fn multiple_groups_insert_middle() {
    let pgm = indoc! {"
        data X = X
        data Z = Z
    "};
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups: Vec<DeclIdx> = parse_indentation_groups(token.clone(), &mut arena);

    assert_eq!(groups.len(), 2);

    let group0 = arena.get(groups[0]);
    assert_eq!(group0.span_start(), Pos::new(0, 0));
    assert_eq!(group0.span_end(), Pos::new(1, 0));

    let group1 = arena.get(groups[1]);
    assert_eq!(group1.span_start(), Pos::new(1, 0));
    assert_eq!(group1.span_end(), Pos::new(2, 0));

    let old_groups: Vec<DeclIdx> = groups.clone();

    insert(
        &mut arena,
        &mut groups,
        Pos { line: 1, char: 0 },
        "data Y = Y\n",
    );

    assert_eq!(groups.len(), 3);

    let old_tokens: Vec<TokenRef> = arena.get(old_groups[0]).first_token.iter().collect();
    let new_tokens: Vec<TokenRef> = arena.get(groups[0]).first_token.iter().collect();

    // First and last 8 tokens should be reused.
    assert_eq!(&old_tokens[0..7], &new_tokens[0..7]);
    assert_eq!(
        &old_tokens[old_tokens.len() - 8..],
        &new_tokens[new_tokens.len() - 8..]
    );

    // FIXME: We current can't reuse the last decl as insertion is done at the beginning of it and
    // we don't increment its line number. So its tokens are never equal to the new tokens
    // generated by the lexer.
    // assert_eq!(groups[2], old_groups[1]);

    let new_pgm: String = arena.get(groups[0]).first_token.iter_chars().collect();
    assert_eq!(
        new_pgm,
        indoc! {"
            data X = X
            data Y = Y
            data Z = Z
        "}
    );
}

#[test]
fn single_group_remove_first() {
    let pgm = "f x = 1 + x";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 1);

    remove(
        &mut arena,
        &mut groups,
        Pos { line: 0, char: 0 },
        Pos { line: 0, char: 2 },
    );
    assert_eq!(groups.len(), 1);

    let new_group_idx = groups[0];
    assert_ne!(arena.get(new_group_idx).first_token, token);

    let new_pgm: String = arena.get(new_group_idx).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "x = 1 + x");
}

#[test]
fn single_group_remove_middle() {
    let pgm = "f x = 1 + x";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 1);

    remove(
        &mut arena,
        &mut groups,
        Pos { line: 0, char: 6 },
        Pos { line: 0, char: 10 },
    );
    assert_eq!(groups.len(), 1);

    let new_group_idx = groups[0];
    assert_eq!(arena.get(new_group_idx).first_token, token);

    let new_pgm: String = arena.get(new_group_idx).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "f x = x");
}

#[test]
fn single_group_remove_last() {
    let pgm = "f x = x + 1";
    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 1);

    remove(
        &mut arena,
        &mut groups,
        Pos { line: 0, char: 7 },
        Pos { line: 0, char: 11 },
    );
    assert_eq!(groups.len(), 1);

    let new_group_idx = groups[0];
    assert_eq!(arena.get(new_group_idx).first_token, token);

    let new_pgm: String = arena.get(new_group_idx).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "f x = x");
}

#[test]
fn multiple_groups_merge() {
    let pgm = indoc! {"
        f x = x + 1
        data X = X
    "};

    let token = lex_full(pgm);
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 2);

    remove(
        &mut arena,
        &mut groups,
        Pos { line: 0, char: 8 },
        Pos { line: 1, char: 9 },
    );
    assert_eq!(groups.len(), 1);

    let new_pgm: String = arena.get(groups[0]).first_token.iter_chars().collect();
    assert_eq!(new_pgm, "f x = x X\n");
}

#[test]
fn insert_to_empty_doc() {
    let token = lex_full("");
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 0);

    insert(
        &mut arena,
        &mut groups,
        Pos::new(0, 0),
        indoc! {"
            f :: Int -> Int

            data Test = Test

            newtype Blah
        "},
    );

    assert_eq!(groups.len(), 3);

    let decl0 = arena.get(groups[0]);
    assert_eq!(decl0.span_start(), Pos::new(0, 0));
    assert_eq!(decl0.span_end(), Pos::new(2, 0));

    let decl1 = arena.get(groups[1]);
    assert_eq!(decl1.span_start(), Pos::new(2, 0));
    assert_eq!(decl1.span_end(), Pos::new(4, 0));

    let decl2 = arena.get(groups[2]);
    assert_eq!(decl2.span_start(), Pos::new(4, 0));
    assert_eq!(decl2.span_end(), Pos::new(5, 0));
}

#[test]
fn append_line_no_newline() {
    let token = lex_full("a");
    let mut arena = DeclArena::new();
    let mut groups = parse_indentation_groups(token.clone(), &mut arena);

    let group0 = arena.get(groups[0]);
    assert_eq!(group0.span_end(), Pos::new(0, 1));

    insert(&mut arena, &mut groups, Pos::new(0, 1), "b");
    assert_eq!(groups.len(), 1);

    let group0 = arena.get(groups[0]);
    assert_eq!(group0.span_end(), Pos::new(0, 2));

    let new_pgm: String = group0.first_token.iter_chars().collect();
    assert_eq!(new_pgm, "ab");
}
