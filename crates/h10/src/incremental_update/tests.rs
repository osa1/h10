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

    // FIXME
    // assert_eq!(groups[0], old_groups[0]);
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
