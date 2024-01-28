use super::*;
use crate::decl_arena::DeclArena;
use crate::indentation_groups::parse_indentation_groups;
use crate::lexing::lex_full;
use crate::pos::Pos;

// use indoc::indoc;

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
    assert_eq!(new_group_idx, group_idx);

    assert_ne!(arena.get(group_idx).first_token, token);

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
    assert_eq!(new_group_idx, group_idx);

    let new_pgm: String = token.iter_chars().collect();
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
    assert_eq!(new_group_idx, group_idx);

    let new_pgm: String = token.iter_chars().collect();

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
}
