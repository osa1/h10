use super::*;
use crate::lexing::lex_full;

use indoc::indoc;

#[test]
fn insertion_iteration_0() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm);

    let insertion_pos = Pos { line: 0, char: 6 };
    let inserted_text = "";
    let new_text: String =
        TokenCharIteratorWithInsertion::new(token.clone(), insertion_pos, inserted_text).collect();

    assert_eq!(new_text, pgm);
}

#[test]
fn insertion_iteration_1() {
    let pgm = indoc! {"
            data B
        "};

    let token = lex_full(pgm);

    let insertion_pos = Pos { line: 0, char: 0 };
    let inserted_text = "data A\n";
    let new_text: String =
        TokenCharIteratorWithInsertion::new(token.clone(), insertion_pos, inserted_text).collect();

    assert_eq!(
        new_text,
        indoc! {"
            data A
            data B
        "}
    );
}

#[test]
fn insertion_iteration_2() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm);

    let insertion_pos = Pos { line: 0, char: 6 };
    let inserted_text = " = A";
    let new_text: String =
        TokenCharIteratorWithInsertion::new(token.clone(), insertion_pos, inserted_text).collect();

    assert_eq!(
        new_text,
        indoc! {"
            data A = A
            data B
        "}
    );
}

#[test]
fn insertion_iteration_3() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm);

    let insertion_pos = Pos { line: 1, char: 6 };
    let inserted_text = "\ndata C";
    let new_text: String =
        TokenCharIteratorWithInsertion::new(token.clone(), insertion_pos, inserted_text).collect();

    assert_eq!(
        new_text,
        indoc! {"
            data A
            data B
            data C
        "}
    );
}

#[test]
fn relex_insertion_same_group() {
    let pgm = indoc! {"
        f x y =
            x y
            z t
    "};

    let token = lex_full(pgm);
    let initial_token_list: Vec<TokenRef> = token.iter().collect();

    let arena = DeclArena::new();

    let relex_start_token = token.iter().nth(8).unwrap();
    assert_eq!(relex_start_token.text(), "x");

    let new_token = relex_insertion(
        relex_start_token.clone(),
        Pos { line: 1, char: 5 },
        " +",
        &arena,
    );

    // The tokens before the first one should not be identical.
    assert_eq!(
        &token.iter().take(8).collect::<Vec<TokenRef>>(),
        &initial_token_list[0..8]
    );

    // Starting from `relex` the whole line should be re-generated.
    assert_eq!(new_token.text(), "x");
    assert_ne!(new_token, relex_start_token);

    assert_eq!(
        new_token.iter_chars().collect::<String>(),
        "x + y\n    z t\n"
    );

    let new_token_list: Vec<TokenRef> = new_token.iter().collect();
    assert_eq!(&new_token_list[6..], &initial_token_list[12..]); // "z t\n"
}