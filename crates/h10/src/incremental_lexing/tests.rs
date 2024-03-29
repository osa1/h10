use super::*;
use crate::lexing::lex_full;
use crate::pos::Pos;

use indoc::indoc;

#[test]
fn insertion_iteration_0() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm, Pos::ZERO);

    let insertion_pos = Pos { line: 0, char: 6 };
    let inserted_text = "";
    let new_text: String = TokenCharIteratorWithInsertion::new(
        token.clone(),
        insertion_pos,
        inserted_text,
        &Arena::new(),
    )
    .collect();

    assert_eq!(new_text, pgm);
}

#[test]
fn insertion_iteration_1() {
    let pgm = indoc! {"
            data B
        "};

    let token = lex_full(pgm, Pos::ZERO);

    let insertion_pos = Pos { line: 0, char: 0 };
    let inserted_text = "data A\n";
    let new_text: String = TokenCharIteratorWithInsertion::new(
        token.clone(),
        insertion_pos,
        inserted_text,
        &Arena::new(),
    )
    .collect();

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

    let token = lex_full(pgm, Pos::ZERO);

    let insertion_pos = Pos { line: 0, char: 6 };
    let inserted_text = " = A";
    let new_text: String = TokenCharIteratorWithInsertion::new(
        token.clone(),
        insertion_pos,
        inserted_text,
        &Arena::new(),
    )
    .collect();

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

    let token = lex_full(pgm, Pos::ZERO);

    let insertion_pos = Pos { line: 1, char: 6 };
    let inserted_text = "\ndata C";
    let new_text: String = TokenCharIteratorWithInsertion::new(
        token.clone(),
        insertion_pos,
        inserted_text,
        &Arena::new(),
    )
    .collect();

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
fn deletion_iteration_0() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm, Pos::ZERO);

    let deletion_start = Pos { line: 1, char: 0 };
    let deletion_end = Pos { line: 2, char: 0 };
    let new_text: String = TokenCharIteratorWithDeletion::new(
        token.clone(),
        deletion_start,
        deletion_end,
        &Arena::new(),
    )
    .collect();

    assert_eq!(new_text, "data A\n",);
}

#[test]
fn deletion_iteration_1() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex_full(pgm, Pos::ZERO);

    let deletion_start = Pos { line: 0, char: 0 };
    let deletion_end = Pos { line: 1, char: 0 };
    let new_text: String = TokenCharIteratorWithDeletion::new(
        token.clone(),
        deletion_start,
        deletion_end,
        &Arena::new(),
    )
    .collect();

    assert_eq!(new_text, "data B\n");
}

#[test]
fn deletion_iteration_2() {
    let pgm = "data A";

    let token = lex_full(pgm, Pos::ZERO);

    let deletion_start = Pos { line: 0, char: 2 };
    let deletion_end = Pos { line: 0, char: 4 };
    let new_text: String = TokenCharIteratorWithDeletion::new(
        token.clone(),
        deletion_start,
        deletion_end,
        &Arena::new(),
    )
    .collect();

    assert_eq!(new_text, "da A");
}

#[test]
fn deletion_iteration_3() {
    let pgm = "y ::";

    let token = lex_full(pgm, Pos::ZERO);

    let deletion_start = Pos { line: 0, char: 3 };
    let deletion_end = Pos { line: 0, char: 4 };
    let new_text: String = TokenCharIteratorWithDeletion::new(
        token.clone(),
        deletion_start,
        deletion_end,
        &Arena::new(),
    )
    .collect();

    assert_eq!(new_text, "y :");
}

#[test]
fn relex_insertion_same_group() {
    let pgm = indoc! {"
        f x y =
            x y
            z t
    "};

    let token = lex_full(pgm, Pos::ZERO);
    let initial_token_list: Vec<TokenRef> = token.iter().collect();

    let arena = Arena::new();

    let relex_start_token = token.iter().nth(8).unwrap();
    assert_eq!(relex_start_token.text(), "x");

    let new_token = relex_insertion(
        relex_start_token.clone(),
        Pos { line: 1, char: 5 },
        " +",
        &arena,
    );

    // The tokens before the first updated one should be identical.
    assert_eq!(
        &token.iter().take(9).collect::<Vec<TokenRef>>(),
        &initial_token_list[0..9]
    );

    assert_eq!(new_token.text(), "x");
    assert_eq!(new_token, relex_start_token);

    assert_eq!(
        new_token.iter_chars().collect::<String>(),
        "x + y\n    z t\n"
    );

    let new_token_list: Vec<TokenRef> = new_token.iter().collect();
    assert_eq!(&new_token_list[6..], &initial_token_list[12..]); // "z t\n"
}

#[test]
fn relex_insertion_new_group() {
    let pgm = indoc! {"
        data X = X
        type Z = Z
    "};

    let token = lex_full(pgm, Pos::ZERO);
    let initial_token_list: Vec<TokenRef> = token.iter().collect();

    let arena = Arena::new();

    let relex_start_token = token.iter().nth(7).unwrap();
    assert_eq!(relex_start_token.text(), "\n");

    let new_token = relex_insertion(
        relex_start_token.clone(),
        Pos { line: 1, char: 0 },
        "newtype Y = Y\n",
        &arena,
    );

    assert_eq!(
        new_token.iter_chars().collect::<String>(),
        "\nnewtype Y = Y\ntype Z = Z\n"
    );

    let new_token_list: Vec<TokenRef> = token.iter().collect();
    assert_eq!(&new_token_list[..6], &initial_token_list[..6]);

    for (new_token, old_token) in new_token_list[16..].iter().zip(&initial_token_list[8..]) {
        assert_eq!(new_token.text(), old_token.text());
        assert_eq!(new_token.span().start.line, old_token.span().start.line + 1);
        assert_eq!(new_token.span().end.line, old_token.span().end.line + 1);
        assert_eq!(new_token.span().start.char, old_token.span().start.char);
        assert_eq!(new_token.span().end.char, old_token.span().end.char);
    }
}
