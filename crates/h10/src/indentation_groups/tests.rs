use super::*;
use h10_lexer::Lexer;

use indoc::indoc;

#[test]
fn simple_parsing_1() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let mut arena = DeclArena::new();
    let token = lex(pgm);
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
    let token = lex(pgm);
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
    let token = lex(pgm);
    let groups = parse_indentation_groups(token.clone(), &mut arena);
    assert_eq!(groups.len(), 3);

    let group0 = arena.get(groups[0]);
    let group1 = arena.get(groups[1]);
    let group2 = arena.get(groups[2]);

    for token in group0.iter_tokens() {
        assert_eq!(token.span().start.line, 0);
    }

    assert_eq!(group0.line_number, 1);
    assert_eq!(group1.line_number, 3);
    assert_eq!(group2.line_number, 7);

    token.check_token_str(pgm);
}

#[test]
fn insertion_iteration_0() {
    let pgm = indoc! {"
            data A
            data B
        "};

    let token = lex(pgm);

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

    let token = lex(pgm);

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

    let token = lex(pgm);

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

    let token = lex(pgm);

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
fn relex_same_group() {
    let pgm = indoc! {"
        f x y =
            x y
            z t
    "};

    let token = lex(pgm);
    let initial_token_list: Vec<TokenRef> = token.iter().collect();

    let arena = DeclArena::new();

    let relex_start_token = token.iter().nth(8).unwrap();
    assert_eq!(relex_start_token.text(), "x");

    let new_token = relex(
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

fn lex(s: &str) -> TokenRef {
    let lexer = Lexer::new(s);
    let mut first_token: Option<TokenRef> = None;
    let mut last_token: Option<TokenRef> = None;
    for t in lexer {
        let t: TokenRef = TokenRef::from_lexer_token(t.unwrap());
        if first_token.is_none() {
            first_token = Some(t.clone());
        } else if let Some(last_token_) = last_token {
            last_token_.set_next(Some(t.clone()));
        }
        last_token = Some(t.clone());
    }
    first_token.unwrap()
}

#[allow(unused)]
fn collect_token_kinds(token: &TokenRef) -> Vec<TokenKind> {
    token.iter().map(|t| t.token()).collect()
}

#[allow(unused)]
fn collect_token_texts(token: &TokenRef) -> Vec<String> {
    token.iter().map(|t| t.text().to_owned()).collect()
}
