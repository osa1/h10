use super::*;
use h10_lexer::Lexer;

use indoc::indoc;

fn lex(s: &str) -> TokenRef {
    let lexer = Lexer::new(s);
    let mut first_token: Option<TokenRef> = None;
    let mut last_token: Option<TokenRef> = None;
    for t in lexer {
        let t: TokenRef = TokenRef::from_lexer_token("test", t.unwrap());
        if first_token.is_none() {
            first_token = Some(t.clone());
        } else if let Some(last_token_) = last_token {
            last_token_.set_next(Some(t.clone()));
        }
        last_token = Some(t.clone());
    }
    first_token.unwrap()
}

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
