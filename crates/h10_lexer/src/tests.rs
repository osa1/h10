use crate::token::Token;
use crate::Lexer;

fn lex(s: &str) -> Vec<Token> {
    Lexer::new(s)
        .map(|t| t.unwrap().1)
        .filter(|t| !matches!(t, Token::Whitespace))
        .collect()
}

#[test]
fn lex_id_sym() {
    assert_eq!(
        lex("a A ++ :+: A.a A.A A.++ A.:+: * ."),
        vec![
            Token::VarId,
            Token::ConId,
            Token::VarSym,
            Token::ConSym,
            Token::QVarId,
            Token::QConId,
            Token::QVarSym,
            Token::QConSym,
            Token::VarSym,
            Token::VarSym,
        ]
    );
}

#[test]
fn comments() {
    let toks = lex(r#"
-- test
--| test
{- test -}
{-| test -}
"#);
    let doc = Token::Comment {
        documentation: true,
    };
    let not_doc = Token::Comment {
        documentation: false,
    };
    assert_eq!(toks, vec![not_doc, doc, not_doc, doc]);
}
