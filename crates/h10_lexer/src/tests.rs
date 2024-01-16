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
