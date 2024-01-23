use crate::token::TokenKind;
use crate::Lexer;

fn lex(s: &str) -> Vec<TokenKind> {
    Lexer::new(s)
        .map(|t| t.unwrap().1)
        .filter(|t| !matches!(t, TokenKind::Whitespace))
        .collect()
}

#[test]
fn lex_id_sym() {
    assert_eq!(
        lex("a A ++ :+: A.a A.A A.++ A.:+: * ."),
        vec![
            TokenKind::VarId,
            TokenKind::ConId,
            TokenKind::VarSym,
            TokenKind::ConSym,
            TokenKind::QVarId,
            TokenKind::QConId,
            TokenKind::QVarSym,
            TokenKind::QConSym,
            TokenKind::VarSym,
            TokenKind::VarSym,
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
    let doc = TokenKind::Comment {
        documentation: true,
    };
    let not_doc = TokenKind::Comment {
        documentation: false,
    };
    assert_eq!(toks, vec![not_doc, doc, not_doc, doc]);
}
