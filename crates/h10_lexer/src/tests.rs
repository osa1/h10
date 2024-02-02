use crate::token::TokenKind;
use crate::Lexer;

fn lex(s: &str) -> Vec<TokenKind> {
    Lexer::new(s)
        .map(|t| t.unwrap().1.kind)
        .filter(|t| !matches!(t, TokenKind::Whitespace))
        .collect()
}

const DOC: TokenKind = TokenKind::Comment {
    documentation: true,
};

const NOT_DOC: TokenKind = TokenKind::Comment {
    documentation: false,
};

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
-- | test
{- test -}
{-| test -}
{- | test -}
"#);
    assert_eq!(toks, vec![NOT_DOC, DOC, DOC, NOT_DOC, DOC, DOC]);
}

#[test]
fn comment_termination() {
    // Check terminating single-line comments at the end of the input.
    let toks = lex("-- Test");
    assert_eq!(toks, vec![NOT_DOC]);

    let toks = lex("-- | Test");
    assert_eq!(toks, vec![DOC]);
}
