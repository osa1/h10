use crate::layout_token_generator::LayoutTokenGenerator;
use crate::lexing::lex_full;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::{Lexer, Literal, ReservedId, ReservedOp, Special, TokenKind};

// `class X where` followed by a declaration at the same indentation with `class`.
//
// Unlike in the test above, after `where`, a `{}` should be generated.
//
// This is similar to the previous test, but the declaration is not considered part of the `where`
// before it. The `L` rule works like this:
//
// ```
// L [class, id, where, id, equals, int_literal] []
// L [{1}, class, id, where, id, equals, int_literal] [] -- the first token is not `module` or `{`
// { : L [class, id, where, id, equals, int_literal] [1] -- n > 0
// { : class : L [id, where, id, equals, int_literal] [1]
// { : class : id : L [where, id, equals, int_literal] [1]
// { : class : id : where : L [id, equals, int_literal] [1]
// { : class : id : where : L [{1}, id, equals, int_literal] [1] -- `where` not followed by `{`
// { : class : id : where : { : } : L [<1>, id, equals, int_literal] [1]
// { : class : id : where : { : } : ; : L [id, equals, int_literal] [1] -- m = n, insert `;`
// { : class : id : where : { : } : ; : id : L [equals, int_literal] [1]
// { : class : id : where : { : } : ; : id : equals : L [int_literal] [1]
// { : class : id : where : { : } : ; : id : equals : int_literal : L [] [1]
// { : class : id : where : { : } : ; : id : equals : int_literal : } : L [] [] -- m != 0
// { : class : id : where : { : } : ; : id : equals : int_literal : } : []
// ```
//
// `class X where` is not valid Haskell 2010 without a variable after the class name, but that's
// not a problem for this test.
#[test]
fn class_where_no_indent() {
    let input = r#"
class X where
a = 1"#;
    let tokens: Vec<TokenKind> = LayoutTokenGenerator::new_top_level(lex_full(input, Pos::ZERO))
        .map(|t| t.unwrap().kind())
        .collect();
    assert_eq!(
        tokens,
        vec![
            TokenKind::Special(Special::LBrace),
            TokenKind::ReservedId(ReservedId::Class),
            TokenKind::ConId,
            TokenKind::ReservedId(ReservedId::Where),
            TokenKind::Special(Special::LBrace),
            TokenKind::Special(Special::RBrace),
            TokenKind::Special(Special::Semi),
            TokenKind::VarId,
            TokenKind::ReservedOp(ReservedOp::Equals),
            TokenKind::Literal(Literal::Int),
            TokenKind::Special(Special::RBrace),
        ]
    );
}

#[test]
fn record_literal() {
    // Test that the layout lexer handles `{` and `}` in a context where an implicit layout is not
    // expected (i.e. after a `let`, `where`, `of`, `do`).
    let input = "data A = A { x :: Int }";
    let tokens: Vec<TokenKind> = LayoutTokenGenerator::new(lex_full(input, Pos::ZERO))
        .map(|t| t.unwrap().kind())
        .collect();
    assert_eq!(
        tokens,
        vec![
            TokenKind::ReservedId(ReservedId::Data),
            TokenKind::ConId,
            TokenKind::ReservedOp(ReservedOp::Equals),
            TokenKind::ConId,
            TokenKind::Special(Special::LBrace),
            TokenKind::VarId,
            TokenKind::ReservedOp(ReservedOp::ColonColon),
            TokenKind::ConId,
            TokenKind::Special(Special::RBrace),
        ]
    );
}
