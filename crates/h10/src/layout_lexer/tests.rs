use crate::layout_lexer::LayoutLexer;
use h10_lexer::{Literal, ReservedId, ReservedOp, Special, TokenKind};

// `module X where <body>` where `<body>` is indented the same amount as `module`.
//
// According to Haskell 2010 section 10.3, lexing should go like this:
//
// ```
// L [module, id, where, id, equals, int_literal] []
// module : L [id, where, id, equals, int_literal] []
// module : id : L [where, id, equals, int_literal] []
// module : id : where : L [id, equals, int_literal] []
// module : id : where : L [{1}, id, equals, int_literal] [] -- `where` is not followed by `{`, insert {1} (note: first column is 1)
// module : id : where : { : L [id, equals, int_literal] [1]
// module : id : where : { : id : L [equals, int_literal] [1]
// module : id : where : { : id : equals : L [int_literal] [1]
// module : id : where : { : id : equals : int_literal : L [] [1]
// module : id : where : { : id : equals : int_literal : } : L [] [] -- since context != 0
// module : id : where : { : id : equals : int_literal : } : []
// ```
#[test]
fn module_where_no_indent() {
    let input = r#"
module X where
a = 1"#;
    let lexer = LayoutLexer::new(input);
    let tokens: Vec<TokenKind> = lexer.map(|t| t.unwrap().1.kind).collect();
    assert_eq!(
        tokens,
        vec![
            TokenKind::ReservedId(ReservedId::Module),
            TokenKind::ConId,
            TokenKind::ReservedId(ReservedId::Where),
            TokenKind::Special(Special::LBrace),
            TokenKind::VarId,
            TokenKind::ReservedOp(ReservedOp::Equals),
            TokenKind::Literal(Literal::Int),
            TokenKind::Special(Special::RBrace),
        ]
    );
}

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
    let lexer = LayoutLexer::new(input);
    let tokens: Vec<TokenKind> = lexer.map(|t| t.unwrap().1.kind).collect();
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
    let lexer = LayoutLexer::new_non_module(input);
    let tokens: Vec<TokenKind> = lexer.map(|t| t.unwrap().1.kind).collect();
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