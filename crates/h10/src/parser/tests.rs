use crate::ast;
use crate::parser::{parse_exp, parse_module};

#[test]
fn test1() {
    let pgm = "f = 1";
    parse_module(pgm).unwrap();
}

#[test]
fn test2() {
    let pgm = "f = 1 + 2";
    parse_module(pgm).unwrap();
}

#[test]
fn test3() {
    let pgm = "\
f =
  1
  +
  2";
    parse_module(pgm).unwrap();
}

#[test]
fn test4() {
    let pgm = "\
f = 1
g = 2";
    parse_module(pgm).unwrap();
}

#[test]
fn test5() {
    let pgm = "\
f = x
 where
  x = 1

g = 2";
    parse_module(pgm).unwrap();
}

#[test]
fn test6() {
    let pgm = "x = (hi `x`)";
    parse_module(pgm).unwrap();
}

#[test]
fn test7() {
    let pgm = "x = (`x` hi)";
    parse_module(pgm).unwrap();
}

#[test]
fn test8() {
    let pgm = "x = ((0, let a = 1 in f a, 1, (++), (:+:)) `g`) \"hi\"";
    parse_module(pgm).unwrap();
}

#[test]
fn test9() {
    let pgm = "x = ((a `b`), (`b` a), (+ 1), (1 +))";
    parse_module(pgm).unwrap();
}

#[test]
fn test10() {
    let pgm = "x = (,,,) 1 2 3 4";
    parse_module(pgm).unwrap();
}

#[test]
fn test11() {
    let pgm = "x = y :: Int";
    parse_module(pgm).unwrap();
}

#[test]
fn test12() {
    let pgm = "\
id :: a -> a
id a = a";
    parse_module(pgm).unwrap();
}

#[test]
fn test13() {
    let pgm = "getA (X a) = a";
    parse_module(pgm).unwrap();
}

#[test]
fn test14() {
    let pgm = "plus2 (A a :+: A b) = a + b";
    parse_module(pgm).unwrap();

    let pgm = "plus3 (A a :+: A b :+: A c) = a + b + c";
    parse_module(pgm).unwrap();
}

#[test]
fn test15() {
    let pgm = "\
plus3 (A a :+: A b :+: A c)
  | a == 0 = b + c
  | otherwise = a + b + c";
    parse_module(pgm).unwrap();
}

/*
#[test]
fn test16() {
    let pgm = "infixl 2 :+:, ><, `ok`";
    parse_module(pgm).unwrap();
}
*/

#[test]
fn test17() {
    let pgm = "\
a, b, (><) :: Int -> Int
";
    parse_module(pgm).unwrap();
}

#[test]
fn test18() {
    let pgm = "\
type String = [Char]
";
    parse_module(pgm).unwrap();
}

#[test]
fn test19() {
    let pgm = "\
data AB = A | B
data Option a = Some a | None
data Coord = Coord { x :: Int, y :: Int }
";
    parse_module(pgm).unwrap();
}

#[test]
fn test20() {
    let pgm = "\
newtype List1 a = List [a]
newtype List2 a b = List2 { list2ToList :: [a] }
";
    parse_module(pgm).unwrap();
}

#[test]
fn test21() {
    let pgm = "\
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  readPrec :: ReadPrec a
  readListPrec :: ReadPrec [a]
";
    parse_module(pgm).unwrap();
}

#[test]
fn test22() {
    let pgm = r#"
instance Read Char where
  readPrec =
    parens
    ( do L.Char c <- lexP
         return c
    )

  readListPrec =
    parens
    ( do L.String s <- lexP
         return s
     +++
      readListPrecDefault
    )

  readList = readListDefault
"#;
    parse_module(pgm).unwrap();
}

#[test]
fn test23() {
    let pgm = r#"
list1 = [1]
list2 = [1, 2]
list3 = [1 ..]
list4 = [1 .. 10]
list5 = [1, 2 .. 10]

update1 x = x { a = 1 }
update2 x = x { a = 1, b = 2 }
"#;
    parse_module(pgm).unwrap();
}

#[test]
fn test24() {
    let pgm = r#"
test1 = - 1 - 2
test2 = - 1 + (- 2)
test3 = case x of
  -1 -> 1
  -2 -> 1
  _ -> 2
"#;
    parse_module(pgm).unwrap();
}

/*

TODO: With the new parser that deals with top-level declaration in isolation, module syntax is no
longer handled as before. We may want to update the module parser, or handle the `module X where`
in the lexer specially like before, but even better we should just drop the module syntax.

#[test]
fn test25() {
    let pgm = r#"
module Test where
"#;
    parse_module(pgm).unwrap();
}

#[test]
fn test26() {
    let pgm = r#"
-- Test
module Test where

x = 123
"#;
    parse_module(pgm).unwrap();
}
*/

#[test]
fn parse_prelude() {
    let pgm = std::fs::read_to_string("../../Prelude.hs").unwrap();
    let ast: Vec<ast::TopDecl> = parse_module(&pgm).unwrap();
    for decl in &ast {
        decl.first_token.check_loops();
    }
}

#[test]
fn empty_class() {
    let pgm = "class X a where";
    parse_module(pgm).unwrap();
}

#[test]
fn empty_class_then_value() {
    let pgm = r#"
class X a where
x = 1
"#;
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 2);
}

#[test]
fn empty_inst() {
    let pgm = "instance Num Int where";
    parse_module(pgm).unwrap();
}

#[test]
fn empty_inst_then_value() {
    let pgm = r#"
instance Num Int where
x = 1
"#;
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 2);
}

#[test]
fn unit() {
    let exp = r#"()"#;
    let ast = parse_exp(exp).unwrap();
    match &ast.node {
        ast::Exp_::Tuple(args) => assert!(args.is_empty()),
        _ => panic!(),
    }
}

#[test]
fn rbrace_insertion_1() {
    let exp = "if e then do { x; y } else z";
    let ast = parse_exp(exp).unwrap();
    assert!(matches!(&ast.node, ast::Exp_::If(_, _, _)));

    let exp = "if e then do x; y; else z";
    let ast = parse_exp(exp).unwrap();
    assert!(matches!(&ast.node, ast::Exp_::If(_, _, _)));
}

#[test]
fn rbrace_insertion_2() {
    let exp = "let { x = 3 } in x";
    let ast = parse_exp(exp).unwrap();
    assert!(matches!(&ast.node, ast::Exp_::Let(_, _)));

    let exp = "let x = 3 in x";
    let ast = parse_exp(exp).unwrap();
    assert!(matches!(&ast.node, ast::Exp_::Let(_, _)));
}

#[test]
fn kind_sig_1() {
    let pgm = r#"
kind T :: (Type -> Type) -> Type
type T f = f Int
"#;
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 2);
    ast[0].kind_sig();
    ast[1].type_syn();
}

#[test]
fn kind_sig_2() {
    // Example from GHC user manual 6.4.12.8.
    // Inferred kind without the signature: `(Type -> Type) -> Type -> Type`.
    let pgm = r#"
kind T :: (k -> Type) -> k -> Type
data T m a = MkT (m a) (T Maybe (m a))
"#;
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 2);
    ast[0].kind_sig();
    ast[1].data();
}

#[test]
fn kind_sig_3() {
    let pgm = "kind Proxy :: forall k . k -> Type";
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 1);
    let kind_sig = ast[0].kind_sig();
    assert_eq!(kind_sig.node.foralls.len(), 1);
}

#[test]
fn kind_sig_with_binder_ty_1() {
    let pgm = "kind Proxy :: forall (k :: Type) . k -> Type";
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 1);
    let kind_sig = ast[0].kind_sig();
    assert_eq!(kind_sig.node.foralls.len(), 1);
}

#[test]
fn explicit_forall() {
    let pgm = "f :: forall f a b . Functor f => (a -> b) -> f a -> f b";
    let ast = parse_module(pgm).unwrap();
    assert_eq!(ast.len(), 1);
    let (vars, foralls, context, _ty) = ast[0].value().type_sig();
    assert_eq!(vars, ["f"]);
    assert_eq!(
        foralls
            .iter()
            .map(|binder| &binder.node.id)
            .collect::<Vec<_>>(),
        ["f", "a", "b"]
    );
    assert_eq!(context.len(), 1);
}
