use crate::ast;
use crate::ast_to_ty::convert_ast_ty;
use crate::class_env::Pred;
use crate::collections::Set;
use crate::id::{type_ty_tyref, Id};
use crate::parser::{parse_module, parse_type};
use crate::renaming::Renamer;
use crate::type_inference::{ti_module, unify};
use crate::type_scheme::Scheme;
use crate::typing::TyRef;

#[test]
fn unify_two_way_left() {
    // An example test that checks unification result of a type variable. A bit too verbose, it
    // would be good to simplify this and add more unification tests.
    let mut renamer = Renamer::new();

    let ty1_ast: ast::RenamedType = renamer.rename_type(&parse_type("a -> a").unwrap().2, true);
    let a_id: Id = ty1_ast.arrow().0.var().clone();
    let a_tyref = TyRef::new_var(type_ty_tyref(), 0);
    let ty1 = convert_ast_ty(
        &|id| {
            if id == &a_id {
                Some(a_tyref.clone())
            } else {
                None
            }
        },
        &Default::default(),
        &ty1_ast,
    );

    let ty2_ast: ast::RenamedType = renamer.rename_type(&parse_type("Int -> Int").unwrap().2, true);
    let ty2 = convert_ast_ty(&|_id| None, &Default::default(), &ty2_ast);
    let int_tyref = &ty2.split_fun_ty().0[0];

    unify(&Default::default(), &ty1, &ty2).unwrap();
    assert_eq!(&a_tyref.normalize(), int_tyref);
}

#[test]
fn infer_id_explicit() {
    let pgm = r#"
id :: a -> a
id x = x
"#;
    check_inferred_ty(pgm, "id", "a -> a");
}

#[test]
fn infer_id_implicit() {
    let pgm = "id x = x";
    check_inferred_ty(pgm, "id", "a -> a");
}

#[test]
fn infer_const_explicit() {
    let pgm = r#"
const :: a -> b -> a
const x _ = x
"#;
    check_inferred_ty(pgm, "const", "x -> y -> x");
}

#[test]
fn infer_const_implicit() {
    let pgm = "const x _ = x";
    check_inferred_ty(pgm, "const", "x -> y -> x");
}

#[test]
fn simple_typeclass() {
    let pgm = r#"
data X = X

class ToX a where
  toX :: a -> X

f = toX
"#;
    check_inferred_ty(pgm, "f", "ToX a => a -> X");
}

#[test]
#[should_panic]
fn simple_typeclass_default_method_fail() {
    let pgm = r#"
data Bool = False | True

class ToInt a where
  toInt :: a -> Int
  toInt _ = False
"#;
    check_inferred_ty(pgm, "toInt", "ToInt a => a -> Int");
}

#[test]
#[should_panic]
fn simple_typeclass_instance_method_fail_1() {
    let pgm = r#"
data Bool = False | True

class ToInt a where
  toInt :: a -> Int

instance ToInt Bool where
  toInt False = True
"#;
    check_inferred_ty(pgm, "toInt", "ToInt a => a -> Int");
}

#[test]
#[should_panic]
fn simple_typeclass_instance_method_fail_2() {
    let pgm = r#"
data Maybe a = Nothing | Just a

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap f (Just a) = Just a
  fmap f Nothing = Nothing
"#;
    check_inferred_ty(pgm, "fmap", "Functor f => (a -> b) -> f a -> f b");
}

#[test]
#[should_panic]
fn type_sig_mismatch_fail() {
    let pgm = r#"
data Maybe a = Nothing | Just a

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ (Just a) = Just a
"#;
    check_inferred_ty(pgm, "fmap", "(a -> b) -> Maybe a -> Maybe b");
}

#[test]
fn functor_example_1() {
    let pgm = r#"
data Bool = False | True

toggle False = True
toggle True = False

class Functor f where
  fmap :: (a -> b) -> f a -> f b

toggleFunctor = fmap toggle
"#;
    check_inferred_ty(pgm, "toggleFunctor", "Functor t => t Bool -> t Bool");
}

#[test]
fn functor_example_2() {
    let pgm = r#"
data Bool = False | True

data T a = T a

toggle False = True
toggle True = False

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor T where
  fmap f (T a) = T (f a)

x = fmap toggle (T True)

y f = fmap f (T True)

z f a = fmap f (T a)
"#;
    check_inferred_ty(pgm, "x", "T Bool");
    check_inferred_ty(pgm, "y", "(Bool -> a) -> T a");
    check_inferred_ty(pgm, "z", "(a -> b) -> a -> T b");
}

#[test]
fn functor_example_3() {
    let pgm = r#"
data Bool = False | True

toggle False = True
toggle True = False

data Either a b = Left a | Right b

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  first :: (a -> b) -> p a c -> p b c
  second :: (b -> c) -> p a b -> p a c

instance Bifunctor Either where
  bimap f1 _ (Left a) = Left (f1 a)
  bimap _ f2 (Right b) = Right (f2 b)

  first f (Left a) = Left (f a)
  first _ (Right b) = Right b

  second _ (Left a) = Left a
  second f (Right b) = Right (f b)

bitoggle = bimap toggle toggle

toggleEitherLeft left f = bimap f f (Left left)

id x = x

bimapFirst = bimap toggle id

bimapSecond = bimap id toggle

bimapNone = bimap id id

bimapBoth = bimap toggle toggle
"#;
    check_inferred_ty(pgm, "bitoggle", "Bifunctor t => t Bool Bool -> t Bool Bool");
    check_inferred_ty(pgm, "toggleEitherLeft", "a -> (a -> b) -> Either b b");
    check_inferred_ty(pgm, "id", "a -> a");
    check_inferred_ty(pgm, "bimapFirst", "Bifunctor t => t Bool a -> t Bool a");
    check_inferred_ty(pgm, "bimapSecond", "Bifunctor t => t a Bool -> t a Bool");
    check_inferred_ty(pgm, "bimapNone", "Bifunctor t => t a b -> t a b");
    check_inferred_ty(
        pgm,
        "bimapBoth",
        "Bifunctor t => t Bool Bool -> t Bool Bool",
    );
}

#[test]
fn infer_higher_kinded_ty_var() {
    let pgm = r#"
class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Bool = False | True

not False = True
not True = False

class Eq a where
  (==) :: a -> a -> Bool

g1 :: (Eq (f Bool), Functor f) => f Bool -> Bool
g1 xs = fmap not xs == xs

g2 xs = fmap not xs == xs

-- Try with lambda
g3 = \xs -> fmap not xs == xs
"#;

    let ty = "(Eq (f Bool), Functor f) => f Bool -> Bool";
    check_inferred_ty(pgm, "g1", ty);
    check_inferred_ty(pgm, "g2", ty);
    check_inferred_ty(pgm, "g3", ty);
}

#[test]
fn simple_complex_pattern_binding() {
    // Test is simple, but the pattern is "complex" (i.e. not just a var).
    let pgm = r#"
class Num a where
  (+) :: a -> a -> a

f n = (n, n + 1)

(x, y) = f 5
    "#;

    check_inferred_ty(pgm, "f", "Num a => a -> (a, a)");
    check_inferred_ty(pgm, "x", "Num a => a");
    check_inferred_ty(pgm, "y", "Num a => a");
}

fn check_inferred_ty(pgm: &str, id: &str, expected_ty: &str) {
    let pgm = parse_module(pgm).unwrap();

    // Use the same renamer in both codes to be able to map same names to same ids.
    let mut renamer = Renamer::new();
    let renamed_pgm = renamer.rename_module(&pgm);

    let ids = top_binder_ids(&renamed_pgm);
    let id = find_id(&ids, id);

    let (ty_kinds, val_tys) = ti_module(&renamed_pgm);
    let inferred_ty_scheme = val_tys.get(&id).unwrap_or_else(|| {
        panic!(
            "Id `{}` is not in type environment: {:?}",
            id,
            val_tys
                .iter()
                .map(|(key, value)| format!("{} : {}", key, value))
                .collect::<Vec<_>>()
        )
    });

    let (_foralls, expected_ty_context, expected_ty) = parse_type(expected_ty).unwrap();
    let renamed_expected_ty = renamer.rename_type(&expected_ty, true);
    let renamed_expected_ty_context: Vec<ast::RenamedType> = expected_ty_context
        .into_iter()
        .map(|ty| renamer.rename_type(&ty, true))
        .collect();

    let expected_ty_scheme = Scheme::from_type_sig(
        ast::Span {
            start: Default::default(),
            end: Default::default(),
        },
        &ty_kinds,
        &Default::default(),
        &renamed_expected_ty_context,
        &renamed_expected_ty,
    );

    println!("expected: {}", expected_ty_scheme);
    println!("inferred: {}", inferred_ty_scheme);
    if !scheme_eq(&expected_ty_scheme, inferred_ty_scheme) {
        panic!(
            "Expected: {}, inferred: {}",
            expected_ty_scheme, inferred_ty_scheme
        );
    }
}

fn top_binder_ids(module: &[ast::RenamedTopDecl]) -> Set<Id> {
    let mut ids: Set<Id> = Default::default();
    for decl in module {
        match &decl.kind {
            ast::TopDeclKind::Value(ast::AstNode {
                node: ast::ValueDecl_::Value { lhs, .. },
                ..
            }) => match &lhs.node {
                ast::Lhs_::Pat(pat) => {
                    collect_pat_ids(&pat.node, &mut ids);
                }
                ast::Lhs_::Fun { var, .. } => {
                    ids.insert(var.clone());
                }
            },

            ast::TopDeclKind::Class(ast::AstNode {
                node: ast::ClassDecl_ { decls, .. },
                ..
            }) => {
                for decl in decls {
                    if let ast::ValueDecl_::TypeSig { vars, .. } = &decl.node {
                        ids.extend(vars.iter().cloned());
                    }
                }
            }

            _ => {}
        }
    }
    ids
}

fn collect_pat_ids(pat: &ast::Pat_<Id>, ids: &mut Set<Id>) {
    match pat {
        ast::Pat_::Var(id) => {
            ids.insert(id.clone());
        }

        ast::Pat_::As(id, p) => {
            ids.insert(id.clone());
            collect_pat_ids(&p.node, ids);
        }

        ast::Pat_::Lit(_) | ast::Pat_::Wildcard => {}

        ast::Pat_::Tuple(ps) | ast::Pat_::List(ps) | ast::Pat_::Con(_, ps) => {
            for p in ps {
                collect_pat_ids(&p.node, ids);
            }
        }

        ast::Pat_::Irrefutable(p) => {
            collect_pat_ids(&p.node, ids);
        }
    }
}

fn find_id(ids: &Set<Id>, id: &str) -> Id {
    for id_ in ids {
        if id_.name() == id {
            return id_.clone();
        }
    }
    panic!(
        "Id '{}' is not bound by the module, bound ids: {:?}",
        id,
        ids.iter().collect::<Vec<_>>()
    );
}

fn scheme_eq(s1: &Scheme, s2: &Scheme) -> bool {
    let Scheme {
        kinds: kinds1,
        preds: preds1,
        ty: ty1,
        span: _,
    } = s1;

    let Scheme {
        kinds: kinds2,
        preds: preds2,
        ty: ty2,
        span: _,
    } = s2;

    if kinds1 != kinds2 {
        return false;
    }

    // Sort predicates on class, then type.
    fn sort_preds(preds: &[Pred]) -> Vec<Pred> {
        let mut preds: Vec<Pred> = preds.to_vec();
        preds.sort();
        preds
    }

    let preds1 = sort_preds(preds1);
    let preds2 = sort_preds(preds2);

    if preds1 != preds2 {
        return false;
    }

    ty1 == ty2
}
