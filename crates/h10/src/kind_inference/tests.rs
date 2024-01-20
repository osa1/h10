use crate::ast;
use crate::collections::Map;
use crate::id::{type_ty_tyref, Id};
use crate::kind_inference::infer_type_kinds;
use crate::parser::parse_module;
use crate::renaming::Renamer;
use crate::type_inference::make_fun_ty;
use crate::typing::TyRef;

#[test]
fn defaulting() {
    let pgm = "data K a = K Int";
    let module = parse_and_rename(pgm);
    let kinds: Map<Id, TyRef> = infer_type_kinds(&module);

    let k_id: Id = module[0].data().node.ty_con.clone();
    assert_eq!(
        kinds.get(&k_id).unwrap(),
        &make_fun_ty(vec![type_ty_tyref()], type_ty_tyref()),
    );
}

#[test]
fn simple() {
    let pgm = r#"
data T a  = T a
data P1 a = MkP1 P2
data P2   = MkP2 (P1 T)
"#;
    let module = parse_and_rename(pgm);
    let kinds: Map<Id, TyRef> = infer_type_kinds(&module);

    let t_id: Id = module[0].data().node.ty_con.clone();
    let p1_id: Id = module[1].data().node.ty_con.clone();
    let p2_id: Id = module[2].data().node.ty_con.clone();
    assert_eq!(
        kinds.get(&t_id).unwrap(),
        &make_fun_ty(vec![type_ty_tyref()], type_ty_tyref()),
    );
    assert_eq!(
        kinds.get(&p1_id).unwrap(),
        // (* -> *) -> *
        &make_fun_ty(
            vec![make_fun_ty(vec![type_ty_tyref()], type_ty_tyref())],
            type_ty_tyref(),
        ),
    );
    assert_eq!(kinds.get(&p2_id).unwrap(), &type_ty_tyref());
}

#[test]
fn not_h10() {
    // In Haskell 2010 this is an error, but we accept it, as we analyze all type definitions as
    // one dependency group.
    let pgm = r#"
data T a  = T a
data Q1 a = MkQ1
data Q2   = MkQ2 (Q1 T)
"#;
    let module = parse_and_rename(pgm);
    let kinds: Map<Id, TyRef> = infer_type_kinds(&module);

    let t_id: Id = module[0].data().node.ty_con.clone();
    let q1_id: Id = module[1].data().node.ty_con.clone();
    let q2_id: Id = module[2].data().node.ty_con.clone();
    assert_eq!(
        kinds.get(&t_id).unwrap(),
        &make_fun_ty(vec![type_ty_tyref()], type_ty_tyref()),
    );
    assert_eq!(
        kinds.get(&q1_id).unwrap(),
        // (* -> *) -> *
        &make_fun_ty(
            vec![make_fun_ty(vec![type_ty_tyref()], type_ty_tyref())],
            type_ty_tyref(),
        ),
    );
    assert_eq!(kinds.get(&q2_id).unwrap(), &type_ty_tyref());
}

fn parse_and_rename(pgm: &str) -> Vec<ast::RenamedTopDecl> {
    let parsed = parse_module(pgm).unwrap();
    let mut renamer = Renamer::new();
    renamer.rename_module(&parsed)
}
