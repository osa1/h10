use super::*;
use crate::ast::TopDecl;
use crate::parser::parse_module;

#[test]
fn id() {
    let pgm = "id x = x";
    let mut decl_infos = program_decl_infos(pgm);
    assert_eq!(decl_infos.len(), 1);

    let DeclInfo {
        defines: Defs {
            tys: def_tys,
            values: def_values,
        },
        uses: Uses {
            tys: used_tys,
            values: used_values,
        },
    } = decl_infos.remove(0);

    assert_eq!(to_sorted_vec(&def_tys), empty_id_vec());
    assert_eq!(to_sorted_vec(&def_values), vec!["id"]);
    assert_eq!(to_sorted_vec(&used_tys), empty_id_vec());
    assert_eq!(to_sorted_vec(&used_values), empty_id_vec());
}

fn to_sorted_vec(set: &Set<String>) -> Vec<&str> {
    let mut values: Vec<&str> = set.iter().map(|s| s.as_str()).collect();
    values.sort();
    values
}

fn empty_id_vec<'a>() -> Vec<&'a str> {
    Vec::<&str>::new()
}

fn program_decl_infos(pgm: &str) -> Vec<DeclInfo> {
    let decls: Vec<TopDecl> = parse_module(pgm).unwrap();
    let mut decl_infos: Vec<DeclInfo> = Vec::with_capacity(decls.len());

    for decl in decls {
        decl_infos.push(DeclInfo::new(&decl.kind));
    }

    decl_infos
}
