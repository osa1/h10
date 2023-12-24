#![allow(clippy::new_without_default, clippy::manual_range_contains)]

pub mod alloc;
mod ast;
mod ast_to_ty;
mod base64;
mod bind_groups;
mod class_collector;
mod class_env;
mod collections;
mod dependency_analysis;
mod id;
mod kind_inference;
mod layout_lexer;
mod lexer;
mod parser;
mod rc_id;
mod renaming;
mod token;
mod type_inference;
mod type_scheme;
mod typing;
mod unification;
pub mod utils;

use crate::type_scheme::Scheme;
use collections::{Map, TrieMap};
use id::Id;
use parser::parse_module;
use renaming::rename_module;
use type_inference::ti_module;
use typing::Kind;

use wasm_bindgen::prelude::wasm_bindgen;

#[global_allocator]
static ALLOC: alloc::AllocStats = alloc::AllocStats;

pub fn type_check_module(input: &str) -> (Map<Id, Kind>, TrieMap<Id, Scheme>) {
    let ast: Vec<ast::ParsedDecl> = parse_module(input).unwrap();
    let renamed: Vec<ast::RenamedDecl> = rename_module(&ast);
    ti_module(&renamed)
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_name = "addCompilerOutput")]
    fn add_compiler_output(s: &str);

    #[wasm_bindgen(js_name = "clearCompilerOutput")]
    fn clear_compiler_output();
}

#[wasm_bindgen(js_name = "initH10")]
pub fn my_init_function() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen(js_name = "typeCheck")]
pub fn type_check(input: &str) {
    clear_compiler_output();

    let allocs_before = alloc::ALLOCS.load(std::sync::atomic::Ordering::SeqCst);

    log(&format!(
        "Type checking {} lines of code...",
        input.lines().count()
    ));

    let (ty_kinds, bind_tys) = type_check_module(input);

    for (ty, kind) in ty_kinds {
        add_compiler_output(&format!("{} : {:?}", ty, kind));
    }

    add_compiler_output("");

    for (value, scheme) in bind_tys.iter() {
        add_compiler_output(&format!("{} : {}", value, scheme));
    }

    let allocs_after = alloc::ALLOCS.load(std::sync::atomic::Ordering::SeqCst);
    let total_allocs = allocs_after - allocs_before;
    add_compiler_output("");
    add_compiler_output(&format!(
        "Total allocations: {} bytes.",
        utils::comma_sep(&total_allocs.to_string())
    ));
}
