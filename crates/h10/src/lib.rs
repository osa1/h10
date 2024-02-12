#![allow(clippy::new_without_default, clippy::manual_range_contains)]

pub mod alloc;
pub mod arena;
pub mod ast;
mod ast_to_ty;
mod bind_groups;
mod class_collector;
mod class_env;
mod collections;
mod decl_info;
mod dependency_analysis;
mod id;
mod incremental_lexing;
pub mod incremental_update;
pub mod indentation_groups;
mod kind_inference;
mod layout_token_generator;
mod lexing;
mod parser;
pub mod pos;
mod scc;
mod scope_map;
pub mod token;
mod type_inference;
mod type_scheme;
mod typing;
mod unification;
pub mod utils;

use crate::type_scheme::Scheme;
use collections::{Map, TrieMap};
use id::Id;
use parser::parse_module;
use type_inference::ti_module;
use typing::TyRef;

use wasm_bindgen::prelude::wasm_bindgen;

#[global_allocator]
static ALLOC: alloc::AllocStats = alloc::AllocStats;

pub fn type_check_module(input: &str) -> (Vec<ast::TopDecl>, Map<Id, TyRef>, TrieMap<Id, Scheme>) {
    let ast: Vec<ast::TopDecl> = parse_module(input).unwrap();
    let (kinds, types) = ti_module(&ast);
    (ast, kinds, types)
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

    let (_, ty_kinds, bind_tys) = type_check_module(input);

    for (ty, kind) in ty_kinds {
        add_compiler_output(&format!("{} : {}", ty, kind));
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
