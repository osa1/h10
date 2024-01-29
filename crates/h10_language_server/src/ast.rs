use h10::ast;
use h10::decl_arena::{DeclArena, DeclIdx};
use h10::incremental_update;
use h10::pos::Pos;

use std::fs::File;
use std::io::Write;
use std::sync::Mutex;

use tower_lsp::lsp_types::Range;

pub struct Ast {
    pub data: Mutex<AstData>,
}

pub struct AstData {
    arena: DeclArena,
    decls: Vec<DeclIdx>,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            data: Mutex::new(AstData::new()),
        }
    }

    pub fn update(&self, range: Range, text: &str, log_file: &mut File) {
        self.data.lock().unwrap().update(range, text, log_file)
    }
}

impl AstData {
    fn new() -> Self {
        Self {
            arena: DeclArena::new(),
            decls: vec![],
        }
    }

    fn update(&mut self, range: Range, text: &str, log_file: &mut File) {
        writeln!(log_file, "AST before update:").unwrap();
        for decl in self.iter_decls() {
            writeln!(log_file, "  {:?}", decl).unwrap();
        }

        let pos_start = Pos {
            line: range.start.line,
            char: range.start.character,
        };

        let pos_end = Pos {
            line: range.end.line,
            char: range.end.character,
        };

        incremental_update::remove(&mut self.arena, &mut self.decls, pos_start, pos_end);
        incremental_update::insert(&mut self.arena, &mut self.decls, pos_start, text);

        writeln!(log_file, "AST after update:").unwrap();
        for decl in self.iter_decls() {
            writeln!(
                log_file,
                "  {:?} {}-{}",
                decl.kind,
                decl.span_start(),
                decl.span_end()
            )
            .unwrap();
        }
    }

    pub fn iter_decls(&self) -> impl Iterator<Item = &ast::TopDecl> {
        self.decls.iter().map(|decl_idx| self.arena.get(*decl_idx))
    }
}
