use h10::decl_arena::{DeclArena, DeclIdx};
use h10::incremental_update;
use h10::indentation_groups::IndentationGroup;
use h10::pos::Pos;

use std::sync::Mutex;

use tower_lsp::lsp_types::Range;

pub struct Ast {
    pub data: Mutex<AstData>,
}

pub struct AstData {
    pub arena: DeclArena,
    pub decls: Vec<DeclIdx>,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            data: Mutex::new(AstData::new()),
        }
    }

    pub fn update(&self, range: Range, text: &str) {
        self.data.lock().unwrap().update(range, text)
    }
}

impl AstData {
    fn new() -> Self {
        Self {
            arena: DeclArena::new(),
            decls: vec![],
        }
    }

    fn update(&mut self, range: Range, text: &str) {
        eprintln!("AST before update:");
        for decl in self.iter_decls() {
            eprintln!(
                "  {}-{} {:?}",
                decl.span_start(&self.arena),
                decl.span_end(&self.arena),
                decl.iter_tokens()
                    .map(|t| t.text().to_owned())
                    .collect::<String>(),
            )
        }

        let pos_start = Pos {
            line: range.start.line,
            char: range.start.character,
        };

        let pos_end = Pos {
            line: range.end.line,
            char: range.end.character,
        };

        eprintln!("Range {} - {}", pos_start, pos_end);
        eprintln!("Text = {:?}", text);

        incremental_update::remove(&mut self.arena, &mut self.decls, pos_start, pos_end);
        incremental_update::insert(&mut self.arena, &mut self.decls, pos_start, text);

        eprintln!("AST after update:");
        for decl in self.iter_decls() {
            eprintln!(
                "  {}-{} {:?}",
                decl.span_start(&self.arena),
                decl.span_end(&self.arena),
                decl.iter_tokens()
                    .map(|t| t.text().to_owned())
                    .collect::<String>(),
            );
        }
    }

    pub fn iter_decls(&self) -> impl Iterator<Item = &IndentationGroup> {
        self.decls.iter().map(|decl_idx| self.arena.get(*decl_idx))
    }
}
