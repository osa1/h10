//! Defines the entry points for LSP updates.

use crate::ast::{self, Span};
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::incremental_lexing::relex_insertion;
use crate::indentation_groups::{
    parse_indentation_groups, reparse_indentation_groups_decl, reparse_indentation_groups_token,
};
use crate::lexing::lex_full;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::Token as LexerToken;
use h10_lexer::TokenKind as LexerTokenKind;

use lexgen_util::Loc;

pub fn insert(arena: &mut DeclArena, defs: &mut Vec<DeclIdx>, pos: Pos, text: &str) {
    if text.is_empty() {
        return;
    }

    if defs.is_empty() {
        let tokens = lex_full(text);
        *defs = parse_indentation_groups(tokens, arena);
        return;
    }

    // Insert after the last group.
    if pos >= arena.get(*defs.last().unwrap()).span_end() {
        todo!()
    }

    let n_lines_inserted = (text.lines().count() - 1) as u32;

    // Insert before the first group.
    if pos < arena.get(defs[0]).span_start() {
        // Update line numbers.
        for decl_idx in defs.iter() {
            arena.get_mut(*decl_idx).line_number += n_lines_inserted;
        }

        let first_token = arena.get(defs[0]).first_token.clone();

        let empty_token = TokenRef::new(
            LexerToken {
                kind: LexerTokenKind::Whitespace,
                text: "".into(),
            },
            Span {
                start: Loc {
                    line: 0,
                    col: 0,
                    byte_idx: 0,
                },
                end: first_token.span().start,
            },
        );

        empty_token.set_next(Some(first_token));

        let token = relex_insertion(empty_token, pos, text, arena);
        reparse_indentation_groups_token(token, None, arena);
        return;
    }

    // TODO: Binary search.
    let (decl_idx_idx, decl_idx) = defs
        .iter()
        .copied()
        .enumerate()
        .find(|(_decl_idx_idx, decl_idx)| arena.get(*decl_idx).contains_location(pos))
        .unwrap();

    // Update line numbers of groups after the current one.
    if n_lines_inserted != 0 {
        for decl_idx in &defs[decl_idx_idx + 1..] {
            arena.get_mut(*decl_idx).line_number += n_lines_inserted;
        }
    }

    let updated_token: TokenRef = find_token(arena.get(decl_idx), pos);
    let relex_start_token: TokenRef = updated_token.prev().unwrap_or(updated_token);

    let token_before_start: Option<TokenRef> = relex_start_token.prev();
    let new_token = relex_insertion(relex_start_token.clone(), pos, text, arena);

    if let Some(prev_token) = token_before_start {
        prev_token.set_next(Some(new_token));
    }

    let modified_ast_node: DeclIdx = relex_start_token.ast_node().unwrap();
    arena.get_mut(modified_ast_node).kind = ast::TopDeclKind::Unparsed;

    let prev_ast_node: Option<DeclIdx> = arena.get(modified_ast_node).prev;
    reparse_indentation_groups_decl(modified_ast_node, prev_ast_node, arena);
}

fn find_token(node: &ast::TopDecl, pos: Pos) -> TokenRef {
    // Update `pos` so that the line number is relative to the declaration.
    let pos = Pos {
        line: pos.line - node.span_start().line,
        char: pos.char,
    };

    node.iter_tokens()
        .find(|t| t.contains_location(pos))
        .unwrap()
}
