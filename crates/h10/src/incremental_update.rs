//! Defines the entry points for LSP updates.

#[cfg(test)]
mod tests;

use crate::ast::{self, Span};
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::incremental_lexing::{relex_deletion, relex_insertion};
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
        let tokens = lex_full(text, pos);
        *defs = parse_indentation_groups(tokens, arena);
        return;
    }

    // NB. We can't use `lines` here as it yields one line for both "a" and "a\n".
    let n_lines_inserted = text.chars().filter(|c| *c == '\n').count() as u32;

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
        let mut decl = reparse_indentation_groups_token(token, None, arena);

        defs.clear();
        loop {
            defs.push(decl);
            decl = match arena.get(decl).next {
                Some(next) => next,
                None => break,
            };
        }

        return;
    }

    let decl_idx_idx = find_ast(arena, defs, pos);
    let decl_idx = defs[decl_idx_idx];

    // TODO: When we insert at the beginning of the declaration at `decl_idx` we can update the
    // line number of the decl and see if we can reuse it.

    // Update line numbers of groups after the current one.
    if n_lines_inserted != 0 {
        for decl_idx in &defs[decl_idx_idx + 1..] {
            arena.get_mut(*decl_idx).line_number += n_lines_inserted;
        }
    }

    let updated_token: TokenRef = find_token(arena.get(decl_idx), pos)
        .unwrap_or_else(|| arena.get(decl_idx).last_token.clone());
    if let Some(decl_idx) = updated_token.ast_node() {
        arena.get_mut(decl_idx).modified = true;
    }

    let relex_start_token: TokenRef = updated_token
        .prev()
        .unwrap_or_else(|| updated_token.clone());
    let token_before_start: Option<TokenRef> = relex_start_token.prev();
    let new_token = relex_insertion(relex_start_token.clone(), pos, text, arena);

    if let Some(prev_token) = token_before_start {
        prev_token.set_next(Some(new_token.clone()));
    }

    if relex_start_token.is_first_token(arena) {
        arena
            .get_mut(relex_start_token.ast_node().unwrap())
            .first_token = new_token;
    }

    let modified_ast_node: DeclIdx = relex_start_token.ast_node().unwrap();
    arena.get_mut(modified_ast_node).modified = true;

    let prev_ast_node: Option<DeclIdx> = arena.get(modified_ast_node).prev;
    let mut decl = reparse_indentation_groups_decl(modified_ast_node, prev_ast_node, arena);

    defs.drain(decl_idx_idx..);
    if relex_start_token.ast_node() != updated_token.ast_node() {
        defs.pop();
    }
    loop {
        defs.push(decl);
        decl = match arena.get(decl).next {
            Some(next) => next,
            None => break,
        };
    }
}

pub fn remove(
    arena: &mut DeclArena,
    defs: &mut Vec<DeclIdx>,
    removal_start: Pos,
    removal_end: Pos,
) {
    if removal_start == removal_end {
        return;
    }

    if defs.is_empty() {
        // Happens when the document is only whitespace.
        return;
    }

    // Since trailing whitespaces are included in the groups, we should be able to find a group
    // at the given position.
    let decl_idx_idx = find_ast(arena, defs, removal_start);
    let decl_idx = defs[decl_idx_idx];

    let updated_token = find_token(arena.get(decl_idx), removal_start).unwrap();
    let relex_start_token: TokenRef = updated_token
        .prev()
        .unwrap_or_else(|| updated_token.clone());

    // New token will be linked to this.
    let token_before_start: Option<TokenRef> = relex_start_token.prev();

    let n_lines_removed = removal_end.line - removal_start.line;

    // Update line numbers of groups after the current one.
    if n_lines_removed != 0 {
        for decl_idx in &defs[decl_idx_idx + 1..] {
            arena.get_mut(*decl_idx).line_number -= n_lines_removed;
        }
    }

    let new_token = relex_deletion(relex_start_token.clone(), removal_start, removal_end, arena);

    // Update links to `relex_start_token` with `new_token`.
    if let Some(prev_token) = token_before_start {
        prev_token.set_next(Some(new_token.clone()));
    }

    if relex_start_token.is_first_token(arena) {
        arena
            .get_mut(relex_start_token.ast_node().unwrap())
            .first_token = new_token;
    }

    let modified_ast_node: DeclIdx = relex_start_token.ast_node().unwrap();
    arena.get_mut(modified_ast_node).modified = true;

    let prev_ast_node: Option<DeclIdx> = arena.get(modified_ast_node).prev;
    let mut decl = reparse_indentation_groups_decl(modified_ast_node, prev_ast_node, arena);

    defs.drain(decl_idx_idx..);
    if relex_start_token.ast_node() != updated_token.ast_node() {
        defs.pop();
    }
    loop {
        defs.push(decl);
        decl = match arena.get(decl).next {
            Some(next) => next,
            None => break,
        };
    }
}

/// Find the declaration for the change applied to [`pos`]. Returns the index of the declaration's
/// index in [`defs`].
fn find_ast(arena: &DeclArena, defs: &[DeclIdx], pos: Pos) -> usize {
    for (decl_idx_idx, decl_idx) in defs.iter().enumerate() {
        if arena.get(*decl_idx).contains_location(pos) {
            return decl_idx_idx;
        }
    }

    assert!(pos >= arena.get(*defs.last().unwrap()).span_end());
    defs.len() - 1
}

fn find_token(node: &ast::TopDecl, pos: Pos) -> Option<TokenRef> {
    // Update `pos` so that the line number is relative to the declaration.
    let pos = Pos {
        line: pos.line - node.span_start().line,
        char: pos.char,
    };

    node.iter_tokens().find(|t| t.contains_location(pos))
}
