#[cfg(test)]
mod tests;

use crate::ast;
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::lexing::lex_full;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::TokenKind;

/// Parse indentation groups as [`ast::TopDeclKind::Unparsed`] declarations.
fn parse_indentation_groups(mut token: TokenRef, arena: &mut DeclArena) -> Vec<DeclIdx> {
    // Skip initial whitespace.
    while matches!(token.token(), TokenKind::Whitespace) {
        match token.next() {
            Some(next) => token = next,
            None => return vec![],
        }
    }

    reparse_indentation_groups_token(token.clone(), None, arena);

    let mut decls: Vec<DeclIdx> = vec![];
    let mut top_decl = token.ast_node().unwrap();
    decls.push(top_decl);
    while let Some(next) = arena.get(top_decl).next {
        decls.push(next);
        top_decl = next;
    }

    decls
}

/// Re-parse indentation groups after re-lexing, starting with the group at [`decl_idx`].
///
/// Reused AST nodes are turned into [`TopDecl::Unparsed`] as they need to be re-parsed.
///
/// New AST nodes are introduces as [`TopDecl::Unparsed`].
///
/// AST nodes with no modified tokens are re-used if the token after the last token of the groups
/// is still a non-whitesapce token at column 0. If not, the two groups are merged in a new
/// [`TopDecl::Unparsed`]. This is the lookahead check before reducing a non-terminal.
fn reparse_indentation_groups_decl(
    decl_idx: DeclIdx,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) {
    let decl = arena.get(decl_idx);

    // A node can be reused if it's not modified and the next token's indentation was not
    // updated.
    //
    // Next token indentation check is basically the lookahead check before reducing a
    // non-terminal.
    let reuse = !matches!(decl.kind, ast::TopDeclKind::Unparsed)
        && match decl.last_token.next() {
            Some(next) => is_group_start(&next),
            None => true,
        };

    if reuse {
        match decl.next {
            Some(next_decl_idx) => {
                return reparse_indentation_groups_decl(next_decl_idx, Some(decl_idx), arena);
            }
            None => {
                return;
            }
        }
    }

    reparse_indentation_groups_token(decl.first_token.clone(), prev_decl_idx, arena);
}

/// Re-parse indentationg groups starting with the token [`new_group_start`].
fn reparse_indentation_groups_token(
    new_group_start: TokenRef,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) {
    let new_group_end = find_group_end(new_group_start.clone());

    let new_group = ast::TopDecl {
        kind: ast::TopDeclKind::Unparsed,
        // TODO: this is probably not right as the token line number will be relative?
        line_number: new_group_start.span().start.line,
        first_token: new_group_start.clone(),
        last_token: new_group_end.clone(),
        next: None,
        prev: prev_decl_idx,
    };
    let new_decl_idx = arena.allocate(new_group);
    if let Some(prev_decl_idx) = prev_decl_idx {
        arena.get_mut(prev_decl_idx).next = Some(new_decl_idx);
    }

    let break_next_down = !new_group_end.is_last_token(arena);

    for token in new_group_start.iter_until(&new_group_end) {
        token.set_ast_node(new_decl_idx, arena);
    }

    if break_next_down {
        if let Some(next_group_start) = new_group_end.next() {
            reparse_indentation_groups_token(next_group_start, Some(new_decl_idx), arena);
        }
    } else if let Some(next_group_start) = new_group_end.next() {
        reparse_indentation_groups_decl(
            next_group_start.ast_node().unwrap(),
            Some(new_decl_idx),
            arena,
        );
    }
}

/// Find the last token of the indentation group of the given token.
///
/// Starting with the given token, follow the links until finding a start of an indentation group
/// and return the token before that.
fn find_group_end(token: TokenRef) -> TokenRef {
    let mut last_token = token;
    while let Some(next_token) = last_token.next() {
        if is_group_start(&next_token) {
            break;
        }
        last_token = next_token;
    }
    last_token
}

/// Whether the token is the start of an indentation group.
fn is_group_start(token: &TokenRef) -> bool {
    !matches!(token.token(), TokenKind::Whitespace) && token.span().start.col == 0
}

#[allow(unused)]
pub fn insert(arena: &mut DeclArena, defs: &mut Vec<DeclIdx>, pos: Pos, text: &str) {
    if text.is_empty() {
        return;
    }

    if defs.is_empty() {
        let tokens = lex_full(text);
        *defs = parse_indentation_groups(tokens, arena);
        return;
    }

    // Insert before the first group.
    if pos < arena.get(defs[0]).span_start() {
        todo!()
    }

    // Insert after the last group.
    if pos >= arena.get(*defs.last().unwrap()).span_end() {
        todo!()
    }

    // TODO: Binary search.
    let (decl_idx_idx, decl_idx) = defs
        .iter()
        .copied()
        .enumerate()
        .find(|(_decl_idx_idx, decl_idx)| arena.get(*decl_idx).contains_location(pos))
        .unwrap();

    // Update line numbers of groups after the current one.
    let n_lines_inserted = (text.lines().count() - 1) as u32;
    if n_lines_inserted != 0 {
        for decl_idx in &defs[decl_idx_idx + 1..] {
            arena.get_mut(*decl_idx).line_number += n_lines_inserted;
        }
    }

    let updated_token = find_token(arena.get(decl_idx), pos);
    let relex_start_token = updated_token.prev().unwrap_or(updated_token);

    // TODO: Reparse.
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
