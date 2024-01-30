#[cfg(test)]
mod tests;

use crate::ast;
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::token::TokenRef;
use h10_lexer::TokenKind;

/// Parse indentation groups as [`ast::TopDeclKind::Unparsed`] declarations.
pub(crate) fn parse_indentation_groups(token: TokenRef, arena: &mut DeclArena) -> Vec<DeclIdx> {
    let mut top_decl = reparse_indentation_groups_token(token.clone(), None, arena);

    let mut decls: Vec<DeclIdx> = vec![];
    decls.push(top_decl);
    while let Some(next) = arena.get(top_decl).next {
        decls.push(next);
        top_decl = next;
    }

    decls
}

/// Re-parse indentation groups after re-lexing, starting with the group at [`decl_idx`].
///
/// Returns the new declaraiton index for the declaration at [`decl_idx`]. When the declaration is
/// reused it's the same as the [`decl_idx`].
///
/// Reused AST nodes are turned into [`TopDecl::Unparsed`] as they need to be re-parsed.
///
/// New AST nodes are introduces as [`TopDecl::Unparsed`].
///
/// AST nodes with no modified tokens are re-used if the token after the last token of the groups
/// is still a non-whitesapce token at column 0. If not, the two groups are merged in a new
/// [`TopDecl::Unparsed`]. This is the lookahead check before reducing a non-terminal.
pub(crate) fn reparse_indentation_groups_decl(
    decl_idx: DeclIdx,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) -> DeclIdx {
    let decl = arena.get(decl_idx);

    // A node can be reused if it's not modified and the next token's indentation was not
    // updated.
    //
    // Next token indentation check is basically the lookahead check before reducing a
    // non-terminal.
    let reuse = !decl.modified
        && match decl.last_token.next() {
            Some(next) => is_group_start(&next),
            None => true,
        };

    if reuse {
        if let Some(prev_decl_idx) = prev_decl_idx {
            arena.get_mut(prev_decl_idx).next = Some(decl_idx);
        }

        let decl = arena.get(decl_idx);

        if let Some(next_token) = decl.last_token.next() {
            match decl.next {
                Some(next_decl_idx) => {
                    if next_token == arena.get(next_decl_idx).first_token {
                        reparse_indentation_groups_decl(next_decl_idx, Some(decl_idx), arena);
                    } else {
                        reparse_indentation_groups_token(next_token, Some(decl_idx), arena);
                    }
                }
                None => {
                    reparse_indentation_groups_token(next_token, Some(decl_idx), arena);
                }
            }
        }

        return decl_idx;
    }

    reparse_indentation_groups_token(decl.first_token.clone(), prev_decl_idx, arena)
}

/// Re-parse indentation groups starting with the token [`new_group_start`].
///
/// Returns the declaration index of the first declaration starting at [`new_group_start`].
pub(crate) fn reparse_indentation_groups_token(
    new_group_start: TokenRef,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) -> DeclIdx {
    let new_group_end = find_group_end(new_group_start.clone());

    let new_group = ast::TopDecl {
        kind: ast::TopDeclKind::Unparsed,
        line_number: new_group_start.absolute_span(arena).start.line,
        first_token: new_group_start.clone(),
        last_token: new_group_end.clone(),
        next: None,
        prev: prev_decl_idx,
        modified: false,
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
        match next_group_start.ast_node() {
            Some(next_group_idx) => {
                reparse_indentation_groups_decl(next_group_idx, Some(new_decl_idx), arena);
            }
            None => {
                reparse_indentation_groups_token(next_group_start, Some(new_decl_idx), arena);
            }
        }
    }

    new_decl_idx
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
