#[cfg(test)]
mod tests;

use crate::ast;
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::token::TokenRef;
use h10_lexer::token::Token;

/// Parse indentation groups as [`ast::TopDeclKind_::Unparsed`] declarations.
pub fn parse_indentation_groups(mut token: TokenRef, arena: &mut DeclArena) -> Vec<DeclIdx> {
    // Skip initial whitespace.
    while matches!(token.token(), Token::Whitespace) {
        match token.next() {
            Some(next) => token = next,
            None => return vec![],
        }
    }

    let mut groups: Vec<DeclIdx> = vec![];

    loop {
        let group_idx = parse_group(token.clone(), arena);
        groups.push(group_idx);
        let next = arena.get(group_idx).last_token.next();

        match next {
            Some(next) => token = next,
            None => break,
        }
    }

    groups
}

#[allow(unused)]
pub fn apply_changes(
    arena: &mut DeclArena,
    defs: &[DeclIdx],
    range_start_line: u32,
    range_start_char: u32,
    range_end_line: u32,
    range_end_char: u32,
    new_text: &str,
) {
    // Find the first modified token. Using lookback (which we don't generate right now, but I
    // think we can assume "1 token"), mark the tokens as "dirty" and update texts of modified
    // tokens. Then re-lex starting from the lookback, marking AST nodes as dirty. Then re-parse
    // the dirty nodes.

    // Update removals.
    if (range_start_line, range_start_char) != (range_end_line, range_end_char) {
        for decl_idx in defs {
            let decl = arena.get_mut(*decl_idx);

            if decl.contains_location(range_start_line, range_start_char) {
                // TODO: This can remove tokens of other declarations, we should mark those
                // declarations for re-parsing.
                // TODO: Handle the `None` case.
                let new_first_token = decl
                    .first_token
                    .remove_range(
                        range_start_line,
                        range_start_char,
                        range_end_line,
                        range_end_char,
                    )
                    .unwrap();

                decl.first_token = new_first_token;

                // TODO: Make sure to update last token after insertions.

                break;
            }
        }
    }

    // Insert new text.
    // TODO: We don't have to search again here, we know the block from the previous pass above.
    let mut inserted = false;
    for decl_idx in defs {
        let decl = arena.get_mut(*decl_idx);
        if decl.contains_location(range_start_line, range_start_char) {
            decl.first_token
                .insert(range_start_line, range_start_char, new_text);
            inserted = true;
            break;
        }
    }

    assert!(inserted);

    // TODO: Re-parse indentation groups.
}

fn parse_group(first_token: TokenRef, arena: &mut DeclArena) -> DeclIdx {
    let mut last_token = first_token.clone();

    while let Some(next_token) = last_token.next() {
        let indent = next_token.span().start.col;
        if !matches!(next_token.token(), Token::Whitespace) && indent == 0 {
            break;
        }
        last_token = next_token;
    }

    let first_token_line_number = first_token.span().start.line;

    let group = ast::ParsedTopDecl {
        kind: ast::TopDeclKind_::Unparsed,
        line_number: first_token_line_number,
        first_token: first_token.clone(),
        last_token: last_token.clone(),
    };

    let group_idx = arena.allocate(group);

    // Set AST nodes of tokens in the group.
    for token in first_token.iter_until(&last_token) {
        token.set_ast_node(group_idx);

        // Line numbers of tokens attached to AST nodes will be relative to the AST node.
        token.span.borrow_mut().start.line -= first_token_line_number;
        token.span.borrow_mut().end.line -= first_token_line_number;
    }

    group_idx
}
