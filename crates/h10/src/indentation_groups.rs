#[cfg(test)]
mod tests;

use crate::ast;
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::Lexer;
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

fn parse_group(first_token: TokenRef, arena: &mut DeclArena) -> DeclIdx {
    let mut last_token = first_token.clone();

    while let Some(next_token) = last_token.next() {
        let indent = next_token.span().start.col;
        if !matches!(next_token.token(), TokenKind::Whitespace) && indent == 0 {
            break;
        }
        last_token = next_token;
    }

    let first_token_line_number = first_token.span().start.line;

    let group = ast::ParsedTopDecl {
        kind: ast::TopDeclKind::Unparsed,
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

#[allow(unused)]
pub fn insert(arena: &mut DeclArena, defs: &mut Vec<DeclIdx>, pos: Pos, text: &str) {
    if text.is_empty() {
        return;
    }

    if defs.is_empty() {
        let tokens = lex(text);
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
        .find(|(decl_idx_idx, decl_idx)| arena.get(*decl_idx).contains_location(pos))
        .unwrap();

    let updated_token = insert_to_ast_node(arena.get_mut(decl_idx), pos, text);

    // Tokenize the updated token, starting from the token at "lookback" of the updated token.
    // TODO: We don't generate lookbacks right now, but I think lookback of 1 token should handle
    // majority of the cases, if not all.

    let relex_start_token = updated_token.prev().unwrap_or(updated_token);

    // Update line numbers of groups after the current one.
    let n_lines_inserted = (text.lines().count() - 1) as u32;
    if n_lines_inserted != 0 {
        for decl_idx in &defs[decl_idx_idx + 1..] {
            arena.get_mut(*decl_idx).line_number += n_lines_inserted;
        }
    }

    // TODO: Reparse.
}

// TODO: This function should never fail, return error tokens instead.
fn lex(s: &str) -> TokenRef {
    let lexer = Lexer::new(s);
    let mut first_token: Option<TokenRef> = None;
    let mut last_token: Option<TokenRef> = None;
    for t in lexer {
        let t: TokenRef = TokenRef::from_lexer_token("", t.unwrap(), s);
        if first_token.is_none() {
            first_token = Some(t.clone());
        } else if let Some(last_token_) = last_token {
            last_token_.set_next(Some(t.clone()));
        }
        last_token = Some(t.clone());
    }
    first_token.unwrap()
}

/// Update the token in `node` with the inserted text.
///
/// Does not update spans of the tokens in the group, of spans of other groups.
///
/// Returns the updated token.
fn insert_to_ast_node(node: &mut ast::ParsedTopDecl, mut pos: Pos, text: &str) -> TokenRef {
    debug_assert!(!text.is_empty());

    // Currently all changes force a full re-parse of the top-level declaration.

    // Update `pos` so that the line number is relative to the declaration.
    pos.line -= node.span_start().line;

    // Find the token to update.
    let token = node
        .iter_tokens()
        .find(|t| t.contains_location(pos))
        .unwrap();

    let insertion_byte_idx = find_byte_idx(
        token.text.borrow().as_str(),
        Pos::from_loc(&token.span().start),
        pos,
    );

    token.text.borrow_mut().insert_str(insertion_byte_idx, text);

    token
}

/// Given a string `text` and its position `text_start`, find the byte index in `text` of `pos`.
///
/// Assumes that `pos` is within the text.
fn find_byte_idx(text: &str, text_start: Pos, pos: Pos) -> usize {
    let mut char_iter = text.char_indices();

    let mut iter_pos = text_start;
    let mut byte_idx = 0;
    while iter_pos != pos {
        let (byte_idx_, char) = char_iter.next().unwrap();
        byte_idx = byte_idx_;
        if char == '\n' {
            iter_pos.line += 1;
            iter_pos.char = 0;
        } else {
            iter_pos.char += 1;
        }
    }

    byte_idx
}

/// Starting with `start_token` lex until re-lexing `relex_token`, then continue re-lexing until
/// finding an identical token.
///
/// `DeclArena` argument is needed to be able to get absolute spans of tokens, to be able to check
/// if we've generated an identical token and stop.
///
/// TokenKind equality is based on: token kind, token text, token absolute position. I think
/// technically it can be more relaxed then this to avoid redundant work when e.g. a string literal
/// or a space (in a non-indentation position) is changed, but for now this will do.
#[allow(unused)]
fn relex(start_token: TokenRef, relex_token: TokenRef, arena: &DeclArena) -> TokenRef {
    let mut chars = start_token.iter_chars();
    let lexer = Lexer::new_from_iter(chars);
    todo!()
}

/// Given the start position of lexing and a position yielded by the lexer, get the absolute
/// position of the lexer position.
#[allow(unused)]
fn lexer_token_absolute_pos(lex_start: Pos, lexer_loc: Pos) -> Pos {
    if lexer_loc.line == 0 {
        Pos {
            line: lex_start.line,
            char: lex_start.char + lexer_loc.char,
        }
    } else {
        Pos {
            line: lex_start.line + lexer_loc.line,
            char: lexer_loc.char,
        }
    }
}
