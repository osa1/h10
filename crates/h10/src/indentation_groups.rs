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

// TODO: This function should never fail, return error tokens instead.
fn lex(s: &str) -> TokenRef {
    let lexer = Lexer::new(s);
    let mut first_token: Option<TokenRef> = None;
    let mut last_token: Option<TokenRef> = None;
    for t in lexer {
        let t: TokenRef = TokenRef::from_lexer_token(t.unwrap());
        if first_token.is_none() {
            first_token = Some(t.clone());
        } else if let Some(last_token_) = last_token {
            last_token_.set_next(Some(t.clone()));
        }
        last_token = Some(t.clone());
    }
    first_token.unwrap()
}

/// Starting with [`token`], lex until re-lexing [`inserted_text`], then continue re-lexing until
/// finding an identical token.
///
/// [`DeclArena`] argument is needed to be able to get absolute spans of tokens, to be able to
/// check if we've generated an identical token and stop.
///
/// [`TokenKind`] equality is based on: token kind, token text, token absolute position. I think
/// technically it can be more relaxed then this to avoid redundant work when e.g. a string literal
/// or a space (in a non-indentation position) is changed, but for now this will do.
#[allow(unused)]
fn relex(token: TokenRef, insertion_pos: Pos, inserted_text: &str, arena: &DeclArena) -> TokenRef {
    let start_loc = token.absolute_span(arena).start;
    let chars = TokenCharIteratorWithInsertion::new(token.clone(), insertion_pos, inserted_text);
    let lexer = Lexer::new_from_iter_with_loc(chars, start_loc);

    let mut inserted_text_end_pos = insertion_pos;
    for char in inserted_text.chars() {
        inserted_text_end_pos = next_pos(inserted_text_end_pos, char);
    }
    let inserted_text_end_pos = inserted_text_end_pos;

    let mut first_token: Option<TokenRef> = None;
    let mut last_token: Option<TokenRef> = None;

    let mut old_token = Some(token);
    'lexing: for token in lexer {
        let token = token.unwrap();
        let new_token = TokenRef::from_lexer_token(token);
        if first_token.is_none() {
            first_token = Some(new_token.clone());
        } else if let Some(last_token_) = &last_token {
            last_token_.set_next(Some(new_token.clone()));
        }

        let new_token_start_pos = Pos::from_loc(&new_token.span().start);
        let new_token_end_pos = Pos::from_loc(&new_token.span().end);
        if Pos::from_loc(&new_token.span().end) > inserted_text_end_pos {
            while let Some(old_token_) = &old_token {
                let old_token_start_pos = Pos::from_loc(&old_token_.span().start);
                let old_token_end_pos = Pos::from_loc(&old_token_.span().end);
                if old_token_start_pos.adjust_for_insertion(insertion_pos, inserted_text_end_pos)
                    < new_token_start_pos
                {
                    old_token = old_token_.next();
                    continue;
                }

                if old_token_.token() == new_token.token()
                    && old_token_start_pos == new_token_start_pos
                    && old_token_end_pos == new_token_end_pos
                    && old_token_.text() == new_token.text()
                {
                    if let Some(last_token_) = last_token {
                        last_token_.set_next(Some(old_token_.clone()));
                    }
                    break 'lexing;
                }

                break;
            }
        }

        last_token = Some(new_token.clone());
    }

    first_token.unwrap()
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

#[derive(Clone)]
struct TokenCharIteratorWithInsertion<'a> {
    /// Absolute position of the current character.
    current_pos: Pos,

    source: TokenCharIterSource,

    token: TokenRef,

    inserted_text: &'a str,

    /// Byte index to `token.text()` or `inserted_text`, depending on which one we're iterating.
    byte_idx: usize,

    /// Where the `inserted_text` should be inserted.
    insertion_pos: Pos,
}

#[derive(Clone, Copy)]
enum TokenCharIterSource {
    TokenBeforeInsertion,

    Insertion {
        /// Where we were at in the token we're iterating when we came to the insertion point.
        token_byte_idx: usize,
    },

    TokenAfterInsertion,
}

impl<'a> TokenCharIteratorWithInsertion<'a> {
    // NB. `insertion_pos` is the absolute position.
    fn new(token: TokenRef, insertion_pos: Pos, inserted_text: &'a str) -> Self {
        Self {
            current_pos: Pos::from_loc(&token.span().start),
            source: TokenCharIterSource::TokenBeforeInsertion,
            token,
            inserted_text,
            byte_idx: 0,
            insertion_pos,
        }
    }
}

impl<'a> Iterator for TokenCharIteratorWithInsertion<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.source {
            TokenCharIterSource::TokenBeforeInsertion => {
                if self.current_pos == self.insertion_pos {
                    self.source = TokenCharIterSource::Insertion {
                        token_byte_idx: self.byte_idx,
                    };
                    self.byte_idx = 0;
                    return self.next();
                }

                if self.byte_idx < self.token.text().len() {
                    let char = self.token.text()[self.byte_idx..].chars().next().unwrap();
                    let char_len = char.len_utf8();

                    self.byte_idx += char_len;
                    self.current_pos = next_pos(self.current_pos, char);

                    Some(char)
                } else {
                    self.token = self.token.next().unwrap();
                    self.byte_idx = 0;
                    self.next()
                }
            }

            TokenCharIterSource::Insertion { token_byte_idx } => {
                if self.byte_idx == self.inserted_text.len() {
                    self.source = TokenCharIterSource::TokenAfterInsertion;
                    self.byte_idx = token_byte_idx;
                    return self.next();
                }

                let char = self.inserted_text[self.byte_idx..].chars().next().unwrap();
                let char_len = char.len_utf8();

                self.byte_idx += char_len;

                Some(char)
            }

            TokenCharIterSource::TokenAfterInsertion => {
                if self.byte_idx < self.token.text().len() {
                    let char = self.token.text()[self.byte_idx..].chars().next().unwrap();
                    let char_len = char.len_utf8();

                    self.byte_idx += char_len;
                    self.current_pos = next_pos(self.current_pos, char);

                    Some(char)
                } else {
                    match self.token.next() {
                        None => None,
                        Some(next) => {
                            self.token = next;
                            self.byte_idx = 0;
                            self.next()
                        }
                    }
                }
            }
        }
    }
}

fn next_pos(pos: Pos, char: char) -> Pos {
    if char == '\n' {
        Pos {
            line: pos.line + 1,
            char: 0,
        }
    } else {
        Pos {
            line: pos.line,
            char: pos.char + 1,
        }
    }
}
