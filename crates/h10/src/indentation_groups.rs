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

    let group = ast::TopDecl {
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

/// Given a string `text` and its position `text_start`, find the byte index in `text` of `pos`.
///
/// Assumes that `pos` is within the text.
#[allow(unused)]
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
    let chars = TokenCharIteratorWithInsertion::new(token, insertion_pos, inserted_text);
    let lexer = Lexer::new_from_iter_with_loc(chars, start_loc);

    let mut inserted_text_end_pos = insertion_pos;
    for char in inserted_text.chars() {
        inserted_text_end_pos = next_pos(inserted_text_end_pos, char);
    }
    let inserted_text_end_pos = inserted_text_end_pos;

    let mut first_token: Option<TokenRef> = None;
    let mut last_token: Option<TokenRef> = None;

    for token in lexer {
        let token = token.unwrap();
        let token_ref = TokenRef::from_lexer_token(token);
        if first_token.is_none() {
            first_token = Some(token_ref.clone());
        } else if let Some(last_token_) = last_token {
            last_token_.set_next(Some(token_ref.clone()));
        }
        last_token = Some(token_ref.clone());

        // TODO: Stop after finding an identical token.
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

                if self.token.text().len() < self.byte_idx {
                    let char = self.token.text()[self.byte_idx..].chars().next().unwrap();
                    let char_len = char.len_utf8();

                    self.byte_idx += char_len;
                    self.current_pos = next_pos(self.current_pos, char);

                    Some(char)
                } else {
                    self.token = self.token.next().unwrap();
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
                if self.token.text().len() < self.byte_idx {
                    let char = self.token.text()[self.byte_idx..].chars().next().unwrap();
                    let char_len = char.len_utf8();

                    self.byte_idx += char_len;
                    self.current_pos = next_pos(self.current_pos, char);

                    Some(char)
                } else {
                    self.token = self.token.next().unwrap();
                    self.next()
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
