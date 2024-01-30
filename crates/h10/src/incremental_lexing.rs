#[cfg(test)]
mod tests;

use crate::decl_arena::DeclArena;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::Lexer;

/// Starting with [`lex_start`], lex until re-lexing [`inserted_text`] when inserted at
/// [`insertion_pos`], then continue re-lexing until finding an identical token.
///
/// [`inserted_text`] should not be empty.
///
/// [`DeclArena`] argument is needed to be able to get absolute spans of tokens, to be able to
/// check if we've generated an identical token and stop.
///
/// The returned token is the replacement for [`lex_start`]. The caller should update:
///
/// - If [`lex_start`] is the first or last token of an AST node, the AST node.
/// - The previous token's `next`.
pub(crate) fn relex_insertion(
    lex_start: TokenRef,
    insertion_pos: Pos,
    inserted_text: &str,
    arena: &DeclArena,
) -> TokenRef {
    debug_assert!(!inserted_text.is_empty());

    let chars =
        TokenCharIteratorWithInsertion::new(lex_start.clone(), insertion_pos, inserted_text, arena);

    let mut inserted_text_end_pos = insertion_pos;
    for char in inserted_text.chars() {
        inserted_text_end_pos = next_pos(inserted_text_end_pos, char);
    }

    // Because `inserted_text` is not empty, `relex` should return at least one token and this
    // `unwrap` is safe.
    relex(lex_start, chars, inserted_text_end_pos, arena, |pos| {
        pos.adjust_for_insertion(insertion_pos, inserted_text_end_pos)
    })
    .unwrap()
}

pub(crate) fn relex_deletion(
    lex_start: TokenRef,
    deletion_start: Pos,
    deletion_end: Pos,
    arena: &DeclArena,
) -> Option<TokenRef> {
    let chars =
        TokenCharIteratorWithDeletion::new(lex_start.clone(), deletion_start, deletion_end, arena);

    relex(lex_start, chars, deletion_end, arena, |pos| {
        pos.adjust_for_deletion(deletion_start, deletion_end)
    })
}

fn relex<I, F>(
    lex_start: TokenRef,
    char_iter: I,
    update_end_pos: Pos,
    arena: &DeclArena,
    adjust_old_token_pos: F,
) -> Option<TokenRef>
where
    I: Iterator<Item = char> + Clone,
    F: Fn(Pos) -> Pos,
{
    let start_loc = lex_start.absolute_span(arena).start;

    let mut lexer = Lexer::new_from_iter_with_loc(char_iter, start_loc);

    // Collect generated tokens in a vector, link them together before returning, to avoid messing
    // with the 'next' pointers while the lexer and `old_token` below holds aliases.
    let mut tokens: Vec<TokenRef> = vec![];

    let mut old_token: Option<TokenRef> = Some(lex_start.clone());
    for token in lexer.by_ref() {
        let token = token.unwrap();
        let new_token = TokenRef::from_lexer_token(token);

        let new_token_start_pos = Pos::from_loc(&new_token.span().start);
        let new_token_end_pos = Pos::from_loc(&new_token.span().end);

        let after_update = Pos::from_loc(&new_token.span().end) > update_end_pos;

        // Find the old token at the current position, or after it if there isn't a token starting
        // at the same position.
        while let Some(old_token_) = &old_token {
            let old_token_start_pos = Pos::from_loc(&old_token_.span().start);

            if adjust_old_token_pos(old_token_start_pos) >= new_token_start_pos {
                break;
            }

            old_token = old_token_.next();
        }

        match &old_token {
            Some(old_token_) => {
                let old_token_start_pos = Pos::from_loc(&old_token_.absolute_span(arena).start);
                let old_token_end_pos = Pos::from_loc(&old_token_.absolute_span(arena).end);

                let generated_same_token = old_token_.kind() == new_token.kind()
                    && old_token_start_pos == new_token_start_pos
                    && old_token_end_pos == new_token_end_pos
                    && old_token_.text() == new_token.text();

                if generated_same_token {
                    // Reuse the old token.
                    // TODO: We can allocate a `TokenRef` only when generating a new token.
                    tokens.push(old_token_.clone());
                    if after_update {
                        break;
                    }
                    old_token = old_token_.next();
                } else {
                    tokens.push(new_token);
                }
            }
            None => {
                tokens.push(new_token);
            }
        }
    }

    if lexer.next().is_none() {
        if let Some(last) = tokens.last() {
            last.set_next(None);
        }
    }

    link_tokens(tokens)
}

fn link_tokens(tokens: Vec<TokenRef>) -> Option<TokenRef> {
    let mut iter = tokens.into_iter();
    let ret = match iter.next() {
        Some(ret) => ret,
        None => return None,
    };

    let mut current = ret.clone();
    for next in iter {
        current.set_next(Some(next.clone()));
        current = next;
    }

    Some(ret)
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
    fn new(token: TokenRef, insertion_pos: Pos, inserted_text: &'a str, arena: &DeclArena) -> Self {
        Self {
            current_pos: Pos::from_loc(&token.absolute_span(arena).start),
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
                    match self.token.next() {
                        Some(next) => {
                            self.token = next;
                        }
                        None => {
                            debug_assert_eq!(self.current_pos, self.insertion_pos);
                            self.source = TokenCharIterSource::Insertion {
                                token_byte_idx: self.byte_idx,
                            };
                        }
                    }
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

#[derive(Clone)]
struct TokenCharIteratorWithDeletion {
    /// Current position.
    current_pos: Pos,

    /// Current token.
    token: TokenRef,

    /// Byte index of the next character in the current token.
    byte_idx: usize,

    /// Where the deletion starts.
    deletion_start: Pos,

    /// Where the deletion ends.
    deletion_end: Pos,
}

impl TokenCharIteratorWithDeletion {
    fn new(token: TokenRef, deletion_start: Pos, deletion_end: Pos, arena: &DeclArena) -> Self {
        Self {
            current_pos: Pos::from_loc(&token.absolute_span(arena).start),
            token,
            byte_idx: 0,
            deletion_start,
            deletion_end,
        }
    }
}

impl Iterator for TokenCharIteratorWithDeletion {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (char, pos) = if self.byte_idx < self.token.text().len() {
                let char = self.token.text()[self.byte_idx..].chars().next().unwrap();
                let pos = self.current_pos;
                let char_len = char.len_utf8();

                self.byte_idx += char_len;
                self.current_pos = next_pos(self.current_pos, char);

                (char, pos)
            } else {
                match self.token.next() {
                    None => return None,
                    Some(next) => {
                        self.token = next;
                        self.byte_idx = 0;
                        return self.next();
                    }
                };
            };

            if pos >= self.deletion_start && pos < self.deletion_end {
                continue;
            }

            return Some(char);
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
