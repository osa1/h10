#[cfg(test)]
mod tests;

use crate::decl_arena::DeclArena;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::Lexer;

/// Starting with [`token`], lex until re-lexing [`inserted_text`] when inserted at
/// [`insertion_pos`], then continue re-lexing until finding an identical token.
///
/// [`DeclArena`] argument is needed to be able to get absolute spans of tokens, to be able to
/// check if we've generated an identical token and stop.
///
/// [`TokenKind`] equality is based on: token kind, token text, token absolute position. I think
/// technically it can be more relaxed then this to avoid redundant work when e.g. a string literal
/// or a space (in a non-indentation position) is changed, but for now this will do.
#[allow(unused)]
fn relex_insertion(
    token: TokenRef,
    insertion_pos: Pos,
    inserted_text: &str,
    arena: &DeclArena,
) -> TokenRef {
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
