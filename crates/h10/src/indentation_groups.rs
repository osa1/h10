#[cfg(test)]
mod tests;

use crate::decl_arena::{DeclArena, DeclIdx};
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::{ReservedId, TokenKind};

/// A list of tokens, with initial and trailing trivia, that belongs to a single declaration.
///
/// Initial trivia includes documentation comments and any whitespace that follows the
/// documentation comments.
///
/// Trailing trivia only includes whitespace and non-documentation comments.
///
/// When the declaration (after skipping the trivia) start with tokens `class` or `instance`,
/// `nested` will point the first token of nested declarations. The nested indentation group will
/// then have the same structure, except it won't have any more nested groups.
#[derive(Debug)]
pub struct IndentationGroup {
    /// Line number of the group in its parent. When the grop is a top-level declaration, this is
    /// the group's line number in the document.
    pub line_number: u32,

    /// First token of the top-level declaration.
    ///
    /// This will include documentation comments and any whitespace that follows the documentation
    /// comments before the actual definition.
    pub first_token: TokenRef,

    /// The last token of the top-level declaration.
    ///
    /// This includes trailing whitespace before the next declaration starts.
    pub last_token: TokenRef,

    /// Next declaration in the group.
    pub next: Option<DeclIdx>,

    /// Previous declaration in the group.
    pub prev: Option<DeclIdx>,

    /// When the declaration start with the tokens `class` or `instance` (after the trivia), this
    /// holds the nested declaration after the token `where`.
    pub nested: Option<DeclIdx>,

    /// When the declaration is nested, this is the parent's index.
    pub parent: Option<DeclIdx>,

    /// Whether the declaration was modified since the last time it was parsed.
    pub modified: bool,
}

impl IndentationGroup {
    fn new(line_number: u32, first_token: TokenRef, last_token: TokenRef) -> Self {
        IndentationGroup {
            line_number,
            first_token,
            last_token,
            next: None,
            prev: None,
            nested: None,
            parent: None,
            modified: false,
        }
    }

    /// Start location of the group in the document.
    pub fn span_start(&self, arena: &DeclArena) -> Pos {
        self.first_token.absolute_span(arena).start
    }

    /// End location of the group in the document.
    pub fn span_end(&self, arena: &DeclArena) -> Pos {
        self.last_token.absolute_span(arena).end
    }

    /// Whether the given position is spanned by the current group.
    pub fn contains_location(&self, pos: Pos, arena: &DeclArena) -> bool {
        pos >= self.span_start(arena) && pos < self.span_end(arena)
    }

    pub fn iter_tokens(&self) -> impl Iterator<Item = TokenRef> {
        self.first_token.iter_until(&self.last_token)
    }

    /// Line number of the group in the document.
    pub fn line_number(&self, arena: &DeclArena) -> u32 {
        let mut line = self.line_number;
        if let Some(parent) = self.parent {
            line += arena.get(parent).line_number(arena);
        }
        line
    }

    // NB. Does not update nested group token.
    fn set_token_ast_nodes(&self, decl_idx: DeclIdx, arena: &DeclArena) {
        let first_token = self.first_token.clone();
        let last_token = match self.nested {
            Some(nested_group) => arena.get(nested_group).first_token.prev().unwrap(),
            None => self.last_token.clone(),
        };
        for token in first_token.iter_until(&last_token) {
            token.set_ast_node(decl_idx, arena);
        }
    }
}

/// Parse top-level declarations starting with [`token`] and return the indices of parsed
/// declarations.
pub(crate) fn parse_indentation_groups(token: TokenRef, arena: &mut DeclArena) -> Vec<DeclIdx> {
    let mut decl_idx = parse(token, arena);
    let mut decls: Vec<DeclIdx> = vec![decl_idx];

    while let Some(next) = arena.get(decl_idx).next {
        decls.push(next);
        decl_idx = next;
    }

    decls
}

pub(crate) fn reparse_indentation_groups_decl(
    decl_idx: DeclIdx,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) -> DeclIdx {
    let token = arena.get(decl_idx).first_token.clone();
    reparse_indentation_groups_token(token, prev_decl_idx, arena)
}

pub(crate) fn reparse_indentation_groups_token(
    new_group_start: TokenRef,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) -> DeclIdx {
    let decl_idx = parse(new_group_start, arena);

    if let Some(prev_decl_idx) = prev_decl_idx {
        arena.get_mut(decl_idx).prev = Some(prev_decl_idx);
        arena.get_mut(prev_decl_idx).next = Some(decl_idx);
    }

    decl_idx
}

pub(crate) fn parse(token: TokenRef, arena: &mut DeclArena) -> DeclIdx {
    let mut parser = Parser::new(token);
    parser.indentation_group(arena, false)
}

#[derive(Debug)]
struct Parser {
    next: Option<TokenRef>,
    first: TokenRef,
    last: TokenRef,
}

impl Parser {
    fn new(next: TokenRef) -> Self {
        Parser {
            next: Some(next.clone()),
            first: next.clone(),
            last: next,
        }
    }

    fn next(&mut self) -> Option<TokenRef> {
        let next = self.peek()?;
        self.next = next.next();
        self.last = next.clone();
        Some(next)
    }

    fn peek(&mut self) -> Option<TokenRef> {
        loop {
            let next = self.next.as_ref()?;
            if is_trivia(next) {
                self.last = next.clone();
                self.next = next.next();
                continue;
            } else {
                return Some(next.clone());
            }
        }
    }

    fn indentation_group(&mut self, arena: &mut DeclArena, nested: bool) -> DeclIdx {
        let token = match self.next() {
            Some(token) => token,
            None => {
                // All whitespace and comments.
                let line_number = self.first.absolute_span(arena).start.line;
                let decl_idx = arena.allocate(IndentationGroup::new(
                    line_number,
                    self.first.clone(),
                    self.last.clone(),
                ));
                arena.get(decl_idx).set_token_ast_nodes(decl_idx, arena);
                return decl_idx;
            }
        };

        if let Some(reused_idx) = reuse(&token, arena) {
            return reused_idx;
        }

        let indentation = token.indentation();

        // If the first non-trivia token is a documentation comment, include the next non-trivia
        // token at the same indentation in the group.
        if is_documentation(&token) {
            if let Some(next) = self.peek() {
                if next.indentation() >= indentation {
                    self.next();
                }
            }
        }

        if indentation == 0
            && matches!(
                self.last.kind(),
                TokenKind::ReservedId(ReservedId::Class | ReservedId::Instance)
            )
        {
            // Skip until `where`, start a nested group. `where` should be indented to avoid
            // including the whole rest of the document when it's missing.
            while let Some(next) = self.peek() {
                if next.indentation() > 0
                    && matches!(next.kind(), TokenKind::ReservedId(ReservedId::Where))
                {
                    // Skip to `where`.
                    self.next();
                    // Start a new group right after `where`.
                    let group_idx = match self.next() {
                        Some(nested_group_start) if nested_group_start.indentation() > 0 => {
                            let nested_group_idx = match reuse(&nested_group_start, arena) {
                                Some(reused_group_idx) => reused_group_idx,
                                None => {
                                    let mut nested_group_parser = Parser::new(nested_group_start);
                                    nested_group_parser.indentation_group(arena, true)
                                }
                            };

                            // TODO: This is a bit hacky: since the nested group can be a list of
                            // groups, we need to walk the list to get the last token.
                            fn group_last_token(
                                mut group_idx: DeclIdx,
                                arena: &DeclArena,
                            ) -> TokenRef {
                                loop {
                                    let group = arena.get(group_idx);
                                    match group.next {
                                        Some(next) => group_idx = next,
                                        None => return group.last_token.clone(),
                                    }
                                }
                            }

                            // End of the nested group also ends the parent group.
                            let line_number = self.first.absolute_span(arena).start.line;
                            let mut parent_group = IndentationGroup::new(
                                line_number,
                                self.first.clone(),
                                group_last_token(nested_group_idx, arena),
                            );
                            parent_group.nested = Some(nested_group_idx);
                            let parent_group_idx = arena.allocate(parent_group);
                            arena
                                .get(parent_group_idx)
                                .set_token_ast_nodes(parent_group_idx, arena);
                            arena.get_mut(nested_group_idx).parent = Some(parent_group_idx);
                            parent_group_idx
                        }
                        _ => {
                            let line_number = self.first.absolute_span(arena).start.line;
                            let group = IndentationGroup::new(
                                line_number,
                                self.first.clone(),
                                self.last.clone(),
                            );
                            let group_idx = arena.allocate(group);
                            arena.get(group_idx).set_token_ast_nodes(group_idx, arena);
                            group_idx
                        }
                    };

                    if let Some(next) = arena.get(group_idx).last_token.next() {
                        let mut parser = Parser::new(next);
                        let next_group_idx = parser.indentation_group(arena, nested);
                        arena.get_mut(group_idx).next = Some(next_group_idx);
                        arena.get_mut(next_group_idx).prev = Some(group_idx);
                        self.last = parser.last;
                    }

                    return group_idx;
                }

                if next.indentation() == 0 {
                    break;
                }

                self.next();
            }
        }

        // No nested groups parsed. Continue as normal.
        // Include indented tokens, then trailing trivia.
        while let Some(next) = self.peek() {
            if next.indentation() > indentation {
                self.next();
            } else {
                break;
            }
        }

        let line_number = self.first.absolute_span(arena).start.line;
        let group = IndentationGroup::new(line_number, self.first.clone(), self.last.clone());
        let next_token = group.last_token.next();
        let group_idx = arena.allocate(group);
        arena.get(group_idx).set_token_ast_nodes(group_idx, arena);

        if let Some(next) = next_token {
            if !nested || next.indentation() > 0 {
                let mut parser = Parser::new(next);
                let next_group_idx = parser.indentation_group(arena, nested);
                arena.get_mut(group_idx).next = Some(next_group_idx);
                arena.get_mut(next_group_idx).prev = Some(group_idx);
                self.last = parser.last;
            }
        }

        group_idx
    }
}

fn is_trivia(token: &TokenRef) -> bool {
    matches!(
        token.kind(),
        TokenKind::Whitespace
            | TokenKind::Comment {
                documentation: false
            }
    )
}

fn is_documentation(token: &TokenRef) -> bool {
    matches!(
        token.kind(),
        TokenKind::Comment {
            documentation: true
        }
    )
}

fn reuse(token: &TokenRef, arena: &DeclArena) -> Option<DeclIdx> {
    let token_ast_node_idx = match token.ast_node() {
        Some(idx) => idx,
        None => return None,
    };

    let token_ast_node = arena.get(token_ast_node_idx);

    if token_ast_node.modified || &token_ast_node.first_token != token {
        return None;
    }

    let next_token = match token.next() {
        Some(next) => next,
        None => {
            // Group is the last in the document and not modified, reuse.
            return Some(token_ast_node_idx);
        }
    };

    let next_group_idx = match next_token.ast_node() {
        Some(next_group_idx) => next_group_idx,
        None => {
            // Not sure when this can happen, but the next token doesn't have an AST node. Do the
            // safe thing and re-parse.
            return None;
        }
    };

    let next_group = arena.get(next_group_idx);

    // TODO: We should just check if some of the next group needs to be merged with the current
    // group, instead of checking `modified`. For example, maybe only the last token modified and
    // the indentation of the first token is still 0, in that case we can reuse the current node.
    if next_group.modified || next_group.first_token != next_token {
        return None;
    }

    Some(token_ast_node_idx)
}
