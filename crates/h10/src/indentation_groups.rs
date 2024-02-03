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
}

/// Parse top-level declarations starting with [`token`] and return the indices of parsed
/// declarations.
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

pub(crate) fn reparse_indentation_groups_decl(
    decl_idx: DeclIdx,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) -> DeclIdx {
    let decl = arena.get(decl_idx);
    let current_indentation = decl.first_token.indentation();

    // A group can be reused if it's not modified and the next token's indentation was not updated.
    //
    // If the next token's indentation was updated then it should be merged to the current token.
    // This can be done more efficiently, but for now we process the entirety of the current group
    // when this happens.
    //
    // Next token indentation check is basically the lookahead check before reducing a
    // non-terminal.
    let reuse = !decl.modified
        && match decl.last_token.next() {
            Some(next) => is_group_start(current_indentation, &next),
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
                        // Next tokens still starts the next group, try to reuse it.
                        reparse_indentation_groups_decl(next_decl_idx, Some(decl_idx), arena);
                    } else {
                        // Next group does not start a group, or does not start the next group.
                        // Re-parse it.
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

pub(crate) fn reparse_indentation_groups_token(
    new_group_start: TokenRef,
    prev_decl_idx: Option<DeclIdx>,
    arena: &mut DeclArena,
) -> DeclIdx {
    let current_indentation = new_group_start.indentation();

    let mut token = skip_initial_trivia_(&new_group_start);

    let mut nested: Option<DeclIdx> = None;

    // If we've consumed initial trivia tokens, include the tokens after the the trivia in the
    // group.
    if token != new_group_start {
        if let Some(next) = token.next() {
            token = next;
        }
    }

    // Parse nested groups if this is a top-level `class` or `instance`.
    if current_indentation == 0
        && matches!(
            token.kind(),
            TokenKind::ReservedId(ReservedId::Class | ReservedId::Instance)
        )
    {
        // Skip until `where`, starting at column 1 and above to avoid including the entire
        // document when the `where` is missing.
        while let Some(next) = next_at_indentation(&token, 1) {
            token = next;
            if matches!(token.kind(), TokenKind::ReservedId(ReservedId::Where)) {
                // Skip until the next indented non-whitespace and non-documentation-comment line.
                while let Some(next) = next_at_indentation(&token, 1) {
                    if matches!(
                        next.kind(),
                        TokenKind::Whitespace
                            | TokenKind::Comment {
                                documentation: false
                            }
                    ) {
                        token = next;
                        continue;
                    }
                    break;
                }

                // Parse the nested group.
                if let Some(next) = next_at_indentation(&token, current_indentation + 1) {
                    let nested_ = reparse_indentation_groups_token(next, None, arena);
                    nested = Some(nested_);
                    token = arena.get(nested_).last_token.clone();
                    break;
                }
            }
        }
    }

    let new_group_end = skip_group_and_trailing_trivia(&token, current_indentation);

    let new_group_line_number = new_group_start.absolute_span(arena).start.line;

    let new_group = IndentationGroup {
        line_number: new_group_line_number,
        first_token: new_group_start.clone(),
        last_token: new_group_end.clone(),
        next: None,
        prev: prev_decl_idx,
        nested,
        parent: None,
        modified: false,
    };
    let new_decl_idx = arena.allocate(new_group);
    if let Some(prev_decl_idx) = prev_decl_idx {
        arena.get_mut(prev_decl_idx).next = Some(new_decl_idx);
    }

    if let Some(nested) = nested {
        arena.get_mut(nested).parent = Some(new_decl_idx);
        arena.get_mut(nested).line_number -= new_group_line_number;
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

fn skip_initial_trivia_(token: &TokenRef) -> TokenRef {
    let current_indentation = token.span().start.line;

    let mut last_token = token.clone();

    let mut consumed_documentation = matches!(
        last_token.kind(),
        TokenKind::Comment {
            documentation: true
        }
    );

    // Skip documentation comments.
    while let Some(next_token) = next_at_indentation(&last_token, current_indentation) {
        if matches!(
            next_token.kind(),
            TokenKind::Comment {
                documentation: true
            }
        ) {
            last_token = next_token;
            consumed_documentation = true;
        } else {
            break;
        }
    }

    // Skip whitespace between the documentation comments and the actual thing.
    if consumed_documentation {
        while let Some(next_token) = next_at_indentation(&last_token, current_indentation) {
            if matches!(next_token.kind(), TokenKind::Whitespace) {
                last_token = next_token;
            } else {
                break;
            }
        }
    }

    last_token
}

fn skip_group_and_trailing_trivia(token: &TokenRef, indentation: u32) -> TokenRef {
    let mut last_token = token.clone();

    while let Some(next_token) = next_at_indentation(&last_token, indentation) {
        if is_group_start(indentation, &next_token) {
            break;
        }
        last_token = next_token;
    }

    last_token
}

fn next_at_indentation(token: &TokenRef, indentation: u32) -> Option<TokenRef> {
    let next = token.next()?;
    if matches!(next.kind(), TokenKind::Whitespace) || next.indentation() >= indentation {
        Some(next)
    } else {
        None
    }
}

/// Whether the token is the start of an indentation group.
fn is_group_start(current_indentation: u32, token: &TokenRef) -> bool {
    !matches!(
        token.kind(),
        TokenKind::Whitespace
            | TokenKind::Comment {
                documentation: false
            }
    ) && token.indentation() <= current_indentation
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(unused)]
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

    // TODO: Set token ast nodes.
    // TODO: Reuse nodes.
    fn indentation_group(&mut self, arena: &mut DeclArena, nested: bool) -> DeclIdx {
        let token = match self.next() {
            Some(token) => token,
            None => {
                // All whitespace and comments.
                let line_number = self.first.absolute_span(arena).start.line;
                return arena.allocate(IndentationGroup::new(
                    line_number,
                    self.first.clone(),
                    self.last.clone(),
                ));
            }
        };

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
                            let mut nested_group_parser = Parser::new(nested_group_start);
                            let nested_group_idx =
                                nested_group_parser.indentation_group(arena, true);
                            // End of the nested group also ends the parent group.
                            let line_number = self.first.absolute_span(arena).start.line;
                            let mut parent_group = IndentationGroup::new(
                                line_number,
                                self.first.clone(),
                                nested_group_parser.last,
                            );
                            parent_group.nested = Some(nested_group_idx);
                            let parent_group_idx = arena.allocate(parent_group);
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
                            arena.allocate(group)
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
