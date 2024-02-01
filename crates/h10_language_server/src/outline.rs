//! Implements outline/symbols generation from partially parsed ASTs (indentation groups).

use h10::decl_arena::{DeclArena, DeclIdx};
use h10::indentation_groups::IndentationGroup;
use h10::token::TokenRef;
use h10_lexer::{ReservedId, TokenKind};

use tower_lsp::lsp_types as lsp;

pub fn generate_symbol_response(
    decls: &[DeclIdx],
    arena: &DeclArena,
) -> lsp::DocumentSymbolResponse {
    // NB. LSP spec says we should return `DocumentSymbol[]` (`DocumentSymbolResponse::Nested` in
    // `lsp_types`) instead of `SymbolInformation[]` (`DocumentSymbolResponse::Flat` in
    // `lsp_types`) when possible, as `DocumentSymbol` contains more information.
    lsp::DocumentSymbolResponse::Nested(generate_document_symbols(decls, arena))
}

fn generate_document_symbols(decls: &[DeclIdx], arena: &DeclArena) -> Vec<lsp::DocumentSymbol> {
    decls
        .iter()
        .filter_map(|decl_idx| generate_decl_symbol(arena.get(*decl_idx), arena))
        .collect()
}

fn generate_decl_symbol(decl: &IndentationGroup, arena: &DeclArena) -> Option<lsp::DocumentSymbol> {
    let mut token_iter = iter_tokens(decl);

    let token0 = token_iter.next()?;
    let decl_range = decl_range(decl, arena);

    // `name` is used by VSCode in "go to symbol". `detail` is used in the outline view.
    Some(match token0.kind() {
        // TODO: This won't work as expected in operator definitions and infix names. E.g.
        //
        // ```
        // a `add` b = ...
        // a :+: b = ...
        // ```
        TokenKind::VarId => lsp::DocumentSymbol {
            name: token0.text().to_owned(),
            // TODO: Consider showing symbol type as detail.
            detail: None,
            kind: lsp::SymbolKind::FUNCTION,
            deprecated: None,
            tags: None,
            range: decl_range,
            selection_range: decl_range,
            children: None,
        },

        TokenKind::ReservedId(ReservedId::Class) => {
            let mut name = String::with_capacity(50);
            let name_token = token0.next()?;
            // Start new iteration as `token_iter` skips whitespace
            for token in name_token.iter() {
                match token.kind() {
                    TokenKind::ReservedId(ReservedId::Where) => break,
                    TokenKind::Whitespace => name.push(' '),
                    TokenKind::Comment { .. } => continue,
                    _ => {
                        let token_str = token.text();
                        if name.len() + token_str.len() >= 50 {
                            break;
                        }
                        name.push_str(token_str);
                    }
                }
            }

            lsp::DocumentSymbol {
                name,
                detail: None,
                kind: lsp::SymbolKind::INTERFACE,
                deprecated: None,
                tags: None,
                range: decl_range,
                selection_range: decl_range,
                children: None,
            }
        }

        TokenKind::ReservedId(ReservedId::Instance) => {
            let mut name = String::with_capacity(50);
            let name_token = token0.next()?;
            for token in name_token.iter() {
                match token.kind() {
                    TokenKind::ReservedId(ReservedId::Where) => break,
                    TokenKind::Whitespace => name.push(' '),
                    _ => {
                        let token_str = token.text();
                        if name.len() + token_str.len() >= 50 {
                            break;
                        }
                        name.push_str(token_str);
                    }
                }
            }

            lsp::DocumentSymbol {
                name,
                detail: None,
                kind: lsp::SymbolKind::METHOD,
                deprecated: None,
                tags: None,
                range: decl_range,
                selection_range: decl_range,
                children: None,
            }
        }

        TokenKind::ReservedId(ReservedId::Data | ReservedId::Newtype | ReservedId::Type) => {
            let name_token = token_iter.next()?;
            let name = match name_token.kind() {
                TokenKind::VarId
                | TokenKind::ConId
                | TokenKind::VarSym
                | TokenKind::ConSym
                | TokenKind::QVarId
                | TokenKind::QVarSym => name_token.text().to_owned(),
                _ => return None,
            };

            lsp::DocumentSymbol {
                name,
                detail: None,
                kind: lsp::SymbolKind::CLASS,
                deprecated: None,
                tags: None,
                range: decl_range,
                selection_range: decl_range,
                children: None,
            }
        }

        _ => return None,
    })
}

/// Iterate tokens of a declaration, skipping whitespace and comments.
fn iter_tokens(decl: &IndentationGroup) -> impl Iterator<Item = TokenRef> {
    decl.iter_tokens()
        .filter(|t| !matches!(t.kind(), TokenKind::Whitespace | TokenKind::Comment { .. }))
}

fn decl_range(decl: &IndentationGroup, arena: &DeclArena) -> lsp::Range {
    let start = decl.span_start(arena);
    let end = decl.span_end(arena);
    lsp::Range {
        start: lsp::Position {
            line: start.line,
            character: start.char,
        },
        end: lsp::Position {
            line: end.line,
            character: end.char,
        },
    }
}
