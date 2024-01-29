use crate::ast::Span;
use crate::token::TokenRef;
use h10_lexer::{Lexer, Token, TokenKind};

use lexgen_util::Loc;

/// Tokenize a file from scrath.
///
/// Tokens are linked together and their line numbers are absolute.
pub(crate) fn lex_full(s: &str) -> TokenRef {
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
    first_token.unwrap_or_else(|| {
        TokenRef::new(
            Token {
                kind: TokenKind::Whitespace,
                text: "".into(),
            },
            Span {
                start: Loc {
                    line: 0,
                    col: 0,
                    byte_idx: 0,
                },
                end: Loc {
                    line: 0,
                    col: 0,
                    byte_idx: 0,
                },
            },
        )
    })
}
