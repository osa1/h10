use tower_lsp::lsp_types::SemanticTokenType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    #[allow(unused)]
    Comment,
    Delimiter,
    Keyword,
    Number,
    Operator,
    String,
    Type,
    Variable,
    Whitespace,
}

pub const TOKEN_TYPES: [SemanticTokenType; 8] = [
    SemanticTokenType::COMMENT,
    SemanticTokenType::DECORATOR,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::STRING,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
];

impl TokenKind {
    pub fn to_lsp_token_type(self) -> Option<u32> {
        match self {
            TokenKind::Comment => Some(0),
            TokenKind::Delimiter => Some(1), // TODO not sure about this
            TokenKind::Keyword => Some(2),
            TokenKind::Number => Some(3),
            TokenKind::Operator => Some(4),
            TokenKind::String => Some(5),
            TokenKind::Type => Some(6),
            TokenKind::Variable => Some(7),
            TokenKind::Whitespace => None,
        }
    }
}
