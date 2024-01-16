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

impl TokenKind {
    #[allow(unused)]
    pub fn to_lsp_token_type(self) -> Option<SemanticTokenType> {
        match self {
            TokenKind::Comment => Some(SemanticTokenType::COMMENT),
            TokenKind::Delimiter => Some(SemanticTokenType::DECORATOR), // TODO not sure about this
            TokenKind::Keyword => Some(SemanticTokenType::KEYWORD),
            TokenKind::Number => Some(SemanticTokenType::NUMBER),
            TokenKind::Operator => Some(SemanticTokenType::OPERATOR),
            TokenKind::String => Some(SemanticTokenType::STRING),
            TokenKind::Type => Some(SemanticTokenType::TYPE),
            TokenKind::Variable => Some(SemanticTokenType::VARIABLE),
            TokenKind::Whitespace => None,
        }
    }
}
