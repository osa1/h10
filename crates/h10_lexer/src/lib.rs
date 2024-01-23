mod token;

#[cfg(test)]
mod tests;

pub use token::*;

#[derive(Debug, Default, Clone)]
pub struct LexerState {
    comment_depth: u32,
    doc_comment: bool,
}

lexgen::lexer! {
    #[derive(Clone)]
    pub Lexer(LexerState) -> Token;

    let octal_digit = ['0'-'7'];
    let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F'];

    // TODO: unicode symbols?
    let symbol = '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?'
        | '@' | '\\' | '^' | '|' | '-' | '~' | ':';

    // Variable identifiers
    let varid = $$lowercase ($$alphanumeric | '\'')*;

    // Constructor identifiers
    let conid = $$uppercase ($$alphanumeric | '\'')*;

    // Variable symbol
    let varsym = ($symbol # ':') $symbol*;

    // Constructor symbol
    let consym = ':' $symbol*;

    let qual = ($conid '.')+;
    let qvarid = $qual $varid;
    let qconid = $qual $conid;
    let qvarsym = $qual $varsym;
    let qconsym = $qual $consym;

    rule Init {
        $$whitespace+ = Token::Whitespace,

        '(' = Token::Special(Special::LParen),
        ')' = Token::Special(Special::RParen),
        ',' = Token::Special(Special::Comma),
        ';' = Token::Special(Special::Semi),
        '[' = Token::Special(Special::LBracket),
        ']' = Token::Special(Special::RBracket),
        '`' = Token::Special(Special::Backtick),
        '{' = Token::Special(Special::LBrace),
        '}' = Token::Special(Special::RBrace),

        // Integer literals
        $$ascii_digit+ = Token::Literal(Literal::Int),

        // Float literals
        $$ascii_digit+ '.' $$ascii_digit+ = Token::Literal(Literal::Float),

        // Char literals
        "'" (_ # '\'') "'" = Token::Literal(Literal::Char),

        // String literals
        '"' (_ # '"')* '"' = Token::Literal(Literal::String),

        // Comments
        "--|" (_ # '\n')* > '\n' = Token::Comment { documentation: true },
        "--" (_ # '\n')* > '\n' = Token::Comment { documentation: false },

        "{-|" => |lexer| {
            lexer.state().comment_depth = 1;
            lexer.state().doc_comment = true;
            lexer.switch(LexerRule::MultiLineComment)
        },
        "{-" => |lexer| {
            lexer.state().comment_depth = 1;
            lexer.state().doc_comment = false;
            lexer.switch(LexerRule::MultiLineComment)
        },

        // Reserved operators
        ".." = Token::ReservedOp(ReservedOp::DotDot),
        ":" = Token::ReservedOp(ReservedOp::Colon),
        "::" = Token::ReservedOp(ReservedOp::ColonColon),
        "=" = Token::ReservedOp(ReservedOp::Equals),
        "\\" = Token::ReservedOp(ReservedOp::Backslash),
        "|" = Token::ReservedOp(ReservedOp::Pipe),
        "<-" = Token::ReservedOp(ReservedOp::LeftArrow),
        "->" = Token::ReservedOp(ReservedOp::RightArrow),
        "@" = Token::ReservedOp(ReservedOp::At),
        "~" = Token::ReservedOp(ReservedOp::Tilde),
        "=>" = Token::ReservedOp(ReservedOp::FatArrow),

        // Reserved identifiers
        "case" = Token::ReservedId(ReservedId::Case),
        "class" = Token::ReservedId(ReservedId::Class),
        "data" = Token::ReservedId(ReservedId::Data),
        "default" = Token::ReservedId(ReservedId::Default),
        "deriving" = Token::ReservedId(ReservedId::Deriving),
        "do" = Token::ReservedId(ReservedId::Do),
        "else" = Token::ReservedId(ReservedId::Else),
        "forall" = Token::ReservedId(ReservedId::Forall),
        "foreign" = Token::ReservedId(ReservedId::Foreign),
        "if" = Token::ReservedId(ReservedId::If),
        "import" = Token::ReservedId(ReservedId::Import),
        "in" = Token::ReservedId(ReservedId::In),
        "infix" = Token::ReservedId(ReservedId::Infix),
        "infixl" = Token::ReservedId(ReservedId::Infixl),
        "infixr" = Token::ReservedId(ReservedId::Infixr),
        "instance" = Token::ReservedId(ReservedId::Instance),
        "kind" = Token::ReservedId(ReservedId::Kind),
        "let" = Token::ReservedId(ReservedId::Let),
        "module" = Token::ReservedId(ReservedId::Module),
        "newtype" = Token::ReservedId(ReservedId::Newtype),
        "of" = Token::ReservedId(ReservedId::Of),
        "then" = Token::ReservedId(ReservedId::Then),
        "type" = Token::ReservedId(ReservedId::Type),
        "where" = Token::ReservedId(ReservedId::Where),
        "_" = Token::ReservedId(ReservedId::Underscore),

        $varid = Token::VarId,
        $conid = Token::ConId,
        $varsym = Token::VarSym,
        $consym = Token::ConSym,
        $qvarid = Token::QVarId,
        $qconid = Token::QConId,
        $qvarsym = Token::QVarSym,
        $qconsym = Token::QConSym,
    }

    rule MultiLineComment {
        _,

        "{-" => |lexer| {
            lexer.state().comment_depth += 1;
            lexer.continue_()
        },

        "-}" => |lexer| {
            lexer.state().comment_depth -= 1;
            if lexer.state().comment_depth == 0 {
                let documentation = lexer.state().doc_comment;
                lexer.switch_and_return(LexerRule::Init, Token::Comment { documentation })
            } else {
                lexer.continue_()
            }
        },
    }
}
