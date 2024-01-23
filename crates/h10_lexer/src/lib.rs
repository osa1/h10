/*!
The lexer scans a character iterator and returns lexical tokens.

Some notes: (TODO: these are not implemented yet)

- The lexer can't ever fail. All sequence of characters should be mapped to some number of tokens.
  Invalid tokens can be mapped to an error token.

- Tokens should carry their own contents (i.e. the text), as the character iterator (the lexer
  input) won't always come from a string and we can't get the token contents from just the span of
  the token.

  In principle, tokens like '(' and ')' which are always a known sequence of characters in the
  input don't need to carry their own contents, but for now we keep things simple (with least
  amount of special cases) and add a string to all tokens.
*/

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
    pub Lexer(LexerState) -> TokenKind;

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
        $$whitespace+ = TokenKind::Whitespace,

        '(' = TokenKind::Special(Special::LParen),
        ')' = TokenKind::Special(Special::RParen),
        ',' = TokenKind::Special(Special::Comma),
        ';' = TokenKind::Special(Special::Semi),
        '[' = TokenKind::Special(Special::LBracket),
        ']' = TokenKind::Special(Special::RBracket),
        '`' = TokenKind::Special(Special::Backtick),
        '{' = TokenKind::Special(Special::LBrace),
        '}' = TokenKind::Special(Special::RBrace),

        // Integer literals
        $$ascii_digit+ = TokenKind::Literal(Literal::Int),

        // Float literals
        $$ascii_digit+ '.' $$ascii_digit+ = TokenKind::Literal(Literal::Float),

        // Char literals
        "'" (_ # '\'') "'" = TokenKind::Literal(Literal::Char),

        // String literals
        '"' (_ # '"')* '"' = TokenKind::Literal(Literal::String),

        // Comments
        "--|" (_ # '\n')* > '\n' = TokenKind::Comment { documentation: true },
        "--" (_ # '\n')* > '\n' = TokenKind::Comment { documentation: false },

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
        ".." = TokenKind::ReservedOp(ReservedOp::DotDot),
        ":" = TokenKind::ReservedOp(ReservedOp::Colon),
        "::" = TokenKind::ReservedOp(ReservedOp::ColonColon),
        "=" = TokenKind::ReservedOp(ReservedOp::Equals),
        "\\" = TokenKind::ReservedOp(ReservedOp::Backslash),
        "|" = TokenKind::ReservedOp(ReservedOp::Pipe),
        "<-" = TokenKind::ReservedOp(ReservedOp::LeftArrow),
        "->" = TokenKind::ReservedOp(ReservedOp::RightArrow),
        "@" = TokenKind::ReservedOp(ReservedOp::At),
        "~" = TokenKind::ReservedOp(ReservedOp::Tilde),
        "=>" = TokenKind::ReservedOp(ReservedOp::FatArrow),

        // Reserved identifiers
        "case" = TokenKind::ReservedId(ReservedId::Case),
        "class" = TokenKind::ReservedId(ReservedId::Class),
        "data" = TokenKind::ReservedId(ReservedId::Data),
        "default" = TokenKind::ReservedId(ReservedId::Default),
        "deriving" = TokenKind::ReservedId(ReservedId::Deriving),
        "do" = TokenKind::ReservedId(ReservedId::Do),
        "else" = TokenKind::ReservedId(ReservedId::Else),
        "forall" = TokenKind::ReservedId(ReservedId::Forall),
        "foreign" = TokenKind::ReservedId(ReservedId::Foreign),
        "if" = TokenKind::ReservedId(ReservedId::If),
        "import" = TokenKind::ReservedId(ReservedId::Import),
        "in" = TokenKind::ReservedId(ReservedId::In),
        "infix" = TokenKind::ReservedId(ReservedId::Infix),
        "infixl" = TokenKind::ReservedId(ReservedId::Infixl),
        "infixr" = TokenKind::ReservedId(ReservedId::Infixr),
        "instance" = TokenKind::ReservedId(ReservedId::Instance),
        "kind" = TokenKind::ReservedId(ReservedId::Kind),
        "let" = TokenKind::ReservedId(ReservedId::Let),
        "module" = TokenKind::ReservedId(ReservedId::Module),
        "newtype" = TokenKind::ReservedId(ReservedId::Newtype),
        "of" = TokenKind::ReservedId(ReservedId::Of),
        "then" = TokenKind::ReservedId(ReservedId::Then),
        "type" = TokenKind::ReservedId(ReservedId::Type),
        "where" = TokenKind::ReservedId(ReservedId::Where),
        "_" = TokenKind::ReservedId(ReservedId::Underscore),

        $varid = TokenKind::VarId,
        $conid = TokenKind::ConId,
        $varsym = TokenKind::VarSym,
        $consym = TokenKind::ConSym,
        $qvarid = TokenKind::QVarId,
        $qconid = TokenKind::QConId,
        $qvarsym = TokenKind::QVarSym,
        $qconsym = TokenKind::QConSym,
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
                lexer.switch_and_return(LexerRule::Init, TokenKind::Comment { documentation })
            } else {
                lexer.continue_()
            }
        },
    }
}
