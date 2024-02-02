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

use smol_str::SmolStr;

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
        $$whitespace+ => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Whitespace, text })
        },

        '(' = Token { kind: TokenKind::Special(Special::LParen), text: SmolStr::new_inline("(") },
        ')' = Token { kind: TokenKind::Special(Special::RParen), text: SmolStr::new_inline(")") },
        ',' = Token { kind: TokenKind::Special(Special::Comma), text: SmolStr::new_inline(",") },
        ';' = Token { kind: TokenKind::Special(Special::Semi), text: SmolStr::new_inline(";") },
        '[' = Token { kind: TokenKind::Special(Special::LBracket), text: SmolStr::new_inline("[") },
        ']' = Token { kind: TokenKind::Special(Special::RBracket), text: SmolStr::new_inline("]") },
        '`' = Token { kind: TokenKind::Special(Special::Backtick), text: SmolStr::new_inline("`") },
        '{' = Token { kind: TokenKind::Special(Special::LBrace), text: SmolStr::new_inline("{") },
        '}' = Token { kind: TokenKind::Special(Special::RBrace), text: SmolStr::new_inline("}") },

        // Integer literals
        $$ascii_digit+ => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Literal(Literal::Int), text })
        },

        // Float literals
        $$ascii_digit+ '.' $$ascii_digit+ => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Literal(Literal::Float), text })
        },

        // Char literals
        "'" (_ # '\'') "'" => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Literal(Literal::Char), text })
        },

        // String literals
        '"' (_ # '"')* '"' => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Literal(Literal::String), text })
        },

        // Comments
        "--" ' '* '|' (_ # '\n')* > ('\n' | $) => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Comment { documentation: true }, text })
        },

        "--" (_ # '\n')* > ('\n' | $) => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::Comment { documentation: false }, text })
        },

        "{-" ' '* "|" => |lexer| {
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
        ".." = Token { kind: TokenKind::ReservedOp(ReservedOp::DotDot), text: SmolStr::new_inline("..") },
        ":" = Token { kind: TokenKind::ReservedOp(ReservedOp::Colon), text: SmolStr::new_inline(":") },
        "::" = Token { kind: TokenKind::ReservedOp(ReservedOp::ColonColon), text: SmolStr::new_inline("::") },
        "=" = Token { kind: TokenKind::ReservedOp(ReservedOp::Equals), text: SmolStr::new_inline("=") },
        "\\" = Token { kind: TokenKind::ReservedOp(ReservedOp::Backslash), text: SmolStr::new_inline("\\") },
        "|" = Token { kind: TokenKind::ReservedOp(ReservedOp::Pipe), text: SmolStr::new_inline("|") },
        "<-" = Token { kind: TokenKind::ReservedOp(ReservedOp::LeftArrow), text: SmolStr::new_inline("<-") },
        "->" = Token { kind: TokenKind::ReservedOp(ReservedOp::RightArrow), text: SmolStr::new_inline("->") },
        "@" = Token { kind: TokenKind::ReservedOp(ReservedOp::At), text: SmolStr::new_inline("@") },
        "~" = Token { kind: TokenKind::ReservedOp(ReservedOp::Tilde), text: SmolStr::new_inline("~") },
        "=>" = Token { kind: TokenKind::ReservedOp(ReservedOp::FatArrow), text: SmolStr::new_inline("=>") },

        // Reserved identifiers
        "case" = Token { kind: TokenKind::ReservedId(ReservedId::Case), text: SmolStr::new_inline("case") },
        "class" = Token { kind: TokenKind::ReservedId(ReservedId::Class), text: SmolStr::new_inline("class") },
        "data" = Token { kind: TokenKind::ReservedId(ReservedId::Data), text: SmolStr::new_inline("data") },
        "default" = Token { kind: TokenKind::ReservedId(ReservedId::Default), text: SmolStr::new_inline("default") },
        "deriving" = Token { kind: TokenKind::ReservedId(ReservedId::Deriving), text: SmolStr::new_inline("deriving") },
        "do" = Token { kind: TokenKind::ReservedId(ReservedId::Do), text: SmolStr::new_inline("do") },
        "else" = Token { kind: TokenKind::ReservedId(ReservedId::Else), text: SmolStr::new_inline("else") },
        "forall" = Token { kind: TokenKind::ReservedId(ReservedId::Forall), text: SmolStr::new_inline("forall") },
        "foreign" = Token { kind: TokenKind::ReservedId(ReservedId::Foreign), text: SmolStr::new_inline("foreign") },
        "if" = Token { kind: TokenKind::ReservedId(ReservedId::If), text: SmolStr::new_inline("if") },
        "import" = Token { kind: TokenKind::ReservedId(ReservedId::Import), text: SmolStr::new_inline("import") },
        "in" = Token { kind: TokenKind::ReservedId(ReservedId::In), text: SmolStr::new_inline("in") },
        "infix" = Token { kind: TokenKind::ReservedId(ReservedId::Infix), text: SmolStr::new_inline("infix") },
        "infixl" = Token { kind: TokenKind::ReservedId(ReservedId::Infixl), text: SmolStr::new_inline("infixl") },
        "infixr" = Token { kind: TokenKind::ReservedId(ReservedId::Infixr), text: SmolStr::new_inline("infixr") },
        "instance" = Token { kind: TokenKind::ReservedId(ReservedId::Instance), text: SmolStr::new_inline("instance") },
        "kind" = Token { kind: TokenKind::ReservedId(ReservedId::Kind), text: SmolStr::new_inline("kind") },
        "let" = Token { kind: TokenKind::ReservedId(ReservedId::Let), text: SmolStr::new_inline("let") },
        "module" = Token { kind: TokenKind::ReservedId(ReservedId::Module), text: SmolStr::new_inline("module") },
        "newtype" = Token { kind: TokenKind::ReservedId(ReservedId::Newtype), text: SmolStr::new_inline("newtype") },
        "of" = Token { kind: TokenKind::ReservedId(ReservedId::Of), text: SmolStr::new_inline("of") },
        "then" = Token { kind: TokenKind::ReservedId(ReservedId::Then), text: SmolStr::new_inline("then") },
        "type" = Token { kind: TokenKind::ReservedId(ReservedId::Type), text: SmolStr::new_inline("type") },
        "where" = Token { kind: TokenKind::ReservedId(ReservedId::Where), text: SmolStr::new_inline("where") },
        "_" = Token { kind: TokenKind::ReservedId(ReservedId::Underscore), text: SmolStr::new_inline("_") },

        $varid => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::VarId, text })
        },

        $conid => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::ConId, text })
        },

        $varsym => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::VarSym, text })
        },

        $consym => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::ConSym, text })
        },

        $qvarid => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::QVarId, text })
        },

        $qconid => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::QConId, text })
        },

        $qvarsym => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::QVarSym, text })
        },

        $qconsym => |lexer| {
            let text = SmolStr::new(lexer.match_());
            lexer.return_(Token { kind: TokenKind::QConSym, text })
        },
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
                let text = SmolStr::new(lexer.match_());
                lexer.switch_and_return(LexerRule::Init, Token { kind: TokenKind::Comment { documentation }, text })
            } else {
                lexer.continue_()
            }
        },
    }
}
