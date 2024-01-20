use crate::ast;
use crate::token_node::TokenNodeRef;
use h10_lexer::token::Token;

/// Parse indentation groups as [`ast::TopDeclKind_::Unparsed`] declarations.
pub fn parse_indentation_groups(mut token: TokenNodeRef) -> Vec<ast::ParsedTopDecl> {
    let mut groups: Vec<ast::ParsedTopDecl> = vec![];

    loop {
        let group = parse_group(token.clone());
        let next = group.last_token.next();
        groups.push(group);

        match next {
            Some(next) => token = next,
            None => break,
        }
    }

    groups
}

fn parse_group(first_token: TokenNodeRef) -> ast::ParsedTopDecl {
    let mut last_token = first_token.clone();

    while let Some(next_token) = last_token.next() {
        let indent = next_token.span().start.col;
        if !matches!(next_token.token(), Token::Whitespace) && indent == 0 {
            break;
        }
        last_token = next_token;
    }

    ast::ParsedTopDecl {
        kind: ast::TopDeclKind_::Unparsed,
        first_token,
        last_token,
    }
}
