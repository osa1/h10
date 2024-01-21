use crate::ast;
use crate::token_node::TokenNodeRef;
use h10_lexer::token::Token;

/// Parse indentation groups as [`ast::TopDeclKind_::Unparsed`] declarations.
pub fn parse_indentation_groups(mut token: TokenNodeRef) -> Vec<ast::ParsedTopDecl> {
    // Skip initial whitespace.
    while matches!(token.token(), Token::Whitespace) {
        match token.next() {
            Some(next) => token = next,
            None => return vec![],
        }
    }

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

#[cfg(test)]
mod tests {
    use super::*;
    use h10_lexer::Lexer;

    use indoc::indoc;

    fn lex(s: &str) -> TokenNodeRef {
        let lexer = Lexer::new(s);
        let mut first_token: Option<TokenNodeRef> = None;
        let mut last_token: Option<TokenNodeRef> = None;
        for t in lexer {
            let t: TokenNodeRef = TokenNodeRef::from_lexer_token("test", t.unwrap());
            if first_token.is_none() {
                first_token = Some(t.clone());
            } else if let Some(last_token_) = last_token {
                last_token_.set_next(t.clone());
            }
            last_token = Some(t.clone());
        }
        first_token.unwrap()
    }

    #[test]
    fn simple1() {
        let pgm = indoc! {"
            data A
            data B
        "};
        let token = lex(pgm);
        let groups = parse_indentation_groups(token);
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].first_token.span().start.line, 0);
        assert_eq!(groups[0].first_token.span().start.col, 0);
        assert_eq!(groups[1].first_token.span().start.line, 1);
        assert_eq!(groups[1].first_token.span().start.col, 0);
    }

    #[test]
    fn simple2() {
        let pgm = indoc! {"
            data A

            data B
        "};
        let token = lex(pgm);
        let groups = parse_indentation_groups(token);
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].first_token.span().start.line, 0);
        assert_eq!(groups[0].first_token.span().start.col, 0);
        assert_eq!(groups[1].first_token.span().start.line, 2);
        assert_eq!(groups[1].first_token.span().start.col, 0);
    }

    #[test]
    fn simple3() {
        let pgm = indoc! {"

            data A          -- 1

            data B          -- 3
              = X
              | Y

            f x y =         -- 7
              x

              where
                t = 5       -- 11
        "};
        let token = lex(pgm);
        let groups = parse_indentation_groups(token);
        assert_eq!(groups.len(), 3);
        assert_eq!(groups[0].first_token.span().start.line, 1);
        assert_eq!(groups[1].first_token.span().start.line, 3);
        assert_eq!(groups[2].first_token.span().start.line, 7);
    }
}
