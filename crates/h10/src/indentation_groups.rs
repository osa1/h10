use crate::ast;
use crate::decl_arena::{DeclArena, DeclIdx};
use crate::token_node::TokenNodeRef;
use h10_lexer::token::Token;

/// Parse indentation groups as [`ast::TopDeclKind_::Unparsed`] declarations.
pub fn parse_indentation_groups(mut token: TokenNodeRef, arena: &mut DeclArena) -> Vec<DeclIdx> {
    // Skip initial whitespace.
    while matches!(token.token(), Token::Whitespace) {
        match token.next() {
            Some(next) => token = next,
            None => return vec![],
        }
    }

    let mut groups: Vec<DeclIdx> = vec![];

    loop {
        let group_idx = parse_group(token.clone(), arena);
        groups.push(group_idx);
        let next = arena.get(group_idx).last_token.next();

        match next {
            Some(next) => token = next,
            None => break,
        }
    }

    groups
}

fn parse_group(first_token: TokenNodeRef, arena: &mut DeclArena) -> DeclIdx {
    let mut last_token = first_token.clone();

    while let Some(next_token) = last_token.next() {
        let indent = next_token.span().start.col;
        if !matches!(next_token.token(), Token::Whitespace) && indent == 0 {
            break;
        }
        last_token = next_token;
    }

    let group = ast::ParsedTopDecl {
        kind: ast::TopDeclKind_::Unparsed,
        first_token: first_token.clone(),
        last_token: last_token.clone(),
    };

    let group_idx = arena.allocate(group);

    // Set AST nodes of tokens in the group.
    let mut token = first_token;
    loop {
        token.set_ast_node(group_idx);
        token = token.next().unwrap();
        if token == last_token {
            break;
        }
    }

    group_idx
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
            let t: TokenNodeRef = TokenNodeRef::from_lexer_token("test", t.unwrap(), s);
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
        let mut arena = DeclArena::new();
        let token = lex(pgm);
        let groups = parse_indentation_groups(token.clone(), &mut arena);
        assert_eq!(groups.len(), 2);
        assert_eq!(arena.get(groups[0]).first_token.span().start.line, 0);
        assert_eq!(arena.get(groups[0]).first_token.span().start.col, 0);
        assert_eq!(arena.get(groups[1]).first_token.span().start.line, 1);
        assert_eq!(arena.get(groups[1]).first_token.span().start.col, 0);

        token.check_token_str(pgm);
    }

    #[test]
    fn simple2() {
        let pgm = indoc! {"
            data A

            data B
        "};
        let mut arena = DeclArena::new();
        let token = lex(pgm);
        let groups = parse_indentation_groups(token.clone(), &mut arena);
        assert_eq!(groups.len(), 2);
        assert_eq!(arena.get(groups[0]).first_token.span().start.line, 0);
        assert_eq!(arena.get(groups[0]).first_token.span().start.col, 0);
        assert_eq!(arena.get(groups[1]).first_token.span().start.line, 2);
        assert_eq!(arena.get(groups[1]).first_token.span().start.col, 0);

        token.check_token_str(pgm);
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
        let mut arena = DeclArena::new();
        let token = lex(pgm);
        let groups = parse_indentation_groups(token.clone(), &mut arena);
        assert_eq!(groups.len(), 3);
        assert_eq!(arena.get(groups[0]).first_token.span().start.line, 1);
        assert_eq!(arena.get(groups[1]).first_token.span().start.line, 3);
        assert_eq!(arena.get(groups[2]).first_token.span().start.line, 7);

        token.check_token_str(pgm);
    }
}
