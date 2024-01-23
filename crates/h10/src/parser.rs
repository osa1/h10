mod decl;
mod error;
mod exp;
mod pat;
mod ty;
mod utils;

#[cfg(test)]
mod tests;

use crate::ast::*;
use crate::layout_lexer::{LayoutError, LayoutLexer};
use crate::parser::error::{Context, Error, ErrorKind, GrammarItem};
use crate::token::TokenRef;
use h10_lexer::{ReservedId, ReservedOp, Special, TokenKind};

use std::str::Chars;

use lexgen_util::{LexerError, Loc};
use rpds::List;

pub type ParserResult<A> = Result<A, Error>;

/// Parse a module. Handles layout.
pub fn parse_module(module_str: &str) -> ParserResult<Vec<ParsedTopDecl>> {
    Parser::new(LayoutLexer::new(module_str)).module()
}

/// Parse a type with predicates. Does not handle layout.
#[cfg(test)]
pub fn parse_type(
    type_str: &str,
) -> ParserResult<(Vec<ParsedTypeBinder>, Vec<ParsedType>, ParsedType)> {
    Parser::new(LayoutLexer::new_non_module(type_str)).type_with_context()
}

/// Parse an expression. Does not handle layout.
#[cfg(test)]
pub fn parse_exp(exp_str: &str) -> ParserResult<ParsedExp> {
    Parser::new(LayoutLexer::new_non_module(exp_str)).exp()
}

/// Parse an expression. Handles layout.
#[cfg(test)]
pub fn parse_exp_with_layout(exp_str: &str) -> ParserResult<ParsedExp> {
    Parser::new(LayoutLexer::new_non_module(exp_str)).exp()
}

#[derive(Clone)]
struct Parser<'input> {
    /// The lexer. Do not use this directly, use the `Self` `next`, `peek` etc. methods instead.
    lexer: LayoutLexer<'input, Chars<'input>>,

    /// Next peeked token. We can't use std `Peekable` as we need to be able to pop current layout
    /// on parse error and `Peekable` doesn't give access to the inner iterator.
    ///
    /// This token has not been consumed yet and not linked to [`last_tok`].
    peeked: Option<Result<TokenRef, LexerError<LayoutError>>>,

    /// The last consumed token from [`lexer`]. This is used to create the linked list of tokens as
    /// we consume tokens.
    last_tok: Option<TokenRef>,

    context: List<Context>,
}

impl<'input> Parser<'input> {
    fn new(lexer: LayoutLexer<'input, Chars<'input>>) -> Self {
        Parser {
            lexer,
            peeked: None,
            last_tok: None,
            context: List::new(),
        }
    }

    /*
    module → module modid [exports] where body
           | body
    */
    fn module(&mut self) -> ParserResult<Vec<ParsedTopDecl>> {
        self.in_context(GrammarItem::Module, |self_| {
            if self_.skip_token(TokenKind::ReservedId(ReservedId::Module)) {
                self_.expect_token_pred(|token| {
                    matches!(token, TokenKind::ConId | TokenKind::QConId)
                })?;
                if self_.skip_token(TokenKind::Special(Special::LParen)) {
                    self_.exports()?;
                }
                self_.expect_token(TokenKind::ReservedId(ReservedId::Where))?;
            }
            let decls = self_.body()?;
            if let Some(token) = self_.lexer.next() {
                panic!("TokenKind after module: {:?}", token);
            }
            Ok(decls)
        })
    }

    // exports → ( export1 , … , exportn [ , ] )         (n ≥ 0)
    //
    // NB. `(` consumed
    fn exports(&mut self) -> ParserResult<Vec<()>> {
        self.in_context(GrammarItem::ExportList, |self_| {
            let mut exports: Vec<()> = vec![];
            exports.push(self_.export()?);
            while self_.skip_token(TokenKind::Special(Special::Comma)) {
                exports.push(self_.export()?);
            }
            self_.expect_token(TokenKind::Special(Special::RBrace))?;
            Ok(exports)
        })
    }

    /*
    export → qvar
           | qtycon [(..) | ( cname1 , … , cnamen )]    (n ≥ 0)
           | qtycls [(..) | ( qvar1 , … , qvarn )]      (n ≥ 0)
           | module modid
    */
    fn export(&mut self) -> ParserResult<()> {
        todo!()
    }

    /*
    body → { impdecls ; topdecls }
         | { impdecls }
         | { topdecls }
    */
    fn body(&mut self) -> ParserResult<Vec<ParsedTopDecl>> {
        // TODO: Currently only parsing topdecls
        self.expect_token(TokenKind::Special(Special::LBrace))?;
        let decls = self.topdecls()?;
        self.expect_token(TokenKind::Special(Special::RBrace))?;
        Ok(decls)
    }

    // qop    → qvarop | qconop             (qualified operator)
    // qvarop → qvarsym | ` qvarid `        (qualified variable operator)
    fn qop(&mut self) -> ParserResult<ParsedExp> {
        let t = self.next_()?;
        let l = t.span().start;
        let r = t.span().end;
        match t.token() {
            TokenKind::QVarSym | TokenKind::VarSym => {
                let str = t.text.borrow().to_owned();
                Ok(self.spanned(l, r, Exp_::Var(str)))
            }

            TokenKind::QConSym | TokenKind::ConSym => {
                let str = t.text.borrow().to_owned();
                Ok(self.spanned(l, r, Exp_::Con(str)))
            }

            TokenKind::ReservedOp(ReservedOp::Colon) => {
                Ok(self.spanned(l, r, Exp_::Con(":".to_owned())))
            }

            TokenKind::Special(Special::Backtick) => {
                let t_ = self.next_()?;
                let exp = match t_.token() {
                    TokenKind::QVarId | TokenKind::VarId => Exp_::Var(t_.text.borrow().to_owned()),
                    TokenKind::QConId | TokenKind::ConId => Exp_::Con(t_.text.borrow().to_owned()),
                    _ => return self.fail(l, ErrorKind::UnexpectedToken),
                };
                self.expect_token(TokenKind::Special(Special::Backtick))?;
                Ok(self.spanned(l, r, exp))
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    fn qop_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::QVarSym
                    | TokenKind::VarSym
                    | TokenKind::QConSym
                    | TokenKind::ConSym
                    | TokenKind::ReservedOp(ReservedOp::Colon)
                    | TokenKind::Special(Special::Backtick),
                _
            ))
        )
    }

    /*
    qual → pat <- exp                       (generator)
         | let decls                        (local declaration)
         | exp                              (guard)
    */
    fn qual(&mut self) -> ParserResult<ParsedStmt> {
        // Same as `stmt` as `stmt` doesn't parse the trailing semicolons
        self.stmt()
    }

    // qvar → qvarid | ( qvarsym )          (qualified variable)
    fn qvar(&mut self) -> ParserResult<String> {
        let t = self.peek_()?;
        let l = t.span().start;

        match t.token() {
            TokenKind::VarId | TokenKind::QVarId => {
                self.skip(); // consume var
                Ok(t.text.borrow().to_owned())
            }

            TokenKind::Special(Special::LParen) => {
                self.skip(); // consume '('
                let t = self.next_()?;
                match t.token() {
                    TokenKind::VarSym | TokenKind::QVarSym => {
                        self.expect_token(TokenKind::Special(Special::RParen))?;
                        Ok(t.text.borrow().to_owned())
                    }

                    _ => self.fail(l, ErrorKind::UnexpectedToken),
                }
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    fn qvar_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::VarId | TokenKind::QVarId | TokenKind::Special(Special::LParen),
                _
            ))
        )
    }

    /*
    stmts → stmt1 … stmtn exp [;]          (n ≥ 0)

    Note: Currently this parses `}` as well, because layout lexer does not really yield a `}` after
    popping a context. We may want to revisit this.
    */
    fn stmts(&mut self) -> ParserResult<Vec<ParsedStmt>> {
        let mut stmts = vec![];
        while !matches!(self.peek(), Ok((_, TokenKind::Special(Special::RBrace), _))) {
            self.skip_all(TokenKind::Special(Special::Semi));
            let mut parser = self.clone();
            match parser.stmt() {
                Ok(stmt) => {
                    stmts.push(stmt);
                    *self = parser;
                }
                Err(err) => {
                    if parser.pop_layout() {
                        println!("pop_layout success");
                        *self = parser;
                        return Ok(stmts);
                    } else {
                        println!("pop_layout failed");
                        return Err(err);
                    }
                }
            }
            self.skip_all(TokenKind::Special(Special::Semi));
        }
        self.skip(); // consume '}'
        Ok(stmts)
    }

    /*
    stmt → exp ;
         | pat <- exp ;
         | let decls ;
         | ;

    Note: this parser does not parse the trailing semicolon and the empty case. It shouldn't be
    directly called, use `stmts` instead.
    */
    fn stmt(&mut self) -> ParserResult<ParsedStmt> {
        if self.skip_token(TokenKind::ReservedId(ReservedId::Let)) {
            let (l, _) = self.last_tok_span();
            let decls = self.value_decls()?;
            if self.skip_token(TokenKind::ReservedId(ReservedId::In)) {
                let exp = self.exp()?;
                let (_, r) = self.last_tok_span();
                return Ok(self.spanned(
                    l,
                    r,
                    Stmt_::Exp(self.spanned(l, r, Exp_::Let(decls, Box::new(exp)))),
                ));
            } else {
                let (_, r) = self.last_tok_span();
                return Ok(self.spanned(l, r, Stmt_::Let(decls)));
            }
        }

        let pat = self.try_(|self_| {
            let pat = self_.pat()?;
            self_.expect_token(TokenKind::ReservedOp(ReservedOp::LeftArrow))?;
            Ok(pat)
        });

        match pat {
            Ok(pat) => {
                // `pat <-` parse successful, parsing a binding.
                let rhs = self.exp()?;
                let (_, r) = self.last_tok_span();
                Ok(self.spanned(pat.span.start, r, Stmt_::Bind(pat, rhs)))
            }
            Err(_) => {
                // Try `exp`.
                match self.try_(|self_| self_.exp()) {
                    Ok(exp) => Ok(self.spanned(exp.span.start, exp.span.end, Stmt_::Exp(exp))),
                    Err(_) => self.fail_with_next(ErrorKind::UnexpectedToken),
                }
            }
        }
    }

    fn context_parser<A, F>(&mut self, f: F) -> ParserResult<Vec<A>>
    where
        F: Fn(&mut Self) -> ParserResult<A>,
    {
        if self.skip_token(TokenKind::Special(Special::LParen)) {
            let mut context = vec![f(self)?];
            if self.skip_token(TokenKind::Special(Special::Comma)) {
                context.push(f(self)?);
            }
            self.expect_token(TokenKind::Special(Special::RParen))?;
            Ok(context)
        } else {
            // TODO: error won't mention '('
            Ok(vec![f(self)?])
        }
    }

    /*
    context → class
            | ( class1 , … , classn )          (n ≥ 0)
    */
    fn context(&mut self) -> ParserResult<Vec<ParsedType>> {
        self.context_parser(Self::class)
    }

    fn try_foralls(&mut self) -> ParserResult<Vec<ParsedTypeBinder>> {
        let mut binders = vec![];

        if !self.skip_token(TokenKind::ReservedId(ReservedId::Forall)) {
            return Ok(binders);
        }

        loop {
            let t = self.next_()?;
            let l = t.span().start;
            let r = t.span().end;
            match t.token() {
                TokenKind::VarId => {
                    let id = t.text.borrow().to_owned();
                    binders.push(self.spanned(l, r, TypeBinder_ { id, ty: None }));
                }

                TokenKind::Special(Special::LParen) => {
                    let (_, id, _) = self.expect_token_string(TokenKind::VarId)?;
                    self.expect_token(TokenKind::ReservedOp(ReservedOp::ColonColon))?;
                    let ty = self.type_()?;
                    let rparen = self.expect_token(TokenKind::Special(Special::RParen))?;
                    binders.push(self.spanned(
                        l,
                        rparen.span().end,
                        TypeBinder_ { id, ty: Some(ty) },
                    ));
                }

                TokenKind::VarSym if *t.text.borrow() == "." => {
                    break;
                }

                _ => return self.fail(l, ErrorKind::UnexpectedToken),
            }
        }

        Ok(binders)
    }

    fn try_context_arrow(&mut self) -> Option<Vec<ParsedType>> {
        self.try_(|self_| {
            let context = self_.context()?;
            self_.expect_token(TokenKind::ReservedOp(ReservedOp::FatArrow))?;
            Ok(context)
        })
        .ok()
    }

    // vars → var1 , …, varn                   (n ≥ 1)
    fn vars(&mut self) -> ParserResult<Vec<String>> {
        let mut vars = vec![self.var()?];
        while self.skip_token(TokenKind::Special(Special::Comma)) {
            vars.push(self.var()?);
        }
        Ok(vars)
    }

    // fixity → infixl | infixr | infix
    fn fixity(&mut self) -> ParserResult<Fixity> {
        match self.next()? {
            (_, TokenKind::ReservedId(ReservedId::Infix), _) => Ok(Fixity::Infix),
            (_, TokenKind::ReservedId(ReservedId::Infixl), _) => Ok(Fixity::InfixL),
            (_, TokenKind::ReservedId(ReservedId::Infixr), _) => Ok(Fixity::InfixR),
            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    // varop → varsym | ` varid `           (variable operator)
    fn varop(&mut self) -> ParserResult<String> {
        let t = self.peek_()?;
        let l = t.span().start;

        match t.token() {
            TokenKind::VarSym => {
                self.skip(); // consume varsym
                Ok(t.text.borrow().to_owned())
            }

            TokenKind::Special(Special::Backtick) => {
                self.skip(); // consume '`'
                let (_, s, _) = self.expect_token_string(TokenKind::VarId)?;
                self.expect_token(TokenKind::Special(Special::Backtick))?;
                Ok(s)
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    // qconop → gconsym | ` qconid `        (qualified constructor operator)
    fn qconop(&mut self) -> ParserResult<(Loc, String, Loc)> {
        // TODO: return value
        if self.gconsym_start() {
            self.gconsym()
        } else {
            let backtick_left = self.expect_token(TokenKind::Special(Special::Backtick))?;
            let (_, s, _) = self.expect_token_string(TokenKind::QConId)?;
            let backtick_right = self.expect_token(TokenKind::Special(Special::Backtick))?;
            Ok((backtick_left.span().start, s, backtick_right.span().end))
        }
    }

    // var → varid | ( varsym )             (variable)
    fn var(&mut self) -> ParserResult<String> {
        let t = self.next_()?;
        let l = t.span().start;
        match t.token() {
            TokenKind::VarId => Ok(t.text.borrow().to_owned()),

            TokenKind::Special(Special::LParen) => {
                let t = self.next_()?;
                let l = t.span().start;
                match t.token() {
                    TokenKind::VarSym => {
                        self.expect_token(TokenKind::Special(Special::RParen))?;
                        Ok(t.text.borrow().to_owned())
                    }
                    _ => self.fail(l, ErrorKind::UnexpectedToken),
                }
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    fn var_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((_, TokenKind::VarId | TokenKind::Special(Special::LParen), _))
        )
    }

    /*
    gcon → ()
         | []
         | (,{,})
         | qcon
    */
    fn gcon(&mut self) -> ParserResult<ParsedGCon> {
        let (l, t, r) = self.peek()?;
        match t {
            TokenKind::Special(Special::LParen) => {
                self.skip(); // consume '('
                let mut arity: u32 = 0;
                while self.skip_token(TokenKind::Special(Special::Comma)) {
                    arity += 1;
                }
                self.expect_token(TokenKind::Special(Special::RParen))?;
                Ok(self.spanned(l, r, GCon_::Tuple(arity)))
            }
            TokenKind::Special(Special::LBracket) => {
                self.skip(); // consume '['
                self.expect_token(TokenKind::Special(Special::RBracket))?;
                Ok(self.spanned(l, r, GCon_::EmptyList))
            }
            _ => match self.qcon() {
                Ok(qcon) => Ok(self.spanned(l, r, GCon_::QCon(qcon))),
                Err(_) => self.fail(l, ErrorKind::UnexpectedToken),
            },
        }
    }

    fn gcon_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::Special(Special::LParen | Special::LBracket),
                _
            ))
        ) || self.qcon_start()
    }

    // qcon → qconid | ( gconsym )          (qualified constructor)
    fn qcon(&mut self) -> ParserResult<String> {
        let t = self.next_()?;
        let l = t.span().start;

        match t.token() {
            TokenKind::QConId | TokenKind::ConId => Ok(t.text.borrow().to_owned()),

            TokenKind::Special(Special::LParen) => {
                let (_, sym, _) = self.gconsym()?;
                self.expect_token(TokenKind::Special(Special::RParen))?;
                Ok(sym)
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    fn qcon_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::QConId | TokenKind::ConId | TokenKind::Special(Special::LParen),
                _
            ))
        )
    }

    // gconsym → : | qconsym
    fn gconsym(&mut self) -> ParserResult<(Loc, String, Loc)> {
        let t = self.next_()?;
        let l = t.span().start;
        let r = t.span().end;
        match t.token() {
            TokenKind::ReservedOp(ReservedOp::Colon) => Ok((l, ":".to_owned(), r)),
            TokenKind::QConSym | TokenKind::ConSym => Ok((l, t.text.borrow().to_owned(), r)),
            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    fn gconsym_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::ReservedOp(ReservedOp::Colon) | TokenKind::QConSym | TokenKind::ConSym,
                _
            ))
        )
    }

    // op → varop | conop                     (operator)
    // conop → consym | ` conid `             (constructor operator)
    #[allow(unused)]
    fn op(&mut self) -> ParserResult<ParsedOp> {
        let t = self.next_()?;
        let l = t.span().start;
        let r = t.span().end;
        match t.token() {
            TokenKind::ConSym => {
                let str = t.text.borrow().to_owned();
                Ok(self.spanned(l, r, Op_::Con(str)))
            }

            TokenKind::VarSym => {
                let str = t.text.borrow().to_owned();
                Ok(self.spanned(l, r, Op_::Var(str)))
            }

            TokenKind::Special(Special::Backtick) => {
                let t_ = self.next_()?;
                let str = t_.text.borrow().to_owned();
                let ret = match t_.token() {
                    TokenKind::ConId => self.spanned(l, r, Op_::Con(str)),
                    TokenKind::VarId => self.spanned(l, r, Op_::Var(str)),
                    _ => return self.fail(l, ErrorKind::UnexpectedToken),
                };
                self.expect_token(TokenKind::Special(Special::Backtick))?;
                Ok(ret)
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }
}
