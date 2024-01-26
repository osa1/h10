mod decl;
mod error;
mod exp;
mod pat;
mod ty;
mod utils;

#[cfg(test)]
mod tests;

use crate::ast::*;
use crate::layout_token_generator::{LayoutError, LayoutTokenGenerator};
use crate::parser::error::{Context, Error, ErrorKind, GrammarItem};
use crate::token::TokenRef;
use h10_lexer::{Lexer, ReservedId, ReservedOp, Special, TokenKind};

use lexgen_util::Loc;
use rpds::List;

pub type ParserResult<A> = Result<A, Error>;

/// Parse a module.
pub fn parse_module(module_str: &str) -> ParserResult<Vec<TopDecl>> {
    Parser::new(LayoutTokenGenerator::new_top_level(tokenize(module_str))).module()
}

/// Parse a type with predicates.
#[cfg(test)]
pub fn parse_type(type_str: &str) -> ParserResult<(Vec<TypeBinder>, Vec<Type>, Type)> {
    Parser::new(LayoutTokenGenerator::new(tokenize(type_str))).type_with_context()
}

/// Parse an expression.
#[cfg(test)]
pub fn parse_exp(exp_str: &str) -> ParserResult<Exp> {
    Parser::new(LayoutTokenGenerator::new(tokenize(exp_str))).exp()
}

fn tokenize(input: &str) -> TokenRef {
    let lexer = Lexer::new(input);
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
    first_token.unwrap()
}

#[derive(Clone)]
struct Parser {
    token_gen: LayoutTokenGenerator,

    /// Next peeked token. We can't use std `Peekable` as we need to be able to pop current layout
    /// on parse error and `Peekable` doesn't give access to the inner iterator.
    ///
    /// This token has not been consumed yet and not linked to [`last_tok`].
    peeked: Option<Result<TokenRef, LayoutError>>,

    /// The last consumed token from [`lexer`]. This is used to create the linked list of tokens as
    /// we consume tokens.
    last_tok: Option<TokenRef>,

    context: List<Context>,
}

impl Parser {
    fn new(token_gen: LayoutTokenGenerator) -> Self {
        Parser {
            token_gen,
            peeked: None,
            last_tok: None,
            context: List::new(),
        }
    }

    /*
    module → module modid [exports] where body
           | body
    */
    fn module(&mut self) -> ParserResult<Vec<TopDecl>> {
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
            if let Some(token_or_error) = self_.token_gen.next() {
                match token_or_error {
                    Ok(token) => {
                        panic!("Token after module: {:?} ({})", token.token(), token.span());
                    }
                    Err(error) => {
                        panic!("Error after module: {:?}", error);
                    }
                }
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
    fn body(&mut self) -> ParserResult<Vec<TopDecl>> {
        // TODO: Currently only parsing topdecls
        self.expect_token(TokenKind::Special(Special::LBrace))?;
        let decls = self.topdecls()?;
        self.expect_token(TokenKind::Special(Special::RBrace))?;
        Ok(decls)
    }

    // qop    → qvarop | qconop             (qualified operator)
    // qvarop → qvarsym | ` qvarid `        (qualified variable operator)
    fn qop(&mut self) -> ParserResult<Exp> {
        let t = self.next_()?;
        let l = t.span().start;
        let r = t.span().end;
        match t.token() {
            TokenKind::QVarSym | TokenKind::VarSym => {
                let str = t.text().to_owned();
                Ok(self.spanned(l, r, Exp_::Var(str)))
            }

            TokenKind::QConSym | TokenKind::ConSym => {
                let str = t.text().to_owned();
                Ok(self.spanned(l, r, Exp_::Con(str)))
            }

            TokenKind::ReservedOp(ReservedOp::Colon) => {
                Ok(self.spanned(l, r, Exp_::Con(":".to_owned())))
            }

            TokenKind::Special(Special::Backtick) => {
                let t_ = self.next_()?;
                let exp = match t_.token() {
                    TokenKind::QVarId | TokenKind::VarId => Exp_::Var(t_.text().to_owned()),
                    TokenKind::QConId | TokenKind::ConId => Exp_::Con(t_.text().to_owned()),
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
    fn qual(&mut self) -> ParserResult<Stmt> {
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
                Ok(t.text().to_owned())
            }

            TokenKind::Special(Special::LParen) => {
                self.skip(); // consume '('
                let t = self.next_()?;
                match t.token() {
                    TokenKind::VarSym | TokenKind::QVarSym => {
                        self.expect_token(TokenKind::Special(Special::RParen))?;
                        Ok(t.text().to_owned())
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
    fn stmts(&mut self) -> ParserResult<Vec<Stmt>> {
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
    fn stmt(&mut self) -> ParserResult<Stmt> {
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
    fn context(&mut self) -> ParserResult<Vec<Type>> {
        self.context_parser(Self::class)
    }

    fn try_foralls(&mut self) -> ParserResult<Vec<TypeBinder>> {
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
                    let id = t.text().to_owned();
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

                TokenKind::VarSym if t.text() == "." => {
                    break;
                }

                _ => return self.fail(l, ErrorKind::UnexpectedToken),
            }
        }

        Ok(binders)
    }

    fn try_context_arrow(&mut self) -> Option<Vec<Type>> {
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
                Ok(t.text().to_owned())
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
            TokenKind::VarId => Ok(t.text().to_owned()),

            TokenKind::Special(Special::LParen) => {
                let t = self.next_()?;
                let l = t.span().start;
                match t.token() {
                    TokenKind::VarSym => {
                        self.expect_token(TokenKind::Special(Special::RParen))?;
                        Ok(t.text().to_owned())
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
    fn gcon(&mut self) -> ParserResult<GCon> {
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
            TokenKind::QConId | TokenKind::ConId => Ok(t.text().to_owned()),

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
            TokenKind::QConSym | TokenKind::ConSym => Ok((l, t.text().to_owned(), r)),
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
    fn op(&mut self) -> ParserResult<Op> {
        let t = self.next_()?;
        let l = t.span().start;
        let r = t.span().end;
        match t.token() {
            TokenKind::ConSym => {
                let str = t.text().to_owned();
                Ok(self.spanned(l, r, Op_::Con(str)))
            }

            TokenKind::VarSym => {
                let str = t.text().to_owned();
                Ok(self.spanned(l, r, Op_::Var(str)))
            }

            TokenKind::Special(Special::Backtick) => {
                let t_ = self.next_()?;
                let str = t_.text().to_owned();
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
