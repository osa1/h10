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

use std::rc::Rc;
use std::str::Chars;

use lexgen_util::{LexerError, Loc};
use rpds::List;

pub type ParserResult<A> = Result<A, Error>;

/// Parse a module. Handles layout.
pub fn parse_module(module_str: &str) -> ParserResult<Vec<ParsedTopDecl>> {
    Parser::new(module_str, "<input>".into(), LayoutLexer::new(module_str)).module()
}

/// Parse a type with predicates. Does not handle layout.
#[cfg(test)]
pub fn parse_type(
    type_str: &str,
) -> ParserResult<(Vec<ParsedTypeBinder>, Vec<ParsedType>, ParsedType)> {
    Parser::new(
        type_str,
        "<input>".into(),
        LayoutLexer::new_non_module(type_str),
    )
    .type_with_context()
}

/// Parse an expression. Does not handle layout.
#[cfg(test)]
pub fn parse_exp(exp_str: &str) -> ParserResult<ParsedExp> {
    Parser::new(
        exp_str,
        "<input>".into(),
        LayoutLexer::new_non_module(exp_str),
    )
    .exp()
}

/// Parse an expression. Handles layout.
#[cfg(test)]
pub fn parse_exp_with_layout(exp_str: &str) -> ParserResult<ParsedExp> {
    Parser::new(
        exp_str,
        "<input>".into(),
        LayoutLexer::new_non_module(exp_str),
    )
    .exp()
}

#[derive(Clone)]
struct Parser<'input> {
    source: Rc<str>,

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

    input: &'input str,

    context: List<Context>,
}

impl<'input> Parser<'input> {
    fn new(input: &'input str, source: Rc<str>, lexer: LayoutLexer<'input, Chars<'input>>) -> Self {
        Parser {
            source,
            lexer,
            peeked: None,
            last_tok: None,
            input,
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
        let (l, t, r) = self.next()?;
        match t {
            TokenKind::QVarSym | TokenKind::VarSym => {
                let str = self.string(l, r);
                Ok(self.spanned(l, r, Exp_::Var(str)))
            }

            TokenKind::QConSym | TokenKind::ConSym => {
                let str = self.string(l, r);
                Ok(self.spanned(l, r, Exp_::Con(str)))
            }

            TokenKind::ReservedOp(ReservedOp::Colon) => {
                Ok(self.spanned(l, r, Exp_::Con(":".to_owned())))
            }

            TokenKind::Special(Special::Backtick) => {
                let (l_, t_, r_) = self.next()?;
                let exp = match t_ {
                    TokenKind::QVarId | TokenKind::VarId => Exp_::Var(self.string(l_, r_)),
                    TokenKind::QConId | TokenKind::ConId => Exp_::Con(self.string(l_, r_)),
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
        match self.peek()? {
            (l, TokenKind::VarId | TokenKind::QVarId, r) => {
                self.skip(); // consume var
                Ok(self.string(l, r))
            }
            (_, TokenKind::Special(Special::LParen), _) => {
                self.skip(); // consume '('
                match self.next()? {
                    (l, TokenKind::VarSym | TokenKind::QVarSym, r) => {
                        self.expect_token(TokenKind::Special(Special::RParen))?;
                        Ok(self.string(l, r))
                    }
                    (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
                }
            }
            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
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
            let (l, t, r) = self.next()?;
            match t {
                TokenKind::VarId => {
                    let id = self.string(l, r);
                    binders.push(self.spanned(l, r, TypeBinder_ { id, ty: None }));
                }

                TokenKind::Special(Special::LParen) => {
                    let (id_l, id_r) = self.expect_token(TokenKind::VarId)?;
                    let id = self.string(id_l, id_r);
                    self.expect_token(TokenKind::ReservedOp(ReservedOp::ColonColon))?;
                    let ty = self.type_()?;
                    let (_, r) = self.expect_token(TokenKind::Special(Special::RParen))?;
                    binders.push(self.spanned(l, r, TypeBinder_ { id, ty: Some(ty) }));
                }

                TokenKind::VarSym if self.str(l, r) == "." => {
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
        match self.peek()? {
            (l, TokenKind::VarSym, r) => {
                self.skip(); // consume varsym
                Ok(self.str(l, r).to_owned())
            }
            (_, TokenKind::Special(Special::Backtick), _) => {
                self.skip(); // consume '`'
                let (l, r) = self.expect_token(TokenKind::VarId)?;
                self.expect_token(TokenKind::Special(Special::Backtick))?;
                Ok(self.str(l, r).to_owned())
            }
            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    // qconop → gconsym | ` qconid `        (qualified constructor operator)
    fn qconop(&mut self) -> ParserResult<(Loc, String, Loc)> {
        // TODO: return value
        if self.gconsym_start() {
            self.gconsym()
        } else {
            let (l, _) = self.expect_token(TokenKind::Special(Special::Backtick))?;
            let (l_, r_) = self.expect_token(TokenKind::QConId)?;
            let (_, r) = self.expect_token(TokenKind::Special(Special::Backtick))?;
            Ok((l, self.string(l_, r_), r))
        }
    }

    // var → varid | ( varsym )             (variable)
    fn var(&mut self) -> ParserResult<String> {
        match self.next()? {
            (l, TokenKind::VarId, r) => Ok(self.string(l, r)),
            (_, TokenKind::Special(Special::LParen), _) => match self.next()? {
                (l, TokenKind::VarSym, r) => {
                    self.expect_token(TokenKind::Special(Special::RParen))?;
                    Ok(self.string(l, r))
                }
                (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
            },
            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
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
        match self.next()? {
            (l, TokenKind::QConId | TokenKind::ConId, r) => Ok(self.string(l, r)),
            (_, TokenKind::Special(Special::LParen), _) => {
                let (_, sym, _) = self.gconsym()?;
                self.expect_token(TokenKind::Special(Special::RParen))?;
                Ok(sym)
            }
            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
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
        let (l, t, r) = self.next()?;
        match t {
            TokenKind::ReservedOp(ReservedOp::Colon) => Ok((l, ":".to_owned(), r)),
            TokenKind::QConSym | TokenKind::ConSym => Ok((l, self.string(l, r), r)),
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
    fn op(&mut self) -> ParserResult<ParsedOp> {
        let (l, t, r) = self.next()?;
        match t {
            TokenKind::ConSym => {
                let str = self.string(l, r);
                Ok(self.spanned(l, r, Op_::Con(str)))
            }

            TokenKind::VarSym => {
                let str = self.string(l, r);
                Ok(self.spanned(l, r, Op_::Var(str)))
            }

            TokenKind::Special(Special::Backtick) => {
                let (l_, t_, r_) = self.next()?;
                let str = self.string(l_, r_);
                let ret = match t_ {
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
