use crate::ast::*;
use crate::parser::error::ErrorKind;
use crate::parser::{Parser, ParserResult};
use h10_lexer::{ReservedId, ReservedOp, Special, TokenKind};

impl<'a> Parser<'a> {
    /*
    exp → infixexp :: [context =>] type    (expression type signature)
        | infixexp
    */
    pub fn exp(&mut self) -> ParserResult<Exp> {
        let exp = self.infixexp()?;
        let l = exp.span.start;
        if self.skip_token(TokenKind::ReservedOp(ReservedOp::ColonColon)) {
            // Type syntax subsumes context syntax
            let context = self.try_context_arrow().unwrap_or_default();
            let type_ = self.type_()?;
            let r = type_.span.end;
            Ok(self.spanned(
                l,
                r,
                Exp_::TypeAnnotation {
                    exp: Box::new(exp),
                    context,
                    type_,
                },
            ))
        } else {
            Ok(exp)
        }
    }

    /*
    infixexp → lexp qop infixexp            (infix operator application)
             | - infixexp                   (prefix negation)
             | lexp
    */
    pub(super) fn infixexp(&mut self) -> ParserResult<Exp> {
        if let Ok(t) = self.peek_() {
            if t.kind() == TokenKind::VarSym && t.text() == "-" {
                let l = t.span().start;
                let r = t.span().end;
                self.skip();
                let exp = self.infixexp()?;
                let r_ = exp.span.end;
                return Ok(self.spanned(
                    l,
                    r_,
                    Exp_::App(
                        Box::new(self.spanned(l, r, Exp_::Var("-".to_owned()))),
                        vec![exp],
                    ),
                ));
            }
        }

        if self.lexp_start() {
            let exp1 = self.lexp()?;
            let l = exp1.span.start;
            return Ok(if self.qop_start() {
                let op = self.qop()?;
                let exp2 = self.infixexp()?;
                let r = exp2.span.end;
                self.spanned(l, r, Exp_::App(Box::new(op), vec![exp1, exp2]))
            } else {
                exp1
            });
        }

        self.fail_with_next(ErrorKind::UnexpectedToken)
    }

    /*
    lexp → \ apat1 … apatn -> exp           (lambda abstraction, n ≥ 1)
         | let decls in exp                 (let expression)
         | if exp [;] then exp [;] else exp (conditional)
         | case exp of { alts }             (case expression)
         | do { stmts }                     (do expression)
         | fexp

    Parses top-level expressions without type annotations. These expressions can't be nested in
    other expressions without being wrapped with parens.
    */
    fn lexp(&mut self) -> ParserResult<Exp> {
        let (l, t, _) = self.peek()?;
        match t {
            TokenKind::ReservedOp(ReservedOp::Backslash) => {
                self.skip(); // consume '\'
                let mut args = vec![self.apat()?];
                while self.apat_start() {
                    args.push(self.apat()?);
                }
                self.expect_token(TokenKind::ReservedOp(ReservedOp::RightArrow))?;
                let body = self.exp()?;
                let r = body.span.end;
                Ok(self.spanned(l, r, Exp_::Lam(args, Box::new(body))))
            }

            TokenKind::ReservedId(ReservedId::Let) => {
                self.skip(); // consume 'let'
                let decls = self.value_decls()?;
                self.expect_token(TokenKind::ReservedId(ReservedId::In))?;
                let exp = self.exp()?;
                let r = exp.span.end;
                Ok(self.spanned(l, r, Exp_::Let(decls, Box::new(exp))))
            }

            TokenKind::ReservedId(ReservedId::If) => {
                self.skip(); // consume 'if'
                let e1 = self.exp()?;
                self.skip_token(TokenKind::Special(Special::Semi));
                self.expect_token(TokenKind::ReservedId(ReservedId::Then))?;
                let e2 = self.exp()?;
                self.skip_token(TokenKind::Special(Special::Semi));
                self.expect_token(TokenKind::ReservedId(ReservedId::Else))?;
                let e3 = self.exp()?;
                let r = e3.span.end;
                Ok(self.spanned(l, r, Exp_::If(Box::new(e1), Box::new(e2), Box::new(e3))))
            }

            TokenKind::ReservedId(ReservedId::Case) => {
                self.skip(); // consume 'case'
                let scrut = self.exp()?;
                self.expect_token(TokenKind::ReservedId(ReservedId::Of))?;
                self.expect_token(TokenKind::Special(Special::LBrace))?;
                let alts = self.alts()?;
                let rbrace = self.expect_token(TokenKind::Special(Special::RBrace))?;
                Ok(self.spanned(l, rbrace.span().end, Exp_::Case(Box::new(scrut), alts)))
            }

            TokenKind::ReservedId(ReservedId::Do) => {
                self.skip(); // consume 'do'
                self.expect_token(TokenKind::Special(Special::LBrace))?;
                let stmts = self.stmts()?; // parses `}` as well
                let (_, r) = self.last_tok_span();
                Ok(self.spanned(l, r, Exp_::Do(stmts)))
            }

            _ => {
                if self.fexp_start() {
                    self.fexp()
                } else {
                    self.fail(l, ErrorKind::UnexpectedToken)
                }
            }
        }
    }

    fn lexp_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::ReservedOp(ReservedOp::Backslash)
                    | TokenKind::ReservedId(ReservedId::Let)
                    | TokenKind::ReservedId(ReservedId::If)
                    | TokenKind::ReservedId(ReservedId::Case)
                    | TokenKind::ReservedId(ReservedId::Do),
                _
            ))
        ) || self.fexp_start()
    }

    // fexp → [fexp] aexp                   (function application)
    fn fexp(&mut self) -> ParserResult<Exp> {
        let exp1 = self.aexp()?;
        let l = exp1.span.start;
        let mut r = exp1.span.end;
        let mut args = vec![];
        while self.aexp_start() {
            let aexp = self.aexp()?;
            r = aexp.span.end;
            args.push(aexp);
        }
        Ok(if args.is_empty() {
            exp1
        } else {
            self.spanned(l, r, Exp_::App(Box::new(exp1), args))
        })
    }

    fn fexp_start(&mut self) -> bool {
        self.aexp_start()
    }

    /*
    aexp → qvar                             (variable)
         | gcon                             (general constructor)
         | literal

         | ( exp )                          (parenthesized expression)
         | ( exp1 , … , expk )              (tuple, k ≥ 2)
         | ( infixexp qop )                 (left section)
         | ( qop⟨-⟩ infixexp )              (right section)

         | [ exp1 , … , expk ]              (list, k ≥ 1)
         | [ exp1 [, exp2] .. [exp3] ]      (arithmetic sequence)
         | [ exp | qual1 , … , qualn ]      (list comprehension, n ≥ 1)

         | qcon { fbind1 , … , fbindn }     (labeled construction, n ≥ 0)
         | aexp⟨qcon⟩ { fbind1 , … , fbindn } (labeled update, n ≥ 1)

    Notes:

    `exp` is just `infixexp` with type signature (annotation, starting with `::`). So `exp` and
    `infixexp` has the same "start".

    It's a bit difficult to implement this directly following the productions above. We need a lot
    of backtracking and it's difficult to give good error messages when all alternatives fail. So
    we don't follow the CFG here.
    */
    fn aexp(&mut self) -> ParserResult<Exp> {
        let mut exp = self.aexp0()?;
        let l = exp.span.start;

        while matches!(self.peek(), Ok((_, TokenKind::Special(Special::LBrace), _))) {
            let updates = self.parse_aexp_updates()?;
            let (_, r) = self.last_tok_span();
            exp = self.spanned(
                l,
                r,
                Exp_::Update {
                    exp: Box::new(exp),
                    updates,
                },
            );
        }

        Ok(exp)
    }

    fn aexp_start(&mut self) -> bool {
        self.aexp0_start()
    }

    /// The part before `{ ... }` in an `aexp`.
    fn aexp0(&mut self) -> ParserResult<Exp> {
        if self.skip_token(TokenKind::Special(Special::LParen)) {
            let val = self.aexp0_parenthesized()?;
            self.expect_token(TokenKind::Special(Special::RParen))?;
            return Ok(val);
        }

        if self.skip_token(TokenKind::Special(Special::LBracket)) {
            let (l, _) = self.last_tok_span();
            // list, arithmetic sequence, or list comprehension
            if self.skip_token(TokenKind::Special(Special::RBracket)) {
                let (_, r) = self.last_tok_span();
                return Ok(self.spanned(l, r, Exp_::List(vec![])));
            }

            let exp1 = self.exp()?;

            if self.skip_token(TokenKind::Special(Special::RBracket)) {
                let (_, r) = self.last_tok_span();
                return Ok(self.spanned(l, r, Exp_::List(vec![exp1])));
            }

            if self.skip_token(TokenKind::Special(Special::Comma)) {
                // list or arithmetic sequence
                let exp2 = self.exp()?;
                return if self.skip_token(TokenKind::ReservedOp(ReservedOp::DotDot)) {
                    // arithmetic sequence
                    if self.skip_token(TokenKind::Special(Special::RBracket)) {
                        let (_, r) = self.last_tok_span();
                        return Ok(self.spanned(
                            l,
                            r,
                            Exp_::ArithmeticSeq {
                                exp1: Box::new(exp1),
                                exp2: Some(Box::new(exp2)),
                                exp3: None,
                            },
                        ));
                    }
                    let exp3 = self.exp()?;
                    let rbracket = self.expect_token(TokenKind::Special(Special::RBracket))?;
                    Ok(self.spanned(
                        l,
                        rbracket.span().end,
                        Exp_::ArithmeticSeq {
                            exp1: Box::new(exp1),
                            exp2: Some(Box::new(exp2)),
                            exp3: Some(Box::new(exp3)),
                        },
                    ))
                } else {
                    // list
                    let mut exps = vec![exp1, exp2];
                    while !self.skip_token(TokenKind::Special(Special::RBracket)) {
                        self.expect_token(TokenKind::Special(Special::Comma))?;
                        exps.push(self.exp()?);
                    }
                    let (_, r) = self.last_tok_span();
                    Ok(self.spanned(l, r, Exp_::List(exps)))
                };
            }

            if self.skip_token(TokenKind::ReservedOp(ReservedOp::DotDot)) {
                // arithmetic sequence
                if self.skip_token(TokenKind::Special(Special::RBracket)) {
                    let (_, r) = self.last_tok_span();
                    return Ok(self.spanned(
                        l,
                        r,
                        Exp_::ArithmeticSeq {
                            exp1: Box::new(exp1),
                            exp2: None,
                            exp3: None,
                        },
                    ));
                }

                let exp3 = self.exp()?;
                let rbracket = self.expect_token(TokenKind::Special(Special::RBracket))?;
                return Ok(self.spanned(
                    l,
                    rbracket.span().end,
                    Exp_::ArithmeticSeq {
                        exp1: Box::new(exp1),
                        exp2: None,
                        exp3: Some(Box::new(exp3)),
                    },
                ));
            }

            if self.skip_token(TokenKind::ReservedOp(ReservedOp::Pipe)) {
                // list comprehension
                let mut quals = vec![self.qual()?];
                while self.skip_token(TokenKind::Special(Special::Comma)) {
                    quals.push(self.qual()?);
                }
                let rbracket = self.expect_token(TokenKind::Special(Special::RBracket))?;
                return Ok(self.spanned(
                    l,
                    rbracket.span().end,
                    Exp_::ListComp {
                        exp: Box::new(exp1),
                        quals,
                    },
                ));
            }

            return self.fail_with_next(ErrorKind::UnexpectedToken);
        }

        let t = match self.peek_() {
            Ok(t) => t,
            Err(_) => return self.fail_with_next(ErrorKind::UnexpectedToken),
        };

        let l = t.span().start;
        let r = t.span().end;

        match t.kind() {
            TokenKind::QConId | TokenKind::ConId => {
                self.skip(); // consume con
                let str = t.text().to_owned();
                Ok(self.spanned(l, r, Exp_::Con(str)))
            }

            TokenKind::QVarId | TokenKind::VarId => {
                self.skip(); // consume var
                let str = t.text().to_owned();
                Ok(self.spanned(l, r, Exp_::Var(str)))
            }

            TokenKind::Literal(lit) => {
                self.skip(); // consume literal
                Ok(self.spanned(l, r, Exp_::Lit(lit)))
            }

            _ => self.fail_with_next(ErrorKind::UnexpectedToken),
        }
    }

    fn aexp0_start(&mut self) -> bool {
        self.qvar_start()
            || self.gcon_start()
            || self.qcon_start()
            || matches!(
                self.peek(),
                Ok((
                    _,
                    TokenKind::VarId
                        | TokenKind::Literal(_)
                        | TokenKind::Special(Special::LParen | Special::LBracket),
                    _
                ))
            )
    }

    // Parenthesized expressions. In addition to top-level expressions, this allows standalone
    // operators, left/right sections (operator at the beginning or end of an expression, instead
    // of infix), and comma-separated expressions (tuples).
    fn aexp0_parenthesized(&mut self) -> ParserResult<Exp> {
        let t = self.peek_()?;
        let l = t.span().start;
        let r = t.span().end;

        if t.kind() == TokenKind::Special(Special::RParen) {
            return Ok(self.spanned(l, r, Exp_::Tuple(vec![])));
        }

        if matches!(
            t.kind(),
            TokenKind::QVarSym
                | TokenKind::VarSym
                | TokenKind::QConSym
                | TokenKind::ConSym
                | TokenKind::ReservedOp(ReservedOp::Colon)
        ) {
            // Right section
            self.skip(); // skip symbol
            let str = t.text().to_owned();
            let fun = self.spanned(
                l,
                r,
                if matches!(t.kind(), TokenKind::QVarSym | TokenKind::VarSym) {
                    Exp_::Var(str)
                } else {
                    Exp_::Con(str)
                },
            );
            if matches!(self.peek(), Ok((_, TokenKind::Special(Special::RParen), _))) {
                return Ok(fun);
            }
            let arg = self.infixexp()?;
            let r = arg.span.end;
            return Ok(self.spanned(l, r, Exp_::App(Box::new(fun), vec![arg])));
        }

        if t.kind() == TokenKind::Special(Special::Backtick) {
            // Right section, id operator
            self.skip();
            let t = self.next_()?;
            let l = t.span().start;
            let r = t.span().end;
            let str = t.text().to_owned();
            let fun = match t.kind() {
                TokenKind::VarId | TokenKind::QVarId => self.spanned(l, r, Exp_::Var(str)),
                TokenKind::ConId | TokenKind::QConId => self.spanned(l, r, Exp_::Con(str)),
                _ => return self.fail(l, ErrorKind::UnexpectedToken),
            };
            self.expect_token(TokenKind::Special(Special::Backtick))?;
            let arg = self.infixexp()?;
            let r = arg.span.end;
            return Ok(self.spanned(l, r, Exp_::App(Box::new(fun), vec![arg])));
        }

        if t.kind() == TokenKind::Special(Special::Comma) {
            self.skip();
            // Tuple constructor
            let mut n_commas = 1;
            while matches!(self.peek(), Ok((_, TokenKind::Special(Special::Comma), _))) {
                self.skip(); // skip ','
                n_commas += 1;
            }
            // TODO: add a con
            let (_, _, r) = self.peek()?; // ')'
            return Ok(self.spanned(
                l,
                r,
                Exp_::Con("(".to_owned() + &(",".repeat(n_commas)) + ")"),
            ));
        }

        let mut e0 = self.lexp()?;
        let mut can_be_left_section = true;
        if self.skip_token(TokenKind::ReservedOp(ReservedOp::ColonColon)) {
            // First expression between parenthesis has a type signature, this can't be left
            // section.
            let context = self.try_context_arrow().unwrap_or_default();
            let type_ = self.type_()?;
            e0 = self.spanned(
                e0.span.start,
                type_.span.end,
                Exp_::TypeAnnotation {
                    exp: Box::new(e0),
                    context,
                    type_,
                },
            );
            can_be_left_section = false;
        }

        if let Ok((_, TokenKind::Special(Special::RParen), _)) = self.peek() {
            return Ok(e0);
        }

        if can_be_left_section {
            let mut parser = self.clone();
            match self.qop() {
                Ok(qop) => {
                    // Left section or infix application
                    parser = self.clone();
                    match self.infixexp() {
                        Ok(right_exp) => {
                            // Infix application
                            e0 = self.spanned(
                                e0.span.start,
                                right_exp.span.end,
                                Exp_::App(Box::new(qop), vec![e0, right_exp]),
                            );
                        }
                        Err(_) => {
                            // Left section
                            // FIXME: Left section AST
                            *self = parser;
                            return Ok(self.spanned(
                                qop.span.start,
                                e0.span.end,
                                Exp_::App(Box::new(qop), vec![e0]),
                            ));
                        }
                    }
                }
                Err(_) => {
                    *self = parser;
                }
            }
        }

        if self.skip_token(TokenKind::Special(Special::Comma)) {
            let l = e0.span.start;
            let mut exps = vec![e0];
            loop {
                exps.push(self.exp()?);
                if let Ok((_, TokenKind::Special(Special::RParen), r)) = self.peek() {
                    return Ok(self.spanned(l, r, Exp_::Tuple(exps)));
                }
                self.expect_token(TokenKind::Special(Special::Comma))?;
            }
        }

        Ok(e0)
    }

    /// `{ ... }` part in an `aexp`.
    fn parse_aexp_updates(&mut self) -> ParserResult<Vec<(String, Exp)>> {
        self.expect_token(TokenKind::Special(Special::LBrace))?;

        if self.skip_token(TokenKind::Special(Special::RBrace)) {
            return Ok(vec![]);
        }

        let mut updates = vec![self.fbind()?];
        while self.skip_token(TokenKind::Special(Special::Comma)) {
            updates.push(self.fbind()?);
        }
        self.expect_token(TokenKind::Special(Special::RBrace))?;

        Ok(updates)
    }

    // fbind → qvar = exp
    fn fbind(&mut self) -> ParserResult<(String, Exp)> {
        let var = self.qvar()?;
        self.expect_token(TokenKind::ReservedOp(ReservedOp::Equals))?;
        let exp = self.exp()?;
        Ok((var, exp))
    }

    // alts → alt1 ; … ; altn               (n ≥ 1)
    fn alts(&mut self) -> ParserResult<Vec<Alt>> {
        let mut alts = vec![self.alt()?];
        while self.skip_token(TokenKind::Special(Special::Semi)) {
            alts.push(self.alt()?);
        }
        Ok(alts)
    }

    /*
    alt → pat -> exp [where decls]
        | pat gdpat [where decls]
        |                                   (empty alternative)

    TODO: I don't understand why empty alt is allowed here, not implementing it for now.
    */
    fn alt(&mut self) -> ParserResult<Alt> {
        let pat = self.pat()?;
        if self.skip_token(TokenKind::ReservedOp(ReservedOp::RightArrow)) {
            let rhs = self.exp()?;
            let where_decls = if self.skip_token(TokenKind::ReservedId(ReservedId::Where)) {
                self.value_decls()?
            } else {
                vec![]
            };
            let (_, r) = self.last_tok_span();
            Ok(self.spanned(
                pat.span.start,
                r,
                Alt_ {
                    pat,
                    guarded_rhss: vec![(vec![], rhs)],
                    where_decls,
                },
            ))
        } else if self.gdpat_start() {
            let guarded_rhss = self.gdpat()?;
            let where_decls = if self.skip_token(TokenKind::ReservedId(ReservedId::Where)) {
                self.value_decls()?
            } else {
                vec![]
            };
            let l = pat.span.start;
            let (_, r) = self.last_tok_span();
            Ok(self.spanned(
                l,
                r,
                Alt_ {
                    pat,
                    guarded_rhss,
                    where_decls,
                },
            ))
        } else {
            self.fail_with_next(ErrorKind::UnexpectedToken)
        }
    }

    // gdpat → guards -> exp [ gdpat ]
    fn gdpat(&mut self) -> ParserResult<Vec<(Vec<Stmt>, Exp)>> {
        let guards = self.guards()?;
        self.expect_token(TokenKind::ReservedOp(ReservedOp::RightArrow))?;
        let exp = self.exp()?;
        let mut pats = vec![(guards, exp)];
        while self.guards_start() {
            let guards = self.guards()?;
            self.expect_token(TokenKind::ReservedOp(ReservedOp::RightArrow))?;
            let exp = self.exp()?;
            pats.push((guards, exp));
        }
        Ok(pats)
    }

    fn gdpat_start(&mut self) -> bool {
        self.guards_start()
    }
}
