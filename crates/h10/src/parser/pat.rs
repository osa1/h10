use crate::ast::*;
use crate::parser::error::ErrorKind;
use crate::parser::{Parser, ParserResult};
use h10_lexer::{Literal, ReservedId, ReservedOp, Special, TokenKind};

impl<'input> Parser<'input> {
    /*
    pat → lpat qconop pat                   (infix constructor)
        | lpat
    */
    pub fn pat(&mut self) -> ParserResult<ParsedPat> {
        let pat1 = self.lpat()?;
        let l = pat1.span.start;
        match self.try_(Parser::qconop) {
            Ok((l_, qcon, r_)) => {
                let pat2 = self.pat()?;
                let r = pat2.span.end;
                Ok(self.spanned(
                    l,
                    r,
                    Pat_::Con(self.spanned(l_, r_, GCon_::QCon(qcon)), vec![pat1, pat2]),
                ))
            }
            Err(_) => Ok(pat1),
        }
    }

    /*
    lpat → apat
         | - (integer | float)              (negative literal)
         | gcon apat1 … apatk               (arity gcon  =  k, k ≥ 1)
    */
    fn lpat(&mut self) -> ParserResult<ParsedPat> {
        if let Ok((l, TokenKind::VarSym, r)) = self.peek() {
            if self.str(l, r) == "-" {
                self.skip(); // skip '-'
                match self.next()? {
                    (_, TokenKind::Literal(lit @ (Literal::Int | Literal::Float)), r_) => {
                        // TODO negation
                        return Ok(self.spanned(l, r_, Pat_::Lit(lit)));
                    }
                    (l, _, _) => {
                        return self.fail(l, ErrorKind::UnexpectedToken);
                    }
                }
            }
        }

        let pat1 = self.apat()?;
        let l = pat1.span.start;
        match pat1.node {
            Pat_::Con(con, pats) if pats.is_empty() && self.apat_start() => {
                let apat0 = self.apat()?;
                let mut r = apat0.span.end;
                let mut pats = vec![apat0];
                while self.apat_start() {
                    let apat = self.apat()?;
                    r = apat.span.end;
                    pats.push(apat);
                }
                Ok(self.spanned(l, r, Pat_::Con(con, pats)))
            }
            _ => Ok(pat1),
        }
    }

    /*
    apat → var [ @ apat]                    (as pattern)
         | gcon                             (arity gcon = 0)
         | qcon { fpat1 , … , fpatk }       (labeled pattern, k ≥ 0)
         | literal
         | _                                (wildcard)
         | ( pat )                          (parenthesized pattern)
         | ( pat1 , … , patk )              (tuple pattern, k ≥ 2)
         | [ pat1 , … , patk ]              (list pattern, k ≥ 1)
         | ~ apat                           (irrefutable pattern)
    */
    pub(super) fn apat(&mut self) -> ParserResult<ParsedPat> {
        match self.peek()? {
            (l, TokenKind::Literal(lit), r) => {
                self.skip(); // consume literal
                Ok(self.spanned(l, r, Pat_::Lit(lit)))
            }

            // Wildcard
            (l, TokenKind::ReservedId(ReservedId::Underscore), r) => {
                self.skip(); // consume '_'
                Ok(self.spanned(l, r, Pat_::Wildcard))
            }

            // Parenthesized pattern or tuple, or
            // var → ( varsym )
            // gcon → () | (,+)
            // qcon → ( gconsym )
            (l, TokenKind::Special(Special::LParen), _) => {
                self.skip(); // consume '('

                // gcon → ()
                if self.skip_token(TokenKind::Special(Special::RParen)) {
                    let (_, r) = self.last_tok_span();
                    return Ok(self.spanned(
                        l,
                        r,
                        Pat_::Con(self.spanned(l, r, GCon_::Tuple(0)), vec![]),
                    ));
                }

                // gcon → (,+)
                if self.skip_token(TokenKind::Special(Special::Comma)) {
                    let mut arity = 2;
                    while self.skip_token(TokenKind::Special(Special::Comma)) {
                        arity += 1;
                    }
                    let (_, r) = self.expect_token(TokenKind::Special(Special::RParen))?;
                    return Ok(self.spanned(
                        l,
                        r,
                        Pat_::Con(self.spanned(l, r, GCon_::Tuple(arity)), vec![]),
                    ));
                }

                // var → ( varsym )
                let varsym = self.try_opt(|self_| {
                    if let Ok((l_, TokenKind::VarSym | TokenKind::QVarSym, r_)) = self_.peek() {
                        self_.skip();
                        let (_, r) = self_
                            .expect_token(TokenKind::Special(Special::RParen))
                            .ok()?;
                        let str = self_.string(l_, r_);
                        Some(self_.spanned(l, r, Pat_::Var(str)))
                    } else {
                        None
                    }
                });

                if let Some(pat) = varsym {
                    return Ok(pat);
                }

                // qcon → ( gconsym )
                let gconsym = self.try_opt(|self_| {
                    if let Ok((
                        l_,
                        TokenKind::ReservedOp(ReservedOp::Colon)
                        | TokenKind::QConSym
                        | TokenKind::ConSym,
                        r_,
                    )) = self_.peek()
                    {
                        self_.skip();
                        let (_, r) = self_
                            .expect_token(TokenKind::Special(Special::RParen))
                            .ok()?;
                        let str = self_.string(l_, r_);
                        Some(self_.spanned(
                            l,
                            r,
                            Pat_::Con(self_.spanned(l, r, GCon_::QCon(str)), vec![]),
                        ))
                    } else {
                        None
                    }
                });

                if let Some(pat) = gconsym {
                    return Ok(pat);
                }

                // apat → ( pat ) | ( pat1 , … , patk )
                let mut pats = vec![self.pat()?];
                while self.skip_token(TokenKind::Special(Special::Comma)) {
                    pats.push(self.pat()?);
                }
                let (_, r) = self.expect_token(TokenKind::Special(Special::RParen))?;
                if pats.len() == 1 {
                    Ok(pats.pop().unwrap())
                } else {
                    Ok(self.spanned(l, r, Pat_::Tuple(pats)))
                }
            }

            // List pattern
            (l, TokenKind::Special(Special::LBracket), _) => {
                self.skip(); // consume '['
                if self.skip_token(TokenKind::Special(Special::RBracket)) {
                    let (_, r) = self.last_tok_span();
                    return Ok(self.spanned(l, r, Pat_::List(vec![])));
                }
                let mut pats = vec![];
                loop {
                    pats.push(self.pat()?);
                    if !self.skip_token(TokenKind::Special(Special::Comma)) {
                        break;
                    }
                }
                let (_, r) = self.expect_token(TokenKind::Special(Special::RBracket))?;
                Ok(self.spanned(l, r, Pat_::List(pats)))
            }

            // Irrefutable pattern
            (l, TokenKind::ReservedOp(ReservedOp::Tilde), _) => {
                self.skip(); // consume '~'
                let apat = self.apat()?;
                let r = apat.span.end;
                Ok(self.spanned(l, r, Pat_::Irrefutable(Box::new(apat))))
            }

            // var [@ apat]
            // gcon
            // qcon { fpat1 , … , fpatk }
            (l, _, _) => {
                let pat = self.try_(|self_| {
                    let var = self_.var()?;
                    if self_.skip_token(TokenKind::ReservedOp(ReservedOp::At)) {
                        let pat = self_.apat()?;
                        let r = pat.span.end;
                        Ok(self_.spanned(l, r, Pat_::As(var, Box::new(pat))))
                    } else {
                        let (_, r) = self_.last_tok_span();
                        Ok(self_.spanned(l, r, Pat_::Var(var)))
                    }
                });

                if let Ok(pat) = pat {
                    return Ok(pat);
                }

                let pat = self.try_(|self_| {
                    let gcon = self_.gcon()?;
                    let l = gcon.span.start;
                    if let GCon_::QCon(_) = &gcon.node {
                        if self_.skip_token(TokenKind::Special(Special::LBrace)) {
                            let mut pats = vec![];
                            while !matches!(
                                self_.peek(),
                                Ok((_, TokenKind::Special(Special::RBrace), _))
                            ) {
                                pats.push(self_.fpat()?);
                                if !self_.skip_token(TokenKind::Special(Special::Comma)) {
                                    break;
                                }
                            }
                            let (_, r) = self_.expect_token(TokenKind::Special(Special::RBrace))?;
                            return Ok(self_.spanned(
                                l,
                                r,
                                Pat_::Con(
                                    gcon,
                                    pats.into_iter().map(|(_, p)| p).collect(), // TODO: names
                                ),
                            ));
                        }
                    }
                    Ok(self_.spanned(l, gcon.span.end, Pat_::Con(gcon, vec![])))
                });

                if let Ok(pat) = pat {
                    return Ok(pat);
                }

                self.fail(l, ErrorKind::UnexpectedToken)
            }
        }
    }

    pub(super) fn apat_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::Literal(_)
                    | TokenKind::ReservedId(ReservedId::Underscore)
                    | TokenKind::Special(Special::LParen)
                    | TokenKind::Special(Special::LBracket)
                    | TokenKind::ReservedOp(ReservedOp::Tilde),
                _
            ))
        ) || self.var_start()
            || self.gcon_start()
            || self.qcon_start()
    }

    // fpat → qvar = pat
    fn fpat(&mut self) -> ParserResult<(String, ParsedPat)> {
        let qvar = self.qvar()?;
        let pat = self.pat()?;
        Ok((qvar, pat))
    }
}
