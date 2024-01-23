use crate::ast::*;
use crate::layout_lexer::LayoutLexer_;
use crate::parser::error::ErrorKind;
use crate::parser::{Parser, ParserResult};
use h10_lexer::{ReservedOp, Special, Token};

impl<'input, L: LayoutLexer_> Parser<'input, L> {
    /// Parses the part after `::` in a type signature. E.g. `x :: Show a => a -> String` the part
    /// `Show a => a -> String`.
    pub(crate) fn type_with_context(
        &mut self,
    ) -> ParserResult<(Vec<ParsedTypeBinder>, Vec<ParsedType>, ParsedType)> {
        let foralls = self.try_foralls()?;
        let context = self.try_context_arrow().unwrap_or_default();
        let ty = self.type_()?;
        Ok((foralls, context, ty))
    }

    /*
    class  → qtycls tyvar
           | qtycls ( tyvar atype1 … atypen )   (n ≥ 1)

    qtycls → [ modid . ] tycls

    tycls  → conid

    tyvar = varid
    */
    pub(super) fn class(&mut self) -> ParserResult<ParsedType> {
        let (l, t, r) = self.next()?;
        match t {
            Token::QConId | Token::ConId => {
                let con_str = self.string(l, r);
                let con = self.spanned(l, r, Type_::Con(self.spanned(l, r, TyCon_::Id(con_str))));
                let arg = if self.skip_token(Token::Special(Special::LParen)) {
                    // qtycls ( tyvar atype1 ... atypen )
                    let (l_, _) = self.last_tok_span();
                    let cls = self.atype()?;
                    let mut args = vec![];
                    while self.atype_start() {
                        args.push(self.atype()?);
                    }
                    self.expect_token(Token::Special(Special::RParen))?;
                    let (_, r_) = self.last_tok_span();
                    self.spanned(l_, r_, Type_::App(Box::new(cls), args))
                } else {
                    // qtycls tyvar
                    let (l_, arg_str, r_) = self.expect_token_string(Token::VarId)?;
                    self.spanned(l_, r_, Type_::Var(arg_str))
                };
                Ok(self.spanned(l, arg.span.end, Type_::App(Box::new(con), vec![arg])))
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    // type → btype [-> type]                  (function type)
    pub(crate) fn type_(&mut self) -> ParserResult<ParsedType> {
        let ty1 = self.btype()?;
        if self.skip_token(Token::ReservedOp(ReservedOp::RightArrow)) {
            let ty2 = self.type_()?;
            Ok(self.spanned(
                ty1.span.start,
                ty2.span.end,
                Type_::Arrow(Box::new(ty1), Box::new(ty2)),
            ))
        } else {
            Ok(ty1)
        }
    }

    fn _type_start(&mut self) -> bool {
        self._btype_start()
    }

    // btype → [btype] atype                   (type application)
    fn btype(&mut self) -> ParserResult<ParsedType> {
        let ty1 = self.atype()?;
        let mut args = vec![];
        while self.atype_start() {
            args.push(self.atype()?);
        }
        Ok(match args.last() {
            None => ty1,
            Some(last_ty) => self.spanned(
                ty1.span.start,
                last_ty.span.end,
                Type_::App(Box::new(ty1), args),
            ),
        })
    }

    fn _btype_start(&mut self) -> bool {
        self.atype_start()
    }

    /*
    atype → gtycon
          | tyvar
          | ( type1 , … , typek )              (tuple type, k ≥ 2)
          | [ type ]                           (list type)
          | ( type )                           (parenthesized constructor)
    */
    pub(super) fn atype(&mut self) -> ParserResult<ParsedType> {
        if let Ok(ty) = self.try_(Self::gtycon) {
            return Ok(self.spanned(ty.span.start, ty.span.end, Type_::Con(ty)));
        }

        let (l, t, r) = self.peek()?;

        match t {
            Token::VarId => {
                self.skip(); // consume type id
                let str = self.string(l, r);
                Ok(self.spanned(l, r, Type_::Var(str)))
            }

            Token::Special(Special::LParen) => {
                self.skip(); // consume '('
                let ty1 = self.type_()?;

                if self.skip_token(Token::Special(Special::RParen)) {
                    return Ok(ty1);
                }

                self.expect_token(Token::Special(Special::Comma))?;
                let mut tys = vec![ty1, self.type_()?];
                while self.skip_token(Token::Special(Special::Comma)) {
                    tys.push(self.type_()?);
                }
                self.expect_token(Token::Special(Special::RParen))?;
                let (_, r) = self.last_tok_span();
                Ok(self.spanned(l, r, Type_::Tuple(tys)))
            }

            Token::Special(Special::LBracket) => {
                self.skip(); // consume '['
                let ty = self.type_()?;
                self.expect_token(Token::Special(Special::RBracket))?;
                let (_, r) = self.last_tok_span();
                Ok(self.spanned(l, r, Type_::List(Box::new(ty))))
            }

            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    pub(super) fn atype_start(&mut self) -> bool {
        self.gtycon_start()
            || matches!(
                self.peek(),
                Ok((
                    _,
                    Token::VarId | Token::Special(Special::LParen | Special::LBracket),
                    _
                ))
            )
    }

    /*
    gtycon → qtycon
           | ()                                (unit type)
           | []                                (list constructor)
           | (->)                              (function constructor)
           | (,+)                              (tupling constructors)

    qtycon = qconid
    */
    fn gtycon(&mut self) -> ParserResult<ParsedTyCon> {
        let (l, t, r) = self.peek()?;
        match t {
            Token::QConId | Token::ConId => {
                self.skip(); // consume qualified type constructor
                let str = self.string(l, r);
                Ok(self.spanned(l, r, TyCon_::Id(str)))
            }
            Token::Special(Special::LParen) => {
                self.skip(); // consume '('
                let (_, t_, r_) = self.peek()?;
                match t_ {
                    Token::Special(Special::RParen) => {
                        self.skip(); // consume ')'
                        Ok(self.spanned(l, r_, TyCon_::Tuple(0)))
                    }
                    Token::ReservedOp(ReservedOp::RightArrow) => {
                        self.skip(); // consume '->'
                        self.expect_token(Token::Special(Special::RParen))?;
                        let (_, r__) = self.last_tok_span();
                        Ok(self.spanned(l, r__, TyCon_::Arrow))
                    }
                    Token::Special(Special::Comma) => {
                        self.skip(); // consume ','
                        let mut arity = 2;
                        while matches!(self.peek()?, (_, Token::Special(Special::Comma), _)) {
                            self.skip(); // consume ','
                            arity += 1;
                        }
                        self.expect_token(Token::Special(Special::RParen))?;
                        let (_, r__) = self.last_tok_span();
                        Ok(self.spanned(l, r__, TyCon_::Tuple(arity)))
                    }
                    _ => self.fail(l, ErrorKind::UnexpectedToken),
                }
            }
            Token::Special(Special::LBracket) => {
                self.skip(); // consume '['
                self.expect_token(Token::Special(Special::RBracket))?;
                let (_, r_) = self.last_tok_span();
                Ok(self.spanned(l, r_, TyCon_::List))
            }
            _ => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    fn gtycon_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                Token::QConId | Token::ConId | Token::Special(Special::LParen | Special::LBracket),
                _
            ))
        )
    }
}
