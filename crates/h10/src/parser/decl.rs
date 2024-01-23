#![allow(unused)]

use crate::ast::*;
use crate::layout_lexer::LayoutLexer_;
use crate::parser::error::{Error, ErrorKind, GrammarItem};
use crate::parser::{Parser, ParserResult};
use crate::token::TokenRef;
use h10_lexer::{Literal, ReservedId, ReservedOp, Special, TokenKind};

impl<'input, L: LayoutLexer_> Parser<'input, L> {
    /*
    impdecls → impdecl1 ; … ; impdecln                      (n ≥ 1)

    impdecl → import [qualified] modid [as modid] [impspec]
            |                                               (empty declaration)

    impspec → ( import1 , … , importn [ , ] )               (n ≥ 0)
            | hiding ( import1 , … , importn [ , ] )        (n ≥ 0)

    import → var
           | tycon [ (..) | ( cname1 , … , cnamen ) ]       (n ≥ 0)
           | tycls [ (..) | ( var1 , … , varn ) ]           (n ≥ 0)

    cname → var | con

    NB. We can't distinguish typeclass import from type import in parse time.
    */
    pub(super) fn impdecls(&mut self) -> ParserResult<Vec<ParsedImportDecl>> {
        let mut imps = vec![];
        while self.skip_token(TokenKind::ReservedId(ReservedId::Import)) {
            let qualified = self.skip_token_pred(TokenKind::VarId, |str| str == "qualified");
            let modid = self.expect_token_pred_string(|token| {
                matches!(token, TokenKind::QConId | TokenKind::ConId)
            })?;
            let as_ = if self.skip_token_pred(TokenKind::VarId, |str| str == "as") {
                Some(self.expect_token_pred_string(|token| {
                    matches!(token, TokenKind::QConId | TokenKind::ConId)
                })?)
            } else {
                None
            };
            let hiding = self.skip_token_pred(TokenKind::VarId, |str| str == "hiding");
            /*
            let type_import: ParsedTypeImport = if self.skip_token(TokenKind::Special(Special::LParen))
            {
                let ret = if self.skip_token(TokenKind::ReservedOp(ReservedOp::DotDot)) {
                    self.expect_token(TokenKind::Special(Special::RParen))?;
                    TypeImport::All
                } else {
                    let mut things: Vec<String> = vec![];
                    while !self.skip_token(TokenKind::Special(Special::RParen)) {
                        todo!()
                    }
                    TypeImport::Things(things)
                };
                self.expect_token(TokenKind::Special(Special::RParen))?;
                ret
            } else {
                TypeImport::JustType
            };
            imps.push(ImportDecl {
                modid,
                qualified,
                as_,
                hiding,
                imports,
            });
            */
            todo!()
        }
        Ok(imps)
    }

    /*
    topdecls → topdecl1 ; … ; topdecln  (n ≥ 0)

    Note: Parses until a `}`.
    */
    pub(super) fn topdecls(&mut self) -> ParserResult<Vec<ParsedTopDecl>> {
        self.in_context(GrammarItem::TopDecls, |self_| {
            let mut decls: Vec<ParsedTopDecl> = vec![];
            while self_.skip_token(TokenKind::Special(Special::Semi)) {}
            while !matches!(
                self_.peek(),
                Ok((_, TokenKind::Special(Special::RBrace), _))
            ) {
                let decl: ParsedTopDecl = self_.topdecl()?;
                decls.push(decl);
                while self_.skip_token(TokenKind::Special(Special::Semi)) {}
            }
            Ok(decls)
        })
    }

    /*
    topdecl → type simpletype = type
            | data [context =>] simpletype [= constrs] [deriving]
            | newtype [context =>] simpletype = newconstr [deriving]
            | class [scontext =>] tycls tyvar [where cdecls]
            | instance [scontext =>] qtycls inst [where idecls]
            | default (type1 , … , typen)  (n ≥ 0)
            | foreign fdecl
            | decl
    */
    fn topdecl(&mut self) -> ParserResult<ParsedTopDecl> {
        let t = self.peek_()?;
        let top_decl_kind: ParsedTopDeclKind = match t.token() {
            TokenKind::ReservedId(ReservedId::Kind) => {
                self.kind_sig_decl().map(TopDeclKind::KindSig)
            }

            TokenKind::ReservedId(ReservedId::Type) => self.type_decl().map(TopDeclKind::Type),

            TokenKind::ReservedId(ReservedId::Data) => self.data_decl().map(TopDeclKind::Data),

            TokenKind::ReservedId(ReservedId::Newtype) => {
                self.newtype_decl().map(TopDeclKind::Newtype)
            }

            TokenKind::ReservedId(ReservedId::Class) => self.class_decl().map(TopDeclKind::Class),

            TokenKind::ReservedId(ReservedId::Instance) => {
                self.instance_decl().map(TopDeclKind::Instance)
            }

            TokenKind::ReservedId(ReservedId::Default) => {
                self.default_decl().map(TopDeclKind::Default)
            }

            TokenKind::ReservedId(ReservedId::Foreign) => unimplemented!("FFI not supported"),

            _ => self.value_decl().map(TopDeclKind::Value),
        }?;
        let (_, r) = self.last_tok_span();
        let line_number = t.span().start.line;
        Ok(TopDecl {
            kind: top_decl_kind,
            line_number,
            first_token: t,
            last_token: self.last_tok.clone().unwrap(),
        })
    }

    // decls → { decl1 ; … ; decln }           (n ≥ 0)
    pub(super) fn value_decls(&mut self) -> ParserResult<Vec<ParsedValueDecl>> {
        self.expect_token(TokenKind::Special(Special::LBrace))?;
        let mut decls = vec![];
        loop {
            // `decl` can be empty
            if self.skip_token(TokenKind::Special(Special::Semi)) {
            } else if matches!(self.peek(), Ok((_, TokenKind::Special(Special::RBrace), _))) {
                break;
            } else if self.in_explicit_layout() {
                decls.push(self.value_decl()?);
            } else {
                let mut parser = self.clone();
                match self.value_decl() {
                    Ok(decl) => {
                        decls.push(decl);
                    }
                    Err(err) => {
                        if parser.pop_layout() {
                            *self = parser;
                            return Ok(decls);
                        } else {
                            return Err(err);
                        }
                    }
                }
            }
        }
        self.expect_token(TokenKind::Special(Special::RBrace))?;
        Ok(decls)
    }

    /*
    decl → gendecl
         | (funlhs | pat) rhs

    funlhs → var apat+
           | pat varop pat
           | '(' funlhs ')' apat+

    gendecl → vars '::' [context '=>'] type      (type signature)
            | 'fixity' [integer] ops             (fixity declaration)

    ops → op1 , … , opn                          (n ≥ 1)

    gendecl cases are easy: `fixity` gives us the production, and for `vars ::` we can backtrack.

    For the rest we parse patterns until `=` or `|` (first tokens in `rhs`).

    TODO: `funlhs` is basically a pattern with variable as prefix or infix in one of the patterns.
    Can we simplify parsing using this?
    */
    fn value_decl(&mut self) -> ParserResult<ParsedValueDecl> {
        // Fixity declaration
        if let Ok((
            l,
            TokenKind::ReservedId(ReservedId::Infix | ReservedId::Infixl | ReservedId::Infixr),
            _,
        )) = self.peek()
        {
            let fixity = self.fixity().unwrap();
            let prec: Option<u32> =
                if let Ok((l, TokenKind::Literal(Literal::Int), r)) = self.peek() {
                    self.skip();
                    Some(self.str(l, r).parse().unwrap())
                } else {
                    None
                };
            let mut ops = vec![self.op()?];
            while self.skip_token(TokenKind::Special(Special::Comma)) {
                ops.push(self.op()?);
            }
            let (_, r) = self.last_tok_span();
            return Ok(self.spanned(l, r, ValueDecl_::Fixity { fixity, prec, ops }));
        }

        // Type signature
        let ty_sig = self.try_(|self_| {
            let (l, _, _) = self_.peek()?;
            let vars = self_.vars()?;
            self_.expect_token(TokenKind::ReservedOp(ReservedOp::ColonColon))?;
            let (foralls, context, ty) = self_.type_with_context()?;
            let r = ty.span.end;
            Ok(self_.spanned(
                l,
                r,
                ValueDecl_::TypeSig {
                    vars,
                    foralls,
                    context,
                    ty,
                },
            ))
        });

        if let Ok(ty_sig) = ty_sig {
            return Ok(ty_sig);
        }

        let (l, _, _) = self.peek()?;
        let lhs: ParsedLhs = self
            .funlhs_try()
            .map(|(var, pats)| {
                let (_, r) = self.last_tok_span();
                self.spanned(l, r, Lhs_::Fun { var, pats })
            })
            .or_else(|_| {
                let (_, r) = self.last_tok_span();
                let pat = self.pat_try()?;
                Ok(self.spanned(l, r, Lhs_::Pat(pat)))
            })
            .or_else(|_: Error| self.fail_with_next(ErrorKind::UnexpectedToken))?;

        let rhs = self.rhs()?;
        let r = rhs.span.end;

        Ok(self.spanned(l, r, ValueDecl_::Value { lhs, rhs }))
    }

    // Tries to parse `funlhs`. Backtracks on failure.
    fn funlhs_try(&mut self) -> ParserResult<(String, Vec<ParsedPat>)> {
        self.try_(Self::funlhs1)
            .or_else(|_| self.try_(Self::funlhs2))
            .or_else(|_| self.try_(Self::funlhs3))
    }

    fn pat_try(&mut self) -> ParserResult<ParsedPat> {
        // TODO: Pat may not have a variable?
        self.try_(Self::pat)
    }

    // funlhs → var apat+
    //
    // Parses patterns until `|` or '=' but does not consume those tokens.
    fn funlhs1(&mut self) -> ParserResult<(String, Vec<ParsedPat>)> {
        let var = self.var()?;
        let mut pats = vec![self.apat()?];
        while !self.funlhs_end() {
            pats.push(self.apat()?);
        }
        Ok((var, pats))
    }

    // funlhs → pat varop pat
    //
    // Parses patterns until `|` or '=' but does not consume those tokens.
    fn funlhs2(&mut self) -> ParserResult<(String, Vec<ParsedPat>)> {
        let pat1 = self.pat()?;
        let varop = self.varop()?;
        let pat2 = self.pat()?;
        if !self.funlhs_end() {
            return self.fail_with_next(ErrorKind::UnexpectedToken);
        }
        Ok((varop, vec![pat1, pat2]))
    }

    // funlhs → '(' funlhs ')' apat+
    //
    // Parses patterns until `|` or '=' but does not consume those tokens.
    fn funlhs3(&mut self) -> ParserResult<(String, Vec<ParsedPat>)> {
        self.expect_token(TokenKind::Special(Special::LParen))?;
        let (var, mut pats) = self.funlhs_try()?;
        self.expect_token(TokenKind::Special(Special::RParen))?;
        pats.push(self.apat()?);
        while !self.funlhs_end() {
            pats.push(self.apat()?);
        }
        Ok((var, pats))
    }

    fn funlhs_end(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((
                _,
                TokenKind::ReservedOp(ReservedOp::Pipe | ReservedOp::Equals),
                _
            ))
        )
    }

    /*
    rhs → = exp [where decls]
        | gdrhs [where decls]
    */
    fn rhs(&mut self) -> ParserResult<ParsedRhs> {
        if self.skip_token(TokenKind::ReservedOp(ReservedOp::Equals)) {
            let (l, _) = self.last_tok_span();
            let rhs = self.exp()?;
            let mut where_decls = vec![];
            if self.skip_token(TokenKind::ReservedId(ReservedId::Where)) {
                where_decls = self.value_decls()?;
            }
            let (_, r) = self.last_tok_span();
            Ok(self.spanned(l, r, Rhs_::Rhs { rhs, where_decls }))
        } else if matches!(
            self.peek(),
            Ok((_, TokenKind::ReservedOp(ReservedOp::Pipe), _))
        ) {
            let (l, _) = self.last_tok_span();
            let guarded_rhss = self.gdrhs()?;
            let mut where_decls = vec![];
            if self.skip_token(TokenKind::ReservedId(ReservedId::Where)) {
                where_decls = self.value_decls()?;
            }
            let (_, r) = self.last_tok_span();
            Ok(self.spanned(
                l,
                r,
                Rhs_::GuardedRhs {
                    rhss: guarded_rhss,
                    where_decls,
                },
            ))
        } else {
            self.fail_with_next(ErrorKind::UnexpectedToken)
        }
    }

    // gdrhs → guards = exp [gdrhs]
    fn gdrhs(&mut self) -> ParserResult<Vec<ParsedGuardedRhs>> {
        let (l, _, _) = self.peek()?; // location of '|'
        let guards = self.guards()?; // one or more
        self.expect_token(TokenKind::ReservedOp(ReservedOp::Equals))?;
        let rhs = self.exp()?;
        let (_, r) = self.last_tok_span();
        let mut rhss = vec![self.spanned(l, r, GuardedRhs_ { guards, rhs })];
        while let Ok((l_, TokenKind::ReservedOp(ReservedOp::Pipe), _)) = self.peek() {
            let guards = self.guards()?;
            self.expect_token(TokenKind::ReservedOp(ReservedOp::Equals))?;
            let rhs = self.exp()?;
            let (_, r_) = self.last_tok_span();
            rhss.push(self.spanned(l_, r_, GuardedRhs_ { guards, rhs }));
        }
        Ok(rhss)
    }

    // guards → '|' guard1, …, guardn          (n ≥ 1)
    pub(super) fn guards(&mut self) -> ParserResult<Vec<ParsedStmt>> {
        self.expect_token(TokenKind::ReservedOp(ReservedOp::Pipe))?;
        let mut guards = vec![self.guard()?];
        if self.skip_token(TokenKind::Special(Special::Comma)) {
            guards.push(self.guard()?);
        }
        Ok(guards)
    }

    pub(super) fn guards_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Ok((_, TokenKind::ReservedOp(ReservedOp::Pipe), _))
        )
    }

    /*
    guard → pat <- infixexp                    (pattern guard)
          | let decls                          (local declaration)
          | infixexp                           (boolean guard)
    */
    fn guard(&mut self) -> ParserResult<ParsedStmt> {
        if self.skip_token(TokenKind::ReservedId(ReservedId::Let)) {
            let (l, _) = self.last_tok_span();
            return self.value_decls().map(|decls| {
                let (_, r) = self.last_tok_span();
                self.spanned(l, r, Stmt_::Let(decls))
            });
        }

        let (l, _, _) = self.peek()?;

        let pat = self.try_(|self_| {
            let pat = self_.pat()?;
            self_.expect_token(TokenKind::ReservedOp(ReservedOp::LeftArrow))?;
            Ok(pat)
        });

        match pat {
            Ok(pat) => {
                // `pat <-` parse successful, parsing a binding
                let rhs = self.infixexp()?;
                let (_, r) = self.last_tok_span();
                Ok(self.spanned(l, r, Stmt_::Bind(pat, rhs)))
            }
            Err(_) => {
                // Try `infixexp`
                match self.try_(|self_| self_.exp()) {
                    Ok(exp) => {
                        let (_, r) = self.last_tok_span();
                        Ok(self.spanned(l, r, Stmt_::Exp(exp)))
                    }
                    Err(_) => self.fail_with_next(ErrorKind::UnexpectedToken),
                }
            }
        }
    }

    fn kind_sig_decl(&mut self) -> ParserResult<ParsedKindSigDecl> {
        self.skip(); // skip 'kind'
        let (l, _) = self.last_tok_span();

        let (_, ty, _) = self.expect_token_string(TokenKind::ConId)?;

        self.expect_token(TokenKind::ReservedOp(ReservedOp::ColonColon))?;

        let (foralls, context, sig) = self.type_with_context()?;
        if !context.is_empty() {
            panic!("Type signatures can't have contexts");
        }
        Ok(self.spanned(l, sig.span.end, KindSigDecl_ { ty, foralls, sig }))
    }

    // type simpletype = type
    fn type_decl(&mut self) -> ParserResult<ParsedTypeDecl> {
        self.skip(); // skip 'type'
        let (l, _) = self.last_tok_span();

        // `tycon` part of `simpletype`.
        let (_, ty, _) = self.expect_token_string(TokenKind::ConId)?;

        // Type synonym declaration.
        let mut tyvars = vec![];
        while let Some(tyvar) = self.skip_token_string(TokenKind::VarId) {
            tyvars.push(tyvar);
        }

        self.expect_token(TokenKind::ReservedOp(ReservedOp::Equals))?;
        let rhs = self.type_()?;

        Ok(self.spanned(
            l,
            rhs.span.end,
            TypeDecl_ {
                ty,
                vars: tyvars,
                rhs,
            },
        ))
    }

    // simpletype → tycon tyvar1 … tyvark         (k ≥ 0)
    // tycon → conid                              (type constructors)
    // tyvar → varid                              (type variables)
    fn simpletype(&mut self) -> ParserResult<(String, Vec<String>)> {
        let (_, tycon, _) = self.expect_token_string(TokenKind::ConId)?;
        let mut tyvars = vec![];
        while let Some(tyvar) = self.skip_token_string(TokenKind::VarId) {
            tyvars.push(tyvar);
        }
        Ok((tycon, tyvars))
    }

    // data [context =>] simpletype [= constrs] [deriving]
    fn data_decl(&mut self) -> ParserResult<ParsedDataDecl> {
        self.skip(); // skip 'data'
        let (l, _) = self.last_tok_span();
        let context = self.try_context_arrow().unwrap_or_default();
        let (ty_con, ty_args) = self.simpletype()?;
        let cons = if self.skip_token(TokenKind::ReservedOp(ReservedOp::Equals)) {
            self.constrs()?
        } else {
            vec![]
        };
        let deriving = if self.skip_token(TokenKind::ReservedId(ReservedId::Deriving)) {
            self.deriving()?
        } else {
            vec![]
        };
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(
            l,
            r,
            DataDecl_ {
                context,
                ty_con,
                ty_args,
                cons,
                deriving,
            },
        ))
    }

    // constrs → constr1 | … | constrn                      (n ≥ 1)
    fn constrs(&mut self) -> ParserResult<Vec<ParsedCon>> {
        let mut constrs = vec![self.constr()?];
        while self.skip_token(TokenKind::ReservedOp(ReservedOp::Pipe)) {
            constrs.push(self.constr()?);
        }
        Ok(constrs)
    }

    /*
    constr  → con [!] atype1 … [!] atypek                   (arity con  =  k, k ≥ 0)
            | (btype | ! atype) conop (btype | ! atype)     (infix conop)
            | con { fielddecl1 , … , fielddecln }           (n ≥ 0)
    */
    fn constr(&mut self) -> ParserResult<ParsedCon> {
        match self.peek()? {
            (l, TokenKind::ConId | TokenKind::Special(Special::LParen), _) => {
                let con = self.con()?;
                let mut fields = vec![];
                if self.skip_token(TokenKind::Special(Special::LBrace)) {
                    loop {
                        fields.push(self.fielddecl()?);
                        if self.skip_token(TokenKind::Special(Special::RBrace)) {
                            break;
                        }
                        self.expect_token(TokenKind::Special(Special::Comma))?;
                    }
                } else {
                    while self.con_field_start() {
                        fields.push(self.con_field()?);
                    }
                }
                let (_, r) = self.last_tok_span();
                Ok(self.spanned(l, r, Con_ { con, fields }))
            }

            (l, TokenKind::VarSym, r) if self.str(l, r) == "!" => todo!(),

            _ => todo!(),
        }
    }

    // [!] atype
    fn con_field(&mut self) -> ParserResult<ParsedFieldDecl> {
        let (l, _, _) = self.peek()?;
        if let Ok((l_, TokenKind::VarSym, r_)) = self.peek() {
            if self.str(l_, r_) == "!" {
                self.skip();
            }
        }
        let ty = self.atype()?;
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(l, r, FieldDecl_ { vars: vec![], ty }))
    }

    fn con_field_start(&mut self) -> bool {
        self.atype_start()
            || (match self.peek() {
                Ok((l, TokenKind::VarSym, r)) => self.str(l, r) == "!",
                _ => false,
            })
    }

    // fielddecl → vars :: (type | ! atype)
    fn fielddecl(&mut self) -> ParserResult<ParsedFieldDecl> {
        let (l, _, _) = self.peek()?;
        let vars = self.vars()?;
        self.expect_token(TokenKind::ReservedOp(ReservedOp::ColonColon))?;
        let ty = match self.peek()? {
            (l, TokenKind::VarSym, r) if self.str(l, r) == "!" => {
                self.skip();
                self.atype()?
            }
            _ => self.type_()?,
        };
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(l, r, FieldDecl_ { vars, ty }))
    }

    // con → conid | ( consym )                              (constructor)
    fn con(&mut self) -> ParserResult<String> {
        match self.next()? {
            (l, TokenKind::ConId, r) => Ok(self.string(l, r)),

            (_, TokenKind::Special(Special::LParen), _) => {
                let (_, con, _) = self.expect_token_string(TokenKind::ConSym)?;
                self.expect_token(TokenKind::Special(Special::RParen))?;
                Ok(con)
            }

            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    /*
    deriving → deriving (dclass | (dclass1, … , dclassn))   (n ≥ 0)
    dclass   → qtycls
    qtycls   → [ modid . ] tycls

    Note: assumes that 'deriving' is consumed.
    */
    fn deriving(&mut self) -> ParserResult<Vec<String>> {
        let mut classes: Vec<String> = vec![];
        if self.skip_token(TokenKind::Special(Special::LParen)) {
            classes.push(
                self.expect_token_pred_string(|token| {
                    matches!(token, TokenKind::QConId | TokenKind::ConId)
                })?
                .1,
            );
            while self.skip_token(TokenKind::Special(Special::Comma)) {
                classes.push(
                    self.expect_token_pred_string(|token| {
                        matches!(token, TokenKind::QConId | TokenKind::ConId)
                    })?
                    .1,
                );
            }
            self.expect_token(TokenKind::Special(Special::RParen))?;
        } else {
            classes.push(
                self.expect_token_pred_string(|token| {
                    matches!(token, TokenKind::QConId | TokenKind::ConId)
                })?
                .1,
            );
        }
        Ok(classes)
    }

    /*
    newtype [context =>] simpletype = newconstr [deriving]
    */
    fn newtype_decl(&mut self) -> ParserResult<ParsedNewtypeDecl> {
        self.skip(); // skip 'newtype'
        let (l, _) = self.last_tok_span();
        let context = self.try_context_arrow().unwrap_or_default();
        let (ty_con, ty_args) = self.simpletype()?;
        self.expect_token(TokenKind::ReservedOp(ReservedOp::Equals))?;
        let con = self.newconstr()?;
        if self.skip_token(TokenKind::ReservedId(ReservedId::Deriving)) {
            self.deriving()?;
        }
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(
            l,
            r,
            NewtypeDecl_ {
                context,
                ty_con,
                ty_args,
                con,
            },
        ))
    }

    /*
    newconstr → con atype
              | con { var :: type }
    */
    fn newconstr(&mut self) -> ParserResult<ParsedCon> {
        let (l, _, _) = self.peek()?;
        let con = self.con()?;
        let field: ParsedFieldDecl = if self.skip_token(TokenKind::Special(Special::LBrace)) {
            let var = self.var()?;
            let (l_, _) = self.last_tok_span();
            self.expect_token(TokenKind::ReservedOp(ReservedOp::ColonColon))?;
            let ty = self.type_()?;
            let (_, r_) = self.last_tok_span();
            self.expect_token(TokenKind::Special(Special::RBrace))?;
            self.spanned(
                l_,
                r_,
                FieldDecl_ {
                    vars: vec![var],
                    ty,
                },
            )
        } else {
            let ty = self.atype()?;
            self.spanned(ty.span.start, ty.span.end, FieldDecl_ { vars: vec![], ty })
        };
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(
            l,
            r,
            Con_ {
                con,
                fields: vec![field],
            },
        ))
    }

    // class [scontext =>] tycls tyvar [where cdecls]
    fn class_decl(&mut self) -> ParserResult<ParsedClassDecl> {
        self.skip(); // skip 'class'
        let (l, _) = self.last_tok_span();
        let context = self.try_scontext_arrow().unwrap_or_default();
        let (_, ty_con, _) = self.expect_token_string(TokenKind::ConId)?;
        let (_, ty_arg, _) = self.expect_token_string(TokenKind::VarId)?;
        let decls = if self.skip_token(TokenKind::ReservedId(ReservedId::Where)) {
            // value decls, but no pattern match
            // TODO: reject patterns
            self.value_decls()?
        } else {
            vec![]
        };
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(
            l,
            r,
            ClassDecl_ {
                context,
                ty_con,
                ty_arg,
                decls,
            },
        ))
    }

    fn try_scontext_arrow(&mut self) -> Option<Vec<ParsedType>> {
        self.try_(|self_| {
            let context = self_.scontext()?;
            self_.expect_token(TokenKind::ReservedOp(ReservedOp::FatArrow))?;
            Ok(context)
        })
        .ok()
    }

    /*
    scontext → simpleclass
             | ( simpleclass1 , … , simpleclassn )         (n ≥ 0)
    */
    fn scontext(&mut self) -> ParserResult<Vec<ParsedType>> {
        self.context_parser(Self::simpleclass)
    }

    // simpleclass → qtycls tyvar
    fn simpleclass(&mut self) -> ParserResult<ParsedType> {
        match self.next()? {
            (l, TokenKind::QConId | TokenKind::ConId, r) => {
                let con_str = self.string(l, r);
                let con = self.spanned(l, r, Type_::Con(self.spanned(l, r, TyCon_::Id(con_str))));
                let (l_, arg, r_) = self.expect_token_string(TokenKind::VarId)?;
                let arg: ParsedType = self.spanned(l_, r_, Type_::Var(arg));
                Ok(self.spanned(l, r_, Type_::App(Box::new(con), vec![arg])))
            }
            (l, _, _) => self.fail(l, ErrorKind::UnexpectedToken),
        }
    }

    /*
    instance [scontext =>] qtycls inst [where idecls]

    idecl → (funlhs | var) rhs
          |                                     (empty)
    */
    fn instance_decl(&mut self) -> ParserResult<ParsedInstanceDecl> {
        self.skip(); // skip 'instance'
        let (l, _) = self.last_tok_span();
        let context = self.try_scontext_arrow().unwrap_or_default();
        let ty_con = self
            .expect_token_pred_string(|token| {
                matches!(token, TokenKind::ConId | TokenKind::QConId)
            })?
            .1;
        let ty = self.inst()?;
        let decls = if self.skip_token(TokenKind::ReservedId(ReservedId::Where)) {
            // TODO: reject patterns
            self.value_decls()?
        } else {
            vec![]
        };
        let (_, r) = self.last_tok_span();
        Ok(self.spanned(
            l,
            r,
            InstanceDecl_ {
                context,
                ty_con,
                ty,
                decls,
            },
        ))
    }

    /*
    inst → gtycon
         | ( gtycon tyvar1 … tyvark )           (k ≥ 0, tyvars distinct)
         | ( tyvar1 , … , tyvark )              (k ≥ 2, tyvars distinct)
         | [ tyvar ]
         | ( tyvar1 -> tyvar2 )                 tyvar1 and tyvar2 distinct

    gtycon → qtycon
           | ()                                (unit type)
           | []                                (list constructor)
           | (->)                              (function constructor)
           | (,+)                              (tupling constructors)

    qtycon = qconid

    tyvar = varid

    This generates a subset of `atype`, so we use `atype` and check.
    */
    fn inst(&mut self) -> ParserResult<ParsedType> {
        // TODO: reject invalid patterns
        self.atype()
    }

    // default (type1 , … , typen)              (n ≥ 0)
    fn default_decl(&mut self) -> ParserResult<ParsedDefaultDecl> {
        self.skip(); // skip 'default'
        let (l, _) = self.last_tok_span();
        self.expect_token(TokenKind::Special(Special::LParen))?;
        let mut tys = vec![self.type_()?];
        while self.skip_token(TokenKind::Special(Special::Comma)) {
            tys.push(self.type_()?);
        }
        let (_, r) = self.expect_token(TokenKind::Special(Special::RParen))?;
        Ok(self.spanned(l, r, DefaultDecl_ { tys }))
    }
}
