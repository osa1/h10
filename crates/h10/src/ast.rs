use crate::collections::Set;
use crate::decl_arena::DeclIdx;
use crate::id::Id;
use crate::pos::Pos;
use crate::token::TokenRef;
use h10_lexer::Literal;

use std::fmt;

#[derive(Debug, Clone)]
pub struct AstNode<T> {
    pub span: Span,
    pub node: T,
}

impl<T> AstNode<T> {
    pub fn new(span: Span, node: T) -> Self {
        Self { span, node }
    }
}

/// A span in a file.
#[derive(Debug, Clone)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.start.line + 1,
            self.start.char + 1,
            self.end.line + 1,
            self.end.char
        )
    }
}

pub struct TopDecl {
    // NB. We don't need the span (`AstNode`) here as we have access to the tokens.
    pub kind: TopDeclKind,

    /// Line number of the `first_token` in the source file.
    ///
    /// Line numbers of tokens attached to this declaration will be relative to this number.
    pub line_number: u32,

    /// The first token where this top-level declaration starts. This token will always be at
    /// column 0.
    pub first_token: TokenRef,

    /// The last token (inclusive) where this top-level declaration ends.
    ///
    /// This will almost always be a whitespace token as there needs to be at least one new line
    /// between top-level declarations, and files usually end with a new line.
    pub last_token: TokenRef,

    /// Next declaration in the declaration's module.
    pub next: Option<DeclIdx>,

    /// Previous declaration in the declaration's module.
    pub prev: Option<DeclIdx>,

    /// Whether this declaration was modified after the last time it was parsed.
    pub modified: bool,
}

impl fmt::Debug for TopDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Skip tokens as they form a double linked list.
        <TopDeclKind as fmt::Debug>::fmt(&self.kind, f)
    }
}

impl TopDecl {
    pub fn span_start(&self) -> Pos {
        let token_pos = self.first_token.span().start;
        Pos {
            line: token_pos.line + self.line_number,
            char: token_pos.char,
        }
    }

    pub fn span_end(&self) -> Pos {
        let token_pos = self.last_token.span().end;
        Pos {
            line: token_pos.line + self.line_number,
            char: token_pos.char,
        }
    }

    pub fn contains_location(&self, pos: Pos) -> bool {
        pos >= self.span_start() && pos < self.span_end()
    }

    pub fn iter_tokens(&self) -> impl Iterator<Item = TokenRef> {
        self.first_token.iter_until(&self.last_token)
    }
}

#[cfg(test)]
impl TopDecl {
    pub fn class(&self) -> &ClassDecl {
        self.kind.class()
    }

    pub fn instance(&self) -> &InstanceDecl {
        self.kind.instance()
    }

    pub fn value(&self) -> &ValueDecl {
        self.kind.value()
    }

    pub fn data(&self) -> &DataDecl {
        self.kind.data()
    }

    pub fn kind_sig(&self) -> &KindSigDecl {
        self.kind.kind_sig()
    }

    pub fn type_syn(&self) -> &TypeDecl {
        self.kind.type_syn()
    }
}

#[cfg(test)]
impl TopDeclKind {
    pub fn class(&self) -> &ClassDecl {
        match self {
            TopDeclKind::Class(class_decl) => class_decl,
            other => panic!("Not class TopDecl: {:?}", other),
        }
    }

    pub fn instance(&self) -> &InstanceDecl {
        match self {
            TopDeclKind::Instance(instance_decl) => instance_decl,
            other => panic!("Not instance TopDecl: {:?}", other),
        }
    }

    pub fn value(&self) -> &ValueDecl {
        match self {
            TopDeclKind::Value(value_decl) => value_decl,
            other => panic!("Not value TopDecl: {:?}", other),
        }
    }

    pub fn data(&self) -> &DataDecl {
        match self {
            TopDeclKind::Data(data_decl) => data_decl,
            other => panic!("Not type TopDecl: {:?}", other),
        }
    }

    pub fn kind_sig(&self) -> &KindSigDecl {
        match self {
            TopDeclKind::KindSig(kind_sig) => kind_sig,
            other => panic!("Not kind signature: {:?}", other),
        }
    }

    pub fn type_syn(&self) -> &TypeDecl {
        match self {
            TopDeclKind::Type(type_syn) => type_syn,
            other => panic!("Not type synonym: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TopDeclKind {
    Value(ValueDecl),
    Type(TypeDecl),
    KindSig(KindSigDecl),
    Data(DataDecl),
    Newtype(NewtypeDecl),
    Class(ClassDecl),
    Instance(InstanceDecl),
    Default(DefaultDecl),
    Unparsed,
}

pub type ValueDecl = AstNode<ValueDecl_>;

#[cfg(test)]
impl ValueDecl {
    // TODO: Add a type for type sig
    #[allow(clippy::type_complexity)]
    pub fn type_sig(&self) -> (&[Id], &[TypeBinder], &[Type], &Type) {
        match &self.node {
            ValueDecl_::TypeSig {
                vars,
                foralls,
                context,
                ty,
            } => (vars, foralls, context, ty),
            other => panic!("Not type sig: {:?}", other),
        }
    }

    // TODO: Add a type for value
    pub fn value(&self) -> (&Lhs, &Rhs) {
        match &self.node {
            ValueDecl_::Value { lhs, rhs } => (lhs, rhs),
            other => panic!("Not value: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueDecl_ {
    /// In `x, y, z :: forall a . Show a => a -> String`:
    ///
    /// - vars    = `[x, y, z]`
    /// - foralls = `[a]`
    /// - context = `[Show a]`
    /// - ty      = `a -> String`
    TypeSig {
        vars: Vec<Id>,
        foralls: Vec<TypeBinder>,
        context: Vec<Type>,
        ty: Type,
    },

    /// E.g. `infixr 3 &&`.
    Fixity {
        fixity: Fixity,
        prec: Option<u32>,
        ops: Vec<Op>,
    },

    /// `<lhs> = <rhs>`.
    Value { lhs: Lhs, rhs: Rhs },
}

pub type Type = AstNode<Type_>;

#[cfg(test)]
impl Type {
    pub fn arrow(&self) -> (&Type, &Type) {
        match &self.node {
            Type_::Arrow(ty1, ty2) => (ty1, ty2),
            _ => panic!("Not arrow: {:?}", self),
        }
    }

    pub fn var(&self) -> &Id {
        match &self.node {
            Type_::Var(id) => id,
            _ => panic!("Not id: {:?}", self),
        }
    }

    pub fn app(&self) -> (&Type, &[Type]) {
        match &self.node {
            Type_::App(con, args) => (con, args),
            _ => panic!("Not app: {:?}", self),
        }
    }

    pub fn con(&self) -> &TyCon {
        match &self.node {
            Type_::Con(con) => con,
            _ => panic!("Not app: {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type_ {
    /// A tuple type with at least two elements: `(T1, T2, ...)`.
    Tuple(Vec<Type>),

    /// A list type: `[T]`.
    List(Box<Type>),

    /// An arrow type: `T1 -> T2`.
    Arrow(Box<Type>, Box<Type>),

    /// A type application: `T1 T2`.
    App(Box<Type>, Vec<Type>),

    /// A type constructor: `IO`, `[]`, `(->)`.
    Con(TyCon),

    /// A type variable: `a`, `b`.
    Var(Id),
}

impl Type_ {
    pub fn vars(&self) -> Set<Id> {
        let mut vars: Set<Id> = Default::default();
        self.vars_(&mut vars);
        vars
    }

    pub fn vars_(&self, vars: &mut Set<Id>) {
        match self {
            Type_::Tuple(tys) => {
                for ty in tys {
                    ty.node.vars_(vars);
                }
            }

            Type_::List(ty) => {
                ty.node.vars_(vars);
            }

            Type_::Arrow(ty1, ty2) => {
                ty1.node.vars_(vars);
                ty2.node.vars_(vars);
            }

            Type_::App(ty1, tys) => {
                ty1.node.vars_(vars);
                for ty in tys {
                    ty.node.vars_(vars);
                }
            }

            Type_::Con(_) => {}

            Type_::Var(id) => {
                vars.insert(id.clone());
            }
        }
    }
}

pub type TyCon = AstNode<TyCon_>;

#[cfg(test)]
impl TyCon {
    pub fn id(&self) -> &Id {
        match &self.node {
            TyCon_::Id(id) => id,
            other => panic!("Not id: {:?}", other),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyCon_ {
    /// An identifier: `IO`, `Maybe`.
    Id(Id),

    /// A tuple type constructor: `(,)`, `(,,)`. Arity will be at least 2.
    Tuple(u32),

    /// Arrow type constructor: `(->)`.
    Arrow,

    /// List type constructor: `[]`.
    List,
}

pub type Lhs = AstNode<Lhs_>;

#[cfg(test)]
impl Lhs {
    pub fn fun(&self) -> (&Id, &[Pat]) {
        match &self.node {
            Lhs_::Fun { var, pats } => (var, &pats),
            other => panic!("Not fun: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Lhs_ {
    Pat(Pat),
    Fun { var: Id, pats: Vec<Pat> },
}

pub type Rhs = AstNode<Rhs_>;

#[cfg(test)]
impl Rhs {
    pub fn rhs(&self) -> (&Exp, &[ValueDecl]) {
        match &self.node {
            Rhs_::Rhs { rhs, where_decls } => (rhs, &where_decls),
            other => panic!("Not rhs: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Rhs_ {
    GuardedRhs {
        rhss: Vec<GuardedRhs>,
        where_decls: Vec<ValueDecl>,
    },
    Rhs {
        rhs: Exp,
        where_decls: Vec<ValueDecl>,
    },
}

pub type GuardedRhs = AstNode<GuardedRhs_>;

#[derive(Debug, Clone)]
pub struct GuardedRhs_ {
    pub guards: Vec<Stmt>,
    pub rhs: Exp,
}

pub type Exp = AstNode<Exp_>;

#[cfg(test)]
impl Exp {
    pub fn var(&self) -> &Id {
        match &self.node {
            Exp_::Var(var) => var,
            other => panic!("Not var: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Exp_ {
    /// A term variable.
    Var(Id),

    /// A constructor.
    Con(Id),

    /// A literal.
    Lit(Literal),

    /// A lambda: `\x -> ...`.
    Lam(Vec<Pat>, Box<Exp>),

    /// An application: `f x y z`.
    App(Box<Exp>, Vec<Exp>),

    /// A tuple: `(a, b, ...)`.
    Tuple(Vec<Exp>),

    /// A list expression: `[a, b, ...]`.
    List(Vec<Exp>),

    /// A do expression: `do { ... }`.
    Do(Vec<Stmt>),

    /// A type annotation: `x :: Show a => a`.
    TypeAnnotation {
        exp: Box<Exp>,
        context: Vec<Type>,
        type_: Type,
    },

    /// An arithmetic sequence: `[1, 3 .. 10]`.
    ArithmeticSeq {
        exp1: Box<Exp>,
        exp2: Option<Box<Exp>>,
        exp3: Option<Box<Exp>>,
    },

    /// A list comprehension: `[x | x <- [0 .. 10], f x]`.
    ListComp { exp: Box<Exp>, quals: Vec<Stmt> },

    /// A pattern match: `case x of { ... }`.
    Case(Box<Exp>, Vec<Alt>),

    /// An if-then-else: `if x then y else z`.
    If(Box<Exp>, Box<Exp>, Box<Exp>),

    /// A let binding: `let { x = 1; y = 2 } in x + y`.
    Let(Vec<ValueDecl>, Box<Exp>),

    /// A record update: `x { a = 1, y = 2 }`.
    Update {
        exp: Box<Exp>,
        updates: Vec<(Id, Exp)>,
    },

    /// An expression tree that needs to be re-associated. This node is used in binary operator
    /// applications in parsing and should be replaced with `App` nodes based on associativity of
    /// the operators after parsing fixity declarations.
    // TODO: We don't use this yet, but we should.
    #[allow(unused)]
    ReAssoc(Box<Exp>, Id, Box<Exp>),
}

pub type Stmt = AstNode<Stmt_>;

#[derive(Debug, Clone)]
pub enum Stmt_ {
    Exp(Exp),
    Bind(Pat, Exp),
    Let(Vec<ValueDecl>),
}

pub type Alt = AstNode<Alt_>;

#[derive(Debug, Clone)]
pub struct Alt_ {
    pub pat: Pat,
    pub guarded_rhss: Vec<(Vec<Stmt>, Exp)>,
    pub where_decls: Vec<ValueDecl>,
}

pub type Pat = AstNode<Pat_>;

impl Pat {
    /// If the pattern is just a variable, get the variable.
    pub(crate) fn simple_pat_var(&self) -> Option<&Id> {
        match &self.node {
            Pat_::Var(var) => Some(var),
            _ => None,
        }
    }
}

impl Pat {
    pub(crate) fn vars(&self) -> Set<Id> {
        self.node.vars()
    }
}

#[derive(Debug, Clone)]
pub enum Pat_ {
    Var(Id),
    As(Id, Box<Pat>),
    Lit(Literal),
    Wildcard,
    Tuple(Vec<Pat>),
    List(Vec<Pat>),
    Irrefutable(Box<Pat>),
    Con(GCon, Vec<Pat>),
}

pub type GCon = AstNode<GCon_>;

#[derive(Debug, Clone)]
pub enum GCon_ {
    Tuple(u32),
    EmptyList,
    QCon(Id),
}

impl Pat_ {
    pub fn vars(&self) -> Set<Id> {
        let mut vars: Set<Id> = Default::default();
        self.vars_(&mut vars);
        vars
    }

    pub fn vars_(&self, vars: &mut Set<Id>) {
        match self {
            Pat_::Var(id) => {
                vars.insert(id.clone());
            }

            Pat_::As(var, pat) => {
                vars.insert(var.clone());
                pat.node.vars_(vars);
            }

            Pat_::Irrefutable(pat) => {
                pat.node.vars_(vars);
            }

            Pat_::Tuple(pats) | Pat_::List(pats) | Pat_::Con(_, pats) => {
                pats.iter().for_each(|pat| pat.node.vars_(vars))
            }

            Pat_::Wildcard | Pat_::Lit(_) => {}
        }
    }
}

impl Rhs_ {
    pub fn where_decls(&self) -> &[ValueDecl] {
        match self {
            Rhs_::GuardedRhs { where_decls, .. } => where_decls,
            Rhs_::Rhs { where_decls, .. } => where_decls,
        }
    }
}

pub type ImportDecl = AstNode<ImportDecl_>;

#[derive(Debug, Clone)]
pub struct ImportDecl_ {
    pub modid: String,
    pub qualified: bool,
    pub as_: Option<String>,
    pub hiding: bool,
    pub imports: Vec<Import>,
}

pub type Import = AstNode<Import_>;

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Import_ {
    Var(Id),
    Type(TypeImport),
}

pub type TypeImport = AstNode<TypeImport_>;

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum TypeImport_ {
    /// Import just the type: `import X (T)`.
    JustType,

    /// Import the type + all constructors (or methods): `import X (T (..))`.
    All,

    /// Import the type + given constructors (or methods): `import X (T (a))`.
    ///
    /// Note that parser will accept import a type with methods, that needs to be checked after
    /// parsing.
    Things(Vec<Id>),
}

pub type TypeDecl = AstNode<TypeDecl_>;

#[derive(Debug, Clone)]
pub struct TypeDecl_ {
    pub ty: Id,
    pub vars: Vec<Id>,
    pub rhs: Type,
}

pub type TypeBinder = AstNode<TypeBinder_>;

#[derive(Debug, Clone)]
pub struct TypeBinder_ {
    pub id: Id,
    pub ty: Option<Type>,
}

pub type KindSigDecl = AstNode<KindSigDecl_>;

#[derive(Debug, Clone)]
pub struct KindSigDecl_ {
    pub ty: Id,
    pub foralls: Vec<TypeBinder>,
    pub sig: Type,
}

pub type DataDecl = AstNode<DataDecl_>;

#[derive(Debug, Clone)]
pub struct DataDecl_ {
    pub context: Vec<Type>,
    pub ty_con: Id,
    pub ty_args: Vec<Id>,
    pub cons: Vec<Con>,
    pub deriving: Vec<Id>,
}

pub type NewtypeDecl = AstNode<NewtypeDecl_>;

#[derive(Debug, Clone)]
pub struct NewtypeDecl_ {
    pub context: Vec<Type>,
    pub ty_con: Id,
    pub ty_args: Vec<Id>,
    pub con: Con,
}

pub type ClassDecl = AstNode<ClassDecl_>;

#[derive(Debug, Clone)]
pub struct ClassDecl_ {
    pub context: Vec<Type>,
    pub ty_con: Id,
    pub ty_arg: Id,
    pub decls: Vec<ValueDecl>,
}

pub type InstanceDecl = AstNode<InstanceDecl_>;

#[derive(Debug, Clone)]
pub struct InstanceDecl_ {
    pub context: Vec<Type>,
    pub ty_con: Id,
    pub ty: Type,
    pub decls: Vec<ValueDecl>,
}

pub type DefaultDecl = AstNode<DefaultDecl_>;

#[derive(Debug, Clone)]
pub struct DefaultDecl_ {
    pub tys: Vec<Type>,
}

pub type FieldDecl = AstNode<FieldDecl_>;

#[derive(Debug, Clone)]
pub struct FieldDecl_ {
    // NB. Empty in non-record constructors
    pub vars: Vec<Id>,
    pub ty: Type,
}

pub type Con = AstNode<Con_>;

#[derive(Debug, Clone)]
pub struct Con_ {
    pub con: Id,
    pub fields: Vec<FieldDecl>,
}

pub type Op = AstNode<Op_>;

impl Op {
    pub fn id(&self) -> &Id {
        match &self.node {
            Op_::Con(id) => id,
            Op_::Var(id) => id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Op_ {
    Con(Id),
    Var(Id),
}

#[derive(Debug, Clone, Copy)]
pub enum Fixity {
    InfixL,
    InfixR,
    Infix,
}
