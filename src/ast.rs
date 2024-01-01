use crate::collections::Set;
use crate::id::Id;
use crate::token::Literal;

use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

use lexgen_util::Loc;

pub type ParsedAlt = Alt<String>;
pub type ParsedClassDecl = ClassDecl<String>;
pub type ParsedCon = Con<String>;
pub type ParsedDataDecl = DataDecl<String>;
pub type ParsedDecl = Decl<String>;
pub type ParsedDefaultDecl = DefaultDecl<String>;
pub type ParsedExp = Exp<String>;
pub type ParsedFieldDecl = FieldDecl<String>;
pub type ParsedGCon = GCon<String>;
pub type ParsedGuardedRhs = GuardedRhs<String>;
pub type ParsedImportDecl = ImportDecl<String>;
pub type ParsedInstanceDecl = InstanceDecl<String>;
pub type ParsedLhs = Lhs<String>;
pub type ParsedNewtypeDecl = NewtypeDecl<String>;
pub type ParsedOp = Op<String>;
pub type ParsedPat = Pat<String>;
pub type ParsedRhs = Rhs<String>;
pub type ParsedStmt = Stmt<String>;
pub type ParsedTyCon = TyCon<String>;
pub type ParsedType = Type<String>;
pub type ParsedTypeDecl = TypeDecl<String>;
#[allow(unused)]
pub type ParsedKindSig = KindSigDecl<String>;
#[allow(unused)]
pub type ParsedTypeImport = TypeImport<String>;
pub type ParsedValueDecl = ValueDecl<String>;

pub type RenamedAlt = Alt<Id>;
pub type RenamedClassDecl = ClassDecl<Id>;
pub type RenamedCon = Con<Id>;
pub type RenamedDataDecl = DataDecl<Id>;
pub type RenamedDecl = Decl<Id>;
pub type RenamedDefaultDecl = DefaultDecl<Id>;
pub type RenamedExp = Exp<Id>;
pub type RenamedFieldDecl = FieldDecl<Id>;
pub type RenamedGCon = GCon<Id>;
pub type RenamedGuardedRhs = GuardedRhs<Id>;
#[allow(unused)]
pub type RenamedImportDecl = ImportDecl<Id>;
pub type RenamedInstanceDecl = InstanceDecl<Id>;
pub type RenamedLhs = Lhs<Id>;
pub type RenamedNewtypeDecl = NewtypeDecl<Id>;
pub type RenamedOp = Op<Id>;
pub type RenamedPat = Pat<Id>;
pub type RenamedRhs = Rhs<Id>;
pub type RenamedStmt = Stmt<Id>;
pub type RenamedTyCon = TyCon<Id>;
pub type RenamedType = Type<Id>;
pub type RenamedTypeDecl = TypeDecl<Id>;
#[allow(unused)]
pub type RenamedTypeImport = TypeImport<Id>;
pub type RenamedValueDecl = ValueDecl<Id>;

#[derive(Debug, Clone)]
pub struct AstNode<T> {
    pub span: Span,
    pub node: T,
}

impl<T> AstNode<T> {
    pub fn new(span: Span, node: T) -> Self {
        Self { span, node }
    }

    pub fn map<T2, F: FnOnce(&T) -> T2>(&self, f: F) -> AstNode<T2> {
        AstNode {
            span: self.span.clone(),
            node: f(&self.node),
        }
    }

    pub fn map_<T2, F: FnOnce(T) -> T2>(self, f: F) -> AstNode<T2> {
        AstNode {
            span: self.span,
            node: f(self.node),
        }
    }

    pub fn with_node<T2>(&self, node: T2) -> AstNode<T2> {
        AstNode {
            span: self.span.clone(),
            node,
        }
    }
}

/// A span in a file.
#[derive(Debug, Clone)]
pub struct Span {
    /// An identifier for the source. This can be a URI, relative file path, a placeholder like
    /// "REPL" etc.
    pub source: Rc<str>,

    pub start: Loc,

    pub end: Loc,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.start.line + 1,
            self.start.col + 1,
            self.end.line + 1,
            self.end.col
        )
    }
}

pub type Decl<Id> = AstNode<Decl_<Id>>;

#[cfg(test)]
impl<Id: fmt::Debug> Decl<Id> {
    pub fn class(&self) -> &ClassDecl<Id> {
        match &self.node {
            Decl_::Class(class_decl) => class_decl,
            other => panic!("Not class decl: {:?}", other),
        }
    }

    pub fn instance(&self) -> &InstanceDecl<Id> {
        match &self.node {
            Decl_::Instance(instance_decl) => instance_decl,
            other => panic!("Not instance decl: {:?}", other),
        }
    }

    pub fn value(&self) -> &ValueDecl<Id> {
        match &self.node {
            Decl_::Value(value_decl) => value_decl,
            other => panic!("Not value decl: {:?}", other),
        }
    }

    pub fn data(&self) -> &DataDecl<Id> {
        match &self.node {
            Decl_::Data(data_decl) => data_decl,
            other => panic!("Not type decl: {:?}", other),
        }
    }

    pub fn kind_sig(&self) -> &KindSigDecl<Id> {
        match &self.node {
            Decl_::KindSig(kind_sig) => kind_sig,
            other => panic!("Not kind signature: {:?}", other),
        }
    }

    pub fn type_syn(&self) -> &TypeDecl<Id> {
        match &self.node {
            Decl_::Type(type_syn) => type_syn,
            other => panic!("Not type synonym: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Decl_<Id> {
    Value(ValueDecl<Id>),
    Type(TypeDecl<Id>),
    KindSig(KindSigDecl<Id>),
    Data(DataDecl<Id>),
    Newtype(NewtypeDecl<Id>),
    Class(ClassDecl<Id>),
    Instance(InstanceDecl<Id>),
    Default(DefaultDecl<Id>),
}

pub type ValueDecl<Id> = AstNode<ValueDecl_<Id>>;

#[cfg(test)]
impl<Id: fmt::Debug> ValueDecl<Id> {
    // TODO: Add a type for type sig
    pub fn type_sig(&self) -> (&[Id], &[Id], &[Type<Id>], &Type<Id>) {
        match &self.node {
            ValueDecl_::TypeSig {
                vars,
                foralls,
                context,
                ty,
            } => (&*vars, &*foralls, &*context, ty),
            other => panic!("Not type sig: {:?}", other),
        }
    }

    // TODO: Add a type for value
    pub fn value(&self) -> (&Lhs<Id>, &Rhs<Id>) {
        match &self.node {
            ValueDecl_::Value { lhs, rhs } => (lhs, rhs),
            other => panic!("Not value: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueDecl_<Id> {
    /// In `x, y, z :: forall a . Show a => a -> String`:
    ///
    /// - vars    = `[x, y, z]`
    /// - foralls = `[a]`
    /// - context = `[Show a]`
    /// - ty      = `a -> String`
    TypeSig {
        vars: Vec<Id>,
        foralls: Vec<Id>,
        context: Vec<Type<Id>>,
        ty: Type<Id>,
    },

    /// E.g. `infixr 3 &&`.
    Fixity {
        fixity: Fixity,
        prec: Option<u32>,
        ops: Vec<Op<Id>>,
    },

    /// `<lhs> = <rhs>`.
    Value { lhs: Lhs<Id>, rhs: Rhs<Id> },
}

pub type Type<Id> = AstNode<Type_<Id>>;

#[cfg(test)]
impl<Id: fmt::Debug> Type<Id> {
    pub fn arrow(&self) -> (&Type<Id>, &Type<Id>) {
        match &self.node {
            Type_::Arrow(ty1, ty2) => (&*ty1, &*ty2),
            _ => panic!("Not arrow: {:?}", self),
        }
    }

    pub fn var(&self) -> &Id {
        match &self.node {
            Type_::Var(id) => id,
            _ => panic!("Not id: {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type_<Id> {
    /// A tuple type with at least two elements: `(T1, T2, ...)`.
    Tuple(Vec<Type<Id>>),

    /// A list type: `[T]`.
    List(Box<Type<Id>>),

    /// An arrow type: `T1 -> T2`.
    Arrow(Box<Type<Id>>, Box<Type<Id>>),

    /// A type application: `T1 T2`.
    App(Box<Type<Id>>, Vec<Type<Id>>),

    /// A type constructor: `IO`, `[]`, `(->)`.
    Con(TyCon<Id>),

    /// A type variable: `a`, `b`.
    Var(Id),
}

impl<Id: Hash + Eq + Clone> Type_<Id> {
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

pub type TyCon<Id> = AstNode<TyCon_<Id>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyCon_<Id> {
    /// An identifier: `IO`, `Maybe`.
    Id(Id),

    /// A tuple type constructor: `(,)`, `(,,)`. Arity will be at least 2.
    Tuple(u32),

    /// Arrow type constructor: `(->)`.
    Arrow,

    /// List type constructor: `[]`.
    List,
}

pub type Lhs<Id> = AstNode<Lhs_<Id>>;

#[cfg(test)]
impl<Id: fmt::Debug> Lhs<Id> {
    pub fn fun(&self) -> (&Id, &[Pat<Id>]) {
        match &self.node {
            Lhs_::Fun { var, pats } => (var, &pats),
            other => panic!("Not fun: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Lhs_<Id> {
    Pat(Pat<Id>),
    Fun { var: Id, pats: Vec<Pat<Id>> },
}

pub type Rhs<Id> = AstNode<Rhs_<Id>>;

#[cfg(test)]
impl<Id: fmt::Debug> Rhs<Id> {
    pub fn rhs(&self) -> (&Exp<Id>, &[ValueDecl<Id>]) {
        match &self.node {
            Rhs_::Rhs { rhs, where_decls } => (rhs, &where_decls),
            other => panic!("Not rhs: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Rhs_<Id> {
    GuardedRhs {
        rhss: Vec<GuardedRhs<Id>>,
        where_decls: Vec<ValueDecl<Id>>,
    },
    Rhs {
        rhs: Exp<Id>,
        where_decls: Vec<ValueDecl<Id>>,
    },
}

pub type GuardedRhs<Id> = AstNode<GuardedRhs_<Id>>;

#[derive(Debug, Clone)]
pub struct GuardedRhs_<Id> {
    pub guards: Vec<Stmt<Id>>,
    pub rhs: Exp<Id>,
}

pub type Exp<Id> = AstNode<Exp_<Id>>;

#[cfg(test)]
impl<Id: fmt::Debug> Exp<Id> {
    pub fn var(&self) -> &Id {
        match &self.node {
            Exp_::Var(var) => var,
            other => panic!("Not var: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Exp_<Id> {
    /// A term variable.
    Var(Id),

    /// A constructor.
    Con(Id),

    /// A literal.
    Lit(Literal),

    /// A lambda: `\x -> ...`.
    Lam(Vec<Pat<Id>>, Box<Exp<Id>>),

    /// An application: `f x y z`.
    App(Box<Exp<Id>>, Vec<Exp<Id>>),

    /// A tuple: `(a, b, ...)`.
    Tuple(Vec<Exp<Id>>),

    /// A list expression: `[a, b, ...]`.
    List(Vec<Exp<Id>>),

    /// A do expression: `do { ... }`.
    Do(Vec<Stmt<Id>>),

    /// A type annotation: `x :: Show a => a`.
    TypeAnnotation {
        exp: Box<Exp<Id>>,
        context: Vec<Type<Id>>,
        type_: Type<Id>,
    },

    /// An arithmetic sequence: `[1, 3 .. 10]`.
    ArithmeticSeq {
        exp1: Box<Exp<Id>>,
        exp2: Option<Box<Exp<Id>>>,
        exp3: Option<Box<Exp<Id>>>,
    },

    /// A list comprehension: `[x | x <- [0 .. 10], f x]`.
    ListComp {
        exp: Box<Exp<Id>>,
        quals: Vec<Stmt<Id>>,
    },

    /// A pattern match: `case x of { ... }`.
    Case(Box<Exp<Id>>, Vec<Alt<Id>>),

    /// An if-then-else: `if x then y else z`.
    If(Box<Exp<Id>>, Box<Exp<Id>>, Box<Exp<Id>>),

    /// A let binding: `let { x = 1; y = 2 } in x + y`.
    Let(Vec<ValueDecl<Id>>, Box<Exp<Id>>),

    /// A record update: `x { a = 1, y = 2 }`.
    Update {
        exp: Box<Exp<Id>>,
        updates: Vec<(Id, Exp<Id>)>,
    },

    /// An expression tree that needs to be re-associated. This node is used in binary operator
    /// applications in parsing and should be replaced with `App` nodes based on associativity of
    /// the operators after parsing fixity declarations.
    // TODO: We don't use this yet, but we should.
    #[allow(unused)]
    ReAssoc(Box<Exp<Id>>, Id, Box<Exp<Id>>),
}

pub type Stmt<Id> = AstNode<Stmt_<Id>>;

#[derive(Debug, Clone)]
pub enum Stmt_<Id> {
    Exp(Exp<Id>),
    Bind(Pat<Id>, Exp<Id>),
    Let(Vec<ValueDecl<Id>>),
}

pub type Alt<Id> = AstNode<Alt_<Id>>;

#[derive(Debug, Clone)]
pub struct Alt_<Id> {
    pub pat: Pat<Id>,
    pub guarded_rhss: Vec<(Vec<Stmt<Id>>, Exp<Id>)>,
    pub where_decls: Vec<ValueDecl<Id>>,
}

pub type Pat<Id> = AstNode<Pat_<Id>>;

impl<Id> Pat<Id> {
    /// If the pattern is just a variable, get the variable.
    pub(crate) fn simple_pat_var(&self) -> Option<&Id> {
        match &self.node {
            Pat_::Var(var) => Some(var),
            _ => None,
        }
    }
}

impl<Id: Hash + Eq + Clone> Pat<Id> {
    pub(crate) fn vars(&self) -> Set<Id> {
        self.node.vars()
    }
}

#[derive(Debug, Clone)]
pub enum Pat_<Id> {
    Var(Id),
    As(Id, Box<Pat<Id>>),
    Lit(Literal),
    Wildcard,
    Tuple(Vec<Pat<Id>>),
    List(Vec<Pat<Id>>),
    Irrefutable(Box<Pat<Id>>),
    Con(GCon<Id>, Vec<Pat<Id>>),
}

pub type GCon<Id> = AstNode<GCon_<Id>>;

#[derive(Debug, Clone)]
pub enum GCon_<Id> {
    Tuple(u32),
    EmptyList,
    QCon(Id),
}

impl<Id: Hash + Eq + Clone> Pat_<Id> {
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

impl<Id> Rhs_<Id> {
    pub fn where_decls(&self) -> &[ValueDecl<Id>] {
        match self {
            Rhs_::GuardedRhs { where_decls, .. } => where_decls,
            Rhs_::Rhs { where_decls, .. } => where_decls,
        }
    }
}

pub type ImportDecl<Id> = AstNode<ImportDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct ImportDecl_<Id> {
    pub modid: String,
    pub qualified: bool,
    pub as_: Option<String>,
    pub hiding: bool,
    pub imports: Vec<Import<Id>>,
}

pub type Import<Id> = AstNode<Import_<Id>>;

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Import_<Id> {
    Var(Id),
    Type(TypeImport<Id>),
}

pub type TypeImport<Id> = AstNode<TypeImport_<Id>>;

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum TypeImport_<Id> {
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

pub type TypeDecl<Id> = AstNode<TypeDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct TypeDecl_<Id> {
    pub ty: Id,
    pub vars: Vec<Id>,
    pub rhs: Type<Id>,
}

pub type KindSigDecl<Id> = AstNode<KindSigDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct KindSigDecl_<Id> {
    pub ty: Id,
    pub sig: Type<Id>,
}

pub type DataDecl<Id> = AstNode<DataDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct DataDecl_<Id> {
    pub context: Vec<Type<Id>>,
    pub ty_con: Id,
    pub ty_args: Vec<Id>,
    pub cons: Vec<Con<Id>>,
    pub deriving: Vec<Id>,
}

pub type NewtypeDecl<Id> = AstNode<NewtypeDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct NewtypeDecl_<Id> {
    pub context: Vec<Type<Id>>,
    pub ty_con: Id,
    pub ty_args: Vec<Id>,
    pub con: Con<Id>,
}

pub type ClassDecl<Id> = AstNode<ClassDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct ClassDecl_<Id> {
    pub context: Vec<Type<Id>>,
    pub ty_con: Id,
    pub ty_arg: Id,
    pub decls: Vec<ValueDecl<Id>>,
}

pub type InstanceDecl<Id> = AstNode<InstanceDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct InstanceDecl_<Id> {
    pub context: Vec<Type<Id>>,
    pub ty_con: Id,
    pub ty: Type<Id>,
    pub decls: Vec<ValueDecl<Id>>,
}

pub type DefaultDecl<Id> = AstNode<DefaultDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct DefaultDecl_<Id> {
    pub tys: Vec<Type<Id>>,
}

pub type FieldDecl<Id> = AstNode<FieldDecl_<Id>>;

#[derive(Debug, Clone)]
pub struct FieldDecl_<Id> {
    // NB. Empty in non-record constructors
    pub vars: Vec<Id>,
    pub ty: Type<Id>,
}

pub type Con<Id> = AstNode<Con_<Id>>;

#[derive(Debug, Clone)]
pub struct Con_<Id> {
    pub con: Id,
    pub fields: Vec<FieldDecl<Id>>,
}

pub type Op<Id> = AstNode<Op_<Id>>;

#[derive(Debug, Clone)]
pub enum Op_<Id> {
    Con(Id),
    Var(Id),
}

#[derive(Debug, Clone, Copy)]
pub enum Fixity {
    InfixL,
    InfixR,
    Infix,
}
