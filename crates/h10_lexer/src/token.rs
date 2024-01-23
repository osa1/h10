#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Whitespace,

    Comment {
        documentation: bool,
    },

    /// Variable identifier, type (`tyvar`) or term (`varid`).
    VarId,

    /// Constructor identifier
    ConId,

    /// Variable symbol
    VarSym,

    /// Constructor symbol
    ConSym,

    /// Qualified variable
    QVarId,

    /// Qualified constructor. Also used as qualified type constructor `qtycon` and type class
    /// `qtycls`.
    QConId,

    /// Qualified variable symbol
    QVarSym,

    /// Qualifieid constructor symbol
    QConSym,

    Literal(Literal),

    Special(Special),

    ReservedOp(ReservedOp),

    ReservedId(ReservedId),
}

const _: () = assert!(std::mem::size_of::<TokenKind>() == 2);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Literal {
    Int,
    Float,
    Char,
    String,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Special {
    LParen,
    RParen,
    Comma,
    Semi,
    LBracket,
    RBracket,
    Backtick,
    LBrace,
    RBrace,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ReservedOp {
    /// `..`
    DotDot,

    /// `:`
    Colon,

    /// `::`
    ColonColon,

    /// `=`
    Equals,

    /// `\\`
    Backslash,

    /// `|`
    Pipe,

    /// `<-`
    LeftArrow,

    /// `->`
    RightArrow,

    /// `@`
    At,

    /// `~`
    Tilde,

    /// `=>`
    FatArrow,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ReservedId {
    Case,
    Class,
    Data,
    Default,
    Deriving,
    Do,
    Else,
    Forall,
    Foreign,
    If,
    Import,
    In,
    Infix,
    Infixl,
    Infixr,
    Instance,
    Kind,
    Let,
    Module,
    Newtype,
    Of,
    Then,
    Type,
    Underscore,
    Where,
}
