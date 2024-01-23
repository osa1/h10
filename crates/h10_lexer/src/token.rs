use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: SmolStr,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Whitespace,

    Comment {
        documentation: bool,
    },

    /// A variable identifier, type (`tyvar`) or term (`varid`).
    VarId,

    /// A constructor identifier.
    ConId,

    /// A variable symbol.
    VarSym,

    /// A constructor symbol.
    ConSym,

    /// A qualified variable.
    QVarId,

    /// A qualified constructor. Also used as qualified type constructor `qtycon` and type class
    /// `qtycls`.
    QConId,

    /// A qualified variable symbol.
    QVarSym,

    /// A qualifieid constructor symbol.
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
