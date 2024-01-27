use aunty::StrongObj;

use crate::util::{span::Span, symbol::Symbol};

use super::{
    parser::AstKeyword,
    token::{TokenCharLit, TokenIdent, TokenNumberLit, TokenStringLit},
};

// === Paths === //

#[derive(Debug, Clone)]
pub struct AstMultiPath {
    /// The base path of the multi-import.
    pub base: AstPath,

    /// The parts to be imported. The prefix of each base path should be `self::`.
    pub imports: AstMultiPathList,
}

impl AstMultiPath {
    pub fn is_empty(&self) -> bool {
        self.base.is_empty() && matches!(&self.imports, AstMultiPathList::List(l) if l.is_empty())
    }
}

#[derive(Debug, Clone)]
pub enum AstMultiPathList {
    List(Box<[AstMultiPath]>),
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct AstPath {
    /// What the path is relative to.
    pub prefix: AstPathPrefix,

    /// The full path.
    pub parts: Box<[AstPathPart]>,
}

impl AstPath {
    pub fn new_local(name: TokenIdent) -> Self {
        Self {
            prefix: AstPathPrefix::Self_,
            parts: Box::from_iter([AstPathPart(name)]),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.prefix == AstPathPrefix::Implicit && self.parts.is_empty()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstPathPrefix {
    /// Relative to the root of the currently compiled crate.
    Crate,

    /// A fully qualified crate name.
    Crates,

    /// Implicitly relative to the current module.
    Implicit,

    /// Relative to the current module.
    Self_,
}

#[derive(Debug, Copy, Clone)]
pub struct AstPathPart(pub TokenIdent);

impl AstPathPart {
    pub fn is_super(&self) -> bool {
        self.0.text == AstKeyword::Super.to_sym()
    }
}

// === Types === //

#[derive(Debug)]
pub struct AstType {
    pub kind: AstTypeKind,
    pub generics: Box<[AstType]>,
}

#[derive(Debug)]
pub enum AstTypeKind {
    Tuple,
    Option,
    Adt(AstPath),
}

// === Expressions === //

#[derive(Debug, Clone)]
pub enum AstExpr {
    Placeholder,
    Path(StrongObj<AstPathExpr>),
    Dot(StrongObj<AstDotExpr>),
    Call(StrongObj<AstCallExpr>),
    Index(StrongObj<AstIndexExpr>),
    BinOp(StrongObj<AstBinOpExpr>),
    UnaryNeg(StrongObj<AstUnaryNegExpr>),
    Literal(StrongObj<AstLiteralExpr>),
    Tuple(StrongObj<AstTupleExpr>),
    Paren(StrongObj<AstParenExpr>),
    Ctor(StrongObj<AstCtorExpr>),
    Block(StrongObj<AstBlockExpr>),
    If(StrongObj<AstIfExpr>),
    While(StrongObj<AstWhileExpr>),
    Loop(StrongObj<AstLoopExpr>),
}

impl From<StrongObj<AstPathExpr>> for AstExpr {
    fn from(value: StrongObj<AstPathExpr>) -> Self {
        Self::Path(value)
    }
}

impl From<StrongObj<AstDotExpr>> for AstExpr {
    fn from(value: StrongObj<AstDotExpr>) -> Self {
        Self::Dot(value)
    }
}

impl From<StrongObj<AstCallExpr>> for AstExpr {
    fn from(value: StrongObj<AstCallExpr>) -> Self {
        Self::Call(value)
    }
}

impl From<StrongObj<AstIndexExpr>> for AstExpr {
    fn from(value: StrongObj<AstIndexExpr>) -> Self {
        Self::Index(value)
    }
}

impl From<StrongObj<AstBinOpExpr>> for AstExpr {
    fn from(value: StrongObj<AstBinOpExpr>) -> Self {
        Self::BinOp(value)
    }
}

impl From<StrongObj<AstUnaryNegExpr>> for AstExpr {
    fn from(value: StrongObj<AstUnaryNegExpr>) -> Self {
        Self::UnaryNeg(value)
    }
}

impl From<StrongObj<AstLiteralExpr>> for AstExpr {
    fn from(value: StrongObj<AstLiteralExpr>) -> Self {
        Self::Literal(value)
    }
}

impl From<StrongObj<AstTupleExpr>> for AstExpr {
    fn from(value: StrongObj<AstTupleExpr>) -> Self {
        Self::Tuple(value)
    }
}

impl From<StrongObj<AstParenExpr>> for AstExpr {
    fn from(value: StrongObj<AstParenExpr>) -> Self {
        Self::Paren(value)
    }
}

impl From<StrongObj<AstCtorExpr>> for AstExpr {
    fn from(value: StrongObj<AstCtorExpr>) -> Self {
        Self::Ctor(value)
    }
}

impl From<StrongObj<AstBlockExpr>> for AstExpr {
    fn from(value: StrongObj<AstBlockExpr>) -> Self {
        Self::Block(value)
    }
}

impl From<StrongObj<AstIfExpr>> for AstExpr {
    fn from(value: StrongObj<AstIfExpr>) -> Self {
        Self::If(value)
    }
}

impl From<StrongObj<AstWhileExpr>> for AstExpr {
    fn from(value: StrongObj<AstWhileExpr>) -> Self {
        Self::While(value)
    }
}

impl From<StrongObj<AstLoopExpr>> for AstExpr {
    fn from(value: StrongObj<AstLoopExpr>) -> Self {
        Self::Loop(value)
    }
}

impl From<AstPathExpr> for AstExpr {
    fn from(value: AstPathExpr) -> Self {
        Self::Path(StrongObj::new(value))
    }
}

impl From<AstDotExpr> for AstExpr {
    fn from(value: AstDotExpr) -> Self {
        Self::Dot(StrongObj::new(value))
    }
}

impl From<AstCallExpr> for AstExpr {
    fn from(value: AstCallExpr) -> Self {
        Self::Call(StrongObj::new(value))
    }
}

impl From<AstIndexExpr> for AstExpr {
    fn from(value: AstIndexExpr) -> Self {
        Self::Index(StrongObj::new(value))
    }
}

impl From<AstBinOpExpr> for AstExpr {
    fn from(value: AstBinOpExpr) -> Self {
        Self::BinOp(StrongObj::new(value))
    }
}

impl From<AstUnaryNegExpr> for AstExpr {
    fn from(value: AstUnaryNegExpr) -> Self {
        Self::UnaryNeg(StrongObj::new(value))
    }
}

impl From<AstLiteralExpr> for AstExpr {
    fn from(value: AstLiteralExpr) -> Self {
        Self::Literal(StrongObj::new(value))
    }
}

impl From<AstTupleExpr> for AstExpr {
    fn from(value: AstTupleExpr) -> Self {
        Self::Tuple(StrongObj::new(value))
    }
}

impl From<AstParenExpr> for AstExpr {
    fn from(value: AstParenExpr) -> Self {
        Self::Paren(StrongObj::new(value))
    }
}

impl From<AstCtorExpr> for AstExpr {
    fn from(value: AstCtorExpr) -> Self {
        Self::Ctor(StrongObj::new(value))
    }
}

impl From<AstBlockExpr> for AstExpr {
    fn from(value: AstBlockExpr) -> Self {
        Self::Block(StrongObj::new(value))
    }
}

impl From<AstIfExpr> for AstExpr {
    fn from(value: AstIfExpr) -> Self {
        Self::If(StrongObj::new(value))
    }
}

impl From<AstWhileExpr> for AstExpr {
    fn from(value: AstWhileExpr) -> Self {
        Self::While(StrongObj::new(value))
    }
}

impl From<AstLoopExpr> for AstExpr {
    fn from(value: AstLoopExpr) -> Self {
        Self::Loop(StrongObj::new(value))
    }
}

#[derive(Debug)]
pub struct AstPathExpr {
    pub path: AstPath,
}

#[derive(Debug)]
pub struct AstDotExpr {
    pub expr: AstExpr,
    pub member: Symbol,
}

#[derive(Debug)]
pub struct AstCallExpr {
    pub expr: AstExpr,
    pub args: Box<[AstExpr]>,
}

#[derive(Debug)]
pub struct AstIndexExpr {
    pub expr: AstExpr,
    pub indexer: AstExpr,
}

#[derive(Debug)]
pub struct AstBinOpExpr {
    pub kind: AstBinOpKind,
    pub lhs: AstExpr,
    pub rhs: AstExpr,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstBinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    ShortOr,
    ShortAnd,
    BitXor,
    BitOr,
    BitAnd,
}

#[derive(Debug)]
pub struct AstUnaryNegExpr {
    pub expr: AstExpr,
}

#[derive(Debug)]
pub enum AstLiteralExpr {
    String(TokenStringLit),
    Number(TokenNumberLit),
    Char(TokenCharLit),
    Bool(Span, bool),
}

#[derive(Debug)]
pub struct AstTupleExpr {
    pub items: Box<[AstExpr]>,
}

#[derive(Debug)]
pub struct AstParenExpr {
    pub expr: AstExpr,
}

#[derive(Debug)]
pub struct AstCtorExpr {
    pub item: AstPath,
    pub fields: Vec<(TokenIdent, AstExpr)>,
}

#[derive(Debug)]
pub struct AstBlockExpr {
    pub body: AstBody,
}

#[derive(Debug)]
pub struct AstIfExpr {
    pub condition: AstExpr,
    pub body: AstBody,
    pub otherwise: Option<AstExpr>,
}

#[derive(Debug)]
pub struct AstWhileExpr {
    pub condition: AstExpr,
    pub body: AstBody,
}

#[derive(Debug)]
pub struct AstLoopExpr {
    pub body: AstBody,
}

// === Statements === ///

#[derive(Debug, Clone)]
pub struct AstBody {
    pub statements: Vec<AstStatement>,
    pub last_stmt_trails: bool,
}

#[derive(Debug, Clone)]
pub enum AstStatement {
    Let(StrongObj<AstLetStatement>),
    Expr(AstExpr),
}

#[derive(Debug)]
pub struct AstLetStatement {
    pub name: TokenIdent,
    pub mutable: bool,
    pub initializer: Option<AstExpr>,
}
