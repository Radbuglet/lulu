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
