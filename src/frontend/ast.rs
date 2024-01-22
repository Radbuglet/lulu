use aunty::StrongObj;

use crate::util::{intern::Intern, span::Span};

use super::{parser::AstKeyword, token::TokenIdent};

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
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstPathPrefix {
    /// Relative to the root of the currently compiled crate.
    Crate,

    /// A fully qualified crate name.
    Crates,

    /// Relative to the current module.
    Self_,
}

#[derive(Debug, Copy, Clone)]
pub struct AstPathPart(pub TokenIdent);

impl AstPathPart {
    pub fn is_super(&self) -> bool {
        self.0.text == AstKeyword::Super.to_intern()
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
    UnaryOp(StrongObj<AstUnaryOpExpr>),
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

impl From<StrongObj<AstUnaryOpExpr>> for AstExpr {
    fn from(value: StrongObj<AstUnaryOpExpr>) -> Self {
        Self::UnaryOp(value)
    }
}

#[derive(Debug)]
pub struct AstPathExpr {
    pub path: AstPath,
}

#[derive(Debug)]
pub struct AstDotExpr {
    pub expr: AstExpr,
    pub member: Intern,
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
    ShortXor,
    ShortOr,
    ShortAnd,
    BitXor,
    BitOr,
    BitAnd,
}

#[derive(Debug)]
pub struct AstUnaryOpExpr {
    pub span: Span,
    pub kind: AstUnaryOpKind,
    pub expr: AstExpr,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstUnaryOpKind {
    Neg,
}
