use aunty::StrongObj;

use crate::util::span::Span;

use super::{
    parser::AstKeyword,
    token::{TokenCharLit, TokenIdent, TokenNumberLit, TokenStringLit},
};

// === Helpers === //

macro_rules! define_convertible_enum {
	(
		$(#[$attr:meta])*
		$vis:vis enum $enum_name:ident {
			$($name:ident $(($inner:ty))?),*
			$(,)?
		}
	) => {
		$(#[$attr])*
		$vis enum $enum_name {
			$($name$((StrongObj<$inner>))?,)*
		}

		$($(
			impl From<StrongObj<$inner>> for $enum_name {
				fn from(value: StrongObj<$inner>) -> Self {
					Self::$name(value)
				}
			}

			impl From<$inner> for $enum_name {
				fn from(value: $inner) -> Self {
					Self::$name(StrongObj::new(value))
				}
			}
		)?)*
	};
}

// === Items === //

#[derive(Debug)]
pub struct AstFileRoot {
    pub items: Box<[AstItem]>,
}

define_convertible_enum! {
    #[derive(Debug, Clone)]
    pub enum AstItem {
        Function(AstFunctionItem),
    }
}

#[derive(Debug)]
pub struct AstFunctionItem {
    pub span: Span,
    pub name: TokenIdent,
    pub args: Box<[(TokenIdent, AstType)]>,
    pub result: AstType,
    pub body: AstExpr,
}

// === Paths === //

#[derive(Debug, Clone)]
pub struct AstMultiPath {
    /// The base path of the multi-import.
    pub base: AstPath,

    /// The parts to be imported. The prefix of each base path should be `self::`.
    pub tree: AstMultiPathList,
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
            prefix: AstPathPrefix::Implicit,
            parts: Box::from_iter([AstPathPart(name)]),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.prefix, AstPathPrefix::Implicit) && self.parts.is_empty()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AstPathPrefix {
    /// Relative to the root of the currently compiled crate.
    Crate(Span),

    /// A fully qualified crate name.
    Crates(Span),

    /// Implicitly relative to the current module.
    Implicit,

    /// Relative to the current module.
    Self_(Span),
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

impl AstType {
    pub fn new_unit() -> Self {
        Self {
            kind: AstTypeKind::Tuple,
            generics: Box::from_iter([]),
        }
    }
}

// === Expressions === //

define_convertible_enum! {
    #[derive(Debug, Clone)]
    pub enum AstExpr {
        Empty,
        Malformed,
        Path(AstPathExpr),
        Dot(AstDotExpr),
        Call(AstCallExpr),
        Index(AstIndexExpr),
        BinOp(AstBinOpExpr),
        UnaryNeg(AstUnaryNegExpr),
        Literal(AstLiteralExpr),
        Tuple(AstTupleExpr),
        Paren(AstParenExpr),
        Ctor(AstCtorExpr),
        Block(AstBlockExpr),
        If(AstIfExpr),
        While(AstWhileExpr),
        Loop(AstLoopExpr),
    }
}

impl AstExpr {
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

#[derive(Debug)]
pub struct AstPathExpr {
    pub path: AstPath,
}

#[derive(Debug)]
pub struct AstDotExpr {
    pub expr: AstExpr,
    pub member: TokenIdent,
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
