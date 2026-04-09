-- | Abstract syntax tree definition
module Ast where

import Data.Text (Text)

type Name = Text

-- | Binding mutability qualifier
data Mutability = Immutable | Mutable
  deriving (Eq, Show)

-- | Complete program definition
newtype Program = Program [Stmt]
  deriving (Eq, Show)

-- | Language Statement
data Stmt
  = SLet Mutability Name (Maybe Type) Expr
  | SFun Name [Param] (Maybe Type) Block
  | SReturn (Maybe Expr)
  | SIf Expr Block (Maybe Block)
  | SWhile Expr Block
  | SExpr Expr
  | SBlock Block
  deriving (Eq, Show)

-- | Code block
newtype Block = Block [Stmt]
  deriving (Eq, Show)

-- | Function parameters
data Param = Param Name (Maybe Type)
  deriving (Eq, Show)

-- | Function arguments
data Arg = Arg Expr
  deriving (Eq, Show)

-- | Language expression
data Expr
  = EVar Name
  | ELit Literal
  | ELam [Param] (Maybe Type) Expr
  | ECall Expr [Arg]
  | EMember Expr Name
  | EIndex Expr Expr
  | EObject [(Name, Expr)]
  | EArray [Expr]
  | EAssign Expr Expr
  | EUnary UnOp Expr
  | EBinary BinOp Expr Expr
  | EIfExpr Expr Expr Expr
  | EParens Expr
  deriving (Eq, Show)

-- | Literals
data Literal
  = LInt Integer
  | LBool Bool
  | LString Text
  | LNull
  deriving (Eq, Show)

-- | Unary operations
data UnOp = Neg | Not
  deriving (Eq, Show)

-- | Binary operations
data BinOp
  = Mul | Div | Mod
  | Add | Sub
  | Lt | Lte | Gt | Gte
  | Eq | Neq
  | And | Or
  deriving (Eq, Show)

-- | Language types
data Type
  = TInt
  | TBool
  | TString
  | TVar Name
  | TArray Type
  | TObject [(Name, Type)]
  | TApp Name [Type]       -- e.g. Result<Int, String>
  | TFun [Type] Type       -- (a, b) -> c
  deriving (Eq, Show)
