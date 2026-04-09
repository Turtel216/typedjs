{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Responsible for lowering the Typed-AST into the Javascript IR.
module Desuger where

import Data.Text (Text)
import qualified JsIr as J
import Ast 
  ( Program(..), Stmt(..), Expr(..), Literal(..), Param(..), Arg(..), Block(..)
  , BinOp(..), UnOp(..), Mutability(..)
  )

-- | Type erasure + structural lowering from TypedJS AST to JS AST.
lowerProgram :: Program -> J.JSProgram
lowerProgram (Program ss) = J.JSProgram (map lowerStmt ss)

-- | Lower Stmt into JsStmt
lowerStmt :: Stmt -> J.JSStmt
lowerStmt = \case
  SLet mut n _ e -> case mut of
    Immutable -> J.JSConst n (lowerExpr e)
    Mutable   -> J.JSLet   n (lowerExpr e)

  SFun name params _retTy body ->
    J.JSFunction name (map paramName params) (lowerBlock body)

  SReturn me ->
    J.JSReturn (lowerExpr <$> me)

  SIf c t me ->
    J.JSIf (lowerExpr c) (lowerBlock t) (lowerBlock <$> me)

  SWhile c b ->
    J.JSWhile (lowerExpr c) (lowerBlock b)

  SExpr e ->
    J.JSExprStmt (lowerExpr e)

  SBlock b ->
    J.JSBlockStmt (lowerBlock b)

lowerBlock :: Block -> J.JSBlock
lowerBlock (Block ss) = J.JSBlock (map lowerStmt ss)

lowerExpr :: Expr -> J.JSExpr
lowerExpr = \case
  EVar x -> J.JSVar x
  ELit l -> J.JSLiteral (lowerLit l)

  ELam params _retTy body ->
    J.JSArrow (map paramName params) (lowerExpr body)

  ECall f args ->
    J.JSCall (lowerExpr f) (map lowerArg args)

  EMember o f ->
    J.JSMember (lowerExpr o) f

  EIndex a i ->
    J.JSIndex (lowerExpr a) (lowerExpr i)

  EObject fs ->
    J.JSObject [ (k, lowerExpr v) | (k,v) <- fs ]

  EArray es ->
    J.JSArray (map lowerExpr es)

  EAssign l r ->
    J.JSAssign (lowerExpr l) (lowerExpr r)

  EUnary op e ->
    J.JSUnary (lowerUnOp op) (lowerExpr e)

  EBinary op a b ->
    J.JSBinary (lowerBinOp op) (lowerExpr a) (lowerExpr b)

  EIfExpr c t f ->
    J.JSTernary (lowerExpr c) (lowerExpr t) (lowerExpr f)

  EParens e ->
    J.JSParens (lowerExpr e)

lowerArg :: Arg -> J.JSExpr
lowerArg (Arg e) = lowerExpr e

lowerLit :: Literal -> J.JSLit
lowerLit = \case
  LInt n    -> J.JSInt n
  LBool b   -> J.JSBool b
  LString s -> J.JSString s
  LNull     -> J.JSNull

paramName :: Param -> Text
paramName (Param n _ty) = n

lowerUnOp :: UnOp -> Text
lowerUnOp = \case
  Neg -> "-"
  Not -> "!"

lowerBinOp :: BinOp -> Text
lowerBinOp = \case
  Mul -> "*"
  Div -> "/"
  Mod -> "%"
  Add -> "+"
  Sub -> "-"
  Lt  -> "<"
  Lte -> "<="
  Gt  -> ">"
  Gte -> ">="
  Eq  -> "=="
  Neq -> "!="
  And -> "&&"
  Or  -> "||"
