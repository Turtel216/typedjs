{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Language Parser
module Parser where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Void as V
import Text.Megaparsec
  ( Parsec, (<|>), between, choice, eof, many, optional, runParser
  , sepBy, sepBy1, try, notFollowedBy, satisfy, some
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( alphaNumChar, char, letterChar, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Ast


type Parser = Parsec V.Void Text

parseProgram :: FilePath -> Text -> Either String Program
parseProgram fp src =
  case runParser (sc *> pProgram <* eof) fp src of
    Left e  -> Left (MP.errorBundlePretty e)
    Right x -> Right x

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens, braces, brackets :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

semi :: Parser Text
semi = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

arrow :: Parser Text
arrow = symbol "=>"

reserved :: [Text]
reserved =
  [ "let","function","return","if","else","while"
  , "true","false","null"
  , "Int","Bool","String"
  ]

identifier :: Parser Text
identifier = lexeme . try $ do
  x  <- letterChar <|> char '_' <|> char '$'
  xs <- many (alphaNumChar <|> char '_' <|> char '$')
  let n = T.pack (x:xs)
  if n `elem` reserved
    then fail ("reserved word " <> T.unpack n)
    else pure n

rword :: Text -> Parser ()
rword w = lexeme . try $ string w *> notFollowedBy identTail
  where identTail = alphaNumChar <|> char '_' <|> char '$'

stringLit :: Parser Text
stringLit = lexeme $ do
  q <- char '"' <|> char '\''
  content <- many (satisfy (/= q))
  void (char q)
  pure (T.pack content)

integer :: Parser Integer
integer = lexeme L.decimal

pProgram :: Parser Program
pProgram = Program <$> many pStmt

pStmt :: Parser Stmt
pStmt = choice
  [ try pFunDecl
  , try pLet
  , pReturn
  , pIf
  , pWhile
  , SBlock <$> pBlock
  , pExprStmt
  ]

pBlock :: Parser Block
pBlock = Block <$> braces (many pStmt)

pLet :: Parser Stmt
pLet = do
  rword "let"
  n <- identifier
  ty <- optional (colon *> pType)
  void (symbol "=")
  e <- pExpr
  void semi
  pure (SLet n ty e)

pFunDecl :: Parser Stmt
pFunDecl = do
  rword "function"
  n <- identifier
  params <- parens (pParam `sepBy` comma)
  retTy <- optional (colon *> pType)
  b <- pBlock
  pure (SFun n params retTy b)

pReturn :: Parser Stmt
pReturn = do
  rword "return"
  e <- optional pExpr
  void semi
  pure (SReturn e)

pIf :: Parser Stmt
pIf = do
  rword "if"
  cond <- parens pExpr
  th <- pBlock
  el <- optional (rword "else" *> pBlock)
  pure (SIf cond th el)

pWhile :: Parser Stmt
pWhile = do
  rword "while"
  cond <- parens pExpr
  b <- pBlock
  pure (SWhile cond b)

pExprStmt :: Parser Stmt
pExprStmt = do
  e <- pExpr
  void semi
  pure (SExpr e)

pParam :: Parser Param
pParam = do
  n <- identifier
  ty <- optional (colon *> pType)
  pure (Param n ty)

pExpr :: Parser Expr
pExpr = pAssign

pAssign :: Parser Expr
pAssign = do
  lhs <- pLogicOr
  optional (symbol "=") >>= \case
    Nothing -> pure lhs
    Just _  -> EAssign lhs <$> pAssign

pLogicOr :: Parser Expr
pLogicOr = chainl1 pLogicAnd (symbol "||" $> EBinary Or)

pLogicAnd :: Parser Expr
pLogicAnd = chainl1 pEquality (symbol "&&" $> EBinary And)

pEquality :: Parser Expr
pEquality = chainl1 pRelational (choice
  [ symbol "==" $> EBinary Eq
  , symbol "!=" $> EBinary Neq
  ])

pRelational :: Parser Expr
pRelational = chainl1 pAdditive (choice
  [ symbol "<=" $> EBinary Lte
  , symbol "<"  $> EBinary Lt
  , symbol ">=" $> EBinary Gte
  , symbol ">"  $> EBinary Gt
  ])

pAdditive :: Parser Expr
pAdditive = chainl1 pMultiplicative (choice
  [ symbol "+" $> EBinary Add
  , symbol "-" $> EBinary Sub
  ])

pMultiplicative :: Parser Expr
pMultiplicative = chainl1 pUnary (choice
  [ symbol "*" $> EBinary Mul
  , symbol "/" $> EBinary Div
  , symbol "%" $> EBinary Mod
  ])

pUnary :: Parser Expr
pUnary =
  choice
    [ symbol "!" *> (EUnary Not <$> pUnary)
    , symbol "-" *> (EUnary Neg <$> pUnary)
    , pPostfix
    ]

pPostfix :: Parser Expr
pPostfix = do
  base <- pPrimary
  pChain base
  where
    pChain e =
      choice
        [ try $ do
            args <- parens (map Arg <$> (pExpr `sepBy` comma))
            pChain (ECall e args)
        , try $ do
            void (symbol ".")
            f <- identifier
            pChain (EMember e f)
        , try $ do
            ix <- brackets pExpr
            pChain (EIndex e ix)
        , pure e
        ]

pPrimary :: Parser Expr
pPrimary = choice
  [ try pLambda
  , try pIfExpr
  , ELit <$> pLiteral
  , try pObject
  , try pArray
  , EVar <$> identifier
  , EParens <$> parens pExpr
  ]

pLambda :: Parser Expr
pLambda = do
  params <- parens (pParam `sepBy` comma)
  retTy <- optional (colon *> pType)
  void arrow
  body <- pExpr
  pure (ELam params retTy body)

pIfExpr :: Parser Expr
pIfExpr = do
  rword "if"
  c <- parens pExpr
  t <- pExpr
  rword "else"
  f <- pExpr
  pure (EIfExpr c t f)

pLiteral :: Parser Literal
pLiteral = choice
  [ rword "true"  $> LBool True
  , rword "false" $> LBool False
  , rword "null"  $> LNull
  , LString <$> stringLit
  , LInt <$> integer
  ]

pArray :: Parser Expr
pArray = EArray <$> brackets (pExpr `sepBy` comma)

pObject :: Parser Expr
pObject = EObject <$> braces (pField `sepBy` comma)
  where
    pField = do
      k <- identifier <|> stringLit
      void colon
      v <- pExpr
      pure (k, v)

pType :: Parser Type
pType = pFunType

-- Right-assoc function type:
-- (A, B) -> C
-- A -> B -> C   as A -> (B -> C)
pFunType :: Parser Type
pFunType = do
  lhs <- pTypeAtomOrTuple
  rest lhs
  where
    rest lhs = (do
      void (symbol "->")
      rhs <- pFunType
      case lhs of
        TFun as r -> pure (TFun as (collapse rhs r))
        _         -> pure (TFun [lhs] rhs)
      ) <|> pure lhs

    collapse r1 r2 = TFun [r2] r1

pTypeAtomOrTuple :: Parser Type
pTypeAtomOrTuple =
  choice
    [ parens $ do
        ts <- pType `sepBy` comma
        case ts of
          []  -> fail "empty type tuple not allowed"
          [t] -> pure t
          xs  -> pure (TFun xs (TVar "_Tuple")) -- simple placeholder strategy
    , pTypeAtom
    ]

pTypeAtom :: Parser Type
pTypeAtom = choice
  [ rword "Int"    $> TInt
  , rword "Bool"   $> TBool
  , rword "String" $> TString
  , try pArrayType
  , try pObjectType
  , try pTypeApp
  , TVar <$> identifier
  ]

pArrayType :: Parser Type
pArrayType = TArray <$> brackets pType

pObjectType :: Parser Type
pObjectType = TObject <$> braces (pField `sepBy` comma)
  where
    pField = do
      k <- identifier <|> stringLit
      void colon
      t <- pType
      pure (k, t)

pTypeApp :: Parser Type
pTypeApp = do
  ctor <- identifier
  args <- angles (pType `sepBy1` comma)
  pure (TApp ctor args)
  where
    angles = between (symbol "<") (symbol ">")

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      (do
          f <- op
          y <- p
          rest (f x y))
      <|> pure x
