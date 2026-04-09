{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TypedJs Parser implemented using Parser combinators
module Parser where

import Ast
import Control.Monad (void)
import Data.Char (isUpper)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Void as V
import Text.Megaparsec
  ( Parsec,
    between,
    choice,
    eof,
    many,
    notFollowedBy,
    optional,
    runParser,
    satisfy,
    sepBy,
    sepBy1,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser combinator
type Parser = Parsec V.Void Text

-- | Parse complete program
parseProgram :: FilePath -> Text -> Either String Program
parseProgram fp src =
  case runParser (sc *> pProgram <* eof) fp src of
    Left e -> Left (MP.errorBundlePretty e)
    Right x -> Right x

-- | Ignores spaces and comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Helper for parsing a lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a given symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Consume parenthesis '('
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Consume braces '{'
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Consume brackets '['
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Consume semicolone ';'
semi :: Parser Text
semi = symbol ";"

-- | Consume comma ','
comma :: Parser Text
comma = symbol ","

-- | Consume colon ':'
colon :: Parser Text
colon = symbol ":"

-- | Consume arrow symbol '=>'
arrow :: Parser Text
arrow = symbol "=>"

-- | TypedJs reserved keywords
reserved :: [Text]
reserved =
  [ "let",
    "mut",
    "function",
    "return",
    "if",
    "else",
    "while",
    "true",
    "false",
    "null",
    "type",
    "Int",
    "Bool",
    "String"
  ]

-- | Parse identifier
identifier :: Parser Text
identifier = lexeme . try $ do
  x <- letterChar <|> char '_' <|> char '$'
  xs <- many (alphaNumChar <|> char '_' <|> char '$')
  let n = T.pack (x : xs)
  if n `elem` reserved
    then fail ("reserved word " <> T.unpack n)
    else pure n

-- | Parse an uppercase identifier (for type names like Point, Pair)
upperIdentifier :: Parser Text
upperIdentifier = lexeme . try $ do
  x <- satisfy (\c -> isUpper c)
  xs <- many (alphaNumChar <|> char '_' <|> char '$')
  let n = T.pack (x : xs)
  if n `elem` reserved
    then fail ("reserved word " <> T.unpack n)
    else pure n

rword :: Text -> Parser ()
rword w = lexeme . try $ string w *> notFollowedBy identTail
  where
    identTail = alphaNumChar <|> char '_' <|> char '$'

-- | Parse String literal. Allows both "" and '', similar to Javascript
stringLit :: Parser Text
stringLit = lexeme $ do
  q <- char '"' <|> char '\''
  content <- many (satisfy (/= q))
  void (char q)
  pure (T.pack content)

-- | Parse integer
integer :: Parser Integer
integer = lexeme L.decimal

pProgram :: Parser Program
pProgram = Program <$> many pStmt

-- | Parse statement
pStmt :: Parser Stmt
pStmt =
  choice
    [ try pTypeDecl,
      try pFunDecl,
      try pLet,
      pReturn,
      pIf,
      pWhile,
      SBlock <$> pBlock,
      pExprStmt
    ]

-- | Parse Code block
pBlock :: Parser Block
pBlock = Block <$> braces (many pStmt)

-- | Parse let statement. Supports optional @mut@ qualifier for mutable bindings.
pLet :: Parser Stmt
pLet = do
  rword "let"
  mutFlag <- MP.option Immutable (rword "mut" $> Mutable)
  n <- identifier
  ty <- optional (colon *> pType)
  void (symbol "=")
  e <- pExpr
  void semi
  pure (SLet mutFlag n ty e)

-- | Parse type alias declaration: @type Point = { x: Int, y: Int };@
-- Also supports parametric aliases: @type Pair\<A, B\> = { first: A, second: B };@
pTypeDecl :: Parser Stmt
pTypeDecl = do
  rword "type"
  name <- upperIdentifier
  params <- MP.option [] (angles (identifier `sepBy1` comma))
  void (symbol "=")
  body <- pType
  void semi
  pure (STypeDecl name params body)
  where
    angles = between (symbol "<") (symbol ">")

-- | Parse Declaration
pFunDecl :: Parser Stmt
pFunDecl = do
  rword "function"
  n <- identifier
  params <- parens (pParam `sepBy` comma)
  retTy <- optional (colon *> pType)
  b <- pBlock
  pure (SFun n params retTy b)

-- | Parse return statement
pReturn :: Parser Stmt
pReturn = do
  rword "return"
  e <- optional pExpr
  void semi
  pure (SReturn e)

-- | Parse if statement
pIf :: Parser Stmt
pIf = do
  rword "if"
  cond <- parens pExpr
  th <- pBlock
  el <- optional (rword "else" *> pBlock)
  pure (SIf cond th el)

-- | Parse while loop
pWhile :: Parser Stmt
pWhile = do
  rword "while"
  cond <- parens pExpr
  b <- pBlock
  pure (SWhile cond b)

-- | Parse Expression Statement
pExprStmt :: Parser Stmt
pExprStmt = do
  e <- pExpr
  void semi
  pure (SExpr e)

-- | Parse function parameters
pParam :: Parser Param
pParam = do
  n <- identifier
  ty <- optional (colon *> pType)
  pure (Param n ty)

-- | Parse Expression
pExpr :: Parser Expr
pExpr = pAssign

-- | Parse Assignment
pAssign :: Parser Expr
pAssign = do
  lhs <- pLogicOr
  optional (symbol "=") >>= \case
    Nothing -> pure lhs
    Just _ -> EAssign lhs <$> pAssign

-- | Parse Logical Or
pLogicOr :: Parser Expr
pLogicOr = chainl1 pLogicAnd (symbol "||" $> EBinary Or)

-- | Parse Logical And
pLogicAnd :: Parser Expr
pLogicAnd = chainl1 pEquality (symbol "&&" $> EBinary And)

-- | Parse Equality expression. Both '==' and '!='
pEquality :: Parser Expr
pEquality =
  chainl1
    pRelational
    ( choice
        [ symbol "==" $> EBinary Eq,
          symbol "!=" $> EBinary Neq
        ]
    )

-- | Parse relational expressions
pRelational :: Parser Expr
pRelational =
  chainl1
    pAdditive
    ( choice
        [ symbol "<=" $> EBinary Lte,
          symbol "<" $> EBinary Lt,
          symbol ">=" $> EBinary Gte,
          symbol ">" $> EBinary Gt
        ]
    )

-- | Parse Additive binary operation:
--
-- * `+`
--
-- * `-`
pAdditive :: Parser Expr
pAdditive =
  chainl1
    pMultiplicative
    ( choice
        [ symbol "+" $> EBinary Add,
          symbol "-" $> EBinary Sub
        ]
    )

-- | Parse multiplcative Binary operation:
--
-- * `*`
--
-- * `/`
--
-- * `&`
pMultiplicative :: Parser Expr
pMultiplicative =
  chainl1
    pUnary
    ( choice
        [ symbol "*" $> EBinary Mul,
          symbol "/" $> EBinary Div,
          symbol "%" $> EBinary Mod
        ]
    )

-- | Parse Unary operation:
--
-- * `!`
--
-- * `-`
pUnary :: Parser Expr
pUnary =
  choice
    [ symbol "!" *> (EUnary Not <$> pUnary),
      symbol "-" *> (EUnary Neg <$> pUnary),
      pPostfix
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
            pChain (ECall e args),
          try $ do
            void (symbol ".")
            f <- identifier
            pChain (EMember e f),
          try $ do
            ix <- brackets pExpr
            pChain (EIndex e ix),
          pure e
        ]

pPrimary :: Parser Expr
pPrimary =
  choice
    [ try pLambda,
      try pIfExpr,
      ELit <$> pLiteral,
      try pObject,
      try pArray,
      EVar <$> identifier,
      EParens <$> parens pExpr
    ]

-- | Parse Lambda Expression
pLambda :: Parser Expr
pLambda = do
  params <- parens (pParam `sepBy` comma)
  retTy <- optional (colon *> pType)
  void arrow
  body <- pExpr
  pure (ELam params retTy body)

-- | Parse if expression
pIfExpr :: Parser Expr
pIfExpr = do
  rword "if"
  c <- parens pExpr
  t <- pExpr
  rword "else"
  f <- pExpr
  pure (EIfExpr c t f)

-- | Parse Literal:
--
-- * true
--
-- * false
--
-- * null
--
-- * String literals
--
-- * Integer literals
pLiteral :: Parser Literal
pLiteral =
  choice
    [ rword "true" $> LBool True,
      rword "false" $> LBool False,
      rword "null" $> LNull,
      LString <$> stringLit,
      LInt <$> integer
    ]

-- | Parse Arrays
pArray :: Parser Expr
pArray = EArray <$> brackets (pExpr `sepBy` comma)

-- | Parse Objects
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
    rest lhs =
      ( do
          void (symbol "->")
          rhs <- pFunType
          case lhs of
            TFun as r -> pure (TFun as (collapse rhs r))
            _ -> pure (TFun [lhs] rhs)
      )
        <|> pure lhs

    collapse r1 r2 = TFun [r2] r1

pTypeAtomOrTuple :: Parser Type
pTypeAtomOrTuple =
  choice
    [ parens $ do
        ts <- pType `sepBy` comma
        case ts of
          [] -> fail "empty type tuple not allowed"
          [t] -> pure t
          xs -> pure (TFun xs (TVar "_Tuple")), -- simple placeholder strategy
      pTypeAtom
    ]

pTypeAtom :: Parser Type
pTypeAtom =
  choice
    [ rword "Int" $> TInt,
      rword "Bool" $> TBool,
      rword "String" $> TString,
      try pArrayType,
      try pObjectType,
      try pTypeApp,
      try pTypeRef,
      TVar <$> identifier
    ]

-- | Parse a bare uppercase type reference (no angle brackets).
-- E.g. @Point@ becomes @TApp "Point" []@.
pTypeRef :: Parser Type
pTypeRef = do
  name <- upperIdentifier
  pure (TApp name [])

-- | Parse Array type declaration
pArrayType :: Parser Type
pArrayType = TArray <$> brackets pType

-- | Parse Object type declaration
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
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> pure x
