{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser (
  parseExpr,
  parseModule
) where

-- XXX: simpler
import Protolude hiding ((<|>), many, try, bool)
import Prelude ((>>), FilePath)

import Data.List (foldl1)

{-import Prelude hiding (foldr)-}
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Expr as Ex

import Data.String (String)
import Data.Foldable (foldr)
import qualified Data.Text.Lazy as L

import Lexer
import Syntax

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- integer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool =  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return $ Lam args body 

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- aexp
  reservedOp "then"
  tr <- aexp
  reserved "else"
  fl <- aexp
  return (If cond tr fl)

aexp :: Parser Expr
aexp =  parens expr
    <|> bool
    <|> number
    <|> ifthen
    <|> try letrecin
    <|> letin
    <|> lambda
    <|> variable

prim :: Prim -> Expr -> Expr -> Expr
prim o l r = App (App (Prim o) l) r

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

term :: Parser Expr
term = Ex.buildExpressionParser table aexp

table :: Operators Expr
table = [
    [
      infixOp "*"  (prim Mul) Ex.AssocLeft
    ],
    [
      infixOp "+"  (prim Add) Ex.AssocLeft,
      infixOp "-"  (prim Sub) Ex.AssocLeft
    ],
    [
      infixOp "==" (prim Eql) Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

letdecl :: Parser Decl
letdecl = do
  reserved "let"
  name <- identifier
  reservedOp "="
  body <- expr
  return $ FunDecl name [] body

decl :: Parser Decl
decl = letdecl

top :: Parser Decl
top = decl <* semi

modl ::  Parser [Decl]
modl = many top

prg :: Parser Program
prg = do
  ds <- modl
  return $ Program ds

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

parseExpr ::  FilePath -> L.Text -> Either ParseError Expr
parseExpr fname input = parse (contents expr) fname input

parseModule ::  FilePath -> L.Text -> Either ParseError Program
parseModule fname input = parse (contents prg) fname input
