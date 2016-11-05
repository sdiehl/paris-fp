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

variable :: Parser (Expr Name)
variable = do
  x <- identifier
  return (Var x)

number :: Parser (Expr Name)
number = do
  n <- integer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser (Expr Name)
bool =  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser (Expr Name)
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return $ Lam args body 

ifthen :: Parser (Expr Name)
ifthen = do
  reserved "if"
  cond <- aexp
  reservedOp "then"
  tr <- aexp
  reserved "else"
  fl <- aexp
  return (If cond tr fl)

aexp :: Parser (Expr Name)
aexp =  parens expr
    <|> bool
    <|> number
    <|> ifthen
    <|> lambda
    <|> variable

prim :: Prim -> Expr Name -> Expr Name -> Expr Name
prim o l r = App (App (Prim o) l) r

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

term :: Parser (Expr Name)
term = Ex.buildExpressionParser table aexp

table :: Operators (Expr Name)
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

expr :: Parser (Expr Name)
expr = do
  es <- many1 term
  return (foldl1 App es)

letdecl :: Parser (Decl Name)
letdecl = do
  reserved "let"
  name <- identifier
  reservedOp "="
  body <- expr
  return $ FunDecl name [] body

decl :: Parser (Decl Name)
decl = letdecl

top :: Parser (Decl Name)
top = decl <* semi

modl ::  Parser [Decl Name]
modl = many top

prg :: Parser (Program Name)
prg = do
  ds <- modl
  return $ Program ds

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

parseExpr ::  FilePath -> L.Text -> Either ParseError (Expr Name)
parseExpr fname input = parse (contents expr) fname input

parseModule ::  FilePath -> L.Text -> Either ParseError (Program Name)
parseModule fname input = parse (contents prg) fname input
