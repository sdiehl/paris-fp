module Lexer (
  Op,
  Operators,
  identifier,
  integer,
  reserved,
  reservedOp,
  parens,
  semi,
  comma,
  contents,
) where

import Protolude hiding ((<|>), many)
import Data.String (String)

import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

type Op a = Ex.Operator T.Text () Identity a
type Operators a = Ex.OperatorTable T.Text () Identity a

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "=",
    "=="
   ]

reservedNames :: [String]
reservedNames = [
    "let",
    "rec",
    "in",
    "fix",
    "if",
    "then",
    "else"
  ]

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser $ Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser T.Text
identifier = T.pack <$> Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semi :: Parser String
semi = Tok.semi lexer

comma :: Parser String
comma = Tok.comma lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

integer :: Parser Integer
integer = Tok.integer lexer
