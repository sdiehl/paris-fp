module Syntax (
  Name,
  Expr(..),
  Literal(..),
  Prim(..),
  Decl(..),
  Program(..),
  mkApp,
  globals,
  remove,
  fv,
) where

import Protolude
import Prelude hiding (foldr)
import Data.List (union, delete)

type Name = LText

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam [a] (Expr a)
  | Lit Literal
  | If (Expr a) (Expr a) (Expr a)
  | Prim Prim
  deriving (Show, Eq, Ord)

data Literal
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Prim
  = Add
  | Sub
  | Mul
  | Eql
  deriving (Eq, Ord, Show)

data Decl a
  = FunDecl a [a] (Expr a)
  deriving (Eq, Show)

data Program a = Program [Decl a]
  deriving (Show, Eq)

mkApp :: Expr a -> [a] -> Expr a
mkApp e vs = foldl' App e (fmap Var vs)

remove :: Eq a => [a] -> [a] -> [a]
remove = foldr (filter . (/=))

globals :: Eq a => Program a -> [a]
globals (Program decls) = [nm | FunDecl nm _ _ <- decls]

fv :: Eq a => Expr a -> [a]
fv (Var x)       = [x]
fv (Lam xs e)    = (fv e) `remove` xs
fv (App e1 e2)   = (fv e1) `union` (fv e2)
fv (Lit n)       = []
fv (Prim n)      = []
fv (If e1 e2 e3) = fv e1 `union` fv e2 `union` fv e3
