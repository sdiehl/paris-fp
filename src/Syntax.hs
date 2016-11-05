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

data Expr
  = Var Name
  | App Expr Expr
  | Lam [Name] Expr
  | Let Name Expr Expr
  | Lit Literal
  | If Expr Expr Expr
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

data Decl
  = FunDecl Name [Name] Expr
  deriving (Eq, Show)

data Program = Program [Decl]
  deriving (Show, Eq)

mkApp :: Expr -> [Name] -> Expr
mkApp e vs = foldl' App e (fmap Var vs)

remove :: Eq a => [a] -> [a] -> [a]
remove = foldr (filter . (/=))

globals :: Program -> [Name]
globals (Program decls) = [nm | FunDecl nm _ _ <- decls]

fv :: Expr -> [Name]
fv (Var x)       = [x]
fv (Lam xs e)    = (fv e) `remove` xs
fv (App e1 e2)   = (fv e1) `union` (fv e2)
fv (Lit n)       = []
fv (Prim n)      = []
fv (Let x e1 e2) = fv e1 `union` delete x (fv e2)
fv (If e1 e2 e3) = fv e1 `union` fv e2 `union` fv e3
