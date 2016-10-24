module Types ( 
  MType(..),
  PType(..),
  Env,
  intTy,
  boolTy,

  pptype,
) where

import Protolude
import Data.String (String)
import Data.List
import Data.Map

import Syntax

-- Monotypes
data MType 
  = TVar Name
  | Con Name
  | TArr MType MType
  deriving (Show)

-- Polytypes
data PType = Forall [Name] MType
  deriving (Show)

type Env = Map Name PType

infixr `TArr`

intTy :: MType
intTy = Con "Int"

boolTy :: MType
boolTy = Con "Bool"

class Pretty a where
  pretty :: a -> String

instance Pretty PType where
  pretty (Forall [] t) = pretty t
  pretty (Forall xs t) = "forall " ++ intercalate " " (fmap toS xs) ++ " . " ++ pretty t

instance Pretty MType where
  pretty = go False
    where
      go :: Bool -> MType -> String
      go _ (TVar  i)   = toS i
      go _ (Con n)     = toS n
      go p (TArr t1 t2) = let b = go True t1 ++ " -> " ++ go False t2
                          in  if p then "(" ++ b ++ ")" else b

pptype :: Pretty a => a -> String
pptype = pretty
