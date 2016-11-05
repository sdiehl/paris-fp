module Types ( 
  MType(..),
  PType(..),
  Env,
  intTy,
  boolTy,
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
