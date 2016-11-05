{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Infer (
  inferProgram,
) where

import Protolude
import Types
import Syntax
import Pretty

import Data.Set (Set)
import Data.Map (Map)
import Data.List (foldl1')
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Set as S

newtype Infer a = Infer (ExceptT Text (State Int) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError Text)

runInfer :: Infer a -> Either Text a
runInfer (Infer i) = evalState (runExceptT i) 0

type Subst = Map Name MType

class Substitutable a where
  free :: a -> Set Name
  subst :: Subst -> a -> a

instance Substitutable MType where
  free (TVar n) = S.singleton n
  free (Con _) = S.empty
  free (TArr t1 t2) = free t1 `S.union` free t2

  subst s (TVar n) = M.findWithDefault (TVar n) n s
  subst _ (Con n) = Con n
  subst s (TArr t1 t2) = TArr (subst s t1) (subst s t2)

instance Substitutable PType where
  free (Forall xs t) = free t `S.difference` S.fromList xs
  subst s (Forall xs t) = Forall xs (subst (foldr M.delete s xs) t)

instance Substitutable Env where
  free gamma = S.unions (map free (M.elems gamma))
  subst s gamma = M.map (subst s) gamma

none :: Subst
none = M.empty

comp :: Subst -> Subst -> Subst
comp s1 s2 = M.union s1 (M.map (subst s1) s2)

extends :: [(Name, PType)] -> Env -> Env
extends xs = M.union (M.fromList xs) 

-------------------------------------------------------------------------------
-- Inference Rules
-------------------------------------------------------------------------------

infer :: Env -> Expr Name -> Infer (Subst, MType)
infer gamma (Var x) = do
  sigma <- lookupEnv x gamma
  tau <- inst sigma
  pure (none, tau)

infer gamma (App e1 e2) = do
  (s1, tau1) <- infer gamma e1
  (s2, tau2) <- infer (subst s1 gamma) e2
  tau3 <- fresh
  s3 <- unify (subst s2 tau1) (TArr tau2 tau3) 
  pure (s3 `comp` s2 `comp` s1, subst s3 tau3)

infer gamma (Lam xs e) = do
  tau1 <- fresh
  ts <- replicateM (length xs) fresh
  let gamma' = extends (zip xs (fmap (Forall []) ts)) gamma
  (te, e') <- infer gamma' e
  pure (te, subst te (foldl1' TArr (tau1 : ts)))

infer gamma (Prim op) = 
  case op of
    Add -> return (none, intTy `TArr` intTy `TArr` intTy)
    Mul -> return (none, intTy `TArr` intTy `TArr` intTy)
    Sub -> return (none, intTy `TArr` intTy `TArr` intTy)
    Eql -> return (none, intTy `TArr` intTy `TArr` boolTy)

infer gamma (Lit val) = 
  case val of
    LInt _  -> pure (none, intTy)
    LBool _ -> pure (none, boolTy)

infer gamma (If cond tr fl) = do 
  (s1, tau1) <- infer gamma cond
  (s2, tau2) <- infer gamma tr
  (s3, tau3) <- infer gamma fl
  s4 <- unify tau1 boolTy
  s5 <- unify tau2 tau3
  pure (s5 `comp` s4 `comp` s3 `comp` s2 `comp` s1, subst s3 tau3)

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

unify :: MType -> MType -> Infer Subst
unify (TVar n) t = bind n t
unify t (TVar n) = bind n t
unify (Con n1) (Con n2) | n1 == n2 = return none
unify (TArr a1 a2) (TArr b1 b2) = do
  s1 <- unify a1 b1
  s2 <- unify (subst s1 a2) (subst s1 b2)
  return (s2 `comp` s1)
unify t1 t2 = cannotUnify t1 t2

bind :: Name -> MType -> Infer Subst
bind n (TVar n') | n == n'     = return none
bind n t | n `S.member` free t = noUnify n t
bind n t                       = return (M.singleton n t)

lookupEnv :: Name -> Env -> Infer PType
lookupEnv n gamma =
  case M.lookup n gamma of
    Nothing -> throwError $ "Unbound reference: " <> toS n
    Just t  -> return t

inst :: PType -> Infer MType
inst (Forall xs t) = do
  let go x = fresh >>= \v -> pure (x,v)
  assocs <- forM xs go
  return (subst (M.fromList assocs) t)

fresh :: Infer MType
fresh = do
  c <- get
  put (c + 1)
  return (TVar ("t" <> show c))

gen :: Env -> MType -> PType
gen gamma tau = Forall xs tau
  where xs = S.toList (free tau `S.difference` free gamma)

cannotUnify :: MType -> MType -> Infer a
cannotUnify t1 t2 = throwError $ "Cannot unify " <> toS (pprint t1) <> " and " <> toS (pprint t2)

noUnify :: Name -> MType -> Infer a
noUnify n t = throwError $ "Occurs check failed for " <> toS n <> " in " <> toS (pprint t)

typeOf :: Env -> Expr Name -> Either Text PType
typeOf gamma e = runInfer $ do 
  (_, t) <- infer gamma e
  return (gen gamma t)

inferProgram :: Env -> Program Name -> Either Text [(Name, PType)]
inferProgram gamma prog@(Program decls) = runInfer $ do
  let topNames = (globals prog)
  ftvs <- replicateM (length topNames) fresh
  let globalScope = M.fromList $ zip topNames (fmap (Forall []) ftvs)

  (s1, tys) <- mapAndUnzipM (inferDecl globalScope) decls
  unifiers <- zipWithM unify tys ftvs
  let unifier = foldl1' comp unifiers
  let sigs = ((gen globalScope . subst unifier) <$> tys)
  pure (zip topNames sigs)

inferDecl :: Env -> Decl Name -> Infer (Subst, MType)
inferDecl gamma (FunDecl nm _ body) = infer gamma body
