module Transform (closureConversion, lambdaLift) where

import Syntax

without :: Eq a => [a] -> [a] -> [a]
without = foldr (filter . (/=))

freeVars :: Expr -> [Var]
freeVars (Lam vars e) = freeVars e `without` vars
freeVars (App e1 e2) = freeVars e1 <> freeVars e2
freeVars (Let var e1 e2) =
  (freeVars e1 <> freeVars e2) `without` [var]
freeVars (Lit _) = []
freeVars (Prim _) = []
freeVars (Var v) = [v]

applyTo :: Expr -> [Var] -> Expr
applyTo = foldl' $ \e v -> App e (Var v)

closureConversion ::
  [Var] ->
  Expr ->
  Expr
closureConversion globals = go
  where
    go (Lam vs e) = do
      let vars = freeVars e `without` (globals ++ vs)
      Lam (vars ++ vs) (closureConversion globals e) `applyTo` vars
    go (Let v e1 e2) = do
      let var1 = freeVars e1 `without` (v : globals)
          var2 = freeVars e2 `without` (v : globals)
      Let
        v
        (closureConversion globals e1 `applyTo` var1)
        (closureConversion globals e2 `applyTo` var2)
    go (App e1 e2) =
      go $
        App
          (closureConversion globals e1)
          (closureConversion globals e2)
    go (If c e1 e2) =
      go $
        If
          (closureConversion globals c)
          (closureConversion globals e1)
          (closureConversion globals e2)
    go x = x

type Lift = WriterT [Def] (State Int)

runLift :: Lift a -> (a, [Def])
runLift action = evalState (runWriterT action) 0

freshVar :: MonadState Int m => m Int
freshVar = get <* modify (+ 1)

-- | Generate top-level definitions
lambdaLift :: Expr -> Lift Expr
lambdaLift (App e1 e2) =
  App <$> lambdaLift e1
    <*> lambdaLift e2
lambdaLift (If c e1 e2) =
  If <$> lambdaLift c
    <*> lambdaLift e1
    <*> lambdaLift e2
lambdaLift (Lam vars e) = do
  fresh <- Gen <$> freshVar
  next <- lambdaLift e
  tell [Def fresh vars next]
  pure (Var fresh)
lambdaLift (Let var e1 e2) =
  Let var <$> lambdaLift e1
    <*> lambdaLift e2
lambdaLift x = pure x

-- | Makes top-level definitions
eliminateLambdas ::
  [Var] ->
  Def ->
  [Def]
eliminateLambdas globals (Def name vars e) =
  Def name vars modified : defs
  where
    (modified, defs) =
      runLift
        $ lambdaLift
        $ smash
        $ closureConversion globals e

-- | Removes extraneous Lambdas by consolidating variables
-- Enables more optimized codegen
smash :: Expr -> Expr
smash (Lam vars1 (Lam vars2 e)) =
  smash (Lam (vars1 <> vars2) e)
smash (App e1 e2) =
  App (smash e1) (smash e2)
smash x = x
