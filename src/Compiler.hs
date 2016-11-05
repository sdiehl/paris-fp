module Compiler (
  codegen,
) where

import Protolude hiding (local, void)

import Syntax
import Codegen

import LLVM.General.AST.Type
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP

import qualified Data.Map as Map

initModule :: AST.Module
initModule = emptyModule "Petit Module"

codegen :: Program Name -> IO (AST.Module)
codegen prog = return (runLLVM initModule (codegenProg prog))

codegenProg :: Program Name -> LLVM ()
codegenProg (Program decls) = mapM_ codegenDecl decls

codegenDecl :: Decl Name -> LLVM ()
codegenDecl (FunDecl fn args body) = do
  let args' = fmap codegenArg args
  define int (toS fn) args' $ do

    forM args $ \a -> do
       let aname = toS a 
       assign aname (local int (AST.Name aname))

    body' <- codegenExpr body
    ret body'

codegenArg :: Syntax.Name -> (AST.Type, AST.Name)
codegenArg nm = (int, AST.Name (toS nm))

viewApp :: Expr Name -> (Expr Name, [Expr Name])
viewApp = go []
  where
    go xs (App a b) = go (b : xs) a
    go xs f = (f, xs)

codegenExpr :: Expr Name -> Codegen AST.Operand
codegenExpr = \case

  Var x -> do
    getvar (toS x)

  App (Var f) arg -> do
    arg' <- codegenExpr arg
    call (externf (fn int [int]) (AST.Name (toS f))) [arg']

  a@(App _ b) -> do
    let (f, args) = viewApp a
    case f of 
      Var f' -> do
        args' <- mapM codegenExpr args
        -- XXX: generalize type
        call (externf (fn int [int, int]) (AST.Name (toS f'))) args'

      Prim Add -> do
        [a,b] <- mapM codegenExpr args
        add a b

      Prim Mul -> do
        [a,b] <- mapM codegenExpr args
        mul a b

      Prim Sub -> do
        [a,b] <- mapM codegenExpr args
        sub a b

      Prim Eql -> do
        [a,b] <- mapM codegenExpr args
        icmp IP.EQ a b

  Lit (LInt x) -> do
    let n = C.Int 64 (fromIntegral x)
    return $ cons n

  Lit (LBool x) -> do
    let n = C.Int 1 (bool 0 1 x)
    return (cons n)
  
  If cond tr fl  -> do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    -- %entry
    ------------------
    cond <- codegenExpr cond
    test <- icmp IP.EQ true cond
    cbr test ifthen ifelse

    -- if.then
    ------------------
    setBlock ifthen
    trval <- codegenExpr tr
    br ifexit
    ifthen <- getBlock

    -- if.else
    ------------------
    setBlock ifelse
    flval <- codegenExpr fl
    br ifexit
    ifelse <- getBlock

    -- if.exit
    ------------------
    setBlock ifexit
    phi int [(trval, ifthen), (flval, ifelse)]

  -- Internal failures, shouldn't be possible if transformations are performed.

  Lam x1 x2    -> do
    panic "Lambda expression not lifted."

  Prim x       -> do
    panic "Prim operation not converted."
