module Compiler (
  pipeline,
  {-bc,-}
  {-full,-}
) where

import Protolude hiding (local, void)

import Parser
import Syntax
import Codegen
import qualified Infer
import qualified Pretty
import qualified Transform
import qualified JIT

import LLVM.General.AST.Type
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP

import LLVM.General.Pretty (ppllvm)

import Text.Show.Pretty
import qualified Data.Map as Map

pipeline :: LText -> IO ()
pipeline expr = do
  case parseModule "stdin" expr of
    Left err  -> print err
    Right ast -> do
      let tcheck = Infer.inferProgram mempty ast
      putText "Typechecked"
      {-putStrLn $ ppShow $ tcheck-}

      let lifted = Transform.lambdaLift ast
      putStrLn $ Pretty.pprint lifted

      -- Generate LLVM
      llast <- codegen lifted

      -- Compile LLVM
      JIT.runJIT llast
      return ()

initModule :: AST.Module
initModule = emptyModule "Petit Module"

codegen :: Program -> IO (AST.Module)
codegen prog = return (runLLVM initModule (codegenProg prog))

codegenProg :: Program -> LLVM ()
codegenProg (Program decls) = mapM_ codegenDecl decls

codegenDecl :: Decl -> LLVM ()
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

viewApp :: Expr -> (Expr, [Expr])
viewApp = go []
  where
    go xs (App a b) = go (b : xs) a
    go xs f = (f, xs)

codegenExpr :: Expr -> Codegen AST.Operand
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
    return $ cons $ C.Int 64 (fromIntegral x)

  Lit (LBool x) -> do
    return $ cons $ C.Int 1 (bool 0 1 x)
  
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

  Lam x1 x2    -> do
    panic "Lambda expression not lifted."

  Prim x       -> do
    panic "Prim operation not converted."

  Let x1 x2 x3 -> do
    panic "Not implemented"
