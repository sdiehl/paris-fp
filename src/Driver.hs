module Driver (
  driver
) where

import Protolude
import Text.Show.Pretty (ppShow)

import qualified Parser
import qualified Infer
import qualified Pretty
import qualified Transform
import qualified JIT
import qualified Compiler
import LLVM.General.Pretty (ppllvm)

driver :: LText -> IO ()
driver expr =
  case Parser.parseModule "stdin" expr of
    Left err  -> print err
    Right ast -> do
      putText "====== Parser ====="
      putStrLn $ ppShow $ ast

      putText "===== Typechecker ====="
      let typechecked = Infer.inferProgram mempty ast
      putStrLn $ Pretty.pptenv $ typechecked

      putText "====== Transformation ====="
      let lifted = Transform.lambdaLift ast
      putStrLn $ Pretty.pprint lifted

      putText "====== Code Generation ====="
      -- Generate LLVM
      llast <- Compiler.codegen lifted
      putStrLn $ ppllvm llast

      putText "====== Execution ====="
      -- Compile LLVM
      JIT.runJIT llast
      return ()

