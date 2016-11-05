{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Entry (
  main,
) where

import Protolude hiding (Prefix)

import Data.String (String)
import qualified Data.List as List

import System.Console.Repline

import Control.Monad.Trans
import qualified Control.Monad.State.Strict as State

import qualified Driver

type Repl a = HaskelineT (State.StateT FilePath IO) a

processFile :: FilePath -> IO ()
processFile fname = do
  contents <- readFile fname
  Driver.driver (fromStrict contents)

-- Evaluation
cmd :: String -> Repl ()
cmd input = return ()

-- Prefix tab completeter
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
  ]

-- Default tab completer
byWord :: Monad m => WordCompleter m
byWord n = do
  let names = [":load"]
  return $ filter (List.isPrefixOf n) names

load :: [String] -> Repl ()
load [fname] = liftIO $ processFile fname
load _ = putText "Invalid filename"

quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", load)
  , ("help", help)
  , ("quit", quit)
  ]

help :: [String] -> Repl ()
help _ = liftIO $ putStrLn $ List.unlines [
    ":load <file>       Load a program from file"
  , ":reload            Run the active file"
  , ":quit              Exit interpreter"
  ]

init :: Repl ()
init = liftIO $ do
  banner <- readFile "misc/logo"
  putStrLn banner
  processFile "example.pet"

main :: IO ()
main = 
  flip State.evalStateT "example.pet"
    $ evalRepl ">> " cmd opts (Prefix (wordCompleter byWord) defaultMatcher) init
