{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Entry (
  main,
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.List (isPrefixOf)

import Control.Monad.Trans
import System.Console.Repline

import Compiler


processFile :: FilePath -> IO ()
processFile fname = do
  contents <- readFile fname
  pipeline (fromStrict contents)

type Repl a = HaskelineT IO a

-- Evaluation
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Prefix tab completeter
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
  ]

-- Default tab completer
byWord :: Monad m => WordCompleter m
byWord n = do
  let names = [":load"]
  return $ filter (isPrefixOf n) names

load :: [String] -> Repl ()
load [fname] = liftIO $ processFile fname
load _ = putText "Invalid filename"

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", load)
  ]

init :: Repl ()
init = do
  banner <- liftIO $ readFile "misc/logo"
  liftIO $ putStrLn banner

main :: IO ()
main = evalRepl ">> " cmd opts (Prefix (wordCompleter byWord) defaultMatcher) init
