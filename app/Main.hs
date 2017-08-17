{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Arrow.Notation
import           Control.Monad
import           Debug.Hoed.Pure
import           Language.Haskell.Exts
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf

usage :: String -> String
usage progName = unlines [
  "usage: " ++ progName ++
  " [FILENAME] [SOURCE] [DEST]",
  "Read arrow notation from SOURCE (derived from FILENAME) and write",
  "standard Haskell to DEST.",
  "If no FILENAME, use SOURCE as the original name.",
  "If no DEST or if DEST is `-', write to standard output.",
  "If no SOURCE or if SOURCE is `-', read standard input."
  ]

main :: IO ()
main = runO $ do
  args <- getArgs
  let exts =
        [
          EnableExtension Arrows
        , EnableExtension DatatypeContexts
        , EnableExtension FlexibleContexts
        , EnableExtension FlexibleInstances
        , EnableExtension FunctionalDependencies
        , EnableExtension GADTs
        , EnableExtension MultiParamTypeClasses
        , EnableExtension MultiWayIf
        , EnableExtension FunctionalDependencies
        , EnableExtension TypeFamilies
        , EnableExtension RecordWildCards
        , EnableExtension ScopedTypeVariables
        , EnableExtension LambdaCase
        ]
  progName <- getProgName
  (orig, inp, out) <- case args of
    ["--help"] -> do
      putStrLn $ usage progName
      exitSuccess
    []     -> return ("input",Nothing,Nothing)
    [i]    -> return (i, Just i, Nothing)
    [i,o]  -> return (i, Just i, Just o)
    [orig,i,o] -> return (orig, Just i, Just o)
    _ -> do
      putStrLn $ usage progName
      error "Unrecognized set of command line arguments"
  hIn  <- maybe (return stdin)  (`openFile` ReadMode) inp
  hOut <- maybe (return stdout) (`openFile` WriteMode) out
  contents <- hGetContents hIn
  case parseFileContentsWithExts exts contents of
        ParseFailed SrcLoc{..} err -> do
          printf "Parse error at %s:%d:%d: %s" orig srcLine srcColumn err
          exitFailure
        ParseOk x -> do
          let x' = translateModule (void x)
          hPutStr hOut $ prettyPrint x'
