{-# LANGUAGE Arrows #-}
module GHC where

import Control.Arrow

ghc,trivial :: Arrow a => a inp chunk -> a chunk out -> a inp out
trivial chunk process = proc inputs -> do
   chunked <- chunk -< inputs
   results <- process -< chunked
   returnA -< results

ghc chunk process =
  arr(\inputs -> (inputs,inputs)) >>> first chunk >>> first process >>> arr fst
