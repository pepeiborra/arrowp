{-# OPTIONS -F -pgmF arrowp-ext #-}
module Static where

import Control.Arrow

ifEx :: Arrow a => Bool -> a v v -> a v () -> a v v
ifEx outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  if outputResultsArg
    then outputSink -< results
    else returnA -< ()
  processor -< results

ifEx' :: Arrow a => Bool -> a v v -> a v v -> a v v
ifEx' outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  results <- if outputResultsArg
    then outputSink -< results
    else returnA -< results
  processor -< results

caseEx :: Arrow a => Bool -> a v v -> a v () -> a v v
caseEx outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  case outputResultsArg of
    True -> outputSink -< results
    False -> returnA -< ()
  processor -< results

caseEx' :: Arrow a => Bool -> a v v -> a v v -> a v v
caseEx' outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  results <- case outputResultsArg of
               True -> outputSink -< results
               False -> returnA -< results
  processor -< results
