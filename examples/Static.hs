{-# OPTIONS -F -pgmF arrowp-ext #-}
module Static where

import Control.Arrow

ifEx :: Arrow a => Bool -> a inp out -> a out () -> a inp out
ifEx outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  if outputResultsArg
    then outputSink -< results
    else returnA -< ()
  returnA -< results

ifEx' :: Arrow a => Bool -> a inp out -> a out () -> a inp out
ifEx' outputResultsArg processor outputSink
  = ((processor >>> arr (\ results -> (results, results))) >>>
       (first
          (if outputResultsArg then outputSink else arr (\ results -> ()))
          >>> arr (\ (_, results) -> results)))

caseEx :: Arrow a => Bool -> a inp out -> a out () -> a inp out
caseEx outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  case outputResultsArg of
    True -> outputSink -< results
    False -> returnA -< ()
  returnA -< results
