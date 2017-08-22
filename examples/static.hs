{-# OPTIONS -F -pgmF arrowp-ext #-}
module Static where

import Control.Arrow

staticEx :: Arrow a => Bool -> a inp out -> a out () -> a inp out
staticEx outputResultsArg processor outputSink = proc inputs -> do
  results <- processor -< inputs
  if outputResultsArg
    then outputSink -< results
    else returnA -< ()
  returnA -< results

staticEx' :: Arrow a => Bool -> a inp out -> a out () -> a inp out
staticEx' outputResultsArg processor outputSink
  = ((processor >>> arr (\ results -> (results, results))) >>>
       (first
          (if outputResultsArg then outputSink else arr (\ results -> ()))
          >>> arr (\ (_, results) -> results)))
