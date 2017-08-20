{- LANGUAGE Arrows -}
{-# OPTIONS -F -pgmF arrowp-ext #-}

module Conditional where

import           Control.Arrow

-- An Arrow command for use with banana brackets?
static :: Arrow a => Bool -> a b c -> a b d -> a (c,d) c -> a b c
static use abc abd adc = proc b -> do
  c <- abc -< b
  res <- if use -- :: a (b,c) c
     then do -- :: a (b,c) c
       d <- abd -< b
       adc -< (c,d)
     else returnA -< c -- :: a (b,c) c
  returnA -< res

simple :: Bool -> a b c -> a b c
simple use abc = proc b -> do
  if use then abc -< b else abc -< b
