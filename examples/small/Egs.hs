{- LANGUAGE Arrows -}
{-# OPTIONS -F -pgmF arrowp-ext #-}
module Egs where

import           Control.Arrow

addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = proc x -> do
		y <- f -< x
		z <- g -< x
		returnA -< y + z

while :: ArrowChoice a => a b Bool -> a b () -> a b ()
while p s = proc x -> do
		b <- p -< x
		if b then do
				s -< x
				while p s -< x
			else
				returnA -< ()
