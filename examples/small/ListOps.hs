{- LANGUAGE Arrows -}
{-# OPTIONS -F -pgmF arrowp-ext #-}
module ListOps where

-- generalizing map and filter to arrows

-- Note that these need ArrowChoice (because they use case) but
-- not ArrowApply (because the arrows used before -< don't involve
-- variables defined inside the arrow abstraction).

import           Control.Arrow

mapA :: ArrowChoice a => a (b, c) d -> a (b, [c]) [d]
mapA f = proc (env, xs) -> case xs of
		[] ->
			returnA -< []
		x:xs -> do
			y <- f -< (env, x)
			ys <- mapA f -< (env, xs)
			returnA -< y:ys

filterA :: ArrowChoice a => a (b, c) Bool -> a (b, [c]) [c]
filterA p = proc (env, xs) -> case xs of
		[] ->
			returnA -< []
		x:xs -> do
			b <- p -< (env, x)
			ys <- filterA p -< (env, xs)
			returnA -< if b then x:ys else ys
