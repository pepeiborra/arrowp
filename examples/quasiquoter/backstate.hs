{-# LANGUAGE QuasiQuotes#-}
module BackStateArrow where

import Control.Arrow
import Control.Arrow.QuasiQuoter
import Control.Category
import Prelude hiding (id, (.))

-- Generalizing the backwards state transformer monad mentioned
-- in Wadler's "The Essence of Functional Programming"

newtype BackStateArrow s a b c = BST (a (b,s) (c,s))

instance ArrowLoop a => Category (BackStateArrow s a) where
	BST g . BST f = BST $ [proc| (b, s) -> do
			rec	(c, s'') <- f -< (b, s')
				(d, s') <- g -< (c, s)
			returnA -< (d, s'')|]

instance ArrowLoop a => Arrow (BackStateArrow s a) where
	arr f = BST [proc| (b, s) ->
			returnA -< (f b, s)|]
	first (BST f) = BST $ [proc| ((b, d), s) -> do
			(c, s') <- f -< (b, s)
			returnA -< ((c, d), s')|]

instance (ArrowLoop a, ArrowChoice a) => ArrowChoice (BackStateArrow s a) where
	left (BST f) = BST $ [proc| (x, s) ->
		case x of
			Left b -> do
				(c, s') <- f -< (b, s)
				returnA -< (Left c, s')
			Right d ->
				returnA -< (Right d, s) |]

instance ArrowLoop a => ArrowLoop (BackStateArrow s a) where
	loop (BST f) = BST $  [proc| (b, s) -> do
			rec	((c, d), s') <- f -< ((b, d), s)
			returnA -< (c, s') |]
