{-# LANGUAGE Arrows      #-}
{-# LANGUAGE QuasiQuotes #-}
module TH.TH where
import           Control.Arrow
import           Control.Arrow.QuasiQuoter
import           Language.Haskell.TH

while :: ArrowChoice a => a b Bool -> a b () -> a b ()
while p s = proc x -> do
		b <- p -< x
		if b then do
				s -< x
				while p s -< x
			else
				returnA -< ()

test p s = [proc| x -> do
		b <- p -< x
		if b then do
				s -< x
				while p s -< x
			else
				returnA -< ()
            |]
