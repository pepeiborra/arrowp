module Lift where

import Control.Arrow

-- lifting a binary operation to arrows (Hughes's paper, s4.1)

liftA2' :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2' op f g = proc x -> do
		y <- f -< x
		z <- g -< x
		returnA -< y `op` z
