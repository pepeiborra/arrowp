> module Circuits where

> import Control.Arrow
> import Control.Arrow.Transformer.Automaton
> import Control.Arrow.Transformer.Stream
> import Data.Monoid
> import Data.Stream

Some simple example circuits

A resettable counter (first example in several Hawk papers):

> counter :: ArrowCircuit a => a Bool Int
> counter = proc reset -> do
>	rec	output <- returnA -< if reset then 0 else next
>		next <- delay 0 -< output+1
>	returnA -< output

Some other basic circuits from the Hawk library.

flush: when reset is True, return d for n ticks, otherwise copy value.
(a variation on the resettable counter)

> flush :: ArrowCircuit a => Int -> b -> a (b, Bool) b
> flush n d = proc (value, reset) -> do
>	rec	count <- returnA -< if reset then n else max (next-1) 0
>		next <- delay 0 -< count
>	returnA -< if count > 0 then d else value

latch: on each tick, return the last value for which reset was True,
or init if there was none.
 
> latch :: ArrowCircuit a => b -> a (b, Bool) b
> latch init = proc (value, reset) -> do
>	rec	out <- returnA -< if reset then value else last
>		last <- delay init -< out
>	returnA -< out

Run a circuit on a partial input

> partRunAutomaton ::
>	(ArrowLoop a, ArrowApply a) => Automaton a b c -> a [b] [c]
> partRunAutomaton f = proc xs -> do
>	s <- (|runAutomaton (\x -> f -< x)|) (listToStream xs)
>	returnA -< take (length xs) (streamToList s)

> partRunStream :: ArrowLoop a => StreamArrow a b c -> a [b] [c]
> partRunStream f = proc xs -> do
>	s <- (|runStream (\x -> f -< x)|) (listToStream xs)
>	returnA -< take (length xs) (streamToList s)

Some tests using the counter

> test_input = [True, False, True, False, False, True, False, True]

A test of the resettable counter.

> ts = partRunStream counter test_input

> ta = partRunAutomaton counter test_input

A step function (cf current in Lustre)

> step :: ArrowCircuit a => b -> a (Either b c) b
> step b = proc x -> do
>		rec	last_b <- delay b -< getLeft last_b x
>		returnA -< last_b
>	where	getLeft _ (Left b) = b
>		getLeft b (Right _) = b
