-- A Haskell-98-compatible subset of the Control.Monad.State module.

module State(State, runState, get, put) where

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
	return x = State (\n -> (x, n))
	State v >>= f = State (\n -> let (x, n') = v n in runState (f x) n')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))
