{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS -F -pgmF arrowp-ext #-}
-- CGI library from Hughes's paper (s.10)

-- This is just an exploratory formulation: it type-checks, but
-- it doesn't actually do anything.

module CGI where

-- Note that this requires a special generalized version of the arrow
-- library, with a special class for first and associates.


import           ArrowContext

data ScriptState
	= Ask
	| Comp (Either ScriptState ScriptState)
	| First ScriptState String

data Input a = Initial a | Resume ScriptState String
data Output a = Final a | Suspend ScriptState String

newtype CGIFunctor a b c = CGI (a (Input b) (Output c))

example :: (ArrowChoice a, ArrowContext a String) => CGIFunctor a b String
example = proc z -> do
		q <- ask -< "What is your question?"
		ans <- ask -< q
		returnA -< "The answer to \"" ++ q ++ "\" is " ++ ans

liftCGI :: ArrowChoice a => a b c -> CGIFunctor a b c
liftCGI f = CGI $ proc (Initial b) -> do
		c <- f -< b
		returnA -< Final c

ask :: ArrowChoice a => CGIFunctor a String String
ask = CGI $ proc x -> case x of
	Initial q -> returnA -< Suspend Ask q
	Resume Ask a -> returnA -< Final a

instance ArrowChoice a => Arrow (CGIFunctor a) where
	arr f = CGI $ proc (Initial b) ->
		returnA -< Final (f b)

	CGI f >>> CGI g = CGI $ proc z -> case z of
			Initial b -> enterf -< Initial                 b
			Resume (Comp (Left s)) a -> enterf -< Resume s a
			Resume (Comp (Right s)) a -> enterg -< Resume s a
		where	enterf = proc x -> do
				y <- f -< x
				case y of
					Final c -> enterg -< Initial c
					Suspend s q -> returnA -<
						Suspend (Comp (Left s)) q
			enterg = proc x -> do
				y <- g -< x
				returnA -< case y of
					Final d -> Final d
					Suspend s q ->
						Suspend (Comp (Right s)) q

instance (ArrowChoice a, ArrowContext a d, Read d, Show d) =>
		ArrowContext (CGIFunctor a) d where
	first (CGI f) = CGI $ proc x -> do
			let (b', d) = case x of
				Initial (b, d) -> (Initial b, d)
				Resume (First s v) a -> (Resume s a, read v)
			c' <- f -< b'
			returnA -< case c' of
				Final c -> Final (c, d)
				Suspend s q -> Suspend (First s (show d)) q


instance ArrowChoice a => ArrowChoice (CGIFunctor a) where
	left (CGI f) = CGI $ proc x -> case x of
			Initial (Left b) -> enterf -< Initial  b
			Initial (Right d) -> returnA -< Final (Right d)
			Resume s a -> enterf -< Resume s       a
		where	enterf = proc x -> do
				y <- f -< x
				case y of
					Final c -> returnA -< Final (Left c)
					Suspend s q -> returnA -< Suspend s q
