Basic definitions from "Generalising Monads to Arrows", by John Hughes,
but with the Arrow class split and generalized.

> module ArrowContext where

> import Control.Monad

> infixr 5 <+>
> infixr 3 ***
> infixr 3 &&&
> infixr 2 +++
> infixr 2 |||
> infixr 1 >>>

Basic arrow definitions (s4.1)

> class Arrow a where
>	arr :: (b -> c) -> a b c
>	(>>>) :: a b c -> a c d -> a b d

> class Arrow a => ArrowContext a d where
>	first :: a b c -> a (b,d) (c,d)

> newtype Kleisli m a b = Kleisli (a -> m b)

> instance Monad m => Arrow (Kleisli m) where
>	arr f = Kleisli (return . f)
>	(Kleisli f) >>> (Kleisli g) = Kleisli (\b -> f b >>= g)

> instance Monad m => ArrowContext (Kleisli m) d where
>	first (Kleisli f) = Kleisli (\(b,d) -> f b >>= \c -> return (c,d))

> second :: ArrowContext a d => a b c -> a (d,b) (d,c)
> second f = arr swap >>> first f >>> arr swap
>		where	swap ~(x,y) = (y,x)

> (***) :: (ArrowContext a b', ArrowContext a c) =>
>	a b c -> a b' c' -> a (b,b') (c,c')
> f *** g = first f >>> second g

> (&&&) :: (ArrowContext a b, ArrowContext a c) =>
>	a b c -> a b c' -> a b (c,c')
> f &&& g = arr (\b -> (b,b)) >>> f *** g

> liftA2 :: (ArrowContext a b, ArrowContext a e) =>
>	(b -> c -> d) -> a e b -> a e c -> a e d
> liftA2 op f g = f &&& g >>> arr (uncurry op)

ArrowPlus and ArrowZero (s4.2)

> class Arrow a => ArrowZero a where
>	zeroArrow :: a b c

> class ArrowZero a => ArrowPlus a where
>	(<+>) :: a b c -> a b c -> a b c

> instance MonadPlus m => ArrowZero (Kleisli m) where
>	zeroArrow = Kleisli (\x -> mzero)

> instance MonadPlus m => ArrowPlus (Kleisli m) where
>	Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

Conditionals (s5.1)

> class Arrow a => ArrowChoice a where
>	left :: a b c -> a (Either b d) (Either c d)

> instance Monad m => ArrowChoice (Kleisli m) where
>	left (Kleisli f) = Kleisli g
>		where	g (Left b) = f b >>= (return . Left)
>			g (Right d) = return (Right d)

> right :: ArrowChoice a => a b c -> a (Either d b) (Either d c)
> right f = arr mirror >>> left f >>> arr mirror
>		where	mirror (Left x) = Right x
>			mirror (Right y) = Left y

> (+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c')
> f +++ g = left f >>> right g

> (|||) :: ArrowChoice a => a b d -> a c d -> a (Either b c) d
> f ||| g = f +++ g >>> arr untag
>		where	untag (Left x) = x
>			untag (Right y) = y

> test :: (ArrowContext a Bool, ArrowContext a b) =>
>	a b Bool -> a b (Either b b)
> test f = f &&& arr id >>> arr (\(b, x) -> if b then Left x else Right x)

Application (s5.2)

> class Arrow a => ArrowApply a where
>	app :: a (a b c, b) c

> leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
> leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
>		 (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app

> instance Monad m => ArrowApply (Kleisli m) where
>	app = Kleisli (\(Kleisli f, x) -> f x)

> newtype ArrowApply a => ArrowMonad a b = ArrowMonad (a () b)

> instance ArrowApply a => Monad (ArrowMonad a) where
>	return x = ArrowMonad (arr (\z -> x))
>	ArrowMonad m >>= f = ArrowMonad (m >>>
>			arr (\x -> let ArrowMonad h = f x in (h, ())) >>>
>			app)

The ordinary function type (s7)

> instance Arrow (->) where
>	arr f = f
>	f >>> g = g . f

> instance ArrowContext (->) a where
>	first f (x,y) = (f x, y)

> instance ArrowChoice (->) where
>	left f (Left x) = Left (f x)
>	left f (Right y) = Right y

> instance ArrowApply (->) where
>	app (f,x) = f x

Auxiliary functions used in stating laws (s7)

> assoc :: ((a,b),c) -> (a,(b,c))
> assoc ~(~(x,y),z) = (x,(y,z))

> assocsum :: Either (Either a b) c -> Either a (Either b c)
> assocsum (Left (Left x)) = Left x
> assocsum (Left (Right y)) = Right (Left y)
> assocsum (Right z) = Right (Right z)

Another definition used by the arrow notation.

> returnA :: Arrow a => a b b
> returnA = arr id
