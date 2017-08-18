> {-# OPTIONS -F -pgmF arrowp-ext #-}

LL(1) parser combinators: an arrow-ized (and greatly cut-down) version
of those of "Deterministic, Error-Correcting Combinator Parsers", by
Swierstra and Duponcheel.  This version uses statically constructed
parse tables, but doesn't do error correction.

> module Parser(
>	Symbol(eof), Sym(Sym),
>	Parser, symbol, runParser
> ) where

> import Control.Arrow
> import Control.Arrow.Operations
> import Control.Arrow.Transformer
> import Control.Arrow.Transformer.Error
> import Control.Arrow.Transformer.State
> import Control.Arrow.Transformer.Static
> import Control.Category
> import Prelude hiding (id, (.))

We require a symbol for EOF, distinguished from all existing symbols:

> class (Ord s, Show s) => Symbol s where
>	eof :: s

Combine a token with other information

> data Sym s v = Sym s v

> token (Sym s _) = s
> value (Sym _ v) = v

> instance (Show s, Show v) => Show (Sym s v) where
>	showsPrec p (Sym s v) = showParen True
>		(shows s . showString ", " . shows v)

> eofSym :: Symbol s => Sym s v
> eofSym = Sym s (error (show s ++ " has no value"))
>	where	s = eof

A dynamic parser may fail or transform a list of symbols.

> type DynamicParser s v a = StateArrow [Sym s v] (ErrorArrow String a)

> liftDynamic :: ArrowChoice a => a b c -> DynamicParser s v a b c
> liftDynamic f = lift (lift f)

The auxilliary definitions fetchHead and advance, with their explicit
type signatures, are needed to avoid nasty type errors.

> fetchHead :: ArrowChoice a => DynamicParser s v a b (Sym s v)
> fetchHead = proc _ -> do
>		(s:_) <- fetch -< ()
>		returnA -< s

> getToken :: ArrowChoice a => DynamicParser s v a b s
> getToken = fetchHead >>> arr token

> advance :: ArrowChoice a => DynamicParser s v a b (Sym s v)
> advance = proc _ -> do
>		(s:ss) <- fetch -< ()
>		store -< ss
>		returnA -< s

The dynamic symbol parser ignores the symbol, as it will already have
been checked by the table lookup.

> unitDP :: ArrowChoice a => DynamicParser s v a b v
> unitDP = advance >>> arr value

Use the static information to construct a dynamic parser.

If the table is empty, we know statically that any lookup will fail.

> mkDynamic :: (Symbol s, ArrowChoice a) =>
>	Maybe (a b c) -> Table s (DynamicParser s v a b c) ->
>		DynamicParser s v a b c
> mkDynamic Nothing t = arr id &&& getToken >>> lookupTable t err
>	where	err = proc _ -> raise -< "expected " ++ show (keys t)
> mkDynamic (Just f) t
>	| isEmptyTable t = liftDynamic f
>	| otherwise = arr id &&& getToken >>> lookupTable t base
>	where	base = proc (b, _) -> liftDynamic f -< b

If a parser arrow can recognize the empty string, it needs a function
to transform input to output.

> data Parser s v a b c = SP {
>		emptyP :: StaticMonadArrow Maybe a b c,
>		table :: Table s (DynamicParser s v a b c),
>		dynamic :: DynamicParser s v a b c
>				-- dynamic = mkDynamic empty table
>	}

> mkParser :: (Symbol s, ArrowChoice a) =>
>	StaticMonadArrow Maybe a b c -> Table s (DynamicParser s v a b c) ->
>		Parser s v a b c
> mkParser e t = SP {
>		emptyP = e,
>		table = t,
>		dynamic = mkDynamic (unwrapM e) t
>	}

> symbol :: (Symbol s, ArrowChoice a) => s -> Parser s v a b v
> symbol s = mkParser (wrapM Nothing) (unitTable s unitDP)

> eofParser :: (Symbol s, ArrowChoice a) => Parser s v a b b
> eofParser = proc x -> do
>	symbol eof -< ()
>	returnA -< x

> instance (Symbol s, ArrowChoice a) => Arrow (Parser s v a) where
>	arr f = mkParser (arr f) emptyTable
>	first (SP{emptyP = e, table = t}) =
>		mkParser (first e) (fmap first t)

> instance (Symbol s, ArrowChoice a) => Category (Parser s v a) where
>       id = arr id
>	~SP{emptyP = e2, table = t2, dynamic = d2} . SP{emptyP = e1, table = t1} =
>		if isEmptyTable common
>		then mkParser (e1 >>> e2) (plusTable t1' t2')
>		else error ("parse conflict (concatenation) on " ++
>				show (keys common))
>		where	common = intersectTable t1' t2'
>			t1' = fmap (>>> d2) t1
>			t2' = seqEmptyTable (unwrapM e1) t2
>			seqEmptyTable Nothing _ = emptyTable
>			seqEmptyTable (Just f) t = fmap (liftDynamic f >>>) t

> instance (Symbol s, ArrowChoice a) => ArrowZero (Parser s v a) where
>	zeroArrow = mkParser (wrapM Nothing) emptyTable

> instance (Symbol s, ArrowChoice a) => ArrowPlus (Parser s v a) where
>	SP{emptyP = e1, table = t1} <+> SP{emptyP = e2, table = t2} =
>		if isEmptyTable common
>		then mkParser (wrapM (plusEmpty (unwrapM e1) (unwrapM e2)))
>			      (plusTable t1 t2)
>		else error ("parse conflict (union) on " ++ show (keys common))
>		where	common = intersectTable t1 t2
>			plusEmpty Nothing e2 = e2
>			plusEmpty e1 Nothing = e1
>			plusEmpty _ _ = error "Empty-Empty"

> instance (Symbol s, ArrowChoice a, ArrowLoop a) =>
>		ArrowLoop (Parser s v a) where
>	loop (SP{emptyP = e, table = t}) =
>		mkParser (wrapM (fmap loop (unwrapM e))) (fmap loop t)

Run a parser on a complete input

> runParser :: (Symbol s, ArrowChoice a) =>
>	Parser s v a () b -> ErrorArrow String a [Sym s v] b
> runParser p = proc ss -> do
>			(v, _) <- rp -< ((), ss ++ [eofSym])
>			returnA -< v
>		where	rp = runState (dynamic (p >>> eofParser))

general combinators

> option :: ArrowPlus a => (b -> c) -> a b c -> a b c
> option f p = arr f <+> p

> many :: ArrowPlus a => a b c -> a b [c]
> many p = option (const []) (some p)

> some :: ArrowPlus a => a b c -> a b [c]
> some p = some_p
>	where	some_p = proc b -> do
>			c <- p -< b
>			cs <- many_p -< b
>			returnA -< c:cs
>		many_p = option (const []) (some_p)

A different design:

> optional :: ArrowPlus a => a b b -> a b b
> optional p = arr id <+> p

> star :: ArrowPlus a => a b b -> a b b
> star p = p' where p' = optional (p >>> p')

> plus :: ArrowPlus a => a b b -> a b b
> plus p = p' where p' = p >>> optional p'

Tables.

During parser construction, these are represented as lists of pairs,
ordered by the key.  Then these are transformed into balanced search
trees for use in parsing.

> newtype Table k v = Table [(k, v)]

> emptyTable :: Table k v
> emptyTable = Table []

> unitTable :: k -> v -> Table k v
> unitTable k v = Table [(k, v)]

> instance Functor (Table k) where
>	fmap f (Table kvs) = Table [(k, f v) | (k, v) <- kvs]

Combine two tables.  In case of conflicts, the first takes precedence.

> plusTable :: Ord k => Table k v -> Table k v -> Table k v
> plusTable (Table t1) (Table t2) = Table (merge t1 t2)
>	where	merge [] kvs2 = kvs2
>		merge kvs1 [] = kvs1
>		merge (kvs1@(p1@(k1, _):kvs1')) (kvs2@(p2@(k2, _):kvs2')) =
>			case compare k1 k2 of
>			LT -> p1:merge kvs1' kvs2
>			EQ -> p1:merge kvs1' kvs2'
>			GT -> p2:merge kvs1 kvs2'

> intersectTable :: Ord k => Table k v1 -> Table k v2 -> Table k (v1, v2)
> intersectTable (Table t1) (Table t2) = Table (merge t1 t2)
>	where	merge [] _ = []
>		merge _ [] = []
>		merge (kvs1@((k1, v1):kvs1')) (kvs2@((k2, v2):kvs2')) =
>			case compare k1 k2 of
>			LT -> merge kvs1' kvs2
>			EQ -> (k1, (v1, v2)):merge kvs1' kvs2'
>			GT -> merge kvs1 kvs2'

> isEmptyTable :: Table k v -> Bool
> isEmptyTable (Table t) = null t

> keys :: Table k v -> [k]
> keys (Table kvs) = map fst kvs

> data SearchTree k v = Empty | Node (SearchTree k v) k v (SearchTree k v)

Make a balanced search tree from a table

> searchTree :: Ord k => Table k v -> SearchTree k v
> searchTree (Table kvs) = fst (mkTree (length kvs) kvs)
>	where	mkTree :: Int -> [(k,v)] -> (SearchTree k v, [(k,v)])
>		mkTree n kvs = if n == 0 then (Empty, kvs)
>			else	let	size_l = (n-1) `div` 2
>					(l, (k, v):kvs') = mkTree size_l kvs
>					(r, kvs'') = mkTree (n-1-size_l) kvs'
>				in (Node l k v r, kvs'')

Construct an arrow that searches for dynamic inputs in the statically
constructed tree of arrows.

> lookupTable :: (ArrowChoice a, Ord k) =>
>	Table k (a b c) -> a (b, k) c -> a (b, k) c
> lookupTable t def = look (searchTree t)
>	where	look Empty = def
>		look (Node l k a r) = proc (v, x) -> case compare x k of
>			LT -> look l -< (v, x)
>			EQ -> a -< v
>			GT -> look r -< (v, x)
