> module ArrCode(
>	Arrow,
>	bind, anon,
>	arr, arrLet, (>>>), arrowExp, applyOp, infixOp, (|||), first,
>	VarDecl(VarDecl), letCmd,
>	context, anonArgs, toHaskell,
>	Tuple(..),
>	isEmptyTuple, unionTuple, minusTuple, intersectTuple,
>	patternTuple, expTuple,
>	returnA_exp, arr_exp, compose_op, choice_op, first_exp,
>	left_exp, right_exp, app_exp, loop_exp
> ) where

> import Utils

> import Data.Set (Set)
> import qualified Data.Set as Set
> import Language.Haskell.Syntax

> data Arrow = Arrow {
>		code :: Code,
>		context :: Tuple, -- named input components used by the arrow
>		anonArgs :: Int   -- number of unnamed arguments
>	}

> data VarDecl a = VarDecl SrcLoc HsName a
>	deriving (Eq,Show)

> instance Functor VarDecl where
>	fmap f (VarDecl loc name a) = VarDecl loc name (f a)

> data Code
>	= ReturnA			-- returnA = arr id
>	| Arr Int HsPat [Binding] HsExp	-- arr (first^n (\p -> ... e))
>	| Compose Code [Code] Code	-- composition of 2 or more elts
>	| Op HsExp [Code]		-- combinator applied to arrows
>	| InfixOp Code HsQOp Code
>	| Let [VarDecl Code] Code

> data Binding = BindLet [HsDecl] | BindCase HsPat HsExp

-----------------------------------------------------------------------------
Arrow constants

> compose_op, choice_op :: HsQOp
> returnA_exp, arr_exp, first_exp :: HsExp
> left_exp, right_exp, app_exp, loop_exp :: HsExp

> returnA_exp	= HsVar (UnQual (HsIdent "returnA"))
> arr_exp	= HsVar (UnQual (HsIdent "arr"))
> compose_op	= HsQVarOp (UnQual (HsSymbol ">>>"))
> choice_op	= HsQVarOp (UnQual (HsSymbol "|||"))
> first_exp	= HsVar (UnQual (HsIdent "first"))
> left_exp	= HsCon (UnQual (HsIdent "Left"))
> right_exp	= HsCon (UnQual (HsIdent "Right"))
> app_exp	= HsVar (UnQual (HsIdent "app"))
> loop_exp	= HsVar (UnQual (HsIdent "loop"))

-----------------------------------------------------------------------------
Arrow constructors

> bind :: Set HsName -> Arrow -> Arrow
> bind vars a = a {
>		context = context a `minusTuple` vars
>	}

> anon :: Int -> Arrow -> Arrow
> anon anonCount a = a {
>		anonArgs = anonArgs a + anonCount
>	}

> arr :: Int -> Tuple -> HsPat -> HsExp -> Arrow
> arr anons t p e = Arrow {
>		code = if same p e then ReturnA else Arr anons p [] e,
>		context = t `intersectTuple` freeVars e,
>		anonArgs = anons
>	}
>	where	same :: HsPat -> HsExp -> Bool
>		same (HsPApp n1 []) (HsCon n2) = n1 == n2
>		same (HsPVar n1) (HsVar n2) = UnQual n1 == n2
>		same (HsPTuple ps) (HsTuple es) =
>			length ps == length es && and (zipWith same ps es)
>		same (HsPAsPat n p) e = e == HsVar (UnQual n) || same p e
>		same (HsPParen p) e = same p e
>		same p (HsParen e) = same p e
>		same _ _ = False	-- other cases don't arise

> arrLet :: Int -> Tuple -> HsPat -> [HsDecl] -> HsExp -> Arrow
> arrLet anons t p ds e = Arrow {
>		code = Arr anons p [BindLet ds] e,
>		context = t `intersectTuple` vs,
>		anonArgs = anons
>	}
>	where	vs = (freeVars e `Set.union` freeVars ds)
>				`Set.difference` definedVars ds

> (>>>) :: Arrow -> Arrow -> Arrow
> a1 >>> a2 = a1 { code = compose (code a1) (code a2) }

> arrowExp :: HsExp -> Arrow
> arrowExp e = Arrow {
>		code = if e == returnA_exp then ReturnA else Op e [],
>		context = emptyTuple,
>		anonArgs = 0
>	}

> applyOp :: HsExp -> [Arrow] -> Arrow
> applyOp e as = Arrow {
>		code = Op e (map code as),
>		context = foldr unionTuple emptyTuple (map context as),
>		anonArgs = 0	-- BUG: see below
>	}

Setting anonArgs to 0 for infixOp is incorrect, but we can't know the
correct value without types.

> infixOp :: Arrow -> HsQOp -> Arrow -> Arrow
> infixOp a1 op a2 = Arrow {
>		code = InfixOp (code a1) op (code a2),
>		context = context a1 `unionTuple` context a2,
>		anonArgs = 0	-- BUG: as above
>	}

> first :: Arrow -> Tuple -> Arrow
> first a ps = Arrow {
>		code = Op first_exp [code a],
>		context = context a `unionTuple` ps,
>		anonArgs = 0
>	}

> (|||) :: Arrow -> Arrow -> Arrow
> a1 ||| a2 = Arrow {
>		code = InfixOp (code a1) choice_op (code a2),
>		context = context a1 `unionTuple` context a2,
>		anonArgs = 0
>	}

> letCmd :: [VarDecl Arrow] -> Arrow -> Arrow
> letCmd defs a = Arrow {
>		code = Let (map (fmap code) defs) (code a),
>		context = context a,
>		anonArgs = anonArgs a
>	}

Composition, with some simplification

> compose :: Code -> Code -> Code
> compose ReturnA a = a
> compose a ReturnA = a
> compose a1@(Arr n1 p1 ds1 e1) a2@(Arr n2 p2 ds2 e2)
>	| n1 /= n2 = Compose a1 [] a2	-- could do better, but can this arise?
>	| same p2 e1 = Arr n1 p1 (ds1 ++ ds2) e2
>	| otherwise = Arr n1 p1 (ds1 ++ BindCase p2 e1:ds2) e2
>	where	same :: HsPat -> HsExp -> Bool
>		same (HsPApp n1 []) (HsCon n2) = n1 == n2
>		same (HsPVar n1) (HsVar n2) = UnQual n1 == n2
>		same (HsPTuple ps) (HsTuple es) =
>			length ps == length es && and (zipWith same ps es)
>		same (HsPParen p) e = same p e
>		same p (HsParen e) = same p e
>		same _ _ = False	-- other cases don't arise
> compose (Compose f1 as1 g1) (Compose f2 as2 g2) =
>	Compose f1 (as1 ++ (compose g1 f2 : as2)) g2
> compose a (Compose f bs g) =
>	Compose (compose a f) bs g
> compose (Compose f as g) b =
>	Compose f as (compose g b)
> compose a1 a2 =
>	Compose a1 [] a2

-----------------------------------------------------------------------------
Conversion to Haskell

> toHaskell :: Arrow -> HsExp
> toHaskell = toHaskellCode . code

> toHaskellCode :: Code -> HsExp
> toHaskellCode ReturnA =
>	returnA_exp
> toHaskellCode (Arr n p bs e) =
>	HsApp arr_exp
>		(times n (HsParen . HsApp first_exp) body)
>	where	body = HsParen (HsLambda undefined [p] (foldr addBinding e bs))
>		addBinding :: Binding -> HsExp -> HsExp
>		addBinding (BindLet ds) e = HsLet ds e
>		addBinding (BindCase p e) e' =
>			HsCase e [HsAlt undefined p (HsUnGuardedAlt e') []]
> toHaskellCode (Compose f as g) =
>	foldr comp (toHaskellArg g) (map toHaskellArg (f:as))
>	where	comp f g = HsInfixApp f compose_op g
> toHaskellCode (Op op as) =
>	foldl HsApp op (map (paren . toHaskellCode) as)
> toHaskellCode (InfixOp a1 op a2) =
>	HsInfixApp (toHaskellArg a1) op (toHaskellArg a2)
> toHaskellCode (Let nas a) =
>	HsLet (map toHaskellDecl nas) (toHaskellCode a)
>	where	toHaskellDecl (VarDecl loc n a) =
>			HsPatBind loc (HsPVar n)
>				(HsUnGuardedRhs (toHaskellCode a)) []

> toHaskellArg :: Code -> HsExp
> toHaskellArg a = parenInfixArg (toHaskellCode a)

-----------------------------------------------------------------------------
Tuples, representing sets of variables.

> newtype Tuple = Tuple (Set HsName)

Tuple extractors, including matching expression and pattern.

> isEmptyTuple :: Tuple -> Bool
> isEmptyTuple (Tuple t) = Set.null t

> patternTuple :: Tuple -> HsPat
> patternTuple (Tuple t) = tupleP (map HsPVar (Set.toList t))

> expTuple :: Tuple -> HsExp
> expTuple (Tuple t) = tuple (map (HsVar . UnQual) (Set.toList t))

Operations on tuples

> emptyTuple :: Tuple
> emptyTuple = Tuple Set.empty

> unionTuple :: Tuple -> Tuple -> Tuple
> unionTuple (Tuple a) (Tuple b) = Tuple (a `Set.union` b)

Remove all usages of a set of variables.

> minusTuple :: Tuple -> Set HsName -> Tuple
> Tuple t `minusTuple` vs = Tuple (t `Set.difference` vs)

> intersectTuple :: Tuple -> Set HsName -> Tuple
> Tuple t `intersectTuple` vs = Tuple (t `Set.intersection` vs)

