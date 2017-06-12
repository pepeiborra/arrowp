Additional abstract syntax for arrow expressions

> module ArrSyn(
>	Cmd(..),
>	Stmts, Stmt(..), CmdDecl, VarDecl(..),
>	Alt(..), GuardedAlts(..), GuardedAlt(..),
>	translate	-- :: HsPat -> Cmd -> HsExp
> ) where

> import ArrCode
> import State		-- Haskell 98 version of Control.Monad.State
> import Utils

> import Data.List(mapAccumL)
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Set (Set)
> import qualified Data.Set as Set
> import Language.Haskell.Syntax

> data Cmd
>	= Input HsExp HsExp
>	| Kappa SrcLoc [HsPat] Cmd
>	| Op HsExp [Cmd]
>	| InfixOp Cmd HsQOp Cmd
>	| Let [HsDecl] Cmd
>	| LetCmd (VarDecl Cmd) Cmd
>	| If HsExp Cmd Cmd
>	| Case HsExp [Alt]
>	| Paren Cmd
>	| Do [Stmt] Cmd
>	| App Cmd HsExp
>	| CmdVar HsName
>   deriving (Eq,Show)

> type CmdDecl = (HsName, Cmd)
> type Stmts = ([Stmt], Cmd)

> data Stmt
>	= Generator SrcLoc HsPat Cmd
>	| RecStmt [Stmt]
>	| LetStmt [HsDecl]
>	| LetCmdStmt (VarDecl Cmd)
>   deriving (Eq,Show)

> data Alt
>	= Alt SrcLoc HsPat GuardedAlts [HsDecl]
>   deriving (Eq,Show)

> data GuardedAlts
>	= UnGuardedAlt Cmd
>	| GuardedAlts [GuardedAlt]
>   deriving (Eq,Show)

> data GuardedAlt
>	= GuardedAlt SrcLoc HsExp Cmd
>   deriving (Eq,Show)

-----------------------------------------------------------------------------
Utilities

> pair :: HsExp -> HsExp -> HsExp
> pair e1 e2 = HsTuple [e1, e2]

Turn redefined variables into wildcards, so the new pattern will be legal.

> pairP :: HsPat -> HsPat -> HsPat
> pairP p1 p2 = HsPTuple [hide p1, p2]
>	where	vs = freeVars p2
>		hide p@(HsPVar n)
>			| n `Set.member` vs = HsPWildCard
>			| otherwise = p
>		hide (HsPNeg p) = HsPNeg (hide p)
>		hide (HsPInfixApp p1 n p2) = HsPInfixApp (hide p1) n (hide p2)
>		hide (HsPApp n ps) = HsPApp n (map hide ps)
>		hide (HsPTuple ps) = HsPTuple (map hide ps)
>		hide (HsPList ps) = HsPList (map hide ps)
>		hide (HsPParen p) = HsPParen (hide p)
>		hide (HsPRec n pfs) = HsPRec n (map hideField pfs)
>			where	hideField (HsPFieldPat f p) =
>					HsPFieldPat f (hide p)
>		hide (HsPAsPat n p)
>			| n `Set.member` vs = hide p
>			| otherwise = HsPAsPat n (hide p)
>		hide (HsPIrrPat p) = HsPIrrPat (hide p)
>		hide p = p

> left, right :: HsExp -> HsExp
> left f = HsApp left_exp (paren f)
> right f = HsApp right_exp (paren f)

> loop :: Arrow -> Arrow
> loop f = applyOp loop_exp [f]

> app, returnA :: Arrow
> app = arrowExp app_exp
> returnA = arrowExp returnA_exp

> returnCmd :: HsExp -> Cmd
> returnCmd = Input returnA_exp

-----------------------------------------------------------------------------
Translation state

> data TransState = TransState {
>	locals :: Set HsName,	-- vars in scope defined in this proc
>	cmdVars :: Map HsName Arrow
> }

> input :: TransState -> Tuple
> input s = Tuple (locals s)

> startPattern :: HsPat -> (TransState, HsPat)
> startPattern p =
>	(TransState {
>		locals = freeVars p,
>		cmdVars = Map.empty
>	 }, p)

> class AddVars a where
>	addVars :: TransState -> a -> (TransState, a)

> instance AddVars a => AddVars [a] where
>	addVars = mapAccumL addVars

> instance AddVars HsPat where
>	addVars s p =
>		(s {locals = locals s `Set.union` freeVars p}, p)


> instance AddVars HsDecl where
>	addVars s d@(HsFunBind (HsMatch _ n _ _ _:_)) =
>		(s', d)
>		where	(s', _) = addVars s (HsPVar n)
>	addVars s (HsPatBind loc p rhs decls) =
>		(s', HsPatBind loc p' rhs decls)
>		where	(s', p') = addVars s p
>	addVars s d = (s, d)

-----------------------------------------------------------------------------
Translation to Haskell

This is a 2-phase process:
- transCmd generates an abstract arrow combinator language represented
  by the Arrow type, and
- toHaskell turns that into Haskell.

> translate :: HsPat -> Cmd -> HsExp
> translate p c = paren (toHaskell (transCmd s p' c))
>	where	(s, p') = startPattern p

The pattern argument is often pseudo-recursively defined in terms of
the context part of the result of these functions.  (It's not real
recursion, because that part is independent of the pattern.)

> transCmd :: TransState -> HsPat -> Cmd -> Arrow
> transCmd s p (Input f e)
>	| Set.null (freeVars f `Set.intersection` locals s) =
>		arr 0 (input s) p e >>> arrowExp f
>	| otherwise =
>		arr 0 (input s) p (pair f e) >>> app
> transCmd s p (Kappa _ ps c) =
>	anon (length ps) $ bind (freeVars ps) $
>		transCmd s' (foldl pairP p ps') c
>	where	(s', ps') = addVars s ps
> transCmd s p (Op op cs) =
>	applyOp op (map (transCmd s p) cs)
> transCmd s p (InfixOp c1 op c2) =
>	infixOp (transCmd s p c1) op (transCmd s p c2)
> transCmd s p (Let decls c) =
>	arrLet (anonArgs a) (input s) p decls' e >>> a
>	where	(s', decls') = addVars s decls
>		(e, a) = transTrimCmd s' c
> transCmd s p (If e c1 c2)
>   | Set.null (freeVars e `Set.intersection` locals s) =
>       ifte e (transCmd s p c1) (transCmd s p c2)
>   | otherwise =
>	arr 0 (input s) p (HsIf e (left e1) (right e2)) >>> (a1 ||| a2)
>	where	(e1, a1) = transTrimCmd s c1
>		(e2, a2) = transTrimCmd s c2
> transCmd s p (Case e as) =
>	transCase s p e as
> transCmd s p (Paren c) =
>	transCmd s p c
> transCmd s p (Do ss c) =
>	transDo s p ss c
> transCmd s p (App c arg) =
>	anon (-1) $
>	arr (anonArgs a) (input s) p (pair e arg) >>> a
>	where	(e, a) = transTrimCmd s c

The following awful hack is there because if the command is recursively
defined, computation of its context will not terminate.  So we plug in
returnA (empty context) to get an arrow whose code is ignored in the
recomputation of the real arrow for a1.
Mutually recursive bindings will be a bit more tricky.

> transCmd s p (LetCmd (VarDecl loc n c1) c2) =
>	letCmd [VarDecl loc n a1] (transCmd s' p c2)
>	where	(_, a1) = transTrimCmd s' c1
>		s' = s { cmdVars = Map.insert n a0 (cmdVars s) }
>		s0 = s { cmdVars = Map.insert n returnA (cmdVars s) }
>		a0 = transCmd s0 p c1	-- hackety hack
> transCmd s p (CmdVar n) =
>	arr (anonArgs a) (input s) p e >>> arrowExp (HsVar (UnQual n))
>	where	Just a = Map.lookup n (cmdVars s)
>		e = expTuple (context a)

Like TransCmd, but use the minimal input pattern.  The first component
of the result is the matching expression to build this input.
That is, the result is (e, proc p' -> c) with the minimal p' such that

	proc p -> c = arr (first^n (p -> e)) >>> (proc p' -> c)

where n is the number of anonymous arguments taken by c.

> transTrimCmd :: TransState -> Cmd -> (HsExp, Arrow)
> transTrimCmd s c = (expTuple (context a), a)
>	where	a = transCmd s (patternTuple (context a)) c

> transDo :: TransState -> HsPat -> [Stmt] -> Cmd -> Arrow
> transDo s p [] c =
>	transCmd s p c
> transDo s p (Generator _ pg cg:ss) c =
>	if isEmptyTuple u then
>		transCmd s p cg >>> transDo s' pg ss c
>	else
>		arr 0 (input s) p (pair eg (expTuple u)) >>> first ag u >>> a
>	where	(s', pg') = addVars s pg
>		a = bind (freeVars pg)
>			(transDo s' (pairP pg' (patternTuple u)) ss c)
>		u = context a
>		(eg, ag) = transTrimCmd s cg
> transDo s p (LetStmt decls:ss) c =
>	transCmd s p (Let decls (Do ss c))
> transDo s p (RecStmt rss:ss) c =
>	bind defined
>		(loop (transDo s' (pairP p (irrPat (patternTuple feedback)))
>			rss'
>			(returnCmd (pair output (expTuple feedback)))
>		      ) >>> a)
>	where	defined = definedVars rss
>		(s', rss') = addVars s rss
>		(output, a) = transTrimCmd s' (Do ss c)
>		feedback = context (transDo s' p rss'
>				(returnCmd (foldr pair output $ map (HsVar . UnQual) $ Set.toList defined)))
>			`intersectTuple` defined
> transDo s p (LetCmdStmt vdecl:ss) c =
>	transCmd s p (LetCmd vdecl (Do ss c))

The set of variables defined by a list of statements in a rec.

> instance DefinedVars Stmt where
>	definedVars (Generator _ p _) = freeVars p
>	definedVars (LetStmt decls) = definedVars decls
>	definedVars (RecStmt stmts) = definedVars stmts
>	definedVars (LetCmdStmt _vdecl) = Set.empty

> instance AddVars Stmt where
>	addVars s (Generator loc p c) =
>		(s', Generator loc p' c)
>		where	(s', p') = addVars s p
>	addVars s (LetStmt decls) =
>		(s', LetStmt decls')
>		where	(s', decls') = addVars s decls
>	addVars s (RecStmt stmts) =
>		(s', RecStmt stmts')
>		where	(s', stmts') = addVars s stmts
>	addVars s stmt@(LetCmdStmt _vdecl) =
>		(s, stmt)

Translation of case commands uses a right-nested sum,
corresponding to the right-associativity of (|||).
(In future: use a balanced sum.)

The state kept while traversing the expression is
	(count of rhss, rhss in reverse order)

> transCase :: TransState -> HsPat -> HsExp -> [Alt] -> Arrow
> transCase s p e as =
>	arr 0 (input s) p (HsCase e as') >>> foldr1 (|||) (reverse cases)
>	where	(as', (ncases, cases)) =
>			runState (mapM (transAlt s) as) (0, [])
>		transAlt s (Alt loc p gas decls) = do
>			let	(s', p') = addVars s p
>				(s'', decls') = addVars s' decls
>			gas' <- transGuardedAlts s'' gas
>			return (HsAlt loc p' gas' decls')
>		transGuardedAlts s (UnGuardedAlt c) = do
>			body <- newAlt s c
>			return (HsUnGuardedAlt body)
>		transGuardedAlts s (GuardedAlts gas) = do
>			gas' <- mapM (transGuardedAlt s) gas
>			return (HsGuardedAlts gas')
>		transGuardedAlt s (GuardedAlt loc e c) = do
>			body <- newAlt s c
>			return (HsGuardedAlt loc e body)
>		newAlt s c = do
>			let (e, a) = transTrimCmd s c
>			(n, as) <- get
>			put (n+1, a:as)
>			return (label n e)
>		label n e = times n right
>				(if n < ncases-1 then left e else e)
