Miscellaneous utilities on ordinary Haskell syntax used by the arrow
translator.

> module Utils(
>	FreeVars(freeVars), DefinedVars(definedVars),
>	failureFree, irrPat, paren, parenInfixArg,
>	tuple, tupleP,
>	times
> ) where

> import Data.Set (Set)
> import qualified Data.Set as Set
> import Language.Haskell.Syntax

The set of free variables in some construct.

> class FreeVars a where
>	freeVars :: a -> Set HsName

> instance FreeVars a => FreeVars [a] where
>	freeVars = Set.unions . map freeVars

> instance FreeVars HsPat where
>	freeVars (HsPVar n) = Set.singleton n
>	freeVars (HsPLit _) = Set.empty
>	freeVars (HsPNeg p) = freeVars p
>	freeVars (HsPInfixApp p1 _ p2) = freeVars p1 `Set.union` freeVars p2
>	freeVars (HsPApp _ ps) = freeVars ps
>	freeVars (HsPTuple ps) = freeVars ps
>	freeVars (HsPList ps) = freeVars ps
>	freeVars (HsPParen p) = freeVars p
>	freeVars (HsPRec _ pfs) = freeVars pfs
>	freeVars (HsPAsPat n p) = Set.insert n (freeVars p)
>	freeVars (HsPWildCard) = Set.empty
>	freeVars (HsPIrrPat p) = freeVars p

> instance FreeVars HsPatField where
>	freeVars (HsPFieldPat _ p) = freeVars p

> instance FreeVars HsFieldUpdate where
>	freeVars (HsFieldUpdate _ e) = freeVars e

> instance FreeVars HsExp where
>	freeVars (HsVar n) = freeVars n
>	freeVars (HsCon _) = Set.empty
>	freeVars (HsLit _) = Set.empty
>	freeVars (HsInfixApp e1 op e2) =
>		freeVars e1 `Set.union` freeVars op `Set.union` freeVars e2
>	freeVars (HsApp f e) = freeVars f `Set.union` freeVars e
>	freeVars (HsNegApp e) = freeVars e
>	freeVars (HsLambda _ ps e) = freeVars e `Set.difference` freeVars ps
>	freeVars (HsLet decls e) =
>		(freeVars decls `Set.union` freeVars e) `Set.difference`
>			definedVars decls
>	freeVars (HsIf e1 e2 e3) =
>		freeVars e1 `Set.union` freeVars e2 `Set.union` freeVars e3
>	freeVars (HsCase e as) = freeVars e `Set.union` freeVars as
>	freeVars (HsDo ss) = freeVarsStmts ss
>	freeVars (HsTuple es) = freeVars es
>	freeVars (HsList es) = freeVars es
>	freeVars (HsParen e) = freeVars e
>	freeVars (HsLeftSection e op) = freeVars e `Set.union` freeVars op
>	freeVars (HsRightSection op e) = freeVars op `Set.union` freeVars e
>	freeVars (HsRecConstr _ us) = freeVars us
>	freeVars (HsRecUpdate e us) = freeVars e `Set.union` freeVars us
>	freeVars (HsEnumFrom e) = freeVars e
>	freeVars (HsEnumFromTo e1 e2) = freeVars e1 `Set.union` freeVars e2
>	freeVars (HsEnumFromThen e1 e2) = freeVars e1 `Set.union` freeVars e2
>	freeVars (HsEnumFromThenTo e1 e2 e3) =
>		freeVars e1 `Set.union` freeVars e2 `Set.union` freeVars e3
>	freeVars (HsListComp e ss) =
>		freeVars e `Set.union` freeVarsStmts ss
>	freeVars (HsExpTypeSig _ e _) = freeVars e
>	freeVars (HsAsPat _ _) = error "freeVars (x @ p)"
>	freeVars (HsWildCard) = error "freeVars _"
>	freeVars (HsIrrPat _) = error "freeVars ~p"

> instance FreeVars HsQOp where
>	freeVars (HsQVarOp n) = freeVars n
>	freeVars (HsQConOp _) = Set.empty

> instance FreeVars HsQName where
>	freeVars (UnQual v) = Set.singleton v
>	freeVars _ = Set.empty

> instance FreeVars HsAlt where
>	freeVars (HsAlt _ p gas decls) =
>		(freeVars gas `Set.union` freeVars decls) `Set.difference`
>		(freeVars p `Set.union` definedVars decls)

> instance FreeVars HsGuardedAlts where
>	freeVars (HsUnGuardedAlt e) = freeVars e
>	freeVars (HsGuardedAlts alts) = freeVars alts

> instance FreeVars HsGuardedAlt where
>	freeVars (HsGuardedAlt _ e1 e2) = freeVars e1 `Set.union` freeVars e2

> instance FreeVars HsDecl where
>	freeVars (HsFunBind ms) = freeVars ms
>	freeVars (HsPatBind _ p rhs decls) =
>		(freeVars rhs `Set.union` freeVars decls) `Set.difference`
>		(freeVars p `Set.union` definedVars decls)
>	freeVars _ = Set.empty

> instance FreeVars HsMatch where
>	freeVars (HsMatch _ n ps rhs decls) =
>		(freeVars rhs `Set.union` freeVars decls) `Set.difference`
>		(Set.insert n (freeVars ps) `Set.union` definedVars decls)

> instance FreeVars HsRhs where
>	freeVars (HsUnGuardedRhs e) = freeVars e
>	freeVars (HsGuardedRhss grs) = freeVars grs

> instance FreeVars HsGuardedRhs where
>	freeVars (HsGuardedRhs _ e1 e2) = freeVars e1 `Set.union` freeVars e2

> freeVarsStmts :: [HsStmt] -> Set HsName
> freeVarsStmts = foldr addStmt Set.empty
>	where	addStmt (HsGenerator _ p e) s =
>			freeVars e `Set.union` (s `Set.difference` freeVars p)
>		addStmt (HsQualifier e) _s = freeVars e
>		addStmt (HsLetStmt decls) s =
>			(freeVars decls `Set.union` s) `Set.difference` definedVars decls

The set of variables defined by a construct.

> class DefinedVars a where
>	definedVars :: a -> Set HsName

> instance DefinedVars a => DefinedVars [a] where
>	definedVars = Set.unions . map definedVars

> instance DefinedVars HsDecl where
>	definedVars (HsFunBind (HsMatch _ n _ _ _:_)) = Set.singleton n
>	definedVars (HsPatBind _ p _ _) = freeVars p
>	definedVars _ = Set.empty

Is the pattern failure-free?
(This is incomplete at the moment, because patterns made with unique
constructors should be failure-free, but we have no way of detecting them.)

> failureFree :: HsPat -> Bool
> failureFree (HsPVar _) = True
> failureFree (HsPApp n ps) = n == unit_con_name && null ps
> failureFree (HsPTuple ps) = all failureFree ps
> failureFree (HsPParen p) = failureFree p
> failureFree (HsPAsPat _ p) = failureFree p
> failureFree (HsPWildCard) = True
> failureFree (HsPIrrPat _) = True
> failureFree _ = False

Irrefutable version of a pattern

> irrPat :: HsPat -> HsPat
> irrPat p@(HsPVar _) = p
> irrPat (HsPParen p) = HsPParen (irrPat p)
> irrPat (HsPAsPat n p) = HsPAsPat n (irrPat p)
> irrPat p@(HsPWildCard) = p
> irrPat p@(HsPIrrPat _) = p
> irrPat p = HsPIrrPat p

Make an expression into an aexp, by adding parentheses if required.

> paren :: HsExp -> HsExp
> paren e = if isAexp e then e else HsParen e
>	where	isAexp (HsVar _) = True
>		isAexp (HsCon _) = True
>		isAexp (HsLit _) = True
>		isAexp (HsParen _) = True
>		isAexp (HsTuple _) = True
>		isAexp (HsList _) = True
>		isAexp (HsEnumFrom _) = True
>		isAexp (HsEnumFromTo _ _) = True
>		isAexp (HsEnumFromThen _ _) = True
>		isAexp (HsEnumFromThenTo _ _ _) = True
>		isAexp (HsListComp _ _) = True
>		isAexp (HsLeftSection _ _) = True
>		isAexp (HsRightSection _ _) = True
>		isAexp (HsRecConstr _ _) = True
>		isAexp (HsRecUpdate _ _) = True
>		isAexp _ = False

Make an expression into an fexp, by adding parentheses if required.

> parenInfixArg :: HsExp -> HsExp
> parenInfixArg e@(HsApp _ _) = e
> parenInfixArg e = paren e

Tuples

> tuple :: [HsExp] -> HsExp
> tuple [] = unit_con
> tuple [e] = e
> tuple es = HsTuple es

> tupleP :: [HsPat] -> HsPat
> tupleP [] = HsPApp unit_con_name []
> tupleP [e] = e
> tupleP es = HsPTuple es

Compose a function n times.

> times :: Int -> (a -> a) -> a -> a
> times n f a = foldr ($) a (replicate n f)
