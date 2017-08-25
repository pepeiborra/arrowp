{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module ArrCode
  ( Arrow(..)
  , bind
  , anon
  , arr
  , arrLet
  , (>>>)
  , arrowExp
  , infixOp
  , (|||)
  , first
  , toHaskell
  , Tuple(..)
  , isEmptyTuple
  , intersectTuple
  , patternTuple
  , expTuple
  , ifte
  , app
  , loop
  ) where
import           Data.Default
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Debug.Hoed.Pure
import           Language.Haskell.Exts.Syntax hiding (Tuple)
import qualified Language.Haskell.Exts.Syntax as H
import           NewCode
import           Utils

data Arrow = Arrow
  { context  :: Tuple -- named input components used by the arrow
  , anonArgs :: Int     -- number of unnamed arguments
  , code     :: Exp Code
  }
  deriving (Eq, Generic, Show)

instance Observable Arrow

loop :: Arrow -> Arrow
loop f = applyOp loop_exp [f]

app :: Arrow
app = arrowExp app_exp

bind :: Set (Name ()) -> Arrow -> Arrow
bind = observe "bind" $ \vars a -> a {context = context a `minusTuple` vars}
anon :: Int -> Arrow -> Arrow
anon anonCount a = a {anonArgs = anonArgs a + anonCount}
arr
  :: Int -> Tuple -> Pat S -> Exp S -> Arrow
arr = observe "arr" $ \anons t p e ->
  Arrow
  { code =
      if same p e
        then ReturnA (ann e)
        else Arr (ann e) anons (Loc <$> p) [] (Loc <$> e)
  , context = t `intersectTuple` freeVars e
  , anonArgs = anons
  }
arrLet :: Int -> Tuple -> Pat S -> Binds S -> Exp S -> Arrow
arrLet anons t p ds e =
  Arrow
  { code = Arr (ann e) anons (Loc <$> p) [BindLet (Loc <$> ds)] (Loc <$> e)
  , context = t `intersectTuple` vs
  , anonArgs = anons
  }
  where
    vs =
      (freeVars e `Set.union` freeVarss ds) `Set.difference` definedVars ds
ifte :: Exp S -> Arrow -> Arrow -> Arrow
ifte c th el =
  Arrow
  { code = If (Loc$ ann c) (Loc <$> c) (code th) (code el)
  , context = context th `unionTuple` context el
  , anonArgs = 0
  }
(>>>) :: Arrow -> Arrow -> Arrow
a1 >>> a2 = a1 { code = compose (code a1) (code a2) }
arrowExp :: Exp S -> Arrow
arrowExp e =
  Arrow
  { code =
      if e == returnA_exp
        then ReturnA (ann e)
        else Op (Loc <$> e) []
  , context = emptyTuple
  , anonArgs = 0
  }
applyOp :: Exp S -> [Arrow] -> Arrow
applyOp e as =
  Arrow
  { code = Op (Loc <$> e) (map code as)
  , context = foldr (unionTuple . context) emptyTuple as
  , anonArgs = 0 -- BUG: see below
  }

infixOp :: Arrow -> QOp S -> Arrow -> Arrow
infixOp a1 op a2 =
  Arrow
  { code = InfixApp def (code a1) (Loc <$> op) (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0 -- BUG: as above
  }
first :: Arrow -> Tuple -> Arrow
first a ps =
  Arrow
  { code = Op first_exp [code a]
  , context = context a `unionTuple` ps
  , anonArgs = 0
  }
(|||) :: Arrow -> Arrow -> Arrow
a1 ||| a2 =
  Arrow
  { code = InfixApp def (code a1) (choice_op) (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0
  }

compose :: Exp Code -> Exp Code -> Exp Code
compose = observe "compose" compose'

compose' :: Exp Code -> Exp Code -> Exp Code
compose' ReturnA{} a = a
compose' a ReturnA{} = a
compose' a1@(Arr l1 n1 p1 ds1 e1) a2@(Arr _l2 n2 p2 ds2 e2)
  | n1 /= n2 = Compose a1 [] a2 -- could do better, but can this arise?
  | same p2 e1 = Arr l1 n1 p1 (ds1 ++ ds2) e2
  | otherwise = Arr l1 n1 p1 (ds1 ++ BindCase p2 e1 : ds2) e2
compose' (Compose f1 as1 g1) (Compose f2 as2 g2) =
  Compose f1 (as1 ++ (g1 : f2 : as2)) g2
compose' a (Compose f bs g) = Compose (compose a f) bs g
compose' (Compose f as g) b = Compose f as (compose g b)
compose' a1 a2 = Compose a1 [] a2

toHaskell :: Arrow -> Exp S
toHaskell = rebracket1 . toHaskellCode . code
  where
    toHaskellCode :: Exp Code -> Exp S
    toHaskellCode (ReturnA l) = const l <$> returnA_exp @ S
    toHaskellCode (Arr l n p bs e) =
      App l arr_exp (times n (Paren def . App def first_exp) body)
      where
        body :: Exp S
        body = Lambda def [getLoc <$> p] (foldr addBinding (getLoc <$> e) bs)
        addBinding :: Binding -> Exp S -> Exp S
        addBinding (BindLet ds) e = H.Let def (getLoc <$> ds) e
        addBinding (BindCase p e) e' =
          Case def (getLoc <$> e) [Alt def (getLoc <$> p) (UnGuardedRhs def e') Nothing]
    toHaskellCode (Compose f as g) =
      foldr (comp . toHaskellArg) (toHaskellArg g) (f : as)
      where
        comp f = InfixApp def f compose_op
    toHaskellCode (Op op as) =
      foldl (App def) (getLoc <$> op) (map (Paren def . toHaskellCode) as)
    toHaskellCode (InfixApp (Loc l) a1 op a2) =
      InfixApp l (toHaskellArg a1) (getLoc <$> op) (toHaskellArg a2)
    toHaskellCode (Let (Loc l) bb a) =
      H.Let l (getLoc <$> bb) (toHaskellCode a)
    toHaskellCode (If (Loc l) cond th el) =
      If l (getLoc <$> cond) (toHaskellCode th) (toHaskellCode el)
    toHaskellCode (Case (Loc l) e alts) =
      Case l (getLoc <$> e) (toHaskellAlt <$> alts)
    toHaskellCode other = error $ "toHaskellCode: " ++ show other
    toHaskellAlt (Alt (Loc l) pat rhs binds) =
      Alt l (getLoc <$> pat) (toHaskellRhs rhs) (getLoc <$$> binds)
    toHaskellAlt other = error $ "toHaskellAlt: " ++ show other
    toHaskellRhs (UnGuardedRhs (Loc l) e) = UnGuardedRhs l (toHaskellCode e)
    toHaskellRhs (GuardedRhss (Loc l) rhss) =
      GuardedRhss l (toHaskellGuardedRhs <$> rhss)
    toHaskellRhs other = error $ "toHaskellRhs: " ++ show other
    toHaskellGuardedRhs (GuardedRhs (Loc l) stmts e) =
      GuardedRhs l (getLoc <$$> stmts) (toHaskellCode e)
    toHaskellGuardedRhs other = error $ "toHaskellGuardedRhs: " ++ show other
    toHaskellArg = Paren def . toHaskellCode

newtype Tuple = Tuple (Set (Name ()))
  deriving (Eq,Generic,Monoid,Show)
instance Observable Tuple

isEmptyTuple :: Tuple -> Bool
isEmptyTuple (Tuple t) = Set.null t

patternTuple :: Tuple -> Pat S
patternTuple (Tuple [])  = PApp def (unit_con_name def) []
patternTuple (Tuple [x]) = PVar def (const def <$> x)
patternTuple (Tuple t) =
  PTuple def Boxed (map (PVar def) (const def <$$> Set.toList t))

expTuple :: Tuple -> Exp S
expTuple (Tuple [])  = unit_con def
expTuple (Tuple [t]) = Var def $ UnQual def (const def <$> t)
expTuple (Tuple t) =
  H.Tuple def Boxed (map (Var def . UnQual def) (const def <$$> Set.toList t))

emptyTuple :: Tuple
emptyTuple = Tuple Set.empty
unionTuple :: Tuple -> Tuple -> Tuple
unionTuple (Tuple a) (Tuple b) = Tuple (a `Set.union` b)

minusTuple :: Tuple -> Set (Name ()) -> Tuple
Tuple t `minusTuple` vs = Tuple (t `Set.difference` vs)
intersectTuple :: Tuple -> Set (Name ()) -> Tuple
intersectTuple = observe "intersectTuple" intersectTuple'
intersectTuple' :: Tuple -> Set (Name ()) -> Tuple
Tuple t `intersectTuple'` vs = Tuple (t `Set.intersection` vs)
