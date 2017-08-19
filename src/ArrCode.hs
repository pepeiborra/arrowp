{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module ArrCode (
      Arrow,
      bind, anon,
      arr, arrLet, (>>>), arrowExp, applyOp, infixOp, (|||), first,
      VarDecl(VarDecl), letCmd,
      context, anonArgs, toHaskell,
      Tuple(..),
      isEmptyTuple, unionTuple, minusTuple, intersectTuple,
      patternTuple, expTuple,
      returnA_exp, arr_exp, compose_op, choice_op, first_exp,
      left_exp, right_exp, app_exp, loop_exp,
      ifte, app, loop, returnA
      ) where
import           Data.Data
import           Data.Default
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Debug.Hoed.Pure
import           Language.Haskell.Exts.Bracket
import           Language.Haskell.Exts.Syntax  hiding (Let, Tuple)
import qualified Language.Haskell.Exts.Syntax  as H
import           Language.Haskell.Exts.Type
import           Utils

data Arrow l = Arrow
  { context  :: Tuple l -- named input components used by the arrow
  , anonArgs :: Int     -- number of unnamed arguments
  , code     :: Code l
  }
  deriving (Eq, Generic, Show)

instance HasSrcLoc (Arrow l) where
  type SrcLocType (Arrow l) = l

instance (Eq l, Observable l, Show l) => Observable (Arrow l)

data VarDecl a = VarDecl (Name (SrcLocType a)) a
      deriving Generic

-- required Undecidable instances
deriving instance (Eq a, Eq(SrcLocType a)) => Eq (VarDecl a)
deriving instance (Show a, Show(SrcLocType a)) => Show (VarDecl a)

mapVarDecl :: SrcLocType a ~ SrcLocType b => (a -> b) -> VarDecl a -> VarDecl b
mapVarDecl f (VarDecl name a) = VarDecl name (f a)

instance (Observable a, Observable l, Observable (Name l), l ~ SrcLocType a) => Observable (VarDecl a)

data Code l
      = ReturnA                       -- returnA = arr id
      | Arr Int (Pat l) [Binding l] (Exp l)   -- arr (first^n (\p -> ... e))
      | Compose (Code l) [Code l] (Code l)  -- composition of 2 or more elts
      | Op (Exp l) [Code l]         -- combinator applied to arrows
      | InfixOp (Code l) (QOp l) (Code l)
      | Let [VarDecl (Code l)] (Code l)
      | Ifte (Exp l) (Code l) (Code l)
  deriving (Eq, Generic, Show)

instance HasSrcLoc (Code l) where
  type SrcLocType (Code l) = l

instance Observable l => Observable (Code l)

data Binding l = BindLet (Binds l) | BindCase (Pat l) (Exp l)
  deriving (Eq,Generic, Show)
instance Observable l => Observable (Binding l)

loop :: Ord l => Default l => Arrow l -> Arrow l
loop f = applyOp loop_exp [f]

app, returnA :: (Eq l, Default l) => Arrow l
app = arrowExp app_exp
returnA = arrowExp returnA_exp

bind :: Ord l => Set (Name l) -> Arrow l -> Arrow l
bind vars a = a {context = context a `minusTuple` vars}
anon :: Int -> Arrow l -> Arrow l
anon anonCount a = a {anonArgs = anonArgs a + anonCount}
arr
  :: ( Data l
     , Ord l
     , Show l
     , Observable l
     , Observable (Exp l)
     , Observable (Pat l)
     , Observable (Set(Name l))
     )
  => Int -> Tuple l -> Pat l -> Exp l -> Arrow l
arr = observe "arr" $ \anons t p e ->
  Arrow
  { code =
      if same p e
        then ReturnA
        else Arr anons p [] e
  , context = t `intersectTuple` freeVars e
  , anonArgs = anons
  }
arrLet
  :: ( Data l
     , Observable l
     , Observable (Exp l)
     , Observable (Set (Name l))
     , Ord l
     )
  => Int -> Tuple l -> Pat l -> Binds l -> Exp l -> Arrow l
arrLet anons t p ds e =
  Arrow
  { code = Arr anons p [BindLet ds] e
  , context = t `intersectTuple` vs
  , anonArgs = anons
  }
  where
    vs =
      (freeVars e `Set.union` freeVarss ds) `Set.difference` definedVars ds
ifte :: Ord l => Exp l -> Arrow l -> Arrow l -> Arrow l
ifte c th el =
  Arrow
  { code = Ifte c (code th) (code el)
  , context = context th `unionTuple` context el
  , anonArgs = 0
  }
(>>>) :: (Eq l, Observable(Exp l), Observable(Pat l)) => Arrow l -> Arrow l -> Arrow l
a1 >>> a2 = a1 { code = compose (code a1) (code a2) }
arrowExp :: (Eq l, Default l) => Exp l -> Arrow l
arrowExp e =
  Arrow
  { code =
      if e == returnA_exp
        then ReturnA
        else Op e []
  , context = emptyTuple
  , anonArgs = 0
  }
applyOp :: Ord l => Exp l -> [Arrow l] -> Arrow l
applyOp e as =
  Arrow
  { code = Op e (map code as)
  , context = foldr (unionTuple . context) emptyTuple as
  , anonArgs = 0 -- BUG: see below
  }

infixOp :: Ord l => Arrow l -> QOp l -> Arrow l -> Arrow l
infixOp a1 op a2 =
  Arrow
  { code = InfixOp (code a1) op (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0 -- BUG: as above
  }
first :: (Default l, Ord l) => Arrow l -> Tuple l -> Arrow l
first a ps =
  Arrow
  { code = Op first_exp [code a]
  , context = context a `unionTuple` ps
  , anonArgs = 0
  }
(|||) :: (Default l, Ord l) => Arrow l -> Arrow l -> Arrow l
a1 ||| a2 =
  Arrow
  { code = InfixOp (code a1) choice_op (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0
  }
letCmd :: [VarDecl (Arrow l)] -> Arrow l -> Arrow l
letCmd defs a =
  Arrow
  { code = Let (map (mapVarDecl code) defs) (code a)
  , context = context a
  , anonArgs = anonArgs a
  }

compose
  :: (Eq l, Observable (Exp l), Observable (Pat l))
  => Code l -> Code l -> Code l
compose ReturnA a = a
compose a ReturnA = a
compose a1@(Arr n1 p1 ds1 e1) a2@(Arr n2 p2 ds2 e2)
  | n1 /= n2 = Compose a1 [] a2 -- could do better, but can this arise?
  | same p2 e1 = Arr n1 p1 (ds1 ++ ds2) e2
  | otherwise = Arr n1 p1 (ds1 ++ BindCase p2 e1 : ds2) e2
compose (Compose f1 as1 g1) (Compose f2 as2 g2) =
  Compose f1 (as1 ++ (g1 : f2 : as2)) g2
compose a (Compose f bs g) = Compose (compose a f) bs g
compose (Compose f as g) b = Compose f as (compose g b)
compose a1 a2 = Compose a1 [] a2

toHaskell :: forall l. (Default l, Data l) => Arrow l -> Exp l
toHaskell = rebracket1 . toHaskellCode . code
  where
    toHaskellCode :: Code l -> Exp l
    toHaskellCode ReturnA = returnA_exp
    toHaskellCode (Arr n p bs e) =
      App def arr_exp (times n (Paren def . App def first_exp) body)
      where
        body = Lambda def [p] (foldr addBinding e bs)
        addBinding (BindLet ds) e = H.Let def ds e
        addBinding (BindCase p e) e' =
          Case def e [Alt def p (UnGuardedRhs def e') Nothing]
    toHaskellCode (Compose f as g) =
      foldr (comp . toHaskellArg) (toHaskellArg g) (f : as)
      where
        comp f = InfixApp def f compose_op
    toHaskellCode (Op op as) = foldl (App def) op (map (Paren def . toHaskellCode) as)
    toHaskellCode (InfixOp a1 op a2) =
      InfixApp def (Paren def $ toHaskellArg a1) op (Paren def $ toHaskellArg a2)
    toHaskellCode (Let nas a) =
      H.Let def (BDecls def $ map toHaskellDecl nas) (toHaskellCode a)
      where
        toHaskellDecl (VarDecl n a) =
          PatBind def (PVar def n) (UnGuardedRhs def (toHaskellCode a)) Nothing
    toHaskellCode (Ifte cond th el) = If def cond (toHaskellCode th) (toHaskellCode el)

    toHaskellArg = toHaskellCode

newtype Tuple l = Tuple (Set (Name l))
  deriving (Eq,Generic,Show)
instance (Eq l, Observable l, Show l) => Observable (Tuple l)

isEmptyTuple :: Tuple l -> Bool
isEmptyTuple (Tuple t) = Set.null t

patternTuple :: (Default l, Ord l) => Tuple l -> Pat l
patternTuple (Tuple [])  = PApp def (unit_con_name def) []
patternTuple (Tuple [x]) = PVar def x
patternTuple (Tuple t)   = PTuple def Boxed (map (PVar def) (Set.toList t))

expTuple :: (Default l, Ord l) => Tuple l -> Exp l
expTuple (Tuple [])  = unit_con def
expTuple (Tuple [t]) = Var def $ UnQual def t
expTuple (Tuple t)   = H.Tuple def Boxed (map (Var def . UnQual def) (Set.toList t))

emptyTuple :: Tuple l
emptyTuple = Tuple Set.empty
unionTuple :: Ord l => Tuple l -> Tuple l -> Tuple l
unionTuple (Tuple a) (Tuple b) = Tuple (a `Set.union` b)

minusTuple :: Ord l => Tuple l -> Set (Name l) -> Tuple l
Tuple t `minusTuple` vs = Tuple (t `Set.difference` vs)
intersectTuple :: Ord l => Tuple l -> Set (Name l) -> Tuple l
Tuple t `intersectTuple` vs = Tuple (t `Set.intersection` vs)
