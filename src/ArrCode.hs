{-# LANGUAGE DeriveFunctor        #-}
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
import           Data.Default
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Debug.Hoed.Pure
import           Language.Haskell.Exts.Bracket
import           Language.Haskell.Exts.Syntax  hiding (Let, Tuple)
import qualified Language.Haskell.Exts.Syntax  as H
import           Language.Haskell.Exts.Type    hiding (S)
import           Utils

data Arrow = Arrow
  { context  :: Tuple -- named input components used by the arrow
  , anonArgs :: Int     -- number of unnamed arguments
  , code     :: Code
  }
  deriving (Eq, Generic, Show)

instance HasSrcLoc Arrow  where
  type SrcLocType Arrow = S

instance Observable Arrow

data VarDecl a = VarDecl (Name S) a
      deriving (Functor, Generic)

-- required Undecidable instances
deriving instance (Eq a) => Eq (VarDecl a)
deriving instance (Show a) => Show (VarDecl a)

instance Observable a => Observable (VarDecl a)

data Code
      = ReturnA                       -- returnA = arr id
      | Arr Int (Pat S) [Binding] (Exp S)   -- arr (first^n (\p -> ... e))
      | Compose Code [Code] Code  -- composition of 2 or more elts
      | Op (Exp S) [Code]         -- combinator applied to arrows
      | InfixOp Code (QOp S) Code
      | Let [VarDecl Code] Code
      | Ifte (Exp S) Code Code
  deriving (Eq, Generic, Show)

instance HasSrcLoc Code where
  type SrcLocType Code = S

instance Observable Code

data Binding = BindLet (Binds S) | BindCase (Pat S) (Exp S)
  deriving (Eq,Generic, Show)
instance Observable Binding

loop :: Arrow -> Arrow
loop f = applyOp loop_exp [f]

app, returnA :: Arrow
app = arrowExp app_exp
returnA = arrowExp returnA_exp

bind :: Set (Name S) -> Arrow -> Arrow
bind = observe "bind" $ \vars a -> a {context = context a `minusTuple` vars}
anon :: Int -> Arrow -> Arrow
anon anonCount a = a {anonArgs = anonArgs a + anonCount}
arr
  :: Int -> Tuple -> Pat S -> Exp S -> Arrow
arr = observe "arr" $ \anons t p e ->
  Arrow
  { code =
      if same p e
        then ReturnA
        else Arr anons p [] e
  , context = t `intersectTuple` freeVars e
  , anonArgs = anons
  }
arrLet :: Int -> Tuple -> Pat S -> Binds S -> Exp S -> Arrow
arrLet anons t p ds e =
  Arrow
  { code = Arr anons p [BindLet ds] e
  , context = t `intersectTuple` vs
  , anonArgs = anons
  }
  where
    vs =
      (freeVars e `Set.union` freeVarss ds) `Set.difference` definedVars ds
ifte :: Exp S -> Arrow -> Arrow -> Arrow
ifte c th el =
  Arrow
  { code = Ifte c (code th) (code el)
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
        then ReturnA
        else Op e []
  , context = emptyTuple
  , anonArgs = 0
  }
applyOp :: Exp S -> [Arrow] -> Arrow
applyOp e as =
  Arrow
  { code = Op e (map code as)
  , context = foldr (unionTuple . context) emptyTuple as
  , anonArgs = 0 -- BUG: see below
  }

infixOp :: Arrow -> QOp S -> Arrow -> Arrow
infixOp a1 op a2 =
  Arrow
  { code = InfixOp (code a1) op (code a2)
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
  { code = InfixOp (code a1) choice_op (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0
  }
letCmd :: [VarDecl Arrow] -> Arrow -> Arrow
letCmd defs a =
  Arrow
  { code = Let (map (fmap code) defs) (code a)
  , context = context a
  , anonArgs = anonArgs a
  }

compose :: Code -> Code -> Code
compose = observe "compose" compose'

compose' :: Code -> Code -> Code
compose' ReturnA a = a
compose' a ReturnA = a
compose' a1@(Arr n1 p1 ds1 e1) a2@(Arr n2 p2 ds2 e2)
  | n1 /= n2 = Compose a1 [] a2 -- could do better, but can this arise?
  | same p2 e1 = Arr n1 p1 (ds1 ++ ds2) e2
  | otherwise = Arr n1 p1 (ds1 ++ BindCase p2 e1 : ds2) e2
compose' (Compose f1 as1 g1) (Compose f2 as2 g2) =
  Compose f1 (as1 ++ (g1 : f2 : as2)) g2
compose' a (Compose f bs g) = Compose (compose a f) bs g
compose' (Compose f as g) b = Compose f as (compose g b)
compose' a1 a2 = Compose a1 [] a2

toHaskell :: Arrow -> Exp S
toHaskell = rebracket1 . toHaskellCode . code
  where
    toHaskellCode :: Code -> Exp S
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
      InfixApp def (toHaskellArg a1) op (toHaskellArg a2)
    toHaskellCode (Let nas a) =
      H.Let def (BDecls def $ map toHaskellDecl nas) (toHaskellCode a)
      where
        toHaskellDecl (VarDecl n a) =
          PatBind def (PVar def n) (UnGuardedRhs def (toHaskellCode a)) Nothing
    toHaskellCode (Ifte cond th el) = If def cond (toHaskellCode th) (toHaskellCode el)

    toHaskellArg = Paren def . toHaskellCode

newtype Tuple = Tuple (Set (Name S))
  deriving (Eq,Generic,Show)
instance Observable Tuple

isEmptyTuple :: Tuple -> Bool
isEmptyTuple (Tuple t) = Set.null t

patternTuple :: Tuple -> Pat S
patternTuple (Tuple [])  = PApp def (unit_con_name def) []
patternTuple (Tuple [x]) = PVar def x
patternTuple (Tuple t)   = PTuple def Boxed (map (PVar def) (Set.toList t))

expTuple :: Tuple -> Exp S
expTuple (Tuple [])  = unit_con def
expTuple (Tuple [t]) = Var def $ UnQual def t
expTuple (Tuple t)   = H.Tuple def Boxed (map (Var def . UnQual def) (Set.toList t))

emptyTuple :: Tuple
emptyTuple = Tuple Set.empty
unionTuple :: Tuple -> Tuple -> Tuple
unionTuple (Tuple a) (Tuple b) = Tuple (a `Set.union` b)

minusTuple :: Tuple -> Set (Name S) -> Tuple
Tuple t `minusTuple` vs = Tuple (t `Set.difference` vs)
intersectTuple :: Tuple -> Set (Name S) -> Tuple
intersectTuple = observe "intersectTuple" intersectTuple'
intersectTuple' :: Tuple -> Set (Name S) -> Tuple
Tuple t `intersectTuple'` vs = Tuple (t `Set.intersection` vs)
