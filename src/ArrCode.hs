{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ImplicitParams #-}
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
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Language.Haskell.Exts.Syntax hiding (Let, Tuple)
import qualified Language.Haskell.Exts.Syntax as H
import           Utils

data Arrow s = Arrow
  { code     :: Code s
  , context  :: Tuple s -- named input components used by the arrow
  , anonArgs :: Int -- number of unnamed arguments
  }
data VarDecl l a = VarDecl l (Name l) a
      deriving (Eq,Show)
instance Functor (VarDecl l) where
      fmap f (VarDecl loc name a) = VarDecl loc name (f a)
data Code s
      = ReturnA                       -- returnA = arr id
      | Arr Int (Pat s) [Binding s] (Exp s)   -- arr (first^n (\p -> ... e))
      | Compose (Code s) [Code s] (Code s)  -- composition of 2 or more elts
      | Op (Exp s) [Code s]         -- combinator applied to arrows
      | InfixOp (Code s) (QOp s) (Code s)
      | Let [VarDecl s (Code s)] (Code s)
      | Ifte (Exp s) (Code s) (Code s)
data Binding s = BindLet (Binds s) | BindCase (Pat s) (Exp s)

loop :: (?l::s, Ord s) => Arrow s -> Arrow s
loop f = applyOp loop_exp [f]

app, returnA :: (?l::s, Ord s) => Arrow s
app = arrowExp app_exp
returnA = arrowExp returnA_exp

bind :: (Ord s) => Set (Name s) -> Arrow s -> Arrow s
bind vars a = a {context = context a `minusTuple` vars}
anon :: Int -> Arrow s -> Arrow s
anon anonCount a = a {anonArgs = anonArgs a + anonCount}
arr :: (Data s, Ord s) => Int -> Tuple s -> Pat s -> Exp s -> Arrow s
arr anons t p e =
  Arrow
  { code =
      if same p e
        then ReturnA
        else Arr anons p [] e
  , context = t `intersectTuple` freeVars e
  , anonArgs = anons
  }
arrLet :: (Data s, Ord s) => Int -> Tuple s -> Pat s -> Binds s -> Exp s -> Arrow s
arrLet anons t p ds e =
  Arrow
  { code = Arr anons p [BindLet ds] e
  , context = t `intersectTuple` vs
  , anonArgs = anons
  }
  where
    vs =
      (freeVars e `Set.union` freeVars ds) `Set.difference`
      definedVars ds
ifte :: (Ord s) => Exp s -> Arrow s -> Arrow s -> Arrow s
ifte c th el =
  Arrow
  { code = Ifte c (code th) (code el)
  , context = context th `unionTuple` context el
  , anonArgs = 0
  }
(>>>) :: Eq s => Arrow s -> Arrow s -> Arrow s
a1 >>> a2 = a1 { code = compose (code a1) (code a2) }
arrowExp :: (?l::s, Ord s) => Exp s -> Arrow s
arrowExp e =
  Arrow
  { code =
      if e == returnA_exp
        then ReturnA
        else Op e []
  , context = emptyTuple
  , anonArgs = 0
  }
applyOp :: (Ord s) => Exp s -> [Arrow s] -> Arrow s
applyOp e as =
  Arrow
  { code = Op e (map code as)
  , context = foldr (unionTuple . context) emptyTuple as
  , anonArgs = 0 -- BUG: see below
  }

infixOp :: Ord s => Arrow s -> QOp s -> Arrow s -> Arrow s
infixOp a1 op a2 =
  Arrow
  { code = InfixOp (code a1) op (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0 -- BUG: as above
  }
first :: (?l::s, Ord s) => Arrow s -> Tuple s -> Arrow s
first a ps =
  Arrow
  { code = Op first_exp [code a]
  , context = context a `unionTuple` ps
  , anonArgs = 0
  }
(|||) :: (?l::s, Ord s) => Arrow s -> Arrow s -> Arrow s
a1 ||| a2 =
  Arrow
  { code = InfixOp (code a1) choice_op (code a2)
  , context = context a1 `unionTuple` context a2
  , anonArgs = 0
  }
letCmd :: [VarDecl s (Arrow s)] -> Arrow s -> Arrow s
letCmd defs a =
  Arrow
  { code = Let (map (fmap code) defs) (code a)
  , context = context a
  , anonArgs = anonArgs a
  }

compose :: (Eq s) =>  Code s -> Code s -> Code s
compose ReturnA a = a
compose a ReturnA = a
compose a1@(Arr n1 p1 ds1 e1) a2@(Arr n2 p2 ds2 e2)
  | n1 /= n2 = Compose a1 [] a2 -- could do better, but can this arise?
  | same p2 e1 = Arr n1 p1 (ds1 ++ ds2) e2
  | otherwise = Arr n1 p1 (ds1 ++ BindCase p2 e1 : ds2) e2
compose (Compose f1 as1 g1) (Compose f2 as2 g2) =
  Compose f1 (as1 ++ (compose g1 f2 : as2)) g2
compose a (Compose f bs g) = Compose (compose a f) bs g
compose (Compose f as g) b = Compose f as (compose g b)
compose a1 a2 = Compose a1 [] a2

toHaskell :: (?l::l, Ord l)=> Arrow l -> Exp l
toHaskell = toHaskellCode . code
  where
    toHaskellCode ReturnA = returnA_exp
    toHaskellCode (Arr n p bs e) =
      App ?l arr_exp (times n (Paren ?l . App ?l first_exp) body)
      where
        body = Paren ?l (Lambda ?l [p] (foldr addBinding e bs))
        addBinding (BindLet ds) e = H.Let ?l ds e
        addBinding (BindCase p e) e' =
          Case ?l e [Alt ?l p (UnGuardedRhs ?l e') Nothing]
    toHaskellCode (Compose f as g) =
      foldr (comp . toHaskellArg) (toHaskellArg g) (f : as)
      where
        comp f g = InfixApp ?l f compose_op g
    toHaskellCode (Op op as) = foldl (App ?l) op (map toHaskellCode as)
    toHaskellCode (InfixOp a1 op a2) =
      InfixApp ?l (toHaskellArg a1) op (toHaskellArg a2)
    toHaskellCode (Let nas a) =
      H.Let ?l (BDecls ?l $ map toHaskellDecl nas) (toHaskellCode a)
      where
        toHaskellDecl (VarDecl loc n a) =
          PatBind loc (PVar loc n) (UnGuardedRhs loc (toHaskellCode a)) Nothing
    toHaskellCode (Ifte cond th el) = If ?l cond (toHaskellCode th) (toHaskellCode el)

    toHaskellArg a = toHaskellCode a

newtype Tuple s = Tuple (Set (Name s))

isEmptyTuple :: Tuple s -> Bool
isEmptyTuple (Tuple t) = Set.null t

patternTuple :: (?l::l) => Tuple l -> Pat l
patternTuple (Tuple t) = PTuple ?l Boxed (map (PVar ?l) (Set.toList t))

expTuple :: (?l::l) => Tuple l -> Exp l
expTuple (Tuple t) = H.Tuple ?l Boxed (map (Var ?l . UnQual ?l) (Set.toList t))

emptyTuple :: Tuple s
emptyTuple = Tuple Set.empty
unionTuple :: Ord s => Tuple s -> Tuple s -> Tuple s
unionTuple (Tuple a) (Tuple b) = Tuple (a `Set.union` b)

minusTuple :: Ord s => Tuple s -> Set (Name s) -> Tuple s
Tuple t `minusTuple` vs = Tuple (t `Set.difference` vs)
intersectTuple :: Ord s => Tuple s -> Set (Name s) -> Tuple s
Tuple t `intersectTuple` vs = Tuple (t `Set.intersection` vs)
