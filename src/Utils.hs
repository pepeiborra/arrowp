{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -Wno-orphans #-}
module Utils
  ( module Utils
  , varss
  , freeVars
  , pvars
  )where

import           Control.Monad.Trans.State
import           Data.Data
import           Data.Functor.Identity
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.Map                       (Map)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Debug.Hoed.Pure                hiding (Module)
import           Language.Haskell.Exts
import           Language.Haskell.Exts.FreeVars
#ifdef DEBUG
import           Language.Haskell.Exts.Observe  ()
#endif

type FreeVarsFun l = Maybe(Set (Name l)) -> (Set(Name l), Maybe(Set(Name l)))
type FreeVars l x = x -> FreeVarsFun l

definedVars :: AllVars a => a -> Set(Name (SrcLocType a))
definedVars = pvars

-- | Are a tuple pattern and an expression tuple equal ?
same ::(Observable(Exp s), Observable(Pat s), Eq s) => Pat s -> Exp s -> Bool
same = observe "same" same'

same' :: (Observable (Pat s), Observable (Exp s), Eq s) => Pat s -> Exp s -> Bool
same' (PApp _ n1 []) (Con _ n2) = n1 == n2
same' (PVar l n1) (Var _ n2) = UnQual l n1 == n2
same' (PTuple _ Boxed []) y = same (PApp (ann y) (unit_con_name (ann y)) []) y
same' (PTuple _ Boxed [pv]) y = same pv y
same' y (Tuple _ Boxed []) = same y (unit_con(ann y))
same' y (Tuple _ Boxed [pv]) = same y pv
same' (PTuple _ boxed ps) (Tuple _ boxed' es) =
  length ps == length es && boxed == boxed' && and (zipWith same ps es)
same' (PAsPat _ n _) (Var _ (UnQual _ n')) = n == n'
same' (PAsPat _ _ p) e = same p e
same' (PParen _ p) e = same p e
same' p (Paren _ e) = same p e
same' _ _ = False

times :: Int -> (a -> a) -> a -> a
times n f x = iterate f x !! n

-- | Hide variables from a pattern
hidePat :: (Data l, Ord l) => Set (Name l) -> Pat l -> Pat l
hidePat vs = transform (go vs) where
  go vs p@(PVar l n)
    | n `Set.member` vs = PWildCard l
    | otherwise = p
  go vs (PAsPat _ n p)
    | n `Set.member` vs = go vs p
  go _ x = x

pair :: Exp s -> Exp s -> Exp s
pair e1 e2 = Tuple (ann e1) Boxed [e1, e2]

pairP
  :: ( Data l
     , Ord l
     , Observable l
     , Observable (Pat l)
     , Observable (Exp l)
     , Observable (Set(Name l))
     , Observable [Stmt l]
     )
  => Pat l -> Pat l -> Pat l
pairP p1 p2 = PTuple (ann p1) Boxed [hidePat (definedVars p2) p1, p2]

left, right :: Exp l -> Exp l
left x = App (ann x) (fmap (const$ ann x) left_exp) x
right x = App (ann x) (fmap (const $ ann x) right_exp) x

returnCmd :: Exp l -> Exp l
returnCmd x = LeftArrApp (ann x) (fmap (const $ ann x) returnA_exp) x

compose_op, choice_op :: QOp ()
returnA_exp, arr_exp, first_exp :: Exp ()
left_exp, right_exp, app_exp, loop_exp :: Exp ()
unqualId :: String -> Exp ()
unqualId   id = Var () $ UnQual () (Ident () id)
unqualOp :: String -> QOp ()
unqualOp id = QVarOp () $ UnQual () (Symbol () id)
unqualCon :: String -> Exp ()
unqualCon  id = Con () $ UnQual () (Symbol () id)
arr_exp       = unqualId "arr"
compose_op    = unqualOp ">>>"
first_exp     = unqualId "first"
returnA_exp   = unqualId "returnA"
choice_op     = unqualOp "|||"
left_exp      = unqualCon "Left"
right_exp     = unqualCon "Right"
app_exp       = unqualId "app"
loop_exp      = unqualId "loop"

observeSt
  :: (Observable a, Observable b, Observable c, Observable s)
  => String -> (a -> b -> State s c) -> a -> b -> State s c
observeSt name f a b = StateT $ \s -> Identity $ observe name f' a b s
  where
    f' a b = runState (f a b)

instance (Eq a, Show a) => Observable (Set a) where
  constrain = constrainBase
  observer = observeBase

instance (Eq a, Eq k, Show a, Show k) => Observable (Map k a) where
  constrain = constrainBase
  observer = observeBase

-- Override some AST instances for comprehension
instance {-# OVERLAPS #-} Observable (Exp()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Name()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (QName()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable [Stmt()] where
  observer lit cxt =
    seq lit $ send (intercalate ";" $ fmap prettyPrint lit) (return lit) cxt
instance {-# OVERLAPS #-} Observable (Stmt()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Pat()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (QOp()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Op()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Rhs()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Alt()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Set (Name())) where
  constrain = constrainBase
  observer x cxt =
    seq x $ send (bracket $ intercalate "," $ prettyPrint <$> Set.toList x) (return x) cxt

observePretty lit cxt = seq lit $ send (prettyPrint lit) (return lit) cxt
bracket s = '[' : s ++ "]"
