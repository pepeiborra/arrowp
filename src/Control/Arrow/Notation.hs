{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Arrow.Notation
  ( translateModule
  , translateExp
  ) where

import           Data.Generics
import           Language.Haskell.Exts as H hiding (Tuple)

import           ArrSyn

translateModule :: forall l. (Data l, Ord l) => Module l -> Module l
translateModule = everywhere (mkT (translateExp :: Exp l -> Exp l))


translateExp :: (Data l, Ord l) => Exp l -> Exp l
translateExp (Proc l pat exp) =
      let ?l = l
      in ArrSyn.translate pat (fromHaskell exp)
translateExp other = other
