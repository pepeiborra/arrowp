{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Arrow.Notation
  ( translateModule
  , translateExp
  ) where

import           Data.Generics.Uniplate.Data
import           Language.Haskell.Exts       as H hiding (Tuple)

import           ArrSyn
import           Utils

translateModule :: Module SrcSpanInfo -> Module SrcSpanInfo
translateModule = transformBi translateExp

translateExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo
translateExp (Proc _ pat exp) =
  getSrcSpanInfo <$> ArrSyn.translate (fmap S pat) (fmap S exp)
translateExp (RightArrApp l e f) = LeftArrApp l f e
translateExp (RightArrHighApp l e f) = LeftArrHighApp l f e
translateExp other = other
