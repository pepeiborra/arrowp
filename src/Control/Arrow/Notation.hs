{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Arrow.Notation
  ( translateModule
  , translateExp
  ) where

import           Data.Generics.Uniplate.Data
import           Debug.Hoed.Pure             (observe)
import           Language.Haskell.Exts       as H hiding (Tuple)

import           ArrSyn
import           Utils

translateModule :: Module SrcSpanInfo -> Module SrcSpanInfo
translateModule = observe "translateModule" (everywhere (mkT translateExp))


translateExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo
translateExp = translateExp'
translateExp' :: Exp SrcSpanInfo -> Exp SrcSpanInfo
translateExp' (Proc _ pat exp) =
  fmap getSrcSpanInfo $ ArrSyn.translate (fmap S pat) (fmap S exp)
translateExp' other            = other
