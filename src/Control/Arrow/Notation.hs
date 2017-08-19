{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Arrow.Notation
  ( translateModule
  , translateExp
  ) where

import           Data.Generics
import           Debug.Hoed.Pure       (observe)
import           Language.Haskell.Exts as H hiding (Tuple)

import           ArrSyn
import           Utils

translateModule :: Module SrcSpanInfo -> Module SrcSpanInfo
translateModule = observe "translateModule" (everywhere (mkT translateExp))


translateExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo
translateExp = observe "translateExp" translateExp'
translateExp' :: Exp SrcSpanInfo -> Exp SrcSpanInfo
translateExp' (Proc _ pat exp) =
  fmap getSrcSpanInfo $ ArrSyn.translate (fmap SrcSpanInfoDef pat) (fmap SrcSpanInfoDef exp)
translateExp' other            = other
