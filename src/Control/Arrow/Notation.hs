{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Arrow.Notation
  ( translateModule
  , translateExp
  ) where

import           Control.Monad
import           Data.Generics
import           Debug.Hoed.Pure       (observe)
import           Language.Haskell.Exts as H hiding (Tuple)

import           ArrSyn

translateModule :: Module () -> Module ()
translateModule = observe "translateModule" (everywhere (mkT translateExp))


translateExp :: Exp () -> Exp ()
translateExp = observe "translateExp" translateExp'
translateExp' :: Exp () -> Exp ()
translateExp' (Proc _ pat exp) = ArrSyn.translate pat exp
translateExp' other            = void other
