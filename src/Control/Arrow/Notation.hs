{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Arrow.Notation
  ( translateModule
  , translateExp
  ) where

import           Control.Monad
import           Data.Generics
import           Language.Haskell.Exts as H hiding (Tuple)

import           ArrSyn

translateModule :: Module () -> Module ()
translateModule = everywhere (mkT translateExp)


translateExp :: Exp () -> Exp ()
translateExp (Proc l pat exp) =
      let ?l = l
      in ArrSyn.translate pat exp
translateExp other = void other
