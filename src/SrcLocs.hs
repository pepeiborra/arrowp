{-# LANGUAGE DeriveDataTypeable #-}
module SrcLocs where

import           Data.Data
import           Data.Default
import           Debug.Hoed.Pure
import           Language.Haskell.Exts

-- | The type of src code locations used by arrowp-qq
newtype S = S {getSrcSpanInfo :: SrcSpanInfo}
  deriving (Data, Typeable)
instance Eq S where _ == _ = True
instance Ord S where compare _ _ = EQ
instance Show S where show _ = "<loc>"

instance Default S where
  def = S noSrcSpan

instance Observable S where
  observer = observeOpaque "<loc>"
  constrain = constrainBase
