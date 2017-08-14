{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Arrow.QuasiQuoter
  ( proc
  ) where

import Control.Arrow.Notation

import Language.Haskell.Exts as Exts hiding (Exp, Loc)
import Language.Haskell.Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Printf

-- | A quasiquoter for arrow notation.
--   To be used as follows:
--
--   @
--      arr f = BST [proc| (b, s) -> do
-- 			returnA -< (f b, s) |]
--   @

proc :: QuasiQuoter
proc = QuasiQuoter
  { quoteExp  = quote
  , quotePat  = error "proc: pattern quotes not supported"
  , quoteType = error "proc: type quotes not supported"
  , quoteDec  = error "proc: dec quotes not supported"
  }

quote :: String -> Q Exp
quote = quoteEx defaultParseMode { extensions = defaultExtensions }
  where
    defaultExtensions = []

quoteEx :: ParseMode -> String -> Q Exp
quoteEx mode inp =
  case parseExpWithMode mode ("proc " ++ inp) of
    ParseOk proc -> return $ toExp $ translateExp proc
    ParseFailed loc err -> do
      Loc{..} <- location
      error $ printf "%s:%d:%d: %s" loc_filename
                                   (fst loc_start + srcLine loc - 1)
                                   (snd loc_start + srcColumn loc - 1)
                                   err



