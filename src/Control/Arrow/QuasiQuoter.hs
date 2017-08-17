{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Arrow.QuasiQuoter
  ( proc
  ) where

import Control.Arrow.Notation
import Control.Monad
import Data.List

import Language.Haskell.Exts as Exts hiding (Exp, Loc)
import Language.Haskell.Meta
import Language.Haskell.TH (Exp, Q, Loc(..), location )
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

quoteEx :: ParseMode -> String -> Q Exp
quoteEx mode inp =
  case parseExpWithMode mode ("proc " ++ inp) of
    ParseOk proc -> return $ toExp $ translateExp (void proc)
    ParseFailed loc err -> do
      Loc{..} <- location
      error $ printf "%s:%d:%d: %s" loc_filename
                                   (fst loc_start + srcLine loc - 1)
                                   (snd loc_start + srcColumn loc - 1)
                                   err
defaultExtensions :: [Extension]
defaultExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

badExtensions :: [KnownExtension]
badExtensions =
    [TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,DoRec, RecursiveDo -- breaks rec
    ,TypeApplications -- HSE fails on @ patterns
    ]
