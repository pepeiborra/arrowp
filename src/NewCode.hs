{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
module NewCode where

import           Data.Data
import           Data.Default
import           Debug.Hoed.Pure
import           GHC.Generics                  (Generic)
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.Util
import           SrcLocs
#ifdef DEBUG
import           Language.Haskell.Exts.Observe ()
#endif

-- | AST annotations to extend the Haskell AST with an arrow core language
data Code
  = ReturnCode
  | ArrCode Int [Binding]
  | ComposeCode
  | OpCode
  | Loc S
  deriving (Eq, Data, Ord, Generic, Show)

instance Default Code where
  def = Loc def

instance Observable Code

getLoc :: Code -> S
getLoc (Loc s) = s
getLoc other   = error $ "getLoc: " ++ show other

pattern ReturnA = ExprHole ReturnCode
pattern Arr i pat bb e = Lambda (ArrCode i bb) [pat] e
pattern Compose a bb c <- List ComposeCode ( split -> (a,bb,c) )
  where
    Compose a bb c = List ComposeCode (a : bb ++ [c])

pattern Op e args = List OpCode (e:args)

split :: [c] -> (c, [c], c)
split (h:rest@(_:_)) = (h, init rest, last rest)
split _              = error "Compose: unreachable"

data Binding = BindLet (Binds Code) | BindCase (Pat Code) (Exp Code)
  deriving (Eq, Data, Ord, Generic, Show)

instance Observable Binding

instance Located Binding where
  type LocType Binding = Code
  location f (BindLet b)    = BindLet <$> location f b
  location f (BindCase p e) = BindCase <$> location f p <*> location f e
