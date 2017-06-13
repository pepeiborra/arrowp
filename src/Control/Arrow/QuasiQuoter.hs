{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Arrow.QuasiQuoter
  ( proc
  , parseModuleWithMode
  ) where

import Data.Maybe

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.ParseMonad
import Language.Haskell.Syntax
import Language.Haskell.Pretty

import Parser

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
quote inp =
  case parseProc ("proc " ++ inp) of
    ParseOk proc -> tr proc
    ParseFailed loc err -> do
      Loc{..} <- location
      error $ printf "%s:%d:%d: %s" loc_filename
                                   (fst loc_start + srcLine loc - 1)
                                   (snd loc_start + srcColumn loc - 1)
                                   err

class Translate hs th | hs -> th where
  tr :: hs -> Q th

trAll xx = traverse tr xx

instance Translate HsExp Exp where
  tr (HsVar name) = VarE <$> tr name
  tr (HsCon name) = ConE <$> tr name
  tr (HsLit lit)  = LitE <$> tr lit
  tr (HsInfixApp a op b) =
    InfixE <$> (Just <$> tr a) <*> tr op <*> (Just <$> tr b)
  tr (HsApp a b) = AppE <$> tr a <*> tr b
  tr (HsLambda _ pats e) = LamE <$> trAll pats <*> tr e
  tr (HsLet decs e) = LetE <$> trAll decs <*> tr e
  tr (HsIf c t e) = CondE <$> tr c <*> tr t <*> tr e
  tr (HsCase e aa) = CaseE <$> tr e <*> trAll aa
  tr (HsDo ss) = DoE <$> trAll ss
  tr (HsTuple ee) = TupE <$> trAll ee
  tr (HsList ee) = ListE <$> trAll ee
  tr (HsParen e) = ParensE <$> tr e
  tr (HsLeftSection  e op) = InfixE <$> (Just <$> tr e) <*> tr op <*> pure Nothing
  tr (HsRightSection op e) = InfixE <$> pure Nothing    <*> tr op <*> (Just <$> tr e)
  tr (HsRecConstr n ff) = RecConE <$> tr n <*> trAll ff
  tr (HsRecUpdate e ff) = RecUpdE <$> tr e <*> trAll ff
  tr (HsEnumFrom e) = ArithSeqE . FromR <$> tr e
  tr (HsEnumFromThen f t) = ArithSeqE <$> (FromThenR <$> tr f <*> tr t)
  tr (HsEnumFromThenTo f t to) = ArithSeqE <$> (FromThenToR <$> tr f <*> tr t <*> tr to)
  tr (HsEnumFromTo f to) = ArithSeqE <$> (FromToR <$> tr f <*> tr to)
  tr (HsListComp e ss) = (\e ss -> CompE (ss ++ [NoBindS e])) <$> tr e <*> trAll ss
  tr (HsExpTypeSig _ e _) = tr e
  tr HsNegApp{} = error "not applicable"
  tr HsWildCard = error "not applicable"
  tr HsAsPat{} = error "not applicable"
  tr HsIrrPat{} = error "not applicable"

instance Translate HsDecl Dec where
  tr (HsFunBind mm@(HsMatch _ n _ _ _ : _)) = FunD <$> (mkName <$> tr n) <*> trAll mm
  tr (HsPatBind _ p r dd) = ValD <$> tr p <*> tr r <*> trAll dd
  tr _ = error "not implemented: HsDecl"

instance Translate HsMatch Clause where
  tr (HsMatch _ _ pats rhs decls) = Clause <$> trAll pats <*> tr rhs <*> trAll decls

instance Translate HsAlt Match where
  tr (HsAlt _ p aa dd ) = Match <$> tr p <*> tr aa <*> trAll dd

instance Translate HsGuardedAlts Body where
  tr (HsGuardedAlts aa) = GuardedB <$> trAll aa
  tr (HsUnGuardedAlt e) = NormalB <$> tr e

instance Translate HsGuardedAlt (Guard,Exp) where
  tr (HsGuardedAlt _ e e') = (,) <$> (NormalG <$> tr e) <*> tr e'

instance Translate HsStmt Stmt where
  tr (HsGenerator _ p e) = BindS <$> tr p <*> tr e
  tr (HsQualifier e) = NoBindS <$> tr e
  tr (HsLetStmt dd)  = LetS <$> trAll dd

instance Translate HsFieldUpdate FieldExp where
  tr (HsFieldUpdate n e) = (,) <$> tr n <*> tr e

instance Translate HsRhs Body where
  tr (HsUnGuardedRhs e) = NormalB <$> tr e
  tr (HsGuardedRhss gg) = GuardedB <$> trAll gg

instance Translate HsGuardedRhs (Guard,Exp) where
  tr (HsGuardedRhs _ e e') = (,) . NormalG <$> tr e <*> tr e'

instance Translate HsLiteral Lit where
  tr (HsChar c) = pure $ CharL c
  tr (HsString s) = pure $ StringL s
  tr (HsInt i) = pure $ IntPrimL i
  tr (HsFrac f) = pure $ RationalL f
  tr (HsCharPrim c) = pure $ CharPrimL c
  tr (HsIntPrim c) = pure $ IntPrimL c
  tr (HsStringPrim s) = pure $ StringL s
  tr (HsFloatPrim s) = pure $ FloatPrimL s
  tr (HsDoublePrim x) = pure $ DoublePrimL x

instance Translate HsQOp Exp where
  tr (HsQVarOp n) = VarE <$> tr n
  tr (HsQConOp n) = VarE <$> tr n

instance Translate HsPat Pat where
  tr (HsPVar n) = VarP . mkName <$> tr n
  tr (HsPLit l) = LitP <$> tr l
  tr (HsPInfixApp p1 n p2) = InfixP <$> tr p1 <*> tr n <*> tr p2
  tr (HsPApp n pats) = ConP <$> tr n <*> trAll pats
  tr (HsPTuple pats) = TupP <$> trAll pats
  tr (HsPList pats)  = ListP <$> trAll pats
  tr (HsPParen pat)  = ParensP <$> tr pat
  tr (HsPRec n pats) = RecP <$> tr n <*> trAll pats
  tr  HsPWildCard    = return WildP
  tr (HsPIrrPat pat) = TildeP <$> tr pat
  tr HsPNeg{} = error "not implemented: HsPNeg"
  tr HsPAsPat{} = error "not implemented: HsPAsPat"

instance Translate HsPatField FieldPat where
  tr (HsPFieldPat n pat) = (,) <$> tr n <*> tr pat

instance Translate HsQName Name where
  tr (UnQual n) = do
    n <- tr n
    mb_n <- lookupValueName n
    case mb_n of
      Just n  -> return n
      Nothing -> return $ mkName n
  tr (Qual (Module m) n) = do
    n <- tr n
    fromMaybe (error $ printf "Not found: %s.%s" m n) <$> lookupValueName (m ++ "." ++ n)
  tr Special{} = error "not implemented: Special"

instance Translate HsName [Char] where
  tr (HsSymbol s) = return s
  tr (HsIdent  n) = return n
