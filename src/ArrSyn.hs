{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module ArrSyn
  ( translate
  ) where

import           ArrCode
import           Utils

import           Control.Monad.Trans.State
import           Data.Data
import           Data.List                    (mapAccumL)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Language.Haskell.Exts        (Alt (..), Annotated (..),
                                               Binds (..), Decl (..), Exp (),
                                               GuardedRhs (..), Match (..),
                                               Name, Pat (..), QOp (..),
                                               Rhs (..), Stmt (..))
import qualified Language.Haskell.Exts        as H
import           Language.Haskell.Exts.Pretty

translate :: (Data s, Ord s) => Pat s -> Exp s -> Exp s
translate p c = toHaskell (transCmd s p' c)
      where   (s, p') = startPattern p

returnCmd :: (?l::s) => Exp s -> Exp s
returnCmd = H.LeftArrApp ?l returnA_exp

data TransState s = TransState {
      locals  :: Set (Name s),   -- vars in scope defined in this proc
      cmdVars :: Map (Name s) (Arrow s)
}

input :: TransState s -> Tuple s
input s = Tuple (locals s)

startPattern :: (Data s, Ord s) => Pat s -> (TransState s, Pat s)
startPattern p =
      (TransState {
              locals = freeVars p,
              cmdVars = Map.empty
       }, p)

class AddVars a where
      addVars :: (Data s, Ord s) => TransState s -> a -> (TransState s, a)

instance AddVars a => AddVars [a] where
      addVars = mapAccumL addVars

instance AddVars a => AddVars (Maybe a) where
  addVars = mapAccumL addVars

instance (Data s, Ord s) => AddVars (Pat s) where
      addVars s p =
              (s {locals = locals s `Set.union` freeVars p}, p)

instance (Data s, Ord s) => AddVars (Decl s) where
      addVars s d@(FunBind l (Match _ n _ _ _:_)) =
              (s', d)
              where   (s', _) = addVars s (PVar l n)
      addVars s (PatBind loc p rhs decls) =
              (s', PatBind loc p' rhs decls)
              where   (s', p') = addVars s p
      addVars s d = (s, d)

instance (Data s, Ord s) => AddVars (Stmt s) where
      addVars s it@Qualifier{} = (s, it)
      addVars s (Generator loc p c) =
              (s', Generator loc p' c)
              where   (s', p') = addVars s p
      addVars s (LetStmt l decls) =
              (s', LetStmt l decls')
              where   (s', decls') = addVars s decls
      addVars s (RecStmt l stmts) =
              (s', RecStmt l stmts')
              where   (s', stmts') = addVars s stmts

instance (Data s, Ord s) => AddVars (Binds s) where
  addVars s (BDecls l decls) = BDecls l <$> addVars s decls
  addVars s it@IPBinds{}     = (s, it)

transCmd :: (Ord s, Data s) => TransState s -> Pat s -> Exp s -> Arrow s
transCmd s p (H.LeftArrApp l f e)
      | Set.null (freeVars f `Set.intersection` locals s) =
              let ?l=l in arr 0 (input s) p e >>> arrowExp f
      | otherwise =
              let ?l=l in arr 0 (input s) p (pair f e) >>> app
transCmd s p (H.LeftArrHighApp  l f e) = transCmd s p (H.LeftArrApp l f e)
transCmd s p (H.RightArrApp     l f e) = transCmd s p (H.LeftArrApp l e f)
transCmd s p (H.RightArrHighApp l f e) = transCmd s p (H.LeftArrHighApp l e f)
-- transCmd s p (H.InfixApp l c1 op c2) =
--   applyOp op (map (transCmd s p) c2)
transCmd s p (H.InfixApp l c1 op c2) =
  let ?l=l in infixOp (transCmd s p c1) op (transCmd s p c2)
transCmd s p (H.Let l decls c) = let ?l=l in
      arrLet (anonArgs a) (input s) p decls' e >>> a
      where   (s', decls') = addVars s decls
              (e, a) = transTrimCmd l s' c
transCmd s p (H.If l e c1 c2)
  | Set.null (freeVars e `Set.intersection` locals s) =
      ifte e (transCmd s p c1) (transCmd s p c2)
  | otherwise = let ?l=l in
      arr 0 (input s) p (H.If l e (left e1) (right e2)) >>> (a1 ||| a2)
      where   (e1, a1) = transTrimCmd l s c1
              (e2, a2) = transTrimCmd l s c2
transCmd s p (H.Case l e as) =
  let ?l = l
  in arr 0 (input s) p (H.Case ?l e as') >>> foldr1 (|||) (reverse cases)
  where
    (as', (ncases, cases)) = runState (mapM (transAlt s) as) (0, [])
    transAlt s (Alt loc p gas decls) = do
      let (s', p') = addVars s p
          (s'', decls') = addVars s' decls
      gas' <- transGuardedRhss s'' gas
      return (H.Alt loc p' gas' decls')
    transGuardedRhss s (UnGuardedRhs l c) = do
      body <- newAlt s c
      return (H.UnGuardedRhs l body)
    transGuardedRhss s (GuardedRhss l gas) = do
      gas' <- mapM (transGuardedRhs s) gas
      return (H.GuardedRhss l gas')
    transGuardedRhs s (GuardedRhs loc e c) = do
      body <- newAlt s c
      return (H.GuardedRhs loc e body)
    newAlt s c = do
      let (e, a) =
            transTrimCmd l s c
      (n, as) <- get
      put (n + 1, a : as)
      return (label n e)
    label n e =
      times
        n
        right
        (if n < ncases - 1
           then left e
           else e)
transCmd s p (H.Paren _ c) =
      transCmd s p c
transCmd s p (H.Do l ss) =
      transDo s p (init ss) (let Qualifier _ e = last ss in e)
transCmd s p (H.App l c arg) =
      anon (-1) $
      arr (anonArgs a) (input s) p (pair e arg) >>> a
      where   (e, a) = transTrimCmd l s c
transCmd _ _ x = error $ "transCmd: " ++ prettyPrint x

-- transCmd s p (CmdVar n) =
--       arr (anonArgs a) (input s) p e >>> arrowExp (H.Var ?l (H.UnQual ?l n))
--       where   Just a = Map.lookup n (cmdVars s)
--               e = expTuple (context a)


transTrimCmd :: (Ord s, Data s) => s -> TransState s -> Exp s -> (Exp s, Arrow s)
transTrimCmd l s c = let ?l=l in (expTuple (context a), a)
      where   a = let ?l=l in transCmd s (patternTuple (context a)) c

transDo
  :: forall s. (Data s, Ord s)
  => TransState s -> Pat s -> [Stmt s] -> Exp s -> Arrow s
transDo s p [] c =
      transCmd s p c
transDo s p (Qualifier l exp : ss) c =
  let ?l=l in transCmd s p exp >>> transDo s p ss c
transDo s p (Generator l pg cg:ss) c = let ?l=l in
      if isEmptyTuple u then
        transCmd s p cg >>> transDo s' pg ss c
      else
        arr 0 (input s) p (pair eg (expTuple u)) >>> first ag u >>> a
      where   (s', pg') = addVars s pg
              a = bind (freeVars pg)
                      (transDo s' (pairP pg' (let ?l=l in patternTuple u)) ss c)
              u = context a
              (eg, ag) = transTrimCmd l s cg
transDo s p (LetStmt l decls : ss) c = let ?l=l in
      transCmd s p (H.Let l decls (H.Do l (ss ++ [Qualifier l c])))
transDo s p (RecStmt l rss:ss) c = let ?l=l in
  bind
    defined
    (loop
       (transDo
          s'
          (pairP p (PIrrPat l (patternTuple feedback)))
          rss'
          (returnCmd (pair output (expTuple feedback)))) >>>
     a)
  where
    defined :: Set (Name s)
    defined = foldMap definedVars rss
    (s', rss') = addVars s rss
    (output, a) = transTrimCmd l s' (H.Do l (ss ++ [Qualifier l c]))
    feedback = let ?l=l in
      context
        (transDo
           s'
           p
           rss'
           (returnCmd
              (foldr (pair . H.Var l . H.UnQual l) output (Set.toList defined)))) `intersectTuple`
      defined
