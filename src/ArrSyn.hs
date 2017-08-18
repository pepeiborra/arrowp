{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module ArrSyn
  ( translate
  ) where

import           ArrCode
import           Utils

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.List                 (mapAccumL)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Debug.Hoed.Pure
import           Language.Haskell.Exts     (Alt (..), Binds (..), Decl (..),
                                            Exp (), GuardedRhs (..), Match (..),
                                            Name, Pat (..), Rhs (..), Stmt (..))

import qualified Language.Haskell.Exts     as H

-- -----------------------------------------------------------------------------
-- Translation to Haskell

-- This is a 2-phase process:
-- - transCmd' generates an abstract arrow combinator language represented
--   by the Arrow type, and
-- - toHaskell turns that into Haskell.

translate :: Pat s -> Exp s -> Exp ()
translate p c = toHaskell (transCmd s p' (void c))
      where   (s, p') = startPattern $ void p

startPattern :: Pat () -> (TransState, Pat ())
startPattern = observe "startPattern" startPattern'

startPattern' :: Pat () -> (TransState, Pat ())
startPattern' p =
      (TransState {
              locals = freeVars p,
              cmdVars = Map.empty
       }, p)
-- The pattern argument is often pseudo-recursively defined in terms of
-- the context part of the result of these functions.  (It's not real
-- recursion, because that part is independent of the pattern.)

transCmd :: TransState -> Pat () -> Exp () -> Arrow
transCmd = observe "transCmd" transCmd'

transCmd' :: TransState -> Pat () -> Exp () -> Arrow
transCmd' s p (H.LeftArrApp () f e)
      | Set.null (freeVars f `Set.intersection` locals s) =
              arr 0 (input s) p e >>> arrowExp f
      | otherwise =
              arr 0 (input s) p (pair f e) >>> app
transCmd' s p (H.LeftArrHighApp  l f e) = transCmd s p (H.LeftArrApp l f e)
transCmd' s p (H.RightArrApp     l f e) = transCmd s p (H.LeftArrApp l e f)
transCmd' s p (H.RightArrHighApp l f e) = transCmd s p (H.LeftArrHighApp l e f)
transCmd' s p (H.InfixApp () c1 op c2) =
  infixOp (transCmd s p c1) op (transCmd s p c2)
transCmd' s p (H.Let () decls c) =
      arrLet (anonArgs a) (input s) p decls' e >>> a
      where   (s', decls') = addVars' s decls
              (e, a) = transTrimCmd s' c
transCmd' s p (H.If l e c1 c2)
  | Set.null (freeVars e `Set.intersection` locals s) =
      ifte e (transCmd s p c1) (transCmd s p c2)
  | otherwise =
      arr 0 (input s) p (H.If l e (left e1) (right e2)) >>> (a1 ||| a2)
      where   (e1, a1) = transTrimCmd s c1
              (e2, a2) = transTrimCmd s c2
transCmd' s p (H.Case () e as) =
   arr 0 (input s) p (H.Case () e as') >>> foldr1 (|||) (reverse cases)
  where
    (as', (ncases, cases)) = runState (mapM (transAlt s) as) (0, [])
    transAlt s (Alt loc p gas decls) = do
      let (s', p') = addVars' s p
          (s'', decls') = addVars' s' decls
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
            transTrimCmd s c
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
transCmd' s p (H.Paren _ c) =
      transCmd s p c
transCmd' s p (H.Do () ss) =
      transDo s p (init ss) (let Qualifier _ e = last ss in e)
transCmd' s p (H.App () c arg) =
      anon (-1) $
      arr (anonArgs a) (input s) p (pair e arg) >>> a
      where   (e, a) = transTrimCmd s c
transCmd' s p (H.Lambda () ps c) =
  anon (length ps) $ bind (freeVars ps) $ transCmd s' (foldl pairP p ps') c
  where
    (s', ps') = addVars' s ps
transCmd' _ _ x = error $ "transCmd: " ++ show x

-- transCmd' s p (CmdVar n) =
--       arr (anonArgs a) (input s) p e >>> arrowExp (H.Var () (H.UnQual () n))
--       where   Just a = Map.lookup n (cmdVars s)
--               e = expTuple (context a)

-- Like TransCmd, but use the minimal input pattern.  The first component
-- of the result is the matching expression to build this input.
-- That is, the result is (e, proc p' -> c) with the minimal p' such that

-- 	proc p -> c = arr (first^n (p -> e)) >>> (proc p' -> c)

-- where n is the number of anonymous arguments taken by c.
transTrimCmd :: TransState -> Exp () -> (Exp (), Arrow)
transTrimCmd = observe "transTrimCmd" transTrimCmd'
transTrimCmd' :: TransState -> Exp () -> (Exp (), Arrow)
transTrimCmd' s c = (expTuple (context a), a)
      where   a = transCmd s (patternTuple (context a)) c

transDo :: TransState -> Pat () -> [Stmt ()] -> Exp () -> Arrow
transDo = observe "transDo" transDo'

transDo' :: TransState -> Pat () -> [Stmt ()] -> Exp () -> Arrow
transDo' s p [] c =
      transCmd s p c
transDo' s p (Qualifier () exp : ss) c =
  transDo' s p (Generator () (PWildCard ()) exp : ss) c
transDo' s p (Generator () pg cg:ss) c =
      if isEmptyTuple u then
        transCmd s p cg >>> transDo s' pg ss c
      else
        arr 0 (input s) p (pair eg (expTuple u)) >>> first ag u >>> a
      where   (s', pg') = addVars' s pg
              a = bind (freeVars pg)
                      (transDo s' (pairP pg' (patternTuple u)) ss c)
              u = context a
              (eg, ag) = transTrimCmd s cg
transDo' s p (LetStmt l decls : ss) c =
      transCmd s p (H.Let l decls (H.Do l (ss ++ [Qualifier l c])))
transDo' s p (RecStmt l rss:ss) c =
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
    defined :: Set (Name ())
    defined = foldMap definedVars rss
    (s', rss') = addVars' s rss
    (output, a) = transTrimCmd s' (H.Do l (ss ++ [Qualifier l c]))
    feedback =
      context
        (transDo
           s'
           p
           rss'
           (returnCmd
              (foldr (pair . H.Var l . H.UnQual l) output (Set.toList defined)))) `intersectTuple`
      defined

data TransState = TransState {
      locals  :: Set (Name ()),   -- vars in scope defined in this proc
      cmdVars :: Map (Name ()) Arrow
  } deriving (Eq, Generic, Show)

instance Observable TransState

input :: TransState -> Tuple
input s = Tuple (locals s)

addVars' :: (Observable a, AddVars a) => TransState -> a -> (TransState, a)
addVars' = observe "addVars" addVars

class AddVars a where
      addVars :: TransState -> a -> (TransState, a)

instance AddVars a => AddVars [a] where
      addVars = mapAccumL addVars

instance AddVars a => AddVars (Maybe a) where
  addVars = mapAccumL addVars

instance AddVars (Pat ()) where
      addVars s p =
              (s {locals = locals s `Set.union` freeVars p}, p)

instance AddVars (Decl ()) where
      addVars s d@(FunBind l (Match _ n _ _ _:_)) =
              (s', d)
              where   (s', _) = addVars s (PVar l n)
      addVars s (PatBind loc p rhs decls) =
              (s', PatBind loc p' rhs decls)
              where   (s', p') = addVars s p
      addVars s d = (s, d)

instance AddVars (Stmt ()) where
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

instance AddVars (Binds ()) where
  addVars s (BDecls l decls) = BDecls l <$> addVars s decls
  addVars s it@IPBinds{}     = (s, it)