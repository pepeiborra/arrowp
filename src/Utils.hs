{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Utils where

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Language.Haskell.Exts

freeVars :: forall a l . (Data a, Data l, Ord l) => a -> Set (Name l)
freeVars = foldMap isName  . universeBi
  where
    isName :: Exp l -> Set (Name l)
    isName (Var _ (UnQual _ n)) = [n]
    isName _                    = []

class DefinedVars a where
  definedVars :: (Ord l, Data l) => a l -> Set (Name l)

instance DefinedVars Decl where
  definedVars (FunBind _ (Match _ n _ _ _:_)) = Set.singleton n
  definedVars (PatBind _ p _ _)               = freeVars p
  definedVars _                               = Set.empty

instance DefinedVars Binds where
  definedVars (BDecls _ stmts) = foldMap definedVars stmts
  definedVars IPBinds{}        = []

instance DefinedVars Stmt where
  definedVars (Generator _ p _) = freeVars p
  definedVars (LetStmt _ decls) = definedVars decls
  definedVars (RecStmt _ stmts) = foldMap definedVars stmts
  definedVars Qualifier{}       = []

same :: Eq s => Pat s -> Exp s -> Bool
same (PApp _ n1 []) (Con _ n2) = n1 == n2
same (PVar l n1) (Var _ n2) = UnQual l n1 == n2
same (PTuple _ boxed ps) (Tuple _ boxed' es) =
  length ps == length es && boxed == boxed' && and (zipWith same ps es)
same (PAsPat _ n _) (Var _ (UnQual _ n')) = n == n'
same (PAsPat _ _ p) e = same p e
same (PParen _ p) e = same p e
same p (Paren _ e) = same p e
same _ _ = False

times :: Int -> (a -> a) -> a -> a
times n f x = iterate f x !! n

hidePat :: (Data l, Ord l) => Set (Name l) -> Pat l -> Pat l
hidePat vs = transform (go vs) where
  go vs p@(PVar l n)
    | n `Set.member` vs = PWildCard l
    | otherwise = p
  go vs (PAsPat _ n p)
    | n `Set.member` vs = go vs p
  go _ x = x

pair :: Exp s -> Exp s -> Exp s
pair e1 e2 = Tuple (ann e1) Boxed [e1, e2]

pairP :: (Data s, Ord s) => Pat s -> Pat s -> Pat s
pairP p1 p2 = PTuple (ann p1) Boxed [hidePat (freeVars p2) p1, p2]

left, right :: Exp ()-> Exp ()
left = App () left_exp
right = App () right_exp

compose_op, choice_op :: QOp ()
returnA_exp, arr_exp, first_exp :: Exp ()
left_exp, right_exp, app_exp, loop_exp :: Exp ()
qualArrowId :: String -> Exp ()
qualArrowId   id = Var () $ Qual () (ModuleName () "Control.Arrow") (Ident () id)
qualArrowSymb :: String -> QOp ()
qualArrowSymb id = QVarOp () $ Qual () (ModuleName () "Control.Arrow") (Symbol () id)
qualArrowCon :: String -> Exp ()
qualArrowCon  id = Con () $ Qual () (ModuleName () "Control.Arrow") (Symbol () id)
arr_exp       = qualArrowId "arr"
compose_op    = qualArrowSymb ">>>"
first_exp     = qualArrowId "first"
returnA_exp   = qualArrowId "returnA"
choice_op     = qualArrowSymb "|||"
left_exp      = qualArrowCon "Left"
right_exp     = qualArrowCon "Right"
app_exp       = qualArrowId "app"
loop_exp      = qualArrowId "loop"
