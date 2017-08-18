{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -Wno-orphans #-}
module Utils where

import           Data.Data
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.Map                      (Map)
import           Data.Monoid                   ((<>))
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Debug.Hoed.Pure               hiding (Module)
import           Language.Haskell.Exts
#ifdef DEBUG
import           Language.Haskell.Exts.Observe ()
#endif

type FreeVarsFun = Maybe(Set (Name ())) -> (Set(Name ()), Maybe(Set(Name())))
type FreeVars x = x -> FreeVarsFun

freeVars :: (Data a, Observable a, Typeable a) => a -> Set(Name())
freeVars = observe "freeVars" freeVars'

freeVars' :: (Data a, Typeable a) => a -> Set(Name())
freeVars' x = everythingWithContext (Just mempty) (<>) collect x
  where
    collect :: GenericQ FreeVarsFun
    collect =
      mkQ (Set.empty, )
       collectExp `extQ`
       collectPat `extQ`
       collectStmts `extQ`
       collectQualStmts `extQ`
       collectGRHS `extQ`
       collectBound @Alt   `extQ`
       collectBound @Decl  `extQ`
       collectBound @Match `extQ`
       collectBound @Binds
    collectExp :: FreeVars (Exp())
    collectExp _ Nothing = ([], Nothing)
    collectExp (Var _ (UnQual _ n)) (Just bound)
      | not (n `Set.member` bound) = ([n], Just bound)
    collectExp (Lambda _ pats _) (Just bound) = ([], Just $ freeVars pats <> bound)
    collectExp _ bound = ([], bound)

    collectPat _ Nothing = ([], Nothing)
    collectPat (PVar _ n)      bound = ([n], bound)
    collectPat (PNPlusK _ n _) bound = ([n], bound)
    collectPat (PAsPat _ n _)  bound = ([n], bound)
    collectPat (PRec _ _ fields) bound =
      (Set.fromList [ n | PFieldPun _ (UnQual _ n) <- fields], bound)
    collectPat _ bound = ([], bound)

    -- For statement sequences we want a different recursion pattern
    -- The bindings defined by a statement are in scope in the rest of the sequence
    collectStmts :: FreeVars [Stmt()]
    collectStmts stmts (Just bound) =
      (foldr addStmt mempty stmts `Set.difference` bound, Nothing)
    collectStmts _ Nothing = ([], Nothing)

    addStmt (Generator _ p e) s =
      freeVars e `Set.union` (s `Set.difference` freeVars p)
    addStmt (Qualifier _ e) _ = freeVars e
    addStmt (LetStmt _ decls) s =
      (freeVars decls `Set.union` s) `Set.difference` definedVars decls
    addStmt (RecStmt _ decls) s =
      (freeVars decls `Set.union` s) `Set.difference` foldMap definedVars decls

    collectQualStmts :: FreeVars [QualStmt()]
    collectQualStmts _ Nothing = ([], Nothing)
    collectQualStmts stmts (Just bound) =
      (foldr addQualStmt mempty stmts `Set.difference` bound, Nothing)

    addQualStmt (QualStmt _ stmt) s      = addStmt stmt s
    addQualStmt (ThenTrans _ e) s        = freeVars e <> s
    addQualStmt (ThenBy _ e1 e2) s       = freeVars e1 <> freeVars e2 <> s
    addQualStmt (GroupBy _ e) s          = freeVars e <> s
    addQualStmt (GroupUsing _ e) s       = freeVars e <> s
    addQualStmt (GroupByUsing _ e1 e2) s = freeVars e1 <> freeVars e2 <> s

    collectBound :: DefinedVars b => FreeVars (b())
    collectBound _ Nothing      = ([], Nothing)
    collectBound d (Just bound) = ([], Just $ definedVars d <> bound)

    collectGRHS :: FreeVars (GuardedRhs())
    collectGRHS _ Nothing = ([], Nothing)
    collectGRHS (GuardedRhs _ stmts _) (Just bound) =
      (freeVars stmts, Just $ bound <> foldMap definedVars stmts)

class DefinedVars a where
  definedVars :: a () -> Set (Name ())

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

instance DefinedVars Match where
  definedVars (Match _ _ pat _ binds) =
    foldMap freeVars pat <> foldMap definedVars binds
  definedVars (InfixMatch _ pat _ pats _ binds) =
    foldMap freeVars (pat:pats) <> foldMap definedVars binds

instance DefinedVars Alt where
  definedVars (Alt _ pat _ binds) =  freeVars pat <> foldMap definedVars binds

instance DefinedVars GuardedRhs where
  definedVars (GuardedRhs _ stmts _) = foldMap definedVars stmts


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

pairP :: Pat () -> Pat () -> Pat ()
pairP p1 p2 = PTuple (ann p1) Boxed [hidePat (freeVars p2) p1, p2]

left, right :: Exp ()-> Exp ()
left = App () left_exp
right = App () right_exp

returnCmd :: Exp () -> Exp ()
returnCmd = LeftArrApp () returnA_exp

compose_op, choice_op :: QOp ()
returnA_exp, arr_exp, first_exp :: Exp ()
left_exp, right_exp, app_exp, loop_exp :: Exp ()
unqualId :: String -> Exp ()
unqualId   id = Var () $ UnQual () (Ident () id)
unqualOp :: String -> QOp ()
unqualOp id = QVarOp () $ UnQual () (Symbol () id)
unqualCon :: String -> Exp ()
unqualCon  id = Con () $ UnQual () (Symbol () id)
arr_exp       = unqualId "arr"
compose_op    = unqualOp ">>>"
first_exp     = unqualId "first"
returnA_exp   = unqualId "returnA"
choice_op     = unqualOp "|||"
left_exp      = unqualCon "Left"
right_exp     = unqualCon "Right"
app_exp       = unqualId "app"
loop_exp      = unqualId "loop"


instance (Eq a, Show a) => Observable (Set a) where
  constrain = constrainBase
  observer = observeBase

instance (Eq a, Eq k, Show a, Show k) => Observable (Map k a) where
  constrain = constrainBase
  observer = observeBase

-- Override some AST instances for comprehension
instance {-# OVERLAPS #-} Observable (Exp()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Name()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (QName()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable [Stmt()] where
  observer lit cxt =
    seq lit $ send (intercalate ";" $ fmap prettyPrint lit) (return lit) cxt
instance {-# OVERLAPS #-} Observable (Stmt()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Pat()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (QOp()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Op()) where
  observer = observePretty
instance {-# OVERLAPS #-} Observable (Set (Name())) where
  constrain = constrainBase
  observer x cxt =
    seq x $ send (bracket $ intercalate "," $ prettyPrint <$> Set.toList x) (return x) cxt

observePretty lit cxt = seq lit $ send (prettyPrint lit) (return lit) cxt
bracket s = '[' : s ++ "]"
