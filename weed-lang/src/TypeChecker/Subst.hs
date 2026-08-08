module TypeChecker.Subst where

import AST
import Data.Map qualified as Map
import Data.Set qualified as Set
import TypeChecker.Types

type Subst = Map.Map TypeVarName WeedType

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TypeVarName

instance Substitutable WeedTypeClass where
  apply s (CFunctor t) = CFunctor (apply s t)
  apply s (CMonad t) = CMonad (apply s t)
  apply s (CRollable t) = CRollable (apply s t)
  apply s (CSelector t u) = CSelector (apply s t) (apply s u)
  apply s (CEq t) = CEq (apply s t)
  apply s (COrd t) = COrd (apply s t)

  ftv (CFunctor t) = ftv t
  ftv (CMonad t) = ftv t
  ftv (CRollable t) = ftv t
  ftv (CSelector t u) = ftv t `Set.union` ftv u
  ftv (CEq t) = ftv t
  ftv (COrd t) = ftv t

instance Substitutable TypeConstraint where
  apply s (CInstanceOf c) = CInstanceOf (apply s c)

  ftv (CInstanceOf c) = ftv c

instance Substitutable WeedType where
  apply _ TNumber = TNumber
  apply _ TBool = TBool
  apply _ TUnit = TUnit
  apply s (TFunction a b) = TFunction (apply s a) (apply s b)
  apply s (TList a) = TList (apply s a)
  apply s (TDice a) = TDice (apply s a)
  apply s (TPool a) = TPool (apply s a)
  apply s v@(TVar n) = Map.findWithDefault v n s
  -- contract applications of constructor bindings (see TDummyArg):
  -- TApp (TList TDummyArg) a is TList a, etc.
  apply s (TApp a b) = case (apply s a, apply s b) of
    (TList TDummyArg, b') -> TList b'
    (TDice TDummyArg, b') -> TDice b'
    (TPool TDummyArg, b') -> TPool b'
    (a', b') -> TApp a' b'

  ftv TNumber = Set.empty
  ftv TBool = Set.empty
  ftv TUnit = Set.empty
  ftv (TFunction a b) = ftv a `Set.union` ftv b
  ftv (TList a) = ftv a
  ftv (TDice a) = ftv a
  ftv (TPool a) = ftv a
  ftv (TVar n) = one n
  ftv (TApp a b) = ftv a `Set.union` ftv b

instance Substitutable WeedTypeScheme where
  apply s (ForAll vars cs t) = ForAll vars (apply s cs) (apply s t)
  ftv (ForAll vars cs t) = (ftv t `Set.union` ftv cs) `Set.difference` Set.fromList vars

instance (Substitutable a, Functor f, Foldable f) => Substitutable (f a) where
  apply s = fmap (apply s)
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable CoreTypedExpr where
  apply _ (CTNumber n) = CTNumber n
  apply _ (CTBool n) = CTBool n
  apply _ CTUnit = CTUnit
  apply s (CTList t xs) = CTList (apply s t) (map (apply s) xs)
  apply s (CTIdentifier t ident) = CTIdentifier (apply s t) ident
  apply s (CTLambda t ident body) = CTLambda (apply s t) ident (apply s body)
  apply s (CTApply t a b) = CTApply (apply s t) (apply s a) (apply s b)
  apply s (CTLet t decl b) = CTLet (apply s t) (apply s decl) (apply s b)
  apply s (CTLetRec t decls b) = CTLetRec (apply s t) (apply s decls) (apply s b)
  apply s (CTIf ty c t f) = CTIf (apply s ty) (apply s c) (apply s t) (apply s f)

  ftv (CTNumber _) = Set.empty
  ftv (CTBool _) = Set.empty
  ftv CTUnit = Set.empty
  ftv (CTList t xs) = ftv t `Set.union` Set.unions (map ftv xs)
  ftv (CTIdentifier t _) = ftv t
  ftv (CTLambda t _ body) = ftv t `Set.union` ftv body
  ftv (CTApply t a b) = ftv t `Set.union` ftv a `Set.union` ftv b
  ftv (CTLet t decl b) = ftv t `Set.union` ftv decl `Set.union` ftv b
  ftv (CTLetRec t decls b) = ftv t `Set.union` ftv decls `Set.union` ftv b
  ftv (CTIf ty c t f) = ftv ty `Set.union` ftv c `Set.union` ftv t `Set.union` ftv f
