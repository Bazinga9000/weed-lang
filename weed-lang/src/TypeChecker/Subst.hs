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

instance Substitutable TypeConstraint where
  apply s (CInstanceOf c t) = CInstanceOf c (apply s t)

  ftv (CInstanceOf _ t) = ftv t

instance Substitutable WeedType where
  apply _ TNumber = TNumber
  apply _ TBool = TBool
  apply _ TUnit = TUnit
  apply s (TFunction a b) = TFunction (apply s a) (apply s b)
  apply _ TList = TList
  apply _ TDice = TDice
  apply _ TPool = TPool
  apply s v@(TVar n) = Map.findWithDefault v n s
  apply s (TApp a b) = TApp (apply s a) (apply s b)

  ftv TNumber = Set.empty
  ftv TBool = Set.empty
  ftv TUnit = Set.empty
  ftv (TFunction a b) = ftv a `Set.union` ftv b
  ftv TList = Set.empty
  ftv TDice = Set.empty
  ftv TPool = Set.empty
  ftv (TVar n) = one n
  ftv (TApp a b) = ftv a `Set.union` ftv b

instance Substitutable WeedTypeScheme where
  apply s (ForAll vars cs t) = ForAll vars (apply s cs) (apply s t)
  ftv (ForAll vars cs t) = ftv t `Set.difference` (ftv cs `Set.union` Set.fromList vars)

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
  apply s (CTMapPool t a b) = CTMapPool (apply s t) (apply s a) (apply s b)

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
  ftv (CTMapPool t a b) = ftv t `Set.union` ftv a `Set.union` ftv b
