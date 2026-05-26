module TypeChecker.Subst where

import AST
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  apply _ CUnconstrained = CUnconstrained
  apply s (CRollable t) = CRollable (apply s t)

  ftv CUnconstrained = Set.empty
  ftv (CRollable t) = ftv t

instance Substitutable WeedType where
  apply _ TNumber = TNumber
  apply _ TBool = TBool
  apply _ TUnit = TUnit
  apply s (TFunction a b) = TFunction (apply s a) (apply s b)
  apply s (TList t) = TList (apply s t)
  apply s (TDice t) = TDice (apply s t)
  apply s (TPool t) = TPool (apply s t)
  apply s v@(TVar (ConstrainedName name _)) = Map.findWithDefault v name s

  ftv TNumber = Set.empty
  ftv TBool = Set.empty
  ftv TUnit = Set.empty
  ftv (TFunction a b) = ftv a `Set.union` ftv b
  ftv (TList t) = ftv t
  ftv (TDice t) = ftv t
  ftv (TPool t) = ftv t
  ftv (TVar (ConstrainedName name _)) = Set.singleton name

instance Substitutable WeedTypeScheme where
  apply s (ForAll vars t) = ForAll vars (apply s t)
  ftv (ForAll vars t) = ftv t `Set.difference` Set.fromList (map dropConstraint vars)

instance (Substitutable a) => Substitutable [a] where
  apply s = map (apply s)
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable CoreTypedExpr where
  apply _ (CTNumber n) = CTNumber n
  apply _ (CTBool n) = CTBool n
  apply _ CTUnit = CTUnit
  apply s (CTList t xs) = CTList (apply s t) (map (apply s) xs)
  apply s (CTIdentifier t ident) = CTIdentifier (apply s t) ident
  apply s (CTLambda t ident body) = CTLambda (apply s t) ident (apply s body)
  apply s (CTApply t a b) = CTApply (apply s t) (apply s a) (apply s b)
  apply s (CTIf t p te fe) = CTIf (apply s t) (apply s p) (apply s te) (apply s fe)
  apply s (CTLet t ident a b) = CTLet (apply s t) ident (apply s a) (apply s b)
  apply s (CTMapPool t a b) = CTMapPool (apply s t) (apply s a) (apply s b)

  ftv (CTNumber _) = Set.empty
  ftv (CTBool _) = Set.empty
  ftv CTUnit = Set.empty
  ftv (CTList t xs) = ftv t `Set.union` Set.unions (map ftv xs)
  ftv (CTIdentifier t _) = ftv t
  ftv (CTLambda t _ body) = ftv t `Set.union` ftv body
  ftv (CTApply t a b) = ftv t `Set.union` ftv a `Set.union` ftv b
  ftv (CTIf t p te fe) = ftv t `Set.union` ftv p `Set.union` ftv te `Set.union` ftv fe
  ftv (CTLet t _ a b) = ftv t `Set.union` ftv a `Set.union` ftv b
  ftv (CTMapPool t a b) = ftv t `Set.union` ftv a `Set.union` ftv b
