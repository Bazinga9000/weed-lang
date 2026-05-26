module TypeChecker.Infer where

import AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeChecker.Subst
import TypeChecker.Types

type TypeError = String

newtype TypeEnv = TypeEnv (Map.Map IdentifierName WeedTypeScheme)

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

type Infer a = ExceptT TypeError (State Int) a

extend :: TypeEnv -> (IdentifierName, WeedTypeScheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

lookupEnv :: TypeEnv -> IdentifierName -> Infer (Subst, WeedType)
lookupEnv (TypeEnv env) x = do
  case Map.lookup x env of
    Nothing -> throwError $ "Unbound identifier: " ++ show x
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

freshName :: Infer TypeVarName
freshName = do
  n <- get
  put (n + 1)
  return $ TypeVarName n

freshConstrainedName :: TypeConstraint -> Infer ConstrainedName
freshConstrainedName c = do
  n <- freshName
  return $ ConstrainedName n c

fresh :: TypeConstraint -> Infer WeedType
fresh c = do
  cn <- freshConstrainedName c
  return $ TVar cn

instantiate :: WeedTypeScheme -> Infer WeedType
instantiate (ForAll as t) = do
  as' <- mapM (\(ConstrainedName _ c) -> fresh c) as
  let subst = Map.fromList $ zip (map dropConstraint as) as'
  return $ apply subst t

-- assumes all free type variables are unconstrained.
generalize :: TypeEnv -> WeedType -> WeedTypeScheme
generalize env t = ForAll as t
  where
    as = map (\x -> ConstrainedName x CUnconstrained) $ Set.toList $ ftv t `Set.difference` ftv env
