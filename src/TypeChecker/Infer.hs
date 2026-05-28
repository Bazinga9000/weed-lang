module TypeChecker.Infer where

import AST
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeChecker.Subst
import TypeChecker.Types

newtype TypeEnv = TypeEnv (Map.Map IdentifierName WeedTypeScheme)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data InferState = InferState
  { freshCount :: Int,
    currentSubst :: Subst
  }

freshState :: InferState
freshState = InferState 0 mempty

type Infer a =
  ( RWST
      TypeEnv -- typing environment
      [TypeConstraint] -- generated constraints for the solver
      InferState -- fresh name counter and equality substitutio
      (Except TypeError) -- error handling inner monad
      a -- result type
  )

-- extend type environment for one inference
inEnv :: (IdentifierName, WeedTypeScheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope (TypeEnv env) = TypeEnv $ Map.insert x sc env
  local scope m

lookupEnv :: IdentifierName -> Infer WeedType
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ "Unbound identifier: " ++ show x
    Just s -> instantiate s

class Freshable a where
  fresh :: Infer a

instance Freshable TypeVarName where
  fresh = do
    n <- freshCount <$> get
    modify (\s -> s {freshCount = n + 1})
    return $ TypeVarName n

instance Freshable WeedType where
  fresh = TVar <$> fresh

bind :: TypeVarName -> WeedType -> Either TypeError Subst
bind n t
  -- t is in fact the tvar already: do nothing
  | t == TVar n = return nullSubst
  -- check for infinite type: n occurs in t's free type variables
  | n `Set.member` ftv t = throwError $ "Infinite type: " ++ show n ++ " occurs in " ++ show t
  -- otherwise, bind the variable to the type
  | otherwise = return (Map.singleton n t)

unify' :: WeedType -> WeedType -> Either TypeError Subst
unify' (a `TFunction` b) (a' `TFunction` b') = do
  s1 <- unify' a a'
  s2 <- unify' (apply s1 b) (apply s1 b')
  return $ s2 `compose` s1
unify' (TVar n) t = bind n t
unify' t (TVar n) = bind n t
unify' TNumber TNumber = return nullSubst
unify' TBool TBool = return nullSubst
unify' TUnit TUnit = return nullSubst
unify' TList TList = return nullSubst
unify' TDice TDice = return nullSubst
unify' TPool TPool = return nullSubst
unify' (TApp a b) (TApp a' b') = do
  s1 <- unify' a a'
  s2 <- unify' (apply s1 b) (apply s1 b')
  return $ s2 `compose` s1
unify' t1' t2' = throwError $ "Could not unify" ++ show t1' ++ " and " ++ show t2'

unify :: WeedType -> WeedType -> Infer ()
unify t1 t2 = do
  cs <- currentSubst <$> get
  -- apply current substitution before unifying
  let t1' = apply cs t1
      t2' = apply cs t2
  case unify' t1' t2' of
    Left err -> throwError err
    Right cs' -> modify (\s -> s {currentSubst = cs' `compose` cs})

instantiate :: WeedTypeScheme -> Infer WeedType
instantiate (ForAll as cs t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  tell $ apply s cs
  return $ apply s t

generalize :: WeedType -> [TypeConstraint] -> Infer WeedTypeScheme
generalize t localConstraints = do
  -- get the correct states
  env <- ask
  s <- currentSubst <$> get
  let env' = apply s env
  let t' = apply s t

  -- decide what to quantify
  let envFtv = ftv env'
  let tFtv = ftv t'
  let genVars = Set.toList $ tFtv `Set.difference` envFtv
  let genSet = Set.fromList genVars

  -- filter for constraints that exclusively mention quantified variables
  let canScoop (CInstanceOf _ ty) =
        let cFtv = ftv ty
         in not (Set.null cFtv) && cFtv `Set.isSubsetOf` genSet

  -- partition the constraints
  -- constraints that exclusively mention quantified variables go into the scheme
  -- constraints that mention free variables go into the environment
  let (scooped, deferred) = List.partition canScoop localConstraints
  tell deferred
  return $ ForAll genVars scooped t'
