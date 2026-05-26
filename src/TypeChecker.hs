module TypeChecker where

import AST
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeChecker.BuiltinTypes
import TypeChecker.Infer
import TypeChecker.Subst
import TypeChecker.Types

occursCheck :: (Substitutable a) => TypeVarName -> a -> Bool
occursCheck a t = a `Set.member` ftv t

bind :: ConstrainedName -> WeedType -> Infer Subst
bind (ConstrainedName a c) t
  | matches a t = return nullSubst
  | occursCheck a t = throwError $ "Infinite type: " ++ show a ++ " occurs in " ++ show t
  | otherwise = case (c, t) of
      -- If constrained to be Rollable, ensure the actual type is Dice or Pool
      -- AND unify the inner type of the constraint with the actual inner type.
      (CRollable expectedInner, TDice actualInner) -> do
        sInner <- unify expectedInner actualInner
        return $ Map.singleton a t `compose` sInner
      (CRollable expectedInner, TPool actualInner) -> do
        sInner <- unify expectedInner actualInner
        return $ Map.singleton a t `compose` sInner
      (CUnconstrained, _) -> return (Map.singleton a t)
      _ -> throwError $ "Type mismatch: " ++ show t ++ " does not satisfy " ++ show c

unify :: WeedType -> WeedType -> Infer Subst
unify (a `TFunction` b) (a' `TFunction` b') = do
  s1 <- unify a a'
  s2 <- unify (apply s1 b) (apply s1 b')
  return $ s2 `compose` s1
unify (TVar (ConstrainedName a c)) t = bind (ConstrainedName a c) t
unify t (TVar (ConstrainedName a c)) = bind (ConstrainedName a c) t
unify TNumber TNumber = return nullSubst
unify TBool TBool = return nullSubst
unify TUnit TUnit = return nullSubst
unify (TList a) (TList a') = unify a a'
unify (TDice a) (TDice a') = unify a a'
unify (TPool a) (TPool a') = unify a a'
unify t1 t2 = throwError $ "Could not unify" ++ show t1 ++ " and " ++ show t2

infer :: TypeEnv -> CoreUntypedExpr -> Infer (Subst, WeedType, CoreTypedExpr)
infer env expr = case expr of
  CUNumber n -> return (nullSubst, TNumber, CTNumber n)
  CUBool b -> return (nullSubst, TBool, CTBool b)
  CUUnit -> return (nullSubst, TUnit, CTUnit)
  CUList [] -> do
    tv <- fresh CUnconstrained
    return (nullSubst, TList tv, CTList (TList tv) [])
  CUList (x : xs) -> do
    (s1, tHead, typedHead) <- infer env x

    let process (subst, typedList) e = do
          (sNext, tNext, typedNext) <- infer (apply subst env) e
          sUnified <- unify (apply sNext (apply subst tHead)) tNext
          let combinedSubst = sUnified `compose` sNext `compose` subst
          return (combinedSubst, typedList ++ [typedNext])

    (sFinal, typedList) <- foldM process (s1, [typedHead]) xs

    let finalType = TList (apply sFinal tHead)
    let finalAST = CTList finalType typedList

    return (sFinal, finalType, finalAST)
  CUIdentifier (B builtin) -> do
    (subst, t) <- lookupBuiltin builtin
    return (subst, t, CTIdentifier t (B builtin))
  CUIdentifier ident -> do
    (subst, t) <- lookupEnv env ident
    return (subst, t, CTIdentifier t ident)
  CULambda ident body -> do
    tv <- fresh CUnconstrained
    let env' = env `extend` (ident, ForAll [] tv)
    (subst, tBody, typedBody) <- infer env' body
    let lambdaType = TFunction (apply subst tv) tBody
    return (subst, lambdaType, CTLambda lambdaType ident typedBody)
  CUApply f arg -> do
    (s1, tF, typedF) <- infer env f
    (s2, tArg, typedArg) <- infer (apply s1 env) arg

    let tF' = apply s2 tF
    tvRes <- fresh CUnconstrained

    let tryStandardApp = do
          s3 <- unify tF' (tArg `TFunction` tvRes)
          let tRes = apply s3 tvRes
          return (s3 `compose` s2 `compose` s1, tRes, CTApply tRes typedF typedArg)

    let tryCoercedApp = case tF' of
          (tExpectedParam `TFunction` tReturn) ->
            -- Rule A: Function expects Dice Number, Arg is Number
            if tExpectedParam == TDice TNumber && tArg == TNumber
              then do
                s3 <- unify tF' (TDice TNumber `TFunction` tvRes)
                let tRes = apply s3 tvRes
                let coercedArg = CTApply (TDice TNumber) (CTIdentifier (TNumber `TFunction` TDice TNumber) (B Constant)) typedArg
                return (s3 `compose` s2 `compose` s1, tRes, CTApply tRes typedF coercedArg)

              -- Rule B: Function expects Dice Number, Arg is Pool Number
              else
                if tExpectedParam == TDice TNumber && tArg == TPool TNumber
                  then do
                    s3 <- unify tF' (TDice TNumber `TFunction` tvRes)
                    let tRes = apply s3 tvRes
                    let coercedArg = CTApply (TDice TNumber) (CTIdentifier (TPool TNumber `TFunction` TDice TNumber) (B Collapse)) typedArg
                    return (s3 `compose` s2 `compose` s1, tRes, CTApply tRes typedF coercedArg)

                  -- Rule C: Function expects [a], Arg is Pool a
                  else case (tExpectedParam, tArg) of
                    (TList tInner1, TPool tInner2) -> do
                      -- Ensure the inner types match (e.g. [Number] matches Pool Number)
                      sInner <- unify tInner1 tInner2
                      -- The result is wrapped in the Dice monad
                      let tRes = TDice (apply sInner tReturn)
                      let mappedAST = CTMapPool tRes typedF typedArg
                      return (sInner `compose` s2 `compose` s1, tRes, mappedAST)
                    _ -> throwError "Coercion structural mismatch."
          _ -> throwError "Not a function."

    tryStandardApp `catchError` \_ ->
      tryCoercedApp `catchError` \_ ->
        throwError $ "Type mismatch in application. Could not apply " ++ show tF' ++ " to " ++ show tArg
  CUIf p te fe -> do
    (s1, tP, typedP) <- infer env p
    (s2, tTe, typedTe) <- infer (apply s1 env) te
    (s3, tFe, typedFe) <- infer (apply (s2 `compose` s1) $ env) fe

    s4 <- unify tP TBool
    s5 <- unify (apply s4 tTe) (apply s4 tFe)

    let subst = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
    let ifType = apply s5 (apply s4 tTe)
    return (subst, ifType, CTIf ifType typedP typedTe typedFe)
  CULet ident binding body -> do
    (s1, tBinding, typedBinding) <- infer env binding
    let env' = apply s1 env
    let t' = generalize env' tBinding
    (s2, tBody, typedBody) <- infer (env' `extend` (ident, t')) body
    let subst = s2 `compose` s1
    return (subst, tBody, CTLet tBody ident typedBinding typedBody)

-- the very last apply subst texpr cleans up any stale types left over in the annotations
runInfer :: Infer (Subst, WeedType, CoreTypedExpr) -> Either TypeError CoreTypedExpr
runInfer m = case evalState (runExceptT m) 0 of
  Left err -> Left err
  Right (subst, _, texpr) -> Right $ apply subst texpr

typeCheck :: CoreUntypedExpr -> Either TypeError CoreTypedExpr
typeCheck expr = runInfer $ infer env expr
  where
    env = TypeEnv Map.empty
