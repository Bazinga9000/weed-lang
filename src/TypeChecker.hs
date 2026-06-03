{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import AST
import Control.Monad.Except
import Control.Monad.RWS
import TypeChecker.BuiltinTypes
import TypeChecker.Infer
import TypeChecker.Subst
import TypeChecker.Types

-- infer the type of an untyped expression,
-- emitting all necesssary constraints for the solver to un
infer :: CoreUntypedExpr -> Infer (WeedType, CoreTypedExpr)
infer (CUNumber n) = return $ (TNumber, CTNumber n)
infer (CUBool b) = return $ (TBool, CTBool b)
infer (CUUnit) = return $ (TUnit, CTUnit)
infer (CUList []) = do
  t <- fresh
  return $ (mkList t, CTList t [])
infer (CUList (x : xs)) = do
  t <- fresh
  (tx, cx) <- infer x
  (txs, cxs) <- infer (CUList xs)
  unify t tx
  unify (mkList t) txs
  let listType = mkList t
  newList <- (CTList listType) <$> appendCList cx cxs
  return $ (listType, newList)
  where
    appendCList :: CoreTypedExpr -> CoreTypedExpr -> Infer [CoreTypedExpr]
    appendCList cx (CTList _ cxs) = return $ (cx : cxs)
    appendCList _ e = throwError $ "TC Bug: Expected a list, got " ++ show e
infer (CUIdentifier (B builtin)) = do
  t <- builtinType builtin >>= instantiate
  return $ (t, CTIdentifier t (B builtin))
infer (CUIdentifier ident) = (\t -> (t, CTIdentifier t ident)) <$> lookupEnv ident
infer (CULambda ident body) = do
  tv <- fresh
  (tb, cb) <- inEnv (ident, ForAll [] [] tv) (infer body)
  let tl = TFunction tv tb
  return (tl, CTLambda tl ident cb)
infer (CUApply f arg) = do
  (tf, cf) <- infer f
  (ta, ca) <- infer arg

  tr <- fresh

  s <- currentSubst <$> get
  let tf' = apply s tf
  let ta' = apply s ta

  -- Utilities for type coersion
  -- Peek without modifying the current substitution

  -- Instantiate, bind, and apply a unary builtin
  let call1 builtin argT argC = do
        tb <- builtinType builtin >>= instantiate
        trNew <- fresh
        unify tb (argT ->> trNew)
        let builtinNode = (CTIdentifier tb (B builtin))
        return (trNew, CTApply trNew builtinNode argC)

  let call2 builtin argT1 argC1 argT2 argC2 = do
        tb <- builtinType builtin >>= instantiate
        tMiddle <- fresh
        trNew <- fresh
        unify tb (argT1 ->> tMiddle)
        unify tMiddle (argT2 ->> trNew)
        let builtinNode = (CTIdentifier tb (B builtin))
        let ap1 = (CTApply tMiddle builtinNode argC1)
        return (trNew, CTApply trNew ap1 argC2)

  -- attempt standard application
  case unify' tf' (ta' ->> tr) of
    Right newSub -> do
      modify (\curr -> curr {currentSubst = newSub `compose` s})
      return (tr, CTApply tr cf ca)
    Left origErr -> do
      -- define all the coersion rules in their priority order
      let rules = case tf' of
            -- f :: a -> b is unwrapped
            TFunction texp tret ->
              [ -- Rule 1: Pool Mapping
                case (texp, ta') of
                  (TApp TList texpInner, TApp TPool taInner) -> do
                    -- check the unification without actually doing it
                    ok <- unifyPeek texpInner taInner
                    if ok
                      then do
                        -- if it works, commit
                        unify texpInner taInner
                        let mapped = TApp TDice tret
                        return $ Just (mapped, CTMapPool mapped cf ca)
                      else
                        return Nothing
                  _ -> return Nothing,
                -- Rule 2: Pool Collapse (Direct)
                case (texp, ta') of
                  (TApp TDice TNumber, TApp TPool TNumber) -> do
                    unify tret tr
                    (_, collapseCall) <- call1 Collapse ta' ca
                    return $ Just (tret, CTApply tret cf collapseCall)
                  _ -> return Nothing,
                -- Also Rule 2: Pool Collapse (Implicit, 3a)
                case (texp, ta') of
                  (TNumber, TApp TPool TNumber) -> do
                    (tCollapsed, collapseCall) <- call1 Collapse ta' ca
                    -- immediately wrap in an fmap
                    (tFMapped, fmapCall) <- call2 Map tf' cf tCollapsed collapseCall
                    unify tr tFMapped
                    return $ Just (tFMapped, fmapCall)
                  _ -> return Nothing,
                -- Rule 3a: Implicit FMap
                case ta' of
                  TApp tWrapper taInner -> do
                    ok <- unifyPeek texp taInner
                    if ok
                      then do
                        unify texp taInner
                        (tRes, fmapCall) <- call2 Map tf' cf ta' ca
                        tell [CInstanceOf CFunctor tWrapper]
                        unify tr tRes
                        return $ Just (tRes, fmapCall)
                      else return Nothing
                  _ -> return Nothing,
                -- Rule 4: Scalar Promotion (Direct)
                case texp of
                  TApp tWrapper texpInner | not (isDiceOrPool ta') -> do
                    ok <- unifyPeek texpInner ta'
                    if ok
                      then do
                        unify texp texpInner
                        (tReturn, returnCall) <- call1 Return ta' ca
                        unify texp tReturn
                        unify tr tret
                        tell [CInstanceOf CMonad tWrapper]
                        return $ Just (tret, CTApply tret cf returnCall)
                      else return Nothing
                  _ -> return Nothing
              ]
            -- f :: m (a -> b) is wrapped
            TApp tWrapper (TFunction taInner _) ->
              [ -- Rule 2: Pool Collapse (Applicative)
                -- f is Dice (a -> b), arg is Pool a
                --
                case (tWrapper, ta') of
                  (TDice, TApp TPool taActual) -> do
                    ok <- unifyPeek taInner taActual
                    if ok
                      then do
                        unify taInner taActual
                        (tCollapsed, collapseCall) <- call1 Collapse ta' ca
                        (tApplicative, applicativeCall) <- call2 Ap tf' cf tCollapsed collapseCall
                        unify tr tApplicative
                        tell [CInstanceOf CMonad tWrapper]
                        return $ Just (tApplicative, applicativeCall)
                      else return Nothing
                  _ -> return Nothing,
                -- Rule 3b: Implicit Applicative <*>
                case ta' of
                  TApp targWrapper taActual -> do
                    ok <- unifyPeek taInner taActual
                    ok2 <- unifyPeek tWrapper targWrapper
                    if ok && ok2
                      then do
                        unify taInner taActual
                        (tApplicative, applicativeCall) <- call2 Ap tf' cf ta' ca
                        unify tr tApplicative
                        tell [CInstanceOf CMonad tWrapper]
                        return $ Just (tApplicative, applicativeCall)
                      else return Nothing
                  _ -> return Nothing,
                -- Rule 4: Scalar Promotion (Indirect)
                case ta' of
                  _ | not (isDiceOrPool ta') -> do
                    ok <- unifyPeek taInner ta'
                    if ok
                      then do
                        unify taInner ta'
                        (tReturn, returnCall) <- call1 Return ta' ca
                        (tApplicative, applicativeCall) <- call2 Ap tf' cf tReturn returnCall
                        unify tr tApplicative
                        tell [CInstanceOf CMonad tWrapper]
                        return $ Just (tApplicative, applicativeCall)
                      else return Nothing
                  _ -> return Nothing
              ]
            _ -> []

      -- try each rule in order. If they all fail, throw the original error.
      firstJustM rules >>= \case
        Just result -> return result
        Nothing -> throwError origErr
infer (CULet ident val body) = do
  -- infer the value and trap its class constraints
  ((tVal, typedVal), valConstraints) <- listen $ censor (const []) (infer val)

  -- generalize the value's type scheme
  scheme <- generalize tVal valConstraints

  -- extend the environment and infer the body
  (tBody, typedBody) <- inEnv (ident, scheme) (infer body)
  return (tBody, CTLet tBody ident typedVal typedBody)

-- solve the constraints generated by infer
solve :: Subst -> [TypeConstraint] -> Either TypeError ()
solve finalSubst constraints =
  mapM_ solveOne $ apply finalSubst constraints
  where
    baseType :: WeedType -> WeedType
    baseType (TApp t _) = baseType t
    baseType t = t

    solveOne :: TypeConstraint -> Either TypeError ()
    solveOne (CInstanceOf cls t) =
      let base = baseType t
       in case (cls, base) of
            -- Functor
            (CFunctor, TList) -> Right ()
            (CFunctor, TDice) -> Right ()
            (CFunctor, TPool) -> Right ()
            -- Monad
            (CMonad, TList) -> Right ()
            (CMonad, TDice) -> Right ()
            (CMonad, TPool) -> Right ()
            -- Rollable
            (CRollable, TDice) -> Right ()
            (CRollable, TPool) -> Right ()
            -- Ambiguous top-level type variable
            (c, TVar tv) -> Left $ "Ambiguous type variable " ++ show tv ++ " for class " ++ show c
            -- Missing instance
            (c, t') -> Left $ "No instance for " ++ show c ++ " for type " ++ show t'

typeCheck :: CoreUntypedExpr -> Either TypeError CoreTypedExpr
typeCheck expr = do
  -- infer, fetch all constraints
  ((_, typed), finalState, constraints) <- runExcept $ runRWST (infer expr) emptyEnv freshState
  let subst = currentSubst finalState
  -- solve the constraints
  solve subst constraints
  -- if we got here, constraint solving succeded, apply the final subs to concretize every node's type
  return $ apply subst typed
