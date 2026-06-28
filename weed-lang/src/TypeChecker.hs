{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import AST
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.RWS.CPS (censor, listen, runRWST, tell)
import TypeChecker.BuiltinTypes
import TypeChecker.Infer
import TypeChecker.Subst
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum, lookupEnv)

call1 :: Builtin -> WeedType -> CoreTypedExpr -> Infer (WeedType, CoreTypedExpr)
call1 builtin argT argC = do
  tb <- builtinType builtin >>= instantiate
  trNew <- fresh
  unify tb (argT ->> trNew)
  let builtinNode = CTIdentifier tb (B builtin)
  return (trNew, CTApply trNew builtinNode argC)

call2 :: Builtin -> WeedType -> CoreTypedExpr -> WeedType -> CoreTypedExpr -> Infer (WeedType, CoreTypedExpr)
call2 builtin argT1 argC1 argT2 argC2 = do
  tb <- builtinType builtin >>= instantiate
  tMiddle <- fresh
  trNew <- fresh
  unify tb (argT1 ->> tMiddle)
  unify tMiddle (argT2 ->> trNew)
  let builtinNode = CTIdentifier tb (B builtin)
  let ap1 = CTApply tMiddle builtinNode argC1
  return (trNew, CTApply trNew ap1 argC2)

-- infer the type of an untyped expression,
-- emitting all necesssary constraints for the solver to un
infer :: CoreUntypedExpr -> Infer (WeedType, CoreTypedExpr)
infer (CUNumber n) = return (TNumber, CTNumber n)
infer (CUBool b) = return (TBool, CTBool b)
infer CUUnit = return (TUnit, CTUnit)
infer (CUList []) = do
  t <- fresh
  return (mkList t, CTList t [])
infer (CUList (x : xs)) = do
  t <- fresh
  (tx, cx) <- infer x
  (txs, cxs) <- infer (CUList xs)
  unify t tx
  unify (mkList t) txs
  let listType = mkList t
  newList <- CTList listType <$> appendCList cx cxs
  return (listType, newList)
  where
    appendCList :: CoreTypedExpr -> CoreTypedExpr -> Infer [CoreTypedExpr]
    appendCList cx (CTList _ cxs) = return (cx : cxs)
    appendCList _ e = throwError $ TypeCheckerBug ("appendCList expected a list, got " <> show e)
infer (CUIdentifier (B builtin)) = do
  t <- builtinType builtin >>= instantiate
  return (t, CTIdentifier t (B builtin))
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
              [ -- Pool Mapping
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
                -- Pool Collapse (Direct)
                case (texp, ta') of
                  (TApp TDice TNumber, TApp TPool TNumber) -> do
                    unify tret tr
                    (_, collapseCall) <- call1 Collapse ta' ca
                    return $ Just (tret, CTApply tret cf collapseCall)
                  _ -> return Nothing,
                -- Pool Collapse (Indirect)
                case (texp, ta') of
                  (TNumber, TApp TPool TNumber) -> do
                    (tCollapsed, collapseCall) <- call1 Collapse ta' ca
                    -- immediately wrap in an fmap
                    (tFMapped, fmapCall) <- call2 Map tf' cf tCollapsed collapseCall
                    unify tr tFMapped
                    return $ Just (tFMapped, fmapCall)
                  _ -> return Nothing,
                -- Implicit map
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
                -- Scalar Promotion (Direct)
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
              [ -- Pool Collapse (Applicative)
                -- f is Dice (a -> b), arg is Pool a
                -- this fails to typecheck if a != Number
                -- but for ease of comprehension we write the general
                -- case
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
                -- Implicit Applicative <*>
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
                -- Scalar Promotion (Indirect)
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
infer (CULet (Decl ident val) body) = do
  -- infer the value and trap its class constraints
  ((tVal, typedVal), valConstraints) <- listen $ censor (const []) (infer val)

  -- generalize the value's type scheme
  scheme <- generalize tVal valConstraints

  -- extend the environment and infer the body
  (tBody, typedBody) <- inEnv (ident, scheme) (infer body)
  return (tBody, CTLet tBody (Decl ident typedVal) typedBody)
infer (CULetRec decls body) = do
  -- make the type schemes
  let idents = map (\(Decl ident _) -> ident) decls
  declVars <- traverse (const fresh) idents
  let declSchemes = map (ForAll [] []) declVars
  let cyclicBinds = zip idents declSchemes

  -- infer the declaration bodies and collect ALL of their constraints together
  let inEnvs binds action = foldr inEnv action binds
  (declResults, declConstraints) <-
    listen $
      censor (const []) $
        inEnvs cyclicBinds $
          traverse (\(Decl _ expr) -> infer expr) decls

  let declTypes = map fst declResults
  let declCTExprs = map snd declResults

  -- unify all the declaration vars with their inferred types
  zipWithM_ unify declVars declTypes

  -- apply all the substitutions, then generalize
  subst <- currentSubst <$> get
  let declTypes' = map (apply subst) declTypes
  declSchemes' <- traverse (`generalize` declConstraints) declTypes'

  -- infer the body in the generalized environment
  let generalizedBinds = zip idents declSchemes'
  (tBody, typedBody) <- inEnvs generalizedBinds (infer body)
  let typedDecls = zipWith Decl idents declCTExprs
  return (tBody, CTLetRec tBody typedDecls typedBody)
infer (CUIf cond t f) = do
  -- for future people who are asking why this is so complicated
  -- morally, what we *should* be doing here is have a builtin If :: Bool -> a -> a -> a
  -- and just desugar into that so we can leverage the function application coersions
  -- and get all of the nice lifting logic like
  -- (if coin then d6 else 0) :: Dice Number
  -- for free, but if we do that, then because the langauge
  -- has strictly evaluated function appplication, both t and f branches are evaluated
  -- and this completely shatters recursion.
  -- So, we have to basically cheat a little bit and do some coersion hacks in the
  -- type checker here and then let the evaluator do the laziness

  -- infer the condition, true, false branches
  (tc, cc) <- infer cond
  (tt, ct) <- infer t
  (tf, cf) <- infer f

  -- figure out what context we're in
  cs <- currentSubst <$> get
  let (lvlC, baseC) = peelEffect (apply cs tc)
      (lvlT, baseT) = peelEffect (apply cs tt)
      (lvlF, baseF) = peelEffect (apply cs tf)

  -- the base type of the condition has to be bool
  -- the base types of the branches have to match
  unify baseC TBool
  unify baseT baseF

  cs' <- currentSubst <$> get
  let finalBase = apply cs' baseT

  -- simulate the CTApply coersions rule for two contexts
  let joinLvl acc (lvl, baseT') =
        if acc == CtxDice && lvl == CtxPool && baseT' == TNumber
          then CtxDice
          else max acc lvl

  -- coerce the context left to right
  let finalLvl =
        CtxBase
          `joinLvl` (lvlC, apply cs' baseC)
          `joinLvl` (lvlT, finalBase)
          `joinLvl` (lvlF, finalBase)

  -- promote the outcome
  let tPromoted = applyEffect finalLvl finalBase

  -- coerce the branches into their correct types by injecting AST nodes
  let coerceBranch branchT branchC = do
        currSubst <- currentSubst <$> get
        let (bLvl, bBase) = peelEffect (apply currSubst branchT)

        -- 1. Explicit Pool Collapse
        (tAfterCollapse, cAfterCollapse) <-
          if finalLvl == CtxDice && bLvl == CtxPool && apply currSubst bBase == TNumber
            then call1 Collapse branchT branchC
            else return (branchT, branchC)

        -- 2. Explicit Scalar Promotion
        cs'' <- currentSubst <$> get
        if finalLvl > CtxBase && fst (peelEffect (apply cs'' tAfterCollapse)) == CtxBase
          then do
            -- 1. Perform the return call to lift the scalar
            (tRet, cRet) <- call1 Return tAfterCollapse cAfterCollapse

            -- 2. Force the 'm' in 'm a' to be our target effect (Dice or Pool)
            -- USE 'finalBase' from the outer scope, NOT peelEffect on tRet.
            let target = applyEffect finalLvl finalBase
            unify tRet target

            return (tRet, cRet)
          else return (tAfterCollapse, cAfterCollapse)

  -- 3. Coerce condition and branches so the typed AST is explicitly wrapped
  (_, ccCoerced) <- coerceBranch tc cc
  (_, ctCoerced) <- coerceBranch tt ct
  (_, cfCoerced) <- coerceBranch tf cf

  return (tPromoted, CTIf tPromoted ccCoerced ctCoerced cfCoerced)

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
            (c, TVar tv) -> Left $ AmbiguousTypeVar tv c
            -- Missing instance
            (c, t') -> Left $ MissingInstance c t'

typeCheck :: CoreUntypedExpr -> Either TypeError CoreTypedExpr
typeCheck expr = do
  -- infer, fetch all constraints
  ((_, typed), finalState, constraints) <- runExcept $ runRWST (infer expr) emptyEnv freshState
  let subst = currentSubst finalState
  -- solve the constraints
  solve subst constraints
  -- if we got here, constraint solving succeded, apply the final subs to concretize every node's type
  return $ apply subst typed
