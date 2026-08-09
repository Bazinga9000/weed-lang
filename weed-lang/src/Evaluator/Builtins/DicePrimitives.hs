{-# LANGUAGE MultiWayIf #-}

module Evaluator.Builtins.DicePrimitives where

import AST
import Control.Lens hiding (elements)
import Control.Monad.Except
import Data.Complex
import Evaluator.Assertions
import Evaluator.Metadata
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.DropList (DropItem(K))
import Numeric (log)
import Test.QuickCheck.Gen
import TowerNumber.Core
import TypeChecker.Singletons
import TypeChecker.Types

-- The wrappers take a domain assertion (WeedNumber -> Eval a) and a sampling
-- function (a -> Gen (Value t)), and produce a typed die builtin.

wrapOne :: SWeedType t -> (Builtin, Text) -> (WeedNumber -> Eval a) -> (a -> Gen (Value t)) -> Value (TFunction TNumber (TDice t))
wrapOne st (builtin, expected) assertion fn = VBuiltin $ TypedFun STNumber (STDice st) $ \(VNumber wn) -> do
  v' <- catchError (assertion wn) (const $ throwError $ BadDieParameter builtin ("expected " <> expected) (SomeValue STNumber (VNumber wn)))
  return . VDice . liftGen . fn $ v'

wrapTwo :: SWeedType t -> (Builtin, Text, Text) -> (WeedNumber -> Eval a) -> (WeedNumber -> Eval b) -> (a -> b -> Gen (Value t)) -> Value (TFunction TNumber (TFunction TNumber (TDice t)))
wrapTwo st (builtin, exp1, exp2) ass1 ass2 fn = VBuiltin $ TypedFun STNumber (STFunction STNumber (STDice st)) $ \(VNumber wa) ->
  return $ VBuiltin $ TypedFun STNumber (STDice st) $ \(VNumber wb) -> do
    va' <- catchError (ass1 wa) (const $ throwError $ BadDieParameter builtin ("expected " <> exp1) (SomeValue STNumber (VNumber wa)))
    vb' <- catchError (ass2 wb) (const $ throwError $ BadDieParameter builtin ("expected " <> exp2) (SomeValue STNumber (VNumber wb)))
    return . VDice . liftGen $ fn va' vb'

crit :: WeedNumber -> WeedNumber
crit = (metadata . _Just . critLevel) .~ Multibool (1, 0)

noCrit :: WeedNumber -> WeedNumber
noCrit = (metadata . _Just . critLevel) .~ Multibool (0, 1)

critFail :: WeedNumber -> WeedNumber
critFail = (metadata . _Just . failLevel) .~ Multibool (1, 0)

noCritFail :: WeedNumber -> WeedNumber
noCritFail = (metadata . _Just . failLevel) .~ Multibool (0, 1)

-- A standard die
d :: Value (TFunction TNumber (TDice TNumber))
d = wrapOne STNumber (DiceD, "a positive integer") (assertPositive DiceD) dCore
  where
    dCore sides = do
      res <- chooseInteger (1, fromPositive sides)
      let wn = blank . R . fromInteger $ res
      let wn' =
            if
              | sides == 1 -> critFail . crit $ wn
              | res == 1 -> critFail . noCrit $ wn
              | res == fromPositive sides -> noCritFail . crit $ wn
              | otherwise -> noCritFail . noCrit $ wn
      return $ VNumber wn'

-- a set die: samples its list uniformly. Needs the list element type.
s :: SWeedType a -> Value (TFunction (TList a) (TDice a))
s sa = VBuiltin $ TypedFun (STList sa) (STDice sa) $ \(VList l) -> do
  ne <- assertNonEmptyList DiceS l
  return $ VDice $ liftGen $ elements [x | K x <- toList ne]

-- fudge die
f :: Value (TFunction TNumber (TDice TNumber))
f = wrapOne STNumber (DiceF, "a natural number") (assertNatural DiceF) fCore
  where
    fCore :: Natural -> Gen (Value TNumber)
    fCore bound = do
      let ibound = toInteger bound
      res <- chooseInteger (-ibound, ibound)
      let wn = blank . R . fromInteger $ res
      let wn' =
            if
              | bound == 0 -> critFail . crit $ wn
              | res == -ibound -> critFail . noCrit $ wn
              | res == ibound -> noCritFail . crit $ wn
              | otherwise -> noCritFail . noCrit $ wn
      return $ VNumber wn'

-- uniform float die
u :: Value (TFunction TNumber (TDice TNumber))
u = wrapOne STNumber (DiceU, "a positive real") (assertPositiveReal DiceU) uCore
  where
    uCore i =
      VNumber . noCritFail . noCrit . blank . D <$> choose (0.0, i)

-- gaussian die
gauss :: Value (TFunction TNumber (TDice TNumber))
gauss = wrapOne STNumber (DiceGauss, "a positive real") (assertPositiveReal DiceGauss) gaussCore
  where
    gaussCore sigma =
      VNumber . noCritFail . noCrit . blank <$> do
        u1 <- choose (0.0, 1.0)
        u2 <- choose (0.0, 1.0)
        let z = sqrt (-(2.0 * log u1)) * cos (2.0 * pi * u2)
        return . D $ sigma * z

-- pareto die
pareto :: Value (TFunction TNumber (TDice TNumber))
pareto = wrapOne STNumber (DicePareto, "a positive real") (assertPositiveReal DicePareto) paretoCore
  where
    paretoCore alpha =
      VNumber . noCritFail . noCrit . blank <$> do
        u' <- choose (0.0, 1.0)
        return . D $ u' ** (1.0 / alpha)

-- binomial die
binomial :: Value (TFunction TNumber (TFunction TNumber (TDice TNumber)))
binomial =
  wrapTwo STNumber
    (DiceBinomial, "a positive integer", "a probability between 0 and 1")
    (assertPositive DiceBinomial)
    (assertRealPredicate DiceBinomial $ \prob -> prob >= 0.0 && prob <= 1.0)
    binomialCore
  where
    binomialCore trials prob = do
      let oneTrial = (\r -> if r < prob then 1 else 0) <$> choose (0.0, 1.0)
      count <- sum <$> replicateM (fromPositive trials) oneTrial
      let wn = blank . R . fromInteger $ count
      let wn' =
            if
              | count == 0 -> critFail . noCrit $ wn
              | count == fromPositive trials -> noCritFail . crit $ wn
              | otherwise -> noCritFail . noCrit $ wn
      return $ VNumber wn'

-- coin
coin :: Value (TDice TBool)
coin = VDice $ liftGen $ elements [VBool True, VBool False]

-- circle
circle :: Value (TFunction TNumber (TDice TNumber))
circle = wrapOne STNumber (DiceCircle, "a positive real") (assertPositiveReal DiceCircle) circleCore
  where
    circleCore r = do
      theta <- choose (0.0, 2.0 * pi)
      return . VNumber $ noCritFail . noCrit . blank . CD $ (r * cos theta) :+ (r * sin theta)
