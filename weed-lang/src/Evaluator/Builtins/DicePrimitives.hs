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
import Numeric (log)
import Test.QuickCheck.Gen
import TowerNumber.Core

wrapOne :: (Builtin, Text) -> (Value -> Eval a) -> (a -> Gen Value) -> Value
wrapOne (builtin, expected) assertion fn = VBuiltin $ \v -> do
  v' <- catchError (assertion v) (const $ throwError $ BadDieParameter builtin ("expected " <> expected) v)
  return . VDice . liftGen . fn $ v'

wrapTwo :: (Builtin, Text, Text) -> (Value -> Eval a) -> (Value -> Eval b) -> (a -> b -> Gen Value) -> Value
wrapTwo (builtin, exp1, exp2) ass1 ass2 fn = VBuiltin $ \va -> return . VBuiltin $ \vb -> do
  va' <- catchError (ass1 va) (const $ throwError $ BadDieParameter builtin ("expected " <> exp1) va)
  vb' <- catchError (ass2 vb) (const $ throwError $ BadDieParameter builtin ("expected " <> exp2) vb)
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
-- Samples a uniform integer from [1, sides]
-- crits on: sides
-- fails on: 1
d :: Value
d = wrapOne (DiceD, "a positive integer") assertPositive dCore
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

-- a set die
-- samples its list uniformly
-- inherits crit data from its list elems if they're numbers
s :: Value
s = wrapOne (DiceS, "a non-empty list") assertNonEmptyList sCore
  where
    sCore nel = elements . toList $ nel

-- fudge die
-- samples a uniform integer from [-bound, bound]
-- crits on: bound
-- fails on: -bound
f :: Value
f = wrapOne (DiceD, "a natural number") assertNatural fCore
  where
    fCore :: Natural -> Gen Value
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
-- samples a uniform double from the interval (0, i)
-- never crits nor fails
u :: Value
u = wrapOne (DiceD, "a positive real") assertPositiveReal uCore
  where
    uCore i =
      return . VDice . liftGen $
        VNumber . noCritFail . noCrit . blank . D <$> choose (0.0, i)

-- gaussian die
-- samples a gaussian with mean 0 and stdev sigma
-- never crits nor fails
gauss :: Value
gauss = wrapOne (DiceGauss, "a positive real") assertPositiveReal gaussCore
  where
    gaussCore sigma =
      VNumber . noCritFail . noCrit . blank <$> do
        u1 <- choose (0.0, 1.0)
        u2 <- choose (0.0, 1.0)
        let z = sqrt (-(2.0 * log u1)) * cos (2.0 * pi * u2)
        return . D $ sigma * z

-- pareto die
-- samples the pareto distribution with shape parameter alpha
-- never crits nor fails
pareto :: Value
pareto = wrapOne (DicePareto, "a positive real") assertPositiveReal paretoCore
  where
    paretoCore alpha =
      VNumber . noCritFail . noCrit . blank <$> do
        u' <- choose (0.0, 1.0)
        return . D $ u' ** (1.0 / alpha)

-- binomial die
-- samples the binomial distribution with n trials succeding with probability p
-- crits on: n
-- fails on: 0
binomial :: Value
binomial =
  wrapTwo
    (DiceBinomial, "a positive integer", "a probability between 0 and 1")
    assertPositive
    (assertRealPredicate $ \prob -> prob >= 0.0 && prob <= 1.0)
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
-- either true or false, with 50% odds
coin :: Value
coin = VDice $ liftGen $ elements [VBool True, VBool False]

-- circle
-- samples a random complex on the circle of radius r, centered at the origin
-- never crits nor fails
circle :: Value
circle = wrapOne (DiceCircle, "a positive real") assertPositiveReal circleCore
  where
    circleCore r = do
      theta <- choose (0.0, 2.0 * pi)
      return . VNumber $ noCritFail . noCrit . blank . CD $ (r * cos theta) :+ (r * sin theta)
