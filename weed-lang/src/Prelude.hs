module Prelude
  ( module Relude,
    module Relude.Extra.Bifunctor,
    PositiveNatural,
    toPositive,
    fromPositive,
  )
where

import Relude
import Relude.Extra.Bifunctor

-- todo: is this a good idea? i really don't know...
newtype PositiveNatural = Positive Natural deriving (Show, Eq, Ord, Enum)

toPositive :: Natural -> Maybe PositiveNatural
toPositive 0 = Nothing
toPositive k = Just $ Positive k

fromPositive :: (Integral a) => PositiveNatural -> a
fromPositive (Positive n) = fromInteger . toInteger $ n

-- yes, i am well aware that this Num instance is unsafe, in that
-- fromInteger and negate are bad
-- however, haskell's numeric hierarchy is a hot mess
-- and so fromInteger is gated behind a Num instance....
-- this is at least as bad as the Num instance for Natural
-- which exists in GHC so....
instance Num PositiveNatural where
  (Positive a) + (Positive b) = Positive (a + b)
  (Positive a) * (Positive b) = Positive (a * b)
  abs (Positive x) = Positive (abs x)
  signum = const 1
  fromInteger 0 = error "PositiveNatural fromInteger called on 0"
  fromInteger k = Positive (fromInteger k)
  negate (Positive x) = Positive $ negate x -- errors, always

instance Real PositiveNatural where
  toRational (Positive k) = toRational k
