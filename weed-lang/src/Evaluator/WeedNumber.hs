module Evaluator.WeedNumber where

import Control.Lens
import Data.Complex
import Evaluator.Metadata
import Numeric (Floating (log))
import TowerNumber.Core

data WeedNumber = WeedNumber
  { _value :: TowerNumber,
    _metadata :: Maybe NumberMetadata
  }
  deriving (Show)

makeLenses ''WeedNumber

literal :: TowerNumber -> WeedNumber
literal x = WeedNumber {_value = x, _metadata = Nothing}

blank :: TowerNumber -> WeedNumber
blank x = WeedNumber {_value = x, _metadata = Just mempty}

lift1WN :: (TowerNumber -> TowerNumber) -> WeedNumber -> WeedNumber
lift1WN f x = x & value %~ f

-- all binary operations on WeedNumbers respect the drop semantics
lift2WN :: (TowerNumber -> TowerNumber -> TowerNumber) -> WeedNumber -> WeedNumber -> WeedNumber
lift2WN f x y
  | wnDropped x && wnDropped y = blank 0
  | wnDropped x = y
  | wnDropped y = x
  | otherwise =
      WeedNumber
        { _value = f (x ^. value) (y ^. value),
          _metadata = (x ^. metadata) <> (y ^. metadata)
        }

instance Num WeedNumber where
  (+) = lift2WN (+)
  (-) = lift2WN (-)
  (*) = lift2WN (*)
  abs = lift1WN abs
  signum = lift1WN signum
  fromInteger = literal . fromInteger

instance Fractional WeedNumber where
  (/) = lift2WN (/)
  recip = lift1WN recip
  fromRational = literal . fromRational

instance Floating WeedNumber where
  pi = literal pi
  exp = lift1WN exp
  log = lift1WN log
  sqrt = lift1WN sqrt
  (**) = lift2WN (**)
  logBase = lift2WN logBase
  sin = lift1WN sin
  cos = lift1WN cos
  tan = lift1WN tan
  asin = lift1WN asin
  acos = lift1WN acos
  atan = lift1WN atan
  sinh = lift1WN sinh
  cosh = lift1WN cosh
  tanh = lift1WN tanh
  asinh = lift1WN asinh
  acosh = lift1WN acosh
  atanh = lift1WN atanh

wnFloor :: WeedNumber -> WeedNumber
wnFloor = lift1WN tnFloor

wnCeil :: WeedNumber -> WeedNumber
wnCeil = lift1WN tnCeil

wnMod :: WeedNumber -> WeedNumber -> WeedNumber
wnMod = lift2WN tnMod

wnCAdd :: WeedNumber -> WeedNumber -> WeedNumber
wnCAdd = lift2WN $ \a b -> a + b * CR (0 :+ 1)

wnCSub :: WeedNumber -> WeedNumber -> WeedNumber
wnCSub = lift2WN $ \a b -> a + b * CR (0 :+ (-1))

infix 4 =~=

(=~=) :: WeedNumber -> WeedNumber -> Bool
(=~=) a b = a ^. value == b ^. value

wnMaybeCompare :: WeedNumber -> WeedNumber -> Maybe Ordering
wnMaybeCompare a b = maybeCompare (a ^. value) (b ^. value)

--- metadata related functions

wnLiftMeta :: (NumberMetadata -> NumberMetadata) -> WeedNumber -> WeedNumber
wnLiftMeta f n = n & metadata . _Just %~ f

wnIsPure :: WeedNumber -> Bool
wnIsPure = has (metadata . _Nothing)

wnDropped :: WeedNumber -> Bool
wnDropped n = maybe False (^. dropped) (n ^. metadata)
