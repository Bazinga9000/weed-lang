module Evaluator.WeedNumber where

import Data.Complex
import Evaluator.Metadata
import Numeric (Floating (log))

data WeedNumber = WeedNumber
  { value :: Complex Double,
    metadata :: Maybe NumberMetadata
  }
  deriving (Show)

literal :: Double -> WeedNumber
literal x = WeedNumber {value = x :+ 0, metadata = Nothing}

complexLiteral :: Double -> Double -> WeedNumber
complexLiteral x y = WeedNumber {value = x :+ y, metadata = Nothing}

lift1WN :: (Complex Double -> Complex Double) -> WeedNumber -> WeedNumber
lift1WN f x = WeedNumber {value = f (value x), metadata = metadata x}

lift2WN :: (Complex Double -> Complex Double -> Complex Double) -> WeedNumber -> WeedNumber -> WeedNumber
lift2WN f x y = WeedNumber {value = f (value x) (value y), metadata = metadata x <> metadata y}

complexFloor :: Complex Double -> Complex Double
complexFloor (r :+ i)
  | 1.0 > x + y = b
  | x >= y = b + (1.0 :+ 0)
  | otherwise = b + (0 :+ 1.0)
  where
    fl = fromInteger . floor
    b = fl r :+ fl i
    x = r - fl r
    y = i - fl i

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
wnFloor = lift1WN complexFloor

wnCeil :: WeedNumber -> WeedNumber
wnCeil = lift1WN $ negate . complexFloor . negate

wnMod :: WeedNumber -> WeedNumber -> WeedNumber
wnMod = lift2WN $ \a b -> a - b * complexFloor (a / b)

wnCAdd :: WeedNumber -> WeedNumber -> WeedNumber
wnCAdd = lift2WN $ \a b -> a + b * (0 :+ 1)

wnCSub :: WeedNumber -> WeedNumber -> WeedNumber
wnCSub = lift2WN $ \a b -> a + b * (0 :+ (-1))

infix 4 =~=

(=~=) :: WeedNumber -> WeedNumber -> Bool
(=~=) a b = value a == value b

--- metadata related functions

wnLiftMeta :: (NumberMetadata -> NumberMetadata) -> WeedNumber -> WeedNumber
wnLiftMeta f n = n {metadata = f <$> metadata n}

wnIsPure :: WeedNumber -> Bool
wnIsPure = isNothing . metadata
