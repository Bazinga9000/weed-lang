module Evaluator.WeedNumber where

import Data.Complex

newtype WeedNumber = WeedNumber
  { value :: Complex Double
  }
  deriving (Show)

literal :: Double -> WeedNumber
literal x = WeedNumber {value = x :+ 0}

complexLiteral :: Double -> Double -> WeedNumber
complexLiteral x y = WeedNumber {value = x :+ y}

lift1WN :: (Complex Double -> Complex Double) -> WeedNumber -> WeedNumber
lift1WN f x = WeedNumber {value = f (value x)}

lift2WN :: (Complex Double -> Complex Double -> Complex Double) -> WeedNumber -> WeedNumber -> WeedNumber
lift2WN f x y = WeedNumber {value = f (value x) (value y)}

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

infix 4 =~=

(=~=) :: WeedNumber -> WeedNumber -> Bool
(=~=) a b = value a == value b
