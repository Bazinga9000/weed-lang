module TowerNumber.Utils where

import Data.Ratio ((%))

-- smart root finder
smartNatRoot :: Natural -> Natural -> Either Double Natural
smartNatRoot n x
  | n == 0 =
      if x == 1 then Right 1 else Left (1 / 0)
  | x == 0 = Right 0
  | otherwise =
      let approx = Left $ fromIntegral x ** (1 / fromIntegral n)
          go lo hi
            | lo > hi = approx
            | otherwise =
                let m = lo + (hi - lo) `div` 2
                    p = m ^ n
                 in case compare p x of
                      EQ -> Right m
                      LT -> go (m + 1) hi
                      GT -> go lo (m - 1)
       in go 0 x

exactRoot :: Natural -> Rational -> Maybe Rational
exactRoot n r
  | r < 0 = if even n then Nothing else fmap negate (exactRoot n (-r))
  | r == 0 = Just 0
  | otherwise =
      let num = fromInteger (numerator r) :: Natural
          den = fromInteger (denominator r) :: Natural
       in case (smartNatRoot n num, smartNatRoot n den) of
            (Right rn, Right rd) -> Just (toInteger rn % toInteger rd)
            _ -> Nothing
