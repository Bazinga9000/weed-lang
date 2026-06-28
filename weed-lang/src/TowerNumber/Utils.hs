module TowerNumber.Utils where

import Data.Complex
import Data.Ratio (approxRational, (%))
import TowerNumber.Internal.Ops

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

exactComplexRoot :: Natural -> Complex Rational -> Maybe (Complex Rational)
exactComplexRoot 0 _ = Nothing
exactComplexRoot 1 c = Just c
exactComplexRoot n target@(r :+ i) =
  let -- generate all nth roots of unity as double
      cd = fromRational r :+ fromRational i :: Complex Double
      principal = cd ** (1 / fromIntegral n)
      angles = [2 * pi * fromIntegral k / fromIntegral n | k <- [0 .. n - 1]]
      allRoots = [principal * exp (0 :+ theta) | theta <- angles]
      -- approximate them as rationals
      epsilon = 1e-10
      snap x = approxRational x epsilon
      guesses = [snap (realPart root) :+ snap (imagPart root) | root <- allRoots]
   in -- get the first one that matches (or nothing)
      listToMaybe [g | g <- guesses, g `powC` n == target]
