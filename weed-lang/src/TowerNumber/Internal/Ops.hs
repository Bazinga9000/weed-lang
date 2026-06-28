module TowerNumber.Internal.Ops where

import Data.Complex

-- helper operations to avoid the silly numeric typeclass structure forcing
-- complex to require realfloat

mulC :: (Num a) => Complex a -> Complex a -> Complex a
mulC (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)

powC :: (Num a, Integral b) => Complex a -> b -> Complex a
powC _ 0 = 1 :+ 0
powC z 1 = z
powC z n
  | even n = let half = powC z (n `div` 2) in mulC half half
  | otherwise = mulC z (powC z (n - 1))

compwiseCR :: (Rational -> Rational -> Rational) -> Complex Rational -> Complex Rational -> Complex Rational
compwiseCR f (a :+ b) (c :+ d) = f a c :+ f b d

addCR :: Complex Rational -> Complex Rational -> Complex Rational
addCR = compwiseCR (+)

mulCR :: Complex Rational -> Complex Rational -> Complex Rational
mulCR (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)

negCR :: Complex Rational -> Complex Rational
negCR (a :+ b) = negate a :+ negate b

-- (a + bi)/(c + di) = ((ac + bd) + (bc - ad)i) / (c^2 + d^2)
divCR :: Complex Rational -> Complex Rational -> Complex Rational
divCR (a :+ b) (c :+ d) =
  let den = c * c + d * d
   in ((a * c + b * d) / den) :+ ((b * c - a * d) / den)

-- 1 / (a + bi) = (a - bi) / (a^2 + b^2)
recipCR :: Complex Rational -> Complex Rational
recipCR (a :+ b) =
  let den = a * a + b * b
   in (a / den) :+ (negate b / den)
