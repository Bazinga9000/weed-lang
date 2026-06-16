{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.TowerNumber (towerNumberTests) where

import Data.Complex
import Data.Ratio ((%))
import Numeric (Floating (log))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TowerNumber.Internal.Core

-- QC Generators

genRational :: Gen Rational
genRational = do
  n <- choose (-100, 100)
  d <- choose (1, 100)
  return (n % d)

genDouble :: Gen Double
genDouble = choose (-100.0, 100.0)

instance Arbitrary TowerNumber where
  arbitrary =
    frequency
      [ (12, R <$> genRational),
        (12, D <$> genDouble),
        (6, CR <$> ((:+) <$> genRational <*> genRational)),
        (6, CD <$> ((:+) <$> genDouble <*> genDouble)),
        -- weirdos
        (1, pure N),
        (1, pure 0),
        (1, pure $ D (1 / 0)),
        (1, pure $ D (-1 / 0)),
        (1, pure $ CD $ 0 :+ (1 / 0)),
        (1, pure $ CD $ (1 / 0) :+ (1 / 0))
      ]

genExactTN :: Gen TowerNumber
genExactTN =
  oneof
    [ R <$> genRational,
      CR <$> ((:+) <$> genRational <*> genRational)
    ]

genGaussianInteger :: Gen TowerNumber
genGaussianInteger = do
  r <- choose (-100, 100) :: Gen Integer
  i <- choose (-100, 100) :: Gen Integer
  return $ CR (fromInteger r % 1 :+ fromInteger i % 1)

-- helpers

isN :: TowerNumber -> Bool
isN N = True
isN _ = False

isFinite :: TowerNumber -> Bool
isFinite (D d) = not (isInfinite d || isNaN d)
isFinite (CD (r :+ i)) = not (isInfinite r || isInfinite i || isNaN r || isNaN i)
isFinite N = False
isFinite _ = True -- R and CR are always perfectly exact and finite

isExact :: TowerNumber -> Bool
isExact (R _) = True
isExact (CR _) = True
isExact _ = False

magAsDouble :: TowerNumber -> Double
magAsDouble n = case tnIntoDouble (abs n) of
  Just d -> d
  Nothing -> 1 / 0 -- unreachable; abs always returns a Real (R or D)

getReIm :: TowerNumber -> (Double, Double)
getReIm n = case approximate n of
  CD (r :+ i) -> (r, i)
  D d -> (d, 0)
  _ -> (0, 0)

-- arithmetic tests

prop_add_comm :: TowerNumber -> TowerNumber -> Property
prop_add_comm x y =
  not (isN x) && not (isN y) ==>
    x + y === y + x

prop_mul_comm :: TowerNumber -> TowerNumber -> Property
prop_mul_comm x y =
  not (isN x) && not (isN y) ==>
    x * y === y * x

-- floats are so mean and evil, and aren't associative
-- so we only test associativity on the exact types

prop_add_assoc_exact :: Property
prop_add_assoc_exact = forAll genExactTN $ \x ->
  forAll genExactTN $ \y ->
    forAll genExactTN $ \z ->
      (x + y) + z === x + (y + z)

prop_mul_assoc_exact :: Property
prop_mul_assoc_exact = forAll genExactTN $ \x ->
  forAll genExactTN $ \y ->
    forAll genExactTN $ \z ->
      (x * y) * z === x * (y * z)

-- exactness tests

prop_downcast_idempotent :: TowerNumber -> Property
prop_downcast_idempotent x = downcast x === downcast (downcast x)

prop_double_integers_downcast :: Integer -> Property
prop_double_integers_downcast i =
  downcast (D (fromInteger i)) === R (toRational i)

-- the roundtrip
-- g -> g^n -> (g^n)^(1/n)
-- should never produce CDs for gaussian integers g
-- and furthermore (g^n)^(1/n) should actually *be* a root
-- we can't enforce that (g^n)^(1/n) = g because roots are multivalued and
-- we can only guarantee that we will find *some* exact root if one exists
prop_exact_complex_powers :: Property
prop_exact_complex_powers =
  forAll (choose (1, 5)) $ \p ->
    forAll genGaussianInteger $ \z ->
      let pTN = R (p % 1)
          zPow = z ** pTN
          zRoot = zPow ** R (1 % p)
       in isExact zPow .&&. isExact zRoot .&&. (zRoot ^ p == zPow)

-- NaN semantics

prop_N_absorbing :: TowerNumber -> Property
prop_N_absorbing x =
  conjoin
    [ check "x + N" (x + N),
      check "x - N" (x - N),
      check "x * N" (x * N),
      check "x / N" (x / N),
      check "N / x" (N / x),
      check "abs N" (abs N),
      check "negate N" (negate N),
      check "signum N" (signum N),
      check "recip N" (recip N),
      check "sqrt N" (sqrt N),
      check "x ** N" (x ** N),
      check "N ** x" (N ** x),
      check "exp N" (exp N),
      check "log N" (log N),
      check "sin N" (sin N),
      check "cos N" (cos N),
      check "tan N" (tan N),
      check "asin N" (asin N),
      check "acos N" (acos N),
      check "atan N" (atan N),
      check "sinh N" (sinh N),
      check "cosh N" (cosh N),
      check "asinh N" (asinh N),
      check "acosh N" (acosh N),
      check "atanh N" (atanh N),
      check "logBase N x" (logBase N x),
      check "logBase x N" (logBase x N),
      check "N == N" N
    ]
  where
    check name result =
      counterexample (name <> " failed! Expected N, got " <> show result) $
        result === N

case_NaN_downcasts_to_N :: TestTree
case_NaN_downcasts_to_N = testCase "NaN downcasts to N" $ N @=? (downcast $ D $ 0 / 0)

case_complexNaN_downcasts_to_N :: TestTree
case_complexNaN_downcasts_to_N = testCase "NaNi downcasts to N" $ N @=? (downcast $ CD $ 0 :+ (0 / 0))

-- complex floor semantics
-- see https://www.aplwiki.com/wiki/Complex_floor

-- Fractionality: |z - ⌊z⌋| < 1
prop_apl_fractionality :: TowerNumber -> Property
prop_apl_fractionality z =
  isFinite z ==>
    magAsDouble (z - tnFloor z) < 1.0

-- Integrity: ⌊⌊z⌋ = ⌊z⌋
prop_apl_integrity :: TowerNumber -> Property
prop_apl_integrity z =
  not (isN z) ==>
    tnFloor (tnFloor z) === tnFloor z

-- Integer Translation: ⌊c + z⌋ = c + ⌊z⌋
prop_apl_translation :: TowerNumber -> Property
prop_apl_translation z = not (isN z) ==>
  forAll genGaussianInteger $ \c ->
    tnFloor (c + z) === c + tnFloor z

-- Convexity: If ⌊z⌋ = g and ⌊w⌋ = g, then for t in [0,1], ⌊t*z + (1-t)*w⌋ = g
prop_apl_convexity :: TowerNumber -> Double -> Double -> Double -> Property
prop_apl_convexity z dr di t' =
  isFinite z ==>
    let t = abs t' `min` 1.0
        -- perturb z slightly to generate w, making it highly likely
        -- to fall in the same floor bucket without exhausting max discards.
        w = z + CR (toRational (dr * 0.1) :+ toRational (di * 0.1))
        g = tnFloor z
     in (tnFloor w == g) ==>
          let mid = z * D t + w * D (1 - t)
           in tnFloor mid === g

-- Compatibility: re(⌊z⌋) <= re(⌈z⌉) and im(⌊z⌋) <= im(⌈z⌉)
prop_apl_compatibility :: TowerNumber -> Property
prop_apl_compatibility z =
  not (isN z) ==>
    let (reF, imF) = getReIm (tnFloor z)
        (reC, imC) = getReIm (tnCeil z)
     in (reF <= reC) .&&. (imF <= imC)

towerNumberTests :: TestTree
towerNumberTests =
  testGroup
    "TowerNumber Properties"
    [ testGroup
        "Standard Arithmetic"
        [ testProperty "Addition is commutative" prop_add_comm,
          testProperty "Multiplication is commutative" prop_mul_comm,
          testProperty "Addition is associative (exact types)" prop_add_assoc_exact,
          testProperty "Multiplication is associative (exact types)" prop_mul_assoc_exact
        ],
      testGroup
        "Exactness"
        [ testProperty "Downcast is idempotent" prop_downcast_idempotent,
          testProperty "Double floats with no fractional part downcast to R" prop_double_integers_downcast,
          testProperty "Integer complex powers and roots stay exact" prop_exact_complex_powers
        ],
      testGroup
        "N Semantics"
        [ testProperty "N is an absorbing element for all operations" prop_N_absorbing,
          case_NaN_downcasts_to_N,
          case_complexNaN_downcasts_to_N
        ],
      testGroup
        "Complex Floor Semantics"
        [ testProperty "Fractionality: Magnitude difference is < 1" prop_apl_fractionality,
          testProperty "Integrity: Floor of floor is identical" prop_apl_integrity,
          testProperty "Integer Translation: Distributes over Gaussian integers" prop_apl_translation,
          testProperty "Convexity: Linear interpolation preserves floor" prop_apl_convexity,
          testProperty "Compatibility: Real/Imaginary parts respect ceiling bounds" prop_apl_compatibility
        ]
    ]
