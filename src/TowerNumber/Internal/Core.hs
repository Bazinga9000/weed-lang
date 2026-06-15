module TowerNumber.Internal.Core where

import Data.Complex
import Data.Ratio (approxRational)
import Numeric (log)
import TowerNumber.Internal.Ops
import TowerNumber.Utils

-- just to make things a bit terser
type F2 a = a -> a -> a

-- invariant: always downcasted
data TowerNumber
  = R Rational
  | D Double
  | CR (Complex Rational)
  | CD (Complex Double)
  | N -- not a number
  deriving (Show)

-- derationalize a TowerNumber
-- ultimately exposed as a builtin
approximate :: TowerNumber -> TowerNumber
approximate (R n) = D $ fromRational n
approximate (D n) = D n
approximate (CR (x :+ y)) = CD (fromRational x :+ fromRational y)
approximate (CD n) = CD n
approximate N = N

-- convert a string into a TowerNumber

-- try to coerce a float back into an arbitrary precision integer
tryInteger :: Double -> Maybe Integer
tryInteger x
  | isNaN x || isInfinite x = Nothing
  | x == fromInteger n = Just n
  | otherwise = Nothing
  where
    n = round x

-- lower a type to the most exact possible type
downcast :: TowerNumber -> TowerNumber
downcast (R r) = R r
downcast (D n)
  | isNaN n = N
  | otherwise = case tryInteger n of
      Just i -> R $ toRational i
      Nothing -> D n
downcast (CR (x :+ 0)) = R x
downcast (CR cr) = CR cr
downcast (CD (x :+ y))
  | isNaN x || isNaN y = N
  | y == 0 = downcast (D x) -- todo: epsilon? though if we use an epsilon everywhere this destroys the reason for using double (small numbers) and not just fixed precision
  | otherwise = case (,) <$> tryInteger x <*> tryInteger y of
      Just (ix, iy) -> downcast $ CR $ toRational ix :+ toRational iy
      Nothing -> CD $ x :+ y
downcast N = N

upcast :: TowerNumber -> TowerNumber
upcast (R r) = CR $ r :+ 0
upcast (D d) = CD $ d :+ 0
upcast (CR cr) = CR cr
upcast (CD cd) = CD cd
upcast N = N

instance Eq TowerNumber where
  a == b = downcast a `piecewiseEq` downcast b
    where
      piecewiseEq (R a') (R b') = a' == b'
      piecewiseEq (D a') (D b') = a' == b'
      piecewiseEq (CR a') (CR b') = a' == b'
      piecewiseEq (CD a') (CD b') = a' == b'
      piecewiseEq N N = True
      piecewiseEq _ _ = False

-- helpers to lift functions into TowerNums
lift1 :: (Complex Rational -> Complex Rational) -> (Complex Double -> Complex Double) -> TowerNumber -> TowerNumber
lift1 rf df n = downcast $ case upcast n of
  (CR cr) -> CR $ rf cr
  (CD cd) -> CD $ df cd
  N -> N
  _ -> error "unreachable"

lift2 :: F2 (Complex Rational) -> F2 (Complex Double) -> F2 TowerNumber
lift2 rf df x y = downcast $ case (upcast x, upcast y) of
  (CR crx, CR cry) -> CR $ rf crx cry
  (CR crx, CD cdy) -> CD $ df (fmap fromRational crx) cdy
  (CD cdx, CR cdy) -> CD $ df cdx (fmap fromRational cdy)
  (CD cdx, CD cdy) -> CD $ df cdx cdy
  (N, _) -> N
  (_, N) -> N
  _ -> error "unreachable"

liftCompare :: (Rational -> Rational -> Ordering) -> (Double -> Double -> Ordering) -> TowerNumber -> TowerNumber -> Maybe Ordering
liftCompare rc _ (R x) (R y) = Just $ rc x y
liftCompare _ dc (R x) (D y) = Just $ dc (fromRational x) y
liftCompare _ dc (D x) (R y) = Just $ dc x (fromRational y)
liftCompare _ dc (D x) (D y) = Just $ dc x y
liftCompare _ _ _ _ = Nothing

-- transcendetal functions will always work on doubles (but still try to downcast after)
unsafeUpcast :: TowerNumber -> Complex Double
unsafeUpcast (R r) = fromRational r :+ 0
unsafeUpcast (D r) = r :+ 0
unsafeUpcast (CR (rx :+ ry)) = fromRational rx :+ fromRational ry
unsafeUpcast (CD n) = n
unsafeUpcast N = 0 / 0 :+ 0 / 0

lift1Trans :: (Complex Double -> Complex Double) -> TowerNumber -> TowerNumber
lift1Trans f = downcast . CD . f . unsafeUpcast

-- as lift1, but special cases an identity
lift1TransID :: (Rational, Rational) -> (Complex Double -> Complex Double) -> TowerNumber -> TowerNumber
lift1TransID (idIn, idOut) f n = case downcast n of
  (R r)
    | r == idIn -> R idOut
  _ -> lift1Trans f n

lift2Trans :: F2 (Complex Double) -> F2 TowerNumber
lift2Trans f a b = downcast . CD . uncurry f . bimapBoth unsafeUpcast $ (a, b)

instance Num TowerNumber where
  (+) = lift2 addCR (+)
  (*) = lift2 mulCR (*)
  negate = lift1 negCR negate
  fromInteger = R . toRational

  abs n = downcast $ case n of
    R r -> R (abs r)
    D d -> D (abs d)
    CR (x :+ y) -> sqrt (fromRational (x * x + y * y))
    CD cd -> CD (abs cd)
    N -> N

  signum n = downcast $ case n of
    R r -> R (signum r)
    D d -> D (signum d)
    cr@(CR _) -> cr / abs cr
    CD cd -> CD (signum cd)
    N -> N

instance Fractional TowerNumber where
  fromRational = R

  (R n) / (R 0)
    | n > 0 = D $ 1 / 0
    | n < 0 = D $ (-1) / 0
    | n == 0 = N
  (D n) / (R 0) = D $ n / 0 -- (D 0) always gets downcasted to R 0
  _ / (R 0) = N -- nonzero imaginary part over 0 -> undefined
  a / b = lift2 divCR (/) a b
  recip = lift1 recipCR recip

instance Floating TowerNumber where
  pi = D pi
  sqrt x = case downcast x of
    R r
      | r >= 0 -> case exactRoot 2 r of
          Just rat -> R rat
          Nothing -> lift1Trans sqrt (R r)
      | r < 0 -> case exactRoot 2 (abs r) of
          Just rat -> CR (0 :+ rat)
          Nothing -> lift1Trans sqrt (R r)
    CR (rx :+ ry) ->
      let normSq = rx * rx + ry * ry
       in case exactRoot 2 normSq of
            Just norm ->
              case (exactRoot 2 ((norm + rx) / 2), exactRoot 2 ((norm - rx) / 2)) of
                (Just a, Just b) ->
                  let b' = if ry < 0 then negate b else b
                   in CR (a :+ b')
                _ -> lift1Trans sqrt (CR (rx :+ ry))
            Nothing -> lift1Trans sqrt (CR (rx :+ ry))
    _ -> lift1Trans sqrt x

  x ** y = case downcast y of
    R ry | denominator ry == 1 -> x ^^ numerator ry
    R ry ->
      let p = numerator ry
          q = fromInteger (denominator ry) :: Natural
       in case downcast x of
            R rx -> case exactRoot q rx of
              Just root -> R root ^^ p
              Nothing ->
                -- fall back to square root to try and get complexes
                -- can't fall back to any other perfect powers because of trig
                if q == 2
                  then sqrt x ^^ p
                  else lift2Trans (**) x y
            CR _ -> if q == 2 then sqrt x ^^ p else lift2Trans (**) x y
            _ -> lift2Trans (**) x y
    _ -> lift2Trans (**) x y

  exp = lift1TransID (0, 1) exp
  log = lift1TransID (1, 0) log
  sin = lift1TransID (0, 0) sin
  cos = lift1TransID (0, 1) cos
  tan = lift1TransID (0, 0) tan
  asin = lift1TransID (0, 0) asin
  acos = lift1TransID (1, 0) acos
  atan = lift1TransID (0, 0) atan
  sinh = lift1TransID (0, 0) sinh
  cosh = lift1TransID (0, 1) cosh
  asinh = lift1TransID (0, 0) asinh
  acosh = lift1TransID (1, 0) acosh
  atanh = lift1TransID (0, 0) atanh

  logBase b x = case (downcast b, downcast x) of
    (R rb, R rx)
      | rb > 0 && rx > 0 && rb /= 1 ->
          let approxFloat = logBase (fromRational rb :: Double) (fromRational rx :: Double)
           in if isNaN approxFloat || isInfinite approxFloat
                then lift2Trans logBase b x
                else
                  let -- guess a rational
                      guess = approxRational approxFloat 1e-9
                   in -- if the guess is correct, use it
                      case downcast (R rb ** R guess) of
                        R exactResult | exactResult == rx -> R guess
                        _ -> lift2Trans logBase b x
    _ -> lift2Trans logBase b x

--- complex floor/ceil/mod
--- from APL's complex floor implementation
complexFloor :: (RealFrac a) => Complex a -> Complex a
complexFloor (r :+ i)
  | 1 > x + y = b
  | x >= y = (fl r + 1) :+ fl i
  | otherwise = fl r :+ (fl i + 1)
  where
    fl = fromInteger . floor
    b = fl r :+ fl i
    x = r - fl r
    y = i - fl i

tnFloor :: TowerNumber -> TowerNumber
tnFloor = lift1 complexFloor complexFloor

tnCeil :: TowerNumber -> TowerNumber
tnCeil = negate . tnFloor . negate

tnMod :: TowerNumber -> TowerNumber -> TowerNumber
tnMod a b = a - b * tnFloor (a / b)

-- Tools for converting tns into another type
-- will return Just if the conversion can be done, nothing if not
tnIntoInteger :: TowerNumber -> Maybe Integer
tnIntoInteger n = case downcast n of
  (R n') -> if denominator n' == 1 then Just $ numerator n' else Nothing
  _ -> Nothing -- integer Ds get downcasted before this

tnIntoNatural :: TowerNumber -> Maybe Natural
tnIntoNatural = tnIntoInteger >=> integerToNatural

tnIntoDouble :: TowerNumber -> Maybe Double
tnIntoDouble n = case downcast n of
  (R n') -> Just $ fromRational n'
  (D n') -> Just n'
  _ -> Nothing
