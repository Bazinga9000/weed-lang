module TowerNumber.Parse (parseTN, formatTN) where

import Data.Complex
import Data.Ratio ((%))
import TowerNumber.Core

-- parses a String decimal literal exactly into an R TowerNumber.
-- assumes the string is a validated shape from the parser (e.g., "4", "-7.6").
parseTN :: String -> TowerNumber
parseTN str =
  R $
    let (isNeg, unsignedStr) = case str of
          ('-' : rest) -> (True, rest)
          _ -> (False, str)
        (intPart, fracWithDot) = break (== '.') unsignedStr
        readInt :: String -> Integer
        readInt "" = 0
        readInt s =
          fromMaybe
            (error $ "Parser invariant violated: Invalid digit sequence: " <> toText s)
            (readMaybe s)
        rat = case fracWithDot of
          [] ->
            readInt intPart % 1
          ('.' : fracPart) ->
            if '.' `elem` fracPart
              then
                error $ "Parser invariant violated: Multiple decimals in " <> toText str
              else
                let val = readInt (intPart <> fracPart)
                    den = 10 ^ length fracPart
                 in val % den
          _ -> error "Unreachable state in parseLiteral"
     in if isNeg then negate rat else rat

-- todo: maybe just define this as a PrettyPrintable instance? TBH I don't really like the current structure of PrettyPrint as a module
formatTN :: TowerNumber -> Text
formatTN = \case
  R r -> formatR r
  D d -> formatD d
  CR cr -> formatCR cr
  CD cd -> formatCD cd
  N -> "NaN"

formatD :: Double -> Text
formatD d
  | isNaN d = "NaN"
  | isInfinite d = if d > 0 then "∞" else "-∞"
  | otherwise = show d

formatR :: Rational -> Text
formatR r
  | denominator r == 1 = show (numerator r)
  | otherwise = show (numerator r) <> "/" <> show (denominator r)

formatCR :: Complex Rational -> Text
formatCR (rx :+ ry) =
  let denX = denominator rx
      denY = denominator ry
      c = lcm denX denY

      a = numerator rx * (c `div` denX)
      b = numerator ry * (c `div` denY)

      (sign, b') = if b < 0 then (" :- ", abs b) else (" :+ ", b)
      numBlock = show a <> sign <> show b' <> "i"
   in if c == 1
        then numBlock
        else numBlock <> "/" <> show c

formatCD :: Complex Double -> Text
formatCD (dx :+ dy)
  | dy == 0 = formatD dx
  | dy < 0 = "(" <> formatD dx <> ":-" <> formatD (abs dy) <> "i)"
  | otherwise = "(" <> formatD dx <> " :+ " <> formatD dy <> "i)"
