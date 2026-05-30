{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecker
import TypeChecker.Types

getType :: CoreTypedExpr -> WeedType
getType (CTNumber _) = TNumber
getType (CTBool _) = TBool
getType CTUnit = TUnit
getType (CTList t _) = t
getType (CTIdentifier t _) = t
getType (CTLambda t _ _) = t
getType (CTApply t _ _) = t
getType (CTIf t _ _ _) = t
getType (CTLet t _ _ _) = t
getType (CTMapPool t _ _) = t

assertType :: CoreUntypedExpr -> WeedType -> Assertion
assertType expr expectedType = case typeCheck expr of
  Left err -> assertFailure $ "Type checking failed with error: " ++ err
  Right typedExpr -> getType typedExpr @?= expectedType

assertTypeError :: CoreUntypedExpr -> Assertion
assertTypeError expr = case typeCheck expr of
  Left _ -> return ()
  Right typedExpr -> assertFailure $ "Expected type error, but succeeded with type: " ++ show (getType typedExpr)

d6 :: CoreUntypedExpr
d6 = CUApply (CUIdentifier (B DiceD)) (CUNumber 6)

d10 :: CoreUntypedExpr
d10 = CUApply (CUIdentifier (B DiceD)) (CUNumber 10)

pool4d6 :: CoreUntypedExpr
pool4d6 = CUApply (CUApply (CUIdentifier (B Poolify)) (CUNumber 4)) d6

add :: CoreUntypedExpr
add = CUIdentifier (B Add)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Type Checker Tests"
    [ testGroup
        "Basic Types & Collections"
        [ testCase "Number literal types as TNumber" $
            assertType (CUNumber 5) TNumber,
          testCase "Bool literal types as TBool" $
            assertType (CUBool True) TBool,
          testCase "List inference correctly extracts list type ([1, 2, 3])" $
            assertType (CUList [CUNumber 1, CUNumber 2, CUNumber 3]) (mkList TNumber),
          testCase "List inference fails on heterogeneous lists" $
            assertTypeError (CUList [CUNumber 1, CUBool True])
        ],
      testGroup
        "Primitive Dice & Pools"
        [ testCase "d6 -> Dice Number" $
            assertType d6 (mkDice TNumber),
          testCase "4d6 (Poolify replication) -> Pool Number" $
            assertType pool4d6 (mkPool TNumber)
        ],
      testGroup
        "Implicit Coercions (Binary Operations)"
        [ testCase "d6 + d10 -> Dice Number (Implicit Applicative <*>)" $
            assertType (CUApply (CUApply add d6) d10) (mkDice TNumber),
          testCase "d6 + 5 -> Dice Number (Implicit Fmap & Scalar Promotion)" $
            assertType (CUApply (CUApply add d6) (CUNumber 5)) (mkDice TNumber),
          testCase "4d6 + 5 -> Dice Number (Pool Collapse & Scalar Promotion)" $
            assertType (CUApply (CUApply add pool4d6) (CUNumber 5)) (mkDice TNumber),
          testCase "4d6 + d10 -> Dice Number (Pool Collapse & Implicit Applicative)" $
            assertType (CUApply (CUApply add pool4d6) d10) (mkDice TNumber)
        ],
      testGroup
        "Pool Operations"
        [ testCase "sum 4d6 -> Dice Number (Pool Mapping / Collapse)" $
            assertType (CUApply (CUIdentifier (B Sum)) pool4d6) (mkDice TNumber)
        ],
      testGroup
        "Type Check Failures"
        [ testCase "Adding Dice to Bool" $
            assertTypeError (CUApply (CUApply add d6) (CUBool True)),
          testCase "If-statement with non-boolean condition" $
            assertTypeError (CUIf (CUNumber 1) (CUNumber 2) (CUNumber 3))
        ]
    ]
