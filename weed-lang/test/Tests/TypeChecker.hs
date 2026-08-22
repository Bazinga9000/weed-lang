{-# OPTIONS_GHC -Wno-orphans #-}
module Tests.TypeChecker (typeCheckerTests) where

import AST
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tests.Common
import TypeChecker
import TypeChecker.Types
import TypeChecker.Singletons (SomeSWeedType(..), toSingleton, fromSingleton)
import Prelude hiding (Ap, Identity, Sum)

-- typeChecker internal tests

prop_singleton_roundtrip :: WeedType -> Property
prop_singleton_roundtrip t =
  case toSingleton t of
    Nothing -> counterexample ("toSingleton failed on " <> show t) False
    Just (SomeSWeedType s) -> fromSingleton s === t

-- generate arbitrary ground WeedTypes (no TVar, no TApp) for the roundtrip
instance Arbitrary WeedType where
  arbitrary = sized genType
    where
      genType 0 = elements [TNumber, TBool, TUnit]
      genType n = oneof
        [ genType 0
        , do
            a <- genType (n `div` 2)
            b <- genType (n `div` 2)
            elements
              [ TFunction a b
              , TApp TList a
              , TApp TDice a
              , TApp TPool a
              ]
        ]

typeCheckerTests :: TestTree
typeCheckerTests =
  testGroup
    "Type Checker Tests"
    [ testGroup
        "Basic Types & Collections"
        [ testCase "Number literal types as TNumber" $
            assertType (CUNumber 5) TNumber,
          testCase "Bool literal types as TBool" $
            assertType (CUBool True) TBool,
          testCase "List inference correctly extracts list type ([1, 2, 3])" $
            assertType (CUList [CUNumber 1, CUNumber 2, CUNumber 3]) (TApp TList TNumber),
          testCase "List inference fails on heterogeneous lists" $
            assertTypeError (CUList [CUNumber 1, CUBool True])
        ],
      testGroup
        "Primitive Dice & Pools"
        [ testCase "d6 -> Dice Number" $
            assertType d6 (TApp TDice TNumber),
          testCase "4d6 (Poolify replication) -> Pool Number" $
            assertType pool4d6 (TApp TPool TNumber)
        ],
      testGroup
        "Implicit Coercions (Binary Operations)"
        [ testCase "d6 + d10 -> Dice Number (Implicit Applicative <*>)" $
            assertType (CUApply (CUApply add d6) d10) (TApp TDice TNumber),
          testCase "d6 + 5 -> Dice Number (Implicit Fmap & Scalar Promotion)" $
            assertType (CUApply (CUApply add d6) (CUNumber 5)) (TApp TDice TNumber),
          testCase "4d6 + 5 -> Dice Number (Pool Collapse & Scalar Promotion)" $
            assertType (CUApply (CUApply add pool4d6) (CUNumber 5)) (TApp TDice TNumber),
          testCase "4d6 + d10 -> Dice Number (Pool Collapse & Implicit Applicative)" $
            assertType (CUApply (CUApply add pool4d6) d10) (TApp TDice TNumber),
          testCase "4d6 + 4d6 -> Dice Number (Pool Collapse & Implicit Applicative)" $
            assertType (CUApply (CUApply add pool4d6) pool4d6) (TApp TDice TNumber)
        ],
      testGroup
        "Pool Operations"
        [ testCase "sum 4d6 -> Dice Number (Pool Mapping / Collapse)" $
            assertType (CUApply (CUIdentifier (B Sum)) pool4d6) (TApp TDice TNumber)
        ],
      testGroup
        "If Expressions"
        [ testCase "if True then 1 else 2 -> Number (If - Basic)" $
            assertType (CUIf (CUBool True) (CUNumber 1) (CUNumber 2)) TNumber,
          testCase "if coin then 1 else 2 -> Dice Number (If - Double Scalar Promotion - Dice)" $
            assertType (CUIf coin (CUNumber 1) (CUNumber 2)) (TApp TDice TNumber),
          testCase "if 4coin then 1 else 2 -> Pool Number (If - Double Scalar Promotion - Pool)" $
            assertType (CUIf pool4coin (CUNumber 1) (CUNumber 2)) (TApp TPool TNumber),
          testCase "if coin then d6 else 0 -> Dice Number (If - One-Sided Scalar Promotion - Dice)" $
            assertType (CUIf coin d6 (CUNumber 0)) (TApp TDice TNumber),
          testCase "if coin then 4d6 else 5 -> Dice Number (If - Simultaneous Promotion / Collapse)" $
            assertType (CUIf coin pool4d6 (CUNumber 5)) (TApp TDice TNumber)
        ],
      testGroup
        "Explicit Monadic Operations (fmap, ap, bind)"
        [ testCase "map (+5) [1, 2, 3] -> List Number" $
            assertType
              (cuMap (CUApply add (CUNumber 5)) (CUList [CUNumber 1, CUNumber 2, CUNumber 3]))
              (TApp TList TNumber),
          testCase "map (+5) d6 -> Dice Number" $
            assertType
              (cuMap (CUApply add (CUNumber 5)) d6)
              (TApp TDice TNumber),
          testCase "map (+5) 4d6 -> Pool Number" $
            assertType
              (cuMap (CUApply add (CUNumber 5)) pool4d6)
              (TApp TPool TNumber),
          testCase "ap (map (+) d6) d10 -> Dice Number" $
            assertType
              (cuAp (cuMap add d6) d10)
              (TApp TDice TNumber),
          testCase "bind d6 (const d10) -> Dice Number" $
            assertType
              (cuBind d6 (CULambda idX d10))
              (TApp TDice TNumber),
          testCase "map (+5) 10 -> Type Error" $
            assertTypeError
              (cuMap (CUApply add (CUNumber 5)) (CUNumber 10)),
          testCase "d6 >>= (const 5) -> Type Error" $
            assertTypeError
              (cuBind d6 (CULambda idX (CUNumber 5))),
          testCase
            "bind (return 5) (\\x -> d6) -> Dice Number"
            $ assertType
              (cuBind (cuReturn (CUNumber 5)) (CULambda idX d6))
              (TApp TDice TNumber),
          testCase "bind (return 5) (\\x -> 4d6) -> Pool Number" $
            assertType
              (cuBind (cuReturn (CUNumber 5)) (CULambda idX pool4d6))
              (TApp TPool TNumber),
          testCase "ap (return (+5)) d10 -> Dice Number" $
            assertType
              (cuAp (cuReturn (CUApply add (CUNumber 5))) d10)
              (TApp TDice TNumber),
          testCase "[return 5, d6] -> List (Dice Number)" $
            assertType
              (CUList [cuReturn (CUNumber 5), d6])
              (TApp TList (TApp TDice TNumber)),
          testCase "return 5 -> Type Error (Contextless return)" $
            assertTypeError
              (cuReturn (CUNumber 5))
        ],
      testGroup
        "Mutual Recursion"
        [ testCase "even and odd infer to (Number -> Bool) and evaluate to Bool" $ do
            typeCheck evenOddCU @?= Right evenOddCT
        ],
      testGroup
        "Type Check Failures"
        [ testCase "d6 + True -> Type Error" $
            assertTypeError (CUApply (CUApply add d6) (CUBool True)),
          testCase "if 1 then 2 else 3 -> Type Error" $
            assertTypeError (CUIf (CUNumber 1) (CUNumber 2) (CUNumber 3))
        ],
      testGroup
        "Internal Tests"
        [ testProperty "toSingleton (fromSingleton s) == Just s" prop_singleton_roundtrip
        ]
    ]
