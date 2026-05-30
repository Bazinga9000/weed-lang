module Tests.TypeChecker (typeCheckerTests) where

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
getType (CTLet t _ _ _) = t
getType (CTMapPool t _ _) = t
getType (CTMap t _ _) = t
getType (CTAp t _ _) = t
getType (CTBind t _ _) = t
getType (CTReturn t _) = t

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

mkIf :: CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr
mkIf cond t f = CUApply (CUApply (CUApply (CUIdentifier (B If)) cond) t) f

coin :: CoreUntypedExpr
coin = CUIdentifier (B DiceCoin)

pool4coin :: CoreUntypedExpr
pool4coin = CUApply (CUApply (CUIdentifier (B Poolify)) (CUNumber 4)) coin

idX :: IdentifierName
idX = S "x"

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
        "If Expressions"
        [ testCase "if True then 1 else 2 -> Number (If - Basic)" $
            assertType (mkIf (CUBool True) (CUNumber 1) (CUNumber 2)) TNumber,
          testCase "if coin then 1 else 2 -> Dice Number (If - Double Scalar Promotion - Dice)" $
            assertType (mkIf coin (CUNumber 1) (CUNumber 2)) (mkDice TNumber),
          testCase "if 4coin then 1 else 2 -> Pool Number (If - Double Scalar Promotion - Pool)" $
            assertType (mkIf pool4coin (CUNumber 1) (CUNumber 2)) (mkPool TNumber),
          testCase "if coin then d6 else 0 -> Dice Number (If - One-Sided Scalar Promotion - Dice)" $
            assertType (mkIf coin d6 (CUNumber 0)) (mkDice TNumber),
          testCase "if coin then 4d6 else 5 -> Dice Number (If - Simultaneous Promotion / Collapse)" $
            assertType (mkIf coin pool4d6 (CUNumber 5)) (mkDice TNumber)
        ],
      testGroup
        "Explicit Monadic Operations (fmap, ap, bind)"
        [ testCase "map (+5) [1, 2, 3] -> List Number" $
            assertType
              (CUMap (CUApply add (CUNumber 5)) (CUList [CUNumber 1, CUNumber 2, CUNumber 3]))
              (mkList TNumber),
          testCase "map (+5) d6 -> Dice Number" $
            assertType
              (CUMap (CUApply add (CUNumber 5)) d6)
              (mkDice TNumber),
          testCase "map (+5) 4d6 -> Pool Number" $
            assertType
              (CUMap (CUApply add (CUNumber 5)) pool4d6)
              (mkPool TNumber),
          testCase "ap (map (+) d6) d10 -> Dice Number" $
            assertType
              (CUAp (CUMap add d6) d10)
              (mkDice TNumber),
          testCase "bind d6 (const d10) -> Dice Number" $
            assertType
              (CUBind d6 (CULambda idX d10))
              (mkDice TNumber),
          testCase "map (+5) 10 -> Type Error" $
            assertTypeError
              (CUMap (CUApply add (CUNumber 5)) (CUNumber 10)),
          testCase "d6 >>= (const 5) -> Type Error" $
            assertTypeError
              (CUBind d6 (CULambda idX (CUNumber 5))),
          testCase
            "bind (return 5) (\\x -> d6) -> Dice Number"
            $ assertType
              (CUBind (CUReturn (CUNumber 5)) (CULambda idX d6))
              (mkDice TNumber),
          testCase "bind (return 5) (\\x -> 4d6) -> Pool Number" $
            assertType
              (CUBind (CUReturn (CUNumber 5)) (CULambda idX pool4d6))
              (mkPool TNumber),
          testCase "ap (return (+5)) d10 -> Dice Number" $
            assertType
              (CUAp (CUReturn (CUApply add (CUNumber 5))) d10)
              (mkDice TNumber),
          testCase "[return 5, d6] -> List (Dice Number)" $
            assertType
              (CUList [CUReturn (CUNumber 5), d6])
              (mkList (mkDice TNumber)),
          testCase "return 5 -> Type Error (Contextless return)" $
            assertTypeError
              (CUReturn (CUNumber 5))
        ],
      testGroup
        "Type Check Failures"
        [ testCase "d6 + True -> Type Error" $
            assertTypeError (CUApply (CUApply add d6) (CUBool True)),
          testCase "if 1 then 2 else 3 -> Type Error" $
            assertTypeError (mkIf (CUNumber 1) (CUNumber 2) (CUNumber 3))
        ]
    ]
