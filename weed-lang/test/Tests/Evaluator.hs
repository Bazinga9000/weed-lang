module Tests.Evaluator (evaluatorTests) where

import AST
import Evaluator.Types (Value (..))
import Evaluator.WeedNumber
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Common
import TypeChecker.Singletons (SWeedType (..))
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

evaluatorTests :: TestTree
evaluatorTests =
  testGroup
    "Evaluator Tests"
    [ testGroup
        "Monadic Operations (List)"
        [ testCase "return 5 -> [5] (return explicitly typed as Number -> List Number)" $ do
            let returnE = CTIdentifier (tNum ->> tListNum) (B Return)
            let expr = CTApply tListNum returnE (cNum 5)
            assertEval expr (vList STNumber [VNumber (literal 5)]),
          testCase "fmap (const 99) [1, 2] -> [99, 99]" $ do
            let listArg = CTList tListNum [cNum 1, cNum 2]
            let mapFunc = CTLambda (TFunction TNumber TNumber) idX (cNum 99)
            let mapE = CTIdentifier ((tNum ->> tNum) ->> tListNum ->> tListNum) (B Map)
            let expr = CTApply tListNum (CTApply (tListNum ->> tListNum) mapE mapFunc) listArg
            assertEval expr (vList STNumber [VNumber (literal 99), VNumber (literal 99)]),
          testCase "[const 8, const 9] <*> [1, 2] -> [8, 8, 9, 9]" $ do
            let listArgs = CTList tListNum [cNum 1, cNum 2]

            let lFuncT = TApp TList (TFunction TNumber TNumber)

            let func1 = CTLambda (TFunction TNumber TNumber) idX (cNum 8)
            let func2 = CTLambda (TFunction TNumber TNumber) idX (cNum 9)
            let listFuncs = CTList lFuncT [func1, func2]

            let apE = CTIdentifier (lFuncT ->> tListNum ->> tListNum) (B Ap)
            let expr = CTApply tListNum (CTApply (tListNum ->> tListNum) apE listFuncs) listArgs
            assertEval expr (vList STNumber [VNumber (literal 8), VNumber (literal 8), VNumber (literal 9), VNumber (literal 9)]),
          testCase "[_ + 1, _ + 2] <*> [5, 10] -> [6, 11, 7, 12]" $ do
            let listArgs = CTList tListNum [cNum 5, cNum 10]

            let lFuncT = TApp TList (TFunction TNumber TNumber)
            let ctAdd = CTIdentifier (TNumber ->> TNumber ->> TNumber) (B Add)

            let func1Body = CTApply TNumber (CTApply (TNumber ->> TNumber) ctAdd (CTIdentifier TNumber idX)) (cNum 1)
            let func1 = CTLambda (TFunction TNumber TNumber) idX func1Body

            let func2Body = CTApply TNumber (CTApply (TNumber ->> TNumber) ctAdd (CTIdentifier TNumber idX)) (cNum 2)
            let func2 = CTLambda (TFunction TNumber TNumber) idX func2Body

            let listFuncs = CTList lFuncT [func1, func2]

            let apE = CTIdentifier (lFuncT ->> tListNum ->> tListNum) (B Ap)
            let expr = CTApply tListNum (CTApply (tListNum ->> tListNum) apE listFuncs) listArgs
            assertEval expr (vList STNumber [VNumber (literal 6), VNumber (literal 11), VNumber (literal 7), VNumber (literal 12)]),
          testCase "[1, 2] >>= \\x -> [x, x] -> [1, 1, 2, 2]" $ do
            let listArgs = CTList tListNum [cNum 1, cNum 2]
            let bindFunc =
                  CTLambda
                    tFuncNumList
                    idX
                    ( CTList
                        tListNum
                        [ CTIdentifier TNumber idX,
                          CTIdentifier TNumber idX
                        ]
                    )

            let bindT = tListNum ->> (tNum ->> tListNum) ->> tListNum
            let bindE = CTIdentifier bindT (B Bind)
            let expr = CTApply tListNum (CTApply ((tNum ->> tListNum) ->> tListNum) bindE listArgs) bindFunc
            assertEval expr (vList STNumber [VNumber (literal 1), VNumber (literal 1), VNumber (literal 2), VNumber (literal 2)]),
          testCase "liftMask (_ == 1) [1, 2, 3] -> [True, False, False]" $ do
            let listArgs = CTList tListNum [cNum 1, cNum 2, cNum 3]
            let predicateT = tListNum ->> TApp TList TBool
            let liftMaskT = (TNumber ->> TBool) ->> predicateT
            let selectorFunc = CTApply (TFunction TNumber TBool) (CTIdentifier (TFunction TNumber (TFunction TNumber TBool)) (B Eq)) (cNum 1)
            let expr = CTApply (TApp TList TBool) (CTApply predicateT (CTIdentifier liftMaskT (B LiftMask)) selectorFunc) listArgs
            assertEval expr (vList STBool [VBool True, VBool False, VBool False]),
          mkHiLoTest Highest "highest is correct (unique)" [49, 16, 100, 45, 25, 60, 87, 81, 30, 34, 21, 56] [False, False, True, False, False, False, True, True, False, False, False, False] 3,
          mkHiLoTest Lowest "lowest is correct (unique)" [49, 16, 100, 45, 25, 60, 87, 81, 30, 34, 21, 56] [False, True, False, False, True, False, False, False, False, False, True, False] 3,
          mkHiLoTest Highest "highest is stable" [1, 2, 3, 5, 5, 5, 5, 4] [False, False, False, True, True, True, False, False] 3,
          mkHiLoTest Lowest "lowest is stable" [5, 4, 3, 1, 1, 1, 1, 2] [False, False, False, True, True, True, False, False] 3
        ],
      testGroup
        "Standard AST Evaluation"
        [ testCase "42 -> 42" $ do
            assertEval (cNum 42) (vNum 42),
          testCase "let x = 5 in x -> 5" $ do
            let expr = CTLet TNumber (Decl idX (cNum 5)) (CTIdentifier TNumber idX)
            assertEval expr (vNum 5),
          testCase "(\\x -> x) 10 -> 10" $ do
            let identityFunc = CTLambda (TFunction TNumber TNumber) idX (CTIdentifier TNumber idX)
            let expr = CTApply TNumber identityFunc (cNum 10)
            assertEval expr (vNum 10),
          testCase "id 10 -> 10" $ do
            let identityBuiltin = CTIdentifier (TFunction TNumber TNumber) (B Identity)
            let expr = CTApply TNumber identityBuiltin (cNum 10)
            assertEval expr (vNum 10)
        ],
      testGroup
        "Dice Evaluation"
        -- these tests have to only assert evaluation, not *correct*
        -- evaluation, since dice can't be checked for correctness
        -- (they're in the moand box)
        [ testCase "7d6 | sum evaluates" $ do
            let input =
                  cMapPool
                    tDiceNum
                    (CTIdentifier (TFunction tListNum tNum) (B Sum))
                    ( CTApply
                        tPoolNum
                        ( CTApply
                            (TFunction tDiceNum tPoolNum)
                            (CTIdentifier (TFunction tNum (TFunction tDiceNum tPoolNum)) (B Poolify))
                            (cNum 7)
                        )
                        ( CTApply
                            tDiceNum
                            (CTIdentifier (TFunction tNum tDiceNum) (B DiceD))
                            (cNum 6)
                        )
                    )
            assertNoError input
        ],
      testGroup
        "Effectful If Expressions (CTIf)"
        [ testCase "if True then 1 else 2 -> 1 (Basic Number If)" $ do
            let expr = CTIf tNum (CTBool True) (cNum 1) (cNum 2)
            assertEval expr (vNum 1),
          testCase "if coin then 1 else 2 -> Evaluates (Dice Context)" $ do
            let tDiceBool = TApp TDice TBool
            let coinE = CTIdentifier tDiceBool (B DiceCoin)
            let expr = CTIf tDiceNum coinE (cReturn tDiceNum (cNum 1)) (cReturn tDiceNum (cNum 2))
            assertNoError expr,
          testCase "if 4coin then 1 else 2 -> Evaluates (Pool Context)" $ do
            let tDiceBool = TApp TDice TBool
            let tPoolBool = TApp TPool TBool

            let poolifyT = TNumber ->> tDiceBool ->> tPoolBool
            let poolifyE = CTIdentifier poolifyT (B Poolify)
            let fourCoin =
                  CTApply
                    tPoolBool
                    (CTApply (tDiceBool ->> tPoolBool) poolifyE (cNum 4))
                    (CTIdentifier tDiceBool (B DiceCoin))

            let expr = CTIf tPoolNum fourCoin (cReturn tPoolNum (cNum 1)) (cReturn tPoolNum (cNum 2))
            assertNoError expr,
          testCase "if coin then d6 else 0 -> Evaluates (Dice Context - Simultaneous Promotion)" $ do
            let tDiceBool = TApp TDice TBool
            let coinE = CTIdentifier tDiceBool (B DiceCoin)

            let d6E =
                  CTApply
                    tDiceNum
                    (CTIdentifier (TNumber ->> tDiceNum) (B DiceD))
                    (cNum 6)

            let returnDice0 = cReturn tDiceNum (cNum 0)
            let expr = CTIf tDiceNum coinE d6E returnDice0
            assertNoError expr
        ],
      testGroup
        "Recursive Evaluation"
        [ testCase "even 4 -> True (Pure LetRec Evaluation)" $ do
            assertEval evenOddCT (vBool True)
        ]
    ]
