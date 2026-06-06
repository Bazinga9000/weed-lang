module Tests.Evaluator (evaluatorTests) where

import AST
import Evaluator
import Evaluator.Types
import Evaluator.WeedNumber
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

eqObservable :: Value -> Value -> Bool
eqObservable (VNumber a) (VNumber b) = a =~= b
eqObservable (VBool a) (VBool b) = a == b
eqObservable VUnit VUnit = True
eqObservable (VList as) (VList bs) = length as == length bs && and (zipWith eqObservable as bs)
eqObservable _ _ = False

-- eqError :: EvaluationError -> EvaluationError -> Bool
-- eqError (InterpreterBug s1) (InterpreterBug s2) = s1 == s2
-- eqError (TypeError t1 v1) (TypeError t2 v2) = t1 == t2 && v1 `eqObservable` v2
-- eqError (BadDieParameter b1 s1 v1) (BadDieParameter b2 s2 v2) = b1 == b2 && s1 == s2 && v1 `eqObservable` v2
-- eqError (DomainError b1) (DomainError b2) = b1 == b2
-- eqError _ _ = False

assertEval :: CoreTypedExpr -> Value -> Assertion
assertEval expr expected = case evalPreSample expr of
  Right actual ->
    if actual `eqObservable` expected
      then return ()
      else
        (assertFailure . toString) ("Expected " <> displayObservable expected <> ", got " <> displayObservable actual)
  Left err -> (assertFailure . toString) (displayError err)

assertNoError :: CoreTypedExpr -> Assertion
assertNoError expr = case evalPreSample expr of
  Right _ -> return ()
  Left err -> (assertFailure . toString) (displayError err)

vNum :: Integer -> Value
vNum n = VNumber (literal $ fromInteger n)

cNum :: Integer -> CoreTypedExpr
cNum n = CTNumber (fromInteger n)

tNum :: WeedType
tNum = TNumber

tListNum :: WeedType
tListNum = TApp TList TNumber

tFuncNumList :: WeedType
tFuncNumList = TFunction TNumber tListNum

tDiceNum :: WeedType
tDiceNum = TApp TDice tNum

tPoolNum :: WeedType
tPoolNum = TApp TPool tNum

idX :: IdentifierName
idX = S "x"

evaluatorTests :: TestTree
evaluatorTests =
  testGroup
    "Evaluator Tests"
    [ testGroup
        "Monadic Operations (List)"
        [ testCase "return 5 -> [5] (return explicitly typed as Number -> List Number)" $ do
            let returnE = CTIdentifier (tNum ->> tListNum) (B Return)
            let expr = CTApply tListNum returnE (cNum 5)
            assertEval expr (VList [vNum 5]),
          testCase "fmap (const 99) [1, 2] -> [99, 99]" $ do
            let listArg = CTList tListNum [cNum 1, cNum 2]
            let mapFunc = CTLambda (TFunction TNumber TNumber) idX (cNum 99)
            let mapE = CTIdentifier ((tNum ->> tNum) ->> tListNum ->> tListNum) (B Map)
            let expr = CTApply tListNum (CTApply (tListNum ->> tListNum) mapE mapFunc) listArg
            assertEval expr (VList [vNum 99, vNum 99]),
          testCase "[const 8, const 9] <*> [1, 2] -> [8, 8, 9, 9]" $ do
            let listArgs = CTList tListNum [cNum 1, cNum 2]

            let lFuncT = TApp TList (TFunction TNumber TNumber)

            let func1 = CTLambda (TFunction TNumber TNumber) idX (cNum 8)
            let func2 = CTLambda (TFunction TNumber TNumber) idX (cNum 9)
            let listFuncs = CTList lFuncT [func1, func2]

            let apE = CTIdentifier (lFuncT ->> tListNum ->> tListNum) (B Ap)
            let expr = CTApply tListNum (CTApply (tListNum ->> tListNum) apE listFuncs) listArgs
            assertEval expr (VList [vNum 8, vNum 8, vNum 9, vNum 9]),
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
            let expr = CTApply tListNum (CTApply bindT bindE listArgs) bindFunc
            assertEval expr (VList [vNum 1, vNum 1, vNum 2, vNum 2])
        ],
      testGroup
        "Standard AST Evaluation"
        [ testCase "42 -> 42" $ do
            assertEval (cNum 42) (vNum 42),
          testCase "let x = 5 in x -> 5" $ do
            let expr = CTLet TNumber idX (cNum 5) (CTIdentifier TNumber idX)
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
                  CTMapPool
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
        ]
    ]
