module Tests.Evaluator (evaluatorTests) where

import AST
import Data.List
import qualified Data.Map as Map
import Evaluator
import Evaluator.Types
import Evaluator.WeedNumber
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecker.Types

eqObservable :: Value -> Value -> Bool
eqObservable (VNumber a) (VNumber b) = a =~= b
eqObservable (VBool a) (VBool b) = a == b
eqObservable VUnit VUnit = True
eqObservable (VList as) (VList bs) = length as == length bs && and (zipWith eqObservable as bs)
eqObservable _ _ = False

eqError :: EvaluationError -> EvaluationError -> Bool
eqError (InterpreterBug s1) (InterpreterBug s2) = s1 == s2
eqError (TypeError t1 v1) (TypeError t2 v2) = t1 == t2 && v1 `eqObservable` v2
eqError (BadDieParameter b1 s1 v1) (BadDieParameter b2 s2 v2) = b1 == b2 && s1 == s2 && v1 `eqObservable` v2
eqError (DomainError b1) (DomainError b2) = b1 == b2
eqError _ _ = False

assertEval :: CoreTypedExpr -> Value -> Assertion
assertEval expr expected = case evalPreSample expr of
  Right actual ->
    if actual `eqObservable` expected
      then return ()
      else
        assertFailure ("Expected " ++ displayObservable expected ++ ", got " ++ displayObservable actual)
  Left err -> assertFailure (displayError err)

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

idX :: IdentifierName
idX = (S "x")

evaluatorTests :: TestTree
evaluatorTests =
  testGroup
    "Evaluator Tests"
    [ testGroup
        "Monadic Operations (List)"
        [ testCase "return 5 -> [5] (return explicitly typed as Number -> List Number)" $ do
            let expr = CTReturn tListNum (cNum 5)
            assertEval expr (VList [vNum 5]),
          testCase "fmap (const 99) [1, 2] -> [99, 99]" $ do
            let listArg = CTList tListNum [cNum 1, cNum 2]
            let mapFunc = CTLambda (TFunction TNumber TNumber) idX (cNum 99)
            let expr = CTMap tListNum mapFunc listArg
            assertEval expr (VList [vNum 99, vNum 99]),
          testCase "[const 8, const 9] <*> [1, 2] -> [8, 8, 9, 9]" $ do
            let listArgs = CTList tListNum [cNum 1, cNum 2]
            let func1 = CTLambda (TFunction TNumber TNumber) idX (cNum 8)
            let func2 = CTLambda (TFunction TNumber TNumber) idX (cNum 9)
            let listFuncs = CTList (TApp TList (TFunction TNumber TNumber)) [func1, func2]

            let expr = CTAp tListNum listFuncs listArgs
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

            let expr = CTBind tListNum listArgs bindFunc
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
            let identityBuiltin = (CTIdentifier (TFunction TNumber TNumber) (B Identity))
            let expr = CTApply TNumber identityBuiltin (cNum 10)
            assertEval expr (vNum 10)
        ]
    ]
