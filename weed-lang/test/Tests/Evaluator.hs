module Tests.Evaluator (evaluatorTests) where

import AST
import Evaluator
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.DropList
import Formatting.Pretty (prettyPrint)
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecker.Singletons (SWeedType (..))
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

eqObservable :: SomeValue -> SomeValue -> Bool
eqObservable (SomeValue _ (VNumber a)) (SomeValue _ (VNumber b)) = a =~= b
eqObservable (SomeValue _ (VBool a)) (SomeValue _ (VBool b)) = a == b
eqObservable (SomeValue _ VUnit) (SomeValue _ VUnit) = True
eqObservable (SomeValue (STList sa) (VList as)) (SomeValue (STList sb) (VList bs)) = length as' == length bs' && and (zipWith eqItem as' bs') where
  as' = getKept as
  bs' = getKept bs
  eqItem a b = eqObservable (SomeValue sa a) (SomeValue sb b)
eqObservable _ _ = False

-- eqError :: EvaluationError -> EvaluationError -> Bool
-- eqError (InterpreterBug s1) (InterpreterBug s2) = s1 == s2
-- eqError (TypeError t1 v1) (TypeError t2 v2) = t1 == t2 && v1 `eqObservable` v2
-- eqError (BadDieParameter b1 s1 v1) (BadDieParameter b2 s2 v2) = b1 == b2 && s1 == s2 && v1 `eqObservable` v2
-- eqError (DomainError b1) (DomainError b2) = b1 == b2
-- eqError _ _ = False

assertEval :: CoreTypedExpr -> SomeValue -> Assertion
assertEval expr expected = case evalPreSample expr of
  Right actual ->
    if actual `eqObservable` expected
      then pass
      else
        (assertFailure . toString) ("Expected " <> prettyPrint expected <> ", got " <> prettyPrint actual)
  Left err -> (assertFailure . toString) (prettyPrint err)

assertNoError :: CoreTypedExpr -> Assertion
assertNoError expr = case evalPreSample expr of
  Right _ -> pass
  Left err -> (assertFailure . toString) (prettyPrint err)

vNum :: Integer -> SomeValue
vNum n = SomeValue STNumber (VNumber (literal $ fromInteger n))

vBool :: Bool -> SomeValue
vBool b = SomeValue STBool (VBool b)

vList :: SWeedType a -> [Value a] -> SomeValue
vList s xs = SomeValue (STList s) (VList $ toDropList xs)

cNum :: Integer -> CoreTypedExpr
cNum n = CTNumber (fromInteger n)

-- emulate the skeleton of the AST CTMapPool
cMapPool :: WeedType -> CoreTypedExpr -> CoreTypedExpr -> CoreTypedExpr
cMapPool ty f p =
  CTApply ty pa p
  where
    pa = CTApply (tp ->> ty) (CTIdentifier (tf ->> tp ->> ty) (B MapP)) f
    tf = getType f
    tp = getType p

tNum :: WeedType
tNum = TNumber

tListNum :: WeedType
tListNum = TList TNumber

tFuncNumList :: WeedType
tFuncNumList = TFunction TNumber tListNum

tDiceNum :: WeedType
tDiceNum = TDice tNum

tPoolNum :: WeedType
tPoolNum = TPool tNum

idX :: IdentifierName
idX = S "x"

mkHiLoTest :: Builtin -> String -> [Integer] -> [Bool] -> Integer -> TestTree
mkHiLoTest hiLo name input output n = testCase name $ do
  let listArgs = CTList tListNum (map cNum input)
  let expected = vList STBool (map VBool output)
  let predicateT = tListNum ->> TList TBool
  let builtinT = TNumber ->> predicateT
  let expr = CTApply (TList TBool) (CTApply predicateT (CTIdentifier builtinT (B hiLo)) (cNum n)) listArgs
  assertEval expr expected

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

            let lFuncT = TList (TFunction TNumber TNumber)

            let func1 = CTLambda (TFunction TNumber TNumber) idX (cNum 8)
            let func2 = CTLambda (TFunction TNumber TNumber) idX (cNum 9)
            let listFuncs = CTList lFuncT [func1, func2]

            let apE = CTIdentifier (lFuncT ->> tListNum ->> tListNum) (B Ap)
            let expr = CTApply tListNum (CTApply (tListNum ->> tListNum) apE listFuncs) listArgs
            assertEval expr (vList STNumber [VNumber (literal 8), VNumber (literal 8), VNumber (literal 9), VNumber (literal 9)]),
          testCase "[_ + 1, _ + 2] <*> [5, 10] -> [6, 11, 7, 12]" $ do
            let listArgs = CTList tListNum [cNum 5, cNum 10]

            let lFuncT = TList (TFunction TNumber TNumber)
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
            let predicateT = tListNum ->> TList TBool
            let liftMaskT = (TNumber ->> TBool) ->> predicateT
            let selectorFunc = CTApply (TFunction TNumber TBool) (CTIdentifier (TFunction TNumber (TFunction TNumber TBool)) (B Eq)) (cNum 1)
            let expr = CTApply (TList TBool) (CTApply predicateT (CTIdentifier liftMaskT (B LiftMask)) selectorFunc) listArgs
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
            let tDiceBool = TDice TBool
            let coinE = CTIdentifier tDiceBool (B DiceCoin)
            let expr = CTIf tDiceNum coinE (cNum 1) (cNum 2)
            assertNoError expr,
          testCase "if 4coin then 1 else 2 -> Evaluates (Pool Context)" $ do
            let tDiceBool = TDice TBool
            let tPoolBool = TPool TBool

            let poolifyT = TNumber ->> tDiceBool ->> tPoolBool
            let poolifyE = CTIdentifier poolifyT (B Poolify)
            let fourCoin =
                  CTApply
                    tPoolBool
                    (CTApply (tDiceBool ->> tPoolBool) poolifyE (cNum 4))
                    (CTIdentifier tDiceBool (B DiceCoin))

            let expr = CTIf tPoolNum fourCoin (cNum 1) (cNum 2)
            assertNoError expr,
          testCase "if coin then d6 else 0 -> Evaluates (Dice Context - Simultaneous Promotion)" $ do
            let tDiceBool = TDice TBool
            let coinE = CTIdentifier tDiceBool (B DiceCoin)

            let d6E =
                  CTApply
                    tDiceNum
                    (CTIdentifier (TNumber ->> tDiceNum) (B DiceD))
                    (cNum 6)

            let expr = CTIf tDiceNum coinE d6E (cNum 0)
            assertNoError expr
        ],
      testGroup
        "Recursive Evaluation"
        [ testCase "even 4 -> True (Pure LetRec Evaluation)" $ do
            let tNumToBool = TNumber ->> TBool
            let tNumToNum = TNumber ->> TNumber

            let ctEq = CTIdentifier (TNumber ->> TNumber ->> TBool) (B Eq)
            let ctSub = CTIdentifier (TNumber ->> TNumber ->> TNumber) (B Sub)

            let ctEvenCond = CTApply TBool (CTApply (TNumber ->> TBool) ctEq (CTIdentifier TNumber (S "n"))) (cNum 0)
            let ctEvenSub = CTApply TNumber (CTApply tNumToNum ctSub (CTIdentifier TNumber (S "n"))) (cNum 1)
            let ctEvenBody = CTIf TBool ctEvenCond (CTBool True) (CTApply TBool (CTIdentifier tNumToBool (S "odd")) ctEvenSub)

            let ctOddCond = CTApply TBool (CTApply (TNumber ->> TBool) ctEq (CTIdentifier TNumber (S "n"))) (cNum 0)
            let ctOddSub = CTApply TNumber (CTApply tNumToNum ctSub (CTIdentifier TNumber (S "n"))) (cNum 1)
            let ctOddBody = CTIf TBool ctOddCond (CTBool False) (CTApply TBool (CTIdentifier tNumToBool (S "even")) ctOddSub)

            let expr =
                  CTLetRec
                    TBool
                    [ Decl (S "even") (CTLambda tNumToBool (S "n") ctEvenBody),
                      Decl (S "odd") (CTLambda tNumToBool (S "n") ctOddBody)
                    ]
                    (CTApply TBool (CTIdentifier tNumToBool (S "even")) (cNum 4))

            assertEval expr (vBool True)
        ]
    ]
