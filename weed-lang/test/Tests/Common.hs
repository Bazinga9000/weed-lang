module Tests.Common where

import AST
import Evaluator
import Evaluator.DropList (getKept, toDropList)
import Evaluator.WeedNumber (literal, (=~=))
import Formatting.Pretty (prettyPrint)
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecker
import TypeChecker.Singletons (SWeedType (..))
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

-- | Compare two values for observational equality.
eqObservable :: SomeValue -> SomeValue -> Bool
eqObservable (SomeValue _ (VNumber a)) (SomeValue _ (VNumber b)) = a =~= b
eqObservable (SomeValue _ (VBool a)) (SomeValue _ (VBool b)) = a == b
eqObservable (SomeValue _ VUnit) (SomeValue _ VUnit) = True
eqObservable (SomeValue (STList sa) (VList as)) (SomeValue (STList sb) (VList bs)) = length as' == length bs' && and (zipWith eqItem as' bs') where
  as' = getKept as
  bs' = getKept bs
  eqItem a b = eqObservable (SomeValue sa a) (SomeValue sb b)
eqObservable _ _ = False

assertEval :: CoreTypedExpr -> SomeValue -> Assertion
assertEval expr expected = case evalPreSample expr of
  Right (actual, _) ->
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

-- used to build well-formed branches for effectful CTIf tests.
cReturn :: WeedType -> CoreTypedExpr -> CoreTypedExpr
cReturn containerT e =
  CTApply containerT (CTIdentifier (getType e ->> containerT) (B Return)) e

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
tListNum = TApp TList TNumber

tFuncNumList :: WeedType
tFuncNumList = TFunction TNumber tListNum

tDiceNum :: WeedType
tDiceNum = TApp TDice tNum

tPoolNum :: WeedType
tPoolNum = TApp TPool tNum

idX :: IdentifierName
idX = S "x"

mkHiLoTest :: Builtin -> String -> [Integer] -> [Bool] -> Integer -> TestTree
mkHiLoTest hiLo name input output n = testCase name $ do
  let listArgs = CTList tListNum (map cNum input)
  let expected = vList STBool (map VBool output)
  let predicateT = tListNum ->> TApp TList TBool
  let builtinT = TNumber ->> predicateT
  let expr = CTApply (TApp TList TBool) (CTApply predicateT (CTIdentifier builtinT (B hiLo)) (cNum n)) listArgs
  assertEval expr expected

assertType :: CoreUntypedExpr -> WeedType -> Assertion
assertType expr expectedType = case typeCheck expr of
  Left err -> (assertFailure . toString) $ "Type checking failed with error: " <> prettyPrint err
  Right typedExpr -> getType typedExpr @?= expectedType

assertTypeError :: CoreUntypedExpr -> Assertion
assertTypeError expr = case typeCheck expr of
  Left _ -> pass
  Right typedExpr -> (assertFailure . toString) $ "Expected type error, but succeeded with type: " <> prettyPrint (getType typedExpr)

d6 :: CoreUntypedExpr
d6 = CUApply (CUIdentifier (B DiceD)) (CUNumber 6)

d10 :: CoreUntypedExpr
d10 = CUApply (CUIdentifier (B DiceD)) (CUNumber 10)

pool4d6 :: CoreUntypedExpr
pool4d6 = CUApply (CUApply (CUIdentifier (B Poolify)) (CUNumber 4)) d6

add :: CoreUntypedExpr
add = CUIdentifier (B Add)

coin :: CoreUntypedExpr
coin = CUIdentifier (B DiceCoin)

pool4coin :: CoreUntypedExpr
pool4coin = CUApply (CUApply (CUIdentifier (B Poolify)) (CUNumber 4)) coin

cuMap :: CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr
cuMap f = CUApply (CUApply (CUIdentifier (B Map)) f)

cuAp :: CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr
cuAp f = CUApply (CUApply (CUIdentifier (B Ap)) f)

cuBind :: CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr
cuBind f = CUApply (CUApply (CUIdentifier (B Bind)) f)

cuReturn :: CoreUntypedExpr -> CoreUntypedExpr
cuReturn = CUApply (CUIdentifier (B Return))

cubId :: Builtin -> CoreUntypedExpr
cubId = CUIdentifier . B

cuId :: String -> CoreUntypedExpr
cuId = CUIdentifier . S

cuUId :: Int -> CoreUntypedExpr
cuUId = CUIdentifier . U

sId :: String -> SurfaceExpr
sId = SIdentifier . S

suId :: Int -> SurfaceExpr
suId = SIdentifier . U

bId :: Builtin -> SurfaceExpr
bId = SIdentifier . B

sAdd, sMul :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr
sAdd = SInfix "+"
sMul = SInfix "*"

sLet1 :: IdentifierName -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
sLet1 ident binding = SLetRec [Decl ident binding]

sMap :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr
sMap f = SApply (SApply (SIdentifier (S "map")) f)

cId :: String -> CoreUntypedExpr
cId = CUIdentifier . S

cbId :: Builtin -> CoreUntypedExpr
cbId = CUIdentifier . B

cApp :: CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr
cApp = CUApply

evenOddSurface :: SurfaceExpr
evenOddSurface =
  SLetRec
    [ Decl (S "even") (SLambda (S "n") sEvenBody),
      Decl (S "odd") (SLambda (S "n") sOddBody)
    ]
    (SApply (sId "even") (SNumber 4))
  where
    sEvenCond = SInfix "==" (sId "n") (SNumber 0)
    sEvenBody = SIf sEvenCond (SBool True) (SApply (sId "odd") (SInfix "-" (sId "n") (SNumber 1)))
    sOddCond = SInfix "==" (sId "n") (SNumber 0)
    sOddBody = SIf sOddCond (SBool False) (SApply (sId "even") (SInfix "-" (sId "n") (SNumber 1)))

evenOddCU :: CoreUntypedExpr
evenOddCU =
  CULetRec
    [ Decl (S "even") (CULambda (S "n") cuEvenBody),
      Decl (S "odd") (CULambda (S "n") cuOddBody)
    ]
    (cApp (cId "even") (CUNumber 4))
  where
    cuEvenCond = cApp (cApp (cbId Eq) (cId "n")) (CUNumber 0)
    cuEvenBody = CUIf cuEvenCond (CUBool True) (cApp (cId "odd") (cApp (cApp (cbId Sub) (cId "n")) (CUNumber 1)))
    cuOddCond = cApp (cApp (cbId Eq) (cId "n")) (CUNumber 0)
    cuOddBody = CUIf cuOddCond (CUBool False) (cApp (cId "even") (cApp (cApp (cbId Sub) (cId "n")) (CUNumber 1)))

evenOddCT :: CoreTypedExpr
evenOddCT =
  CTLetRec
    TBool
    [ Decl (S "even") (CTLambda tNumToBool (S "n") ctEvenBody),
      Decl (S "odd") (CTLambda tNumToBool (S "n") ctOddBody)
    ]
    (CTApply TBool (CTIdentifier tNumToBool (S "even")) (CTNumber 4))
  where
    tNumToBool = TNumber ->> TBool
    tNumToNum = TNumber ->> TNumber
    ctEq = CTIdentifier (TNumber ->> TNumber ->> TBool) (B Eq)
    ctSub = CTIdentifier (TNumber ->> TNumber ->> TNumber) (B Sub)
    ctEvenCond = CTApply TBool (CTApply (TNumber ->> TBool) ctEq (CTIdentifier TNumber (S "n"))) (CTNumber 0)
    ctEvenSub = CTApply TNumber (CTApply tNumToNum ctSub (CTIdentifier TNumber (S "n"))) (CTNumber 1)
    ctEvenBody = CTIf TBool ctEvenCond (CTBool True) (CTApply TBool (CTIdentifier tNumToBool (S "odd")) ctEvenSub)
    ctOddCond = CTApply TBool (CTApply (TNumber ->> TBool) ctEq (CTIdentifier TNumber (S "n"))) (CTNumber 0)
    ctOddSub = CTApply TNumber (CTApply tNumToNum ctSub (CTIdentifier TNumber (S "n"))) (CTNumber 1)
    ctOddBody = CTIf TBool ctOddCond (CTBool False) (CTApply TBool (CTIdentifier tNumToBool (S "even")) ctOddSub)
