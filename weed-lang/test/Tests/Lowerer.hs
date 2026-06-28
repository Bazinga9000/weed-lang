module Tests.Lowerer (lowererTests) where

import AST
import Parser.Lowerer
import Test.Tasty
import Test.Tasty.HUnit

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

cId :: String -> CoreUntypedExpr
cId = CUIdentifier . S

cuId :: Int -> CoreUntypedExpr
cuId = CUIdentifier . U

cbId :: Builtin -> CoreUntypedExpr
cbId = CUIdentifier . B

cApp :: CoreUntypedExpr -> CoreUntypedExpr -> CoreUntypedExpr
cApp = CUApply

testCrackLets :: TestTree
testCrackLets =
  testGroup
    "crackLets (Dependency Sorting)"
    [ testCase "let x = 5; y = x + 2 in y -> let x = 5 in (let y = x + 2 in y)" $ do
        let input =
              SLetRec
                [ Decl (S "x") (SNumber 5),
                  Decl (S "y") (sAdd (sId "x") (SNumber 2))
                ]
                (sId "y")

        let expected =
              sLet1 (S "x") (SNumber 5) $
                sLet1 (S "y") (sAdd (sId "x") (SNumber 2)) $
                  sId "y"

        crackLets input @?= expected
    ]

testDesugarPoolify :: TestTree
testDesugarPoolify =
  let mkIn x d y = SApply (SApply (SNumber x) (SIdentifier (B d))) (SNumber y)
      mkOut x d y = SInfix "#" (SNumber x) (SApply (SIdentifier (B d)) (SNumber y))
      check x d y = desugarPoolify (mkIn x d y) @?= mkOut x d y
   in testGroup
        "desguarPoolify (XdY -> X # dY)"
        [ testCase "7d6 -> 7 # d6" $
            check 7 DiceD 6,
          testCase "7gauss4.5 -> 7 # gauss4.5" $
            check 7 DiceGauss 4.5,
          testCase "8coin -> 8 # coin" $
            let inp = SApply (SNumber 8) (SIdentifier (B DiceCoin))
                outp = SInfix "#" (SNumber 8) (SIdentifier (B DiceCoin))
             in desugarPoolify inp @?= outp
        ]

testHoleLifting :: TestTree
testHoleLifting =
  testGroup
    "liftHoles (Hole Sectioning)"
    [ testCase "(_ + 2) -> (\\u1 -> u1 + 2) - Unary holes" $
        let input = sAdd SHole (SNumber 2)
            expected = SLambda (U 1) (sAdd (suId 1) (SNumber 2))
         in liftHoles input @?= Right expected,
      testCase "(_ * _) -> (\\u1 u2 -> u1 * u2) - Binary ordered holes" $
        let input = sMul SHole SHole
            expected = SLambda (U 1) (SLambda (U 2) (sMul (suId 1) (suId 2)))
         in liftHoles input @?= Right expected,
      testCase "(map (_ * 2)) -> (map (\\u1 -> u1 * 2)) - Parentheses isolate holes" $
        let input = SApply (sId "map") (SParens (sMul SHole (SNumber 2)))
            expected = SApply (sId "map") (SParens (SLambda (U 1) (sMul (suId 1) (SNumber 2))))
         in liftHoles input @?= Right expected,
      testCase "4d6 | keep (highest _) -> 4d6 | (\\u1 -> keep (highest u1)) - Pipes isolate holes" $
        let input = SPipe (sId "4d6") (SApply (sId "keep") (SParens (SApply (sId "highest") SHole)))
            expected = SPipe (sId "4d6") (SLambda (U 1) (SApply (sId "keep") (SParens (SApply (sId "highest") (suId 1)))))
         in liftHoles input @?= Right expected,
      testCase "let foo = _ + _ in foo 5 2 -> let foo = (\\u1 u2 -> u1 + u2) in foo 5 2 - Let bindings capture immediately" $
        let input = sLet1 (S "foo") (sAdd SHole SHole) (SApply (SApply (sId "foo") (SNumber 5)) (SNumber 2))
            expected =
              sLet1
                (S "foo")
                (SLambda (U 1) (SLambda (U 2) (sAdd (suId 1) (suId 2))))
                (SApply (SApply (sId "foo") (SNumber 5)) (SNumber 2))
         in liftHoles input @?= Right expected,
      testCase "[1, _, 3] -> (\\u1 -> [1, u1, 3]) - Lists capture holes" $
        let input = SList [SNumber 1, SHole, SNumber 3]
            expected = SLambda (U 1) (SList [SNumber 1, suId 1, SNumber 3])
         in liftHoles input @?= Right expected,
      testCase "_ -> Error - Rejects bare top-level hole" $
        let input = SHole
         in isLeft (liftHoles input) @?= True,
      testCase "foo _ -> Error - Rejects unresolvable top-level hole" $
        let input = SApply (sId "foo") SHole
         in isLeft (liftHoles input) @?= True
    ]

testBuiltinResolution :: TestTree
testBuiltinResolution =
  testGroup
    "resolveBuiltins (Scope & Name Resolution)"
    [ testCase "\"map\" -> Map - Resolves standard builtins" $
        let input = sId "map"
            expected = bId Map
         in resolveBuiltins input @?= Right expected,
      testCase "\"foo\" unchanged - Ignores unknown identifiers" $
        let input = sId "foo"
            expected = sId "foo"
         in resolveBuiltins input @?= Right expected,
      testCase "let \"map\" = 1 in \"map\" unchanged - Respects shadowing in let" $
        let input = sLet1 (S "map") (SNumber 1) (sId "map")
            expected = sLet1 (S "map") (SNumber 1) (sId "map") -- Remains an S, not a B
         in resolveBuiltins input @?= Right expected,
      testCase "(\\\"add\" -> \"add\" unchanged - Respects shadowing in lambdas" $
        let input = SLambda (S "add") (sId "add")
            expected = SLambda (S "add") (sId "add") -- Remains an S, not a B
         in resolveBuiltins input @?= Right expected,
      testCase "(let \"map\" = 1 in \"map\") + \"map\" -> (let \"map\" = 1 in \"map\") + Map - Shadowing scope does not leak" $
        let input = sAdd (sLet1 (S "map") (SNumber 1) (sId "map")) (sId "map")
            expected = sAdd (sLet1 (S "map") (SNumber 1) (sId "map")) (bId Map)
         in resolveBuiltins input @?= Right expected
    ]

testOperatorDissolving :: TestTree
testOperatorDissolving =
  testGroup
    "dissolveOps (Surface -> Core)"
    [ testCase "-5 -> (negate 5) - Dissolves unary operators" $
        let input = SUnaryOp "-" (SNumber 5)
            expected = cApp (cbId Negate) (CUNumber 5)
         in dissolveOps input @?= Right expected,
      testCase "1 + 2 -> (add 1) 2 - Dissolves infix operators" $
        let input = SInfix "+" (SNumber 1) (SNumber 2)
            expected = cApp (cApp (cbId Add) (CUNumber 1)) (CUNumber 2)
         in dissolveOps input @?= Right expected,
      testCase "x | f -> f x - Dissolves pipes into applications" $
        let input = SPipe (sId "x") (sId "f")
            expected = cApp (cId "f") (cId "x")
         in dissolveOps input @?= Right expected
    ]

testEndToEndLowering :: TestTree
testEndToEndLowering =
  testGroup
    "Full Pipeline"
    [ testCase "map (_ + 1) [1] -> ((map (\\u1 -> ((Add u1) 1)) [1]) - Simple map" $
        let input = SApply (SApply (sId "map") (SParens (sAdd SHole (SNumber 1)))) (SList [SNumber 1])
            expected = cApp (cApp (cbId Map) (CULambda (U 1) (cApp (cApp (cbId Add) (cuId 1)) (CUNumber 1)))) (CUList [CUNumber 1])
         in lower input @?= Right expected,
      testCase "mutually recursive even and odd lower to correct CoreUntyped AST" $ do
        -- even = \n -> if n == 0 then True else odd (n - 1)
        let sEvenCond = SInfix "==" (sId "n") (SNumber 0)
        let sEvenBody = SIf sEvenCond (SBool True) (SApply (sId "odd") (SInfix "-" (sId "n") (SNumber 1)))

        -- odd = \n -> if n == 0 then False else even (n - 1)
        let sOddCond = SInfix "==" (sId "n") (SNumber 0)
        let sOddBody = SIf sOddCond (SBool False) (SApply (sId "even") (SInfix "-" (sId "n") (SNumber 1)))

        let input =
              SLetRec
                [ Decl (S "even") (SLambda (S "n") sEvenBody),
                  Decl (S "odd") (SLambda (S "n") sOddBody)
                ]
                (SApply (sId "even") (SNumber 4))

        let cuEvenCond = cApp (cApp (cbId Eq) (cId "n")) (CUNumber 0)
        let cuEvenBody = CUIf cuEvenCond (CUBool True) (cApp (cId "odd") (cApp (cApp (cbId Sub) (cId "n")) (CUNumber 1)))

        let cuOddCond = cApp (cApp (cbId Eq) (cId "n")) (CUNumber 0)
        let cuOddBody = CUIf cuOddCond (CUBool False) (cApp (cId "even") (cApp (cApp (cbId Sub) (cId "n")) (CUNumber 1)))

        let expected =
              CULetRec
                [ Decl (S "even") (CULambda (S "n") cuEvenBody),
                  Decl (S "odd") (CULambda (S "n") cuOddBody)
                ]
                (cApp (cId "even") (CUNumber 4))

        lower input @?= Right expected
    ]

lowererTests :: TestTree
lowererTests =
  testGroup
    "Lowerer Tests"
    [ testCrackLets,
      testDesugarPoolify,
      testHoleLifting,
      testBuiltinResolution,
      testOperatorDissolving,
      testEndToEndLowering
    ]
