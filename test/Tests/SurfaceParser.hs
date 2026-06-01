{-# LANGUAGE OverloadedStrings #-}

module Tests.SurfaceParser (surfaceParserTests) where

import AST
import Parser.Lexer (Token (..))
import Parser.SurfaceParser (parseSurface)
import Test.Tasty
import Test.Tasty.HUnit

parse :: [Token] -> SurfaceExpr
parse = parseSurface

surfaceParserTests :: TestTree
surfaceParserTests =
  testGroup
    "SurfaceParser Tests"
    [ testGroup
        "Basic Literals & Identifiers"
        [ testCase "42 - Parses numbers" $
            parse [TokenNum 42.0] @?= SNumber 42.0,
          testCase "True - Parses booleans" $
            parse [TokenBool True] @?= SBool True,
          testCase "foo - Parses identifiers" $
            parse [TokenIdent "foo"] @?= SIdentifier (S "foo"),
          testCase "d - Parses builtins" $
            parse [TokenBuiltin DiceD] @?= SIdentifier (B DiceD)
        ],
      testGroup
        "Control Flow"
        [ testCase "let x = 5 in x - Parses let bindings" $
            parse [TokenLet, TokenIdent "x", TokenOp "=", TokenNum 5.0, TokenIn, TokenIdent "x"]
              @?= SLet (S "x") (SNumber 5.0) (SIdentifier (S "x")),
          testCase "if True then 1 else 0 - Parses conditional" $
            parse [TokenIf, TokenBool True, TokenThen, TokenNum 1.0, TokenElse, TokenNum 0.0]
              @?= SIf (SBool True) (SNumber 1.0) (SNumber 0.0)
        ],
      testGroup
        "Functions & Lambdas"
        [ testCase "\\x y -> x - Parses explicit lambdas" $
            parse [TokenLambda, TokenIdent "x", TokenIdent "y", TokenArrow, TokenIdent "x"]
              @?= SLambda (S "x") (SLambda (S "y") (SIdentifier (S "x")))
        ],
      testGroup
        "Operator Precedence & Fixity"
        [ testCase "1 + 2 * 3 - Multiplication binds tighter than addition" $
            parse [TokenNum 1.0, TokenOp "+", TokenNum 2.0, TokenOp "*", TokenNum 3.0]
              @?= SInfix "+" (SNumber 1.0) (SInfix "*" (SNumber 2.0) (SNumber 3.0)),
          testCase "1 + 2 == 3 - Addition binds tighter than equality" $
            parse [TokenNum 1.0, TokenOp "+", TokenNum 2.0, TokenOp "==", TokenNum 3.0]
              @?= SInfix "==" (SInfix "+" (SNumber 1.0) (SNumber 2.0)) (SNumber 3.0),
          testCase "3 :+ 4 - Complex number construction" $
            parse [TokenNum 3.0, TokenOp ":+", TokenNum 4.0]
              @?= SInfix ":+" (SNumber 3.0) (SNumber 4.0)
        ],
      testGroup
        "Holes, Pipes, and Dice"
        [ testCase "_ + 2 - Parses holes correctly" $
            parse [TokenHole, TokenOp "+", TokenNum 2.0]
              @?= SInfix "+" SHole (SNumber 2.0),
          testCase "4d6 - Parses dice correctly" $
            -- the desugaring is done in the lowering step
            parse [TokenNum 4.0, TokenBuiltin DiceD, TokenNum 6.0]
              @?= SApply (SApply (SNumber 4.0) (SIdentifier (B DiceD))) (SNumber 6.0),
          testCase "3 | (_ + 5) - Parses pipes" $
            parse [TokenNum 3.0, TokenOp "|", TokenHole, TokenOp "+", TokenNum 5.0]
              @?= SPipe (SNumber 3.0) (SInfix "+" SHole (SNumber 5.0)),
          testCase "4 # d6 - Parses pool hash operator" $
            -- # has fixity 5, Application has fixity 10
            parse [TokenNum 4.0, TokenOp "#", TokenBuiltin DiceD, TokenNum 6.0]
              @?= SInfix "#" (SNumber 4.0) (SApply (SIdentifier (B DiceD)) (SNumber 6.0))
        ],
      testGroup
        "Monadic Operations"
        [ testCase "x >>= f - Parses bind operator" $
            parse [TokenIdent "x", TokenOp ">>=", TokenIdent "f"]
              @?= SBind (SIdentifier (S "x")) (SIdentifier (S "f")),
          testCase "map f [1] - Parses explicit map word" $
            parse [TokenMap, TokenIdent "f", TokenLBracket, TokenNum 1.0, TokenRBracket]
              @?= SMap (SIdentifier (S "f")) (SList [SNumber 1.0])
        ],
      testGroup
        "Functions and Pipes"
        [ testCase "a b c - Application is left-associative" $
            parse [TokenIdent "a", TokenIdent "b", TokenIdent "c"]
              @?= SApply (SApply (SIdentifier (S "a")) (SIdentifier (S "b"))) (SIdentifier (S "c"))
        ]
    ]
