{-# LANGUAGE OverloadedStrings #-}

module Tests.Lexer (lexerTests) where

import AST (Builtin (..))
import Parser.Lexer (Token (..), scanTokens)
import Test.Tasty
import Test.Tasty.HUnit

lexerTests :: TestTree
lexerTests =
  testGroup
    "Lexer Test"
    [ testGroup
        "Primitives and Literals"
        [ testCase "Lexes positive integers and floats" $ do
            scanTokens "42 3.14" @?= [TokenNum 42.0, TokenNum 3.14],
          testCase "Lexes negatives with unary minus" $ do
            scanTokens "-42" @?= [TokenOp "-", TokenNum 42.0]
            scanTokens "-42 -3.14" @?= [TokenOp "-", TokenNum 42.0, TokenOp "-", TokenNum 3.14],
          testCase "Lexes booleans" $ do
            scanTokens "True False" @?= [TokenBool True, TokenBool False],
          testCase "Lexes complex number syntax" $ do
            scanTokens "3 :+ 4" @?= [TokenNum 3.0, TokenOp ":+", TokenNum 4.0]
            scanTokens "5 :- 2" @?= [TokenNum 5.0, TokenOp ":-", TokenNum 2.0]
        ],
      testGroup
        "Identifiers and Builtins"
        [ -- testCase "Lexes standard builtins" $ do
          --     scanTokens "keep drop highest sum"
          --       @?= [ TokenBuiltin Keep,
          --             TokenBuiltin Drop,
          --             TokenBuiltin Highest,
          --             TokenBuiltin Sum
          --           ],
          testCase "Lexes standard identifiers" $ do
            scanTokens "myVar custom_dice" @?= [TokenIdent "myVar", TokenIdent "custom_dice"]
        ],
      testGroup
        "Monadic Operations"
        [ testCase "Lexes standard monadic functions" $ do
            scanTokens "map ap return bind"
              @?= [TokenIdent "map", TokenIdent "ap", TokenIdent "return", TokenIdent "bind"],
          testCase "Lexes symbolic monadic aliases" $ do
            scanTokens "<$> <*> >>="
              @?= [TokenOp "<$>", TokenOp "<*>", TokenOp ">>="]
        ],
      testGroup
        "Holes and Lambdas"
        [ testCase "Lexes syntactic holes" $ do
            scanTokens "_" @?= [TokenHole]
            scanTokens "_ + _" @?= [TokenHole, TokenOp "+", TokenHole],
          testCase "Lexes explicit lambdas" $ do
            scanTokens "\\x -> x + 1"
              @?= [TokenLambda, TokenIdent "x", TokenArrow, TokenIdent "x", TokenOp "+", TokenNum 1.0]
        ],
      testGroup
        "Dice Syntax Splitting"
        [ testCase "Lexes builtin dice" $ do
            scanTokens "d s f u gauss pareto binomial coin circle"
              @?= [TokenBuiltin DiceD, TokenBuiltin DiceS, TokenBuiltin DiceF, TokenBuiltin DiceU, TokenBuiltin DiceGauss, TokenBuiltin DicePareto, TokenBuiltin DiceBinomial, TokenBuiltin DiceCoin, TokenBuiltin DiceCircle],
          testCase "Lexes standard dice space-separated" $ do
            scanTokens "d 6" @?= [TokenBuiltin DiceD, TokenNum 6.0],
          testCase "Splits concatenated dice (d6)" $ do
            scanTokens "d6" @?= [TokenBuiltin DiceD, TokenNum 6.0]
            scanTokens "d20" @?= [TokenBuiltin DiceD, TokenNum 20.0],
          testCase "Lexes d% alias" $ do
            scanTokens "d%" @?= [TokenBuiltin DiceD, TokenNum 100.0],
          testCase "Lexes dF alias" $ do
            scanTokens "dF" @?= [TokenBuiltin DiceF, TokenNum 1.0],
          testCase "Splits concatenated list dice (s)" $ do
            scanTokens "3s" @?= [TokenNum 3.0, TokenBuiltin DiceS],
          testCase "Splits concatenated dice pools (4d6)" $ do
            scanTokens "4d6"
              @?= [TokenNum 4.0, TokenBuiltin DiceD, TokenNum 6.0],
          testCase "Lexes space-separated pools cleanly (4 d6)" $ do
            scanTokens "4 d6"
              @?= [TokenNum 4.0, TokenBuiltin DiceD, TokenNum 6.0],
          testCase "Lexes other primitive dice" $ do
            scanTokens "gauss 5" @?= [TokenBuiltin DiceGauss, TokenNum 5.0]
            scanTokens "binomial 10 0.5" @?= [TokenBuiltin DiceBinomial, TokenNum 10.0, TokenNum 0.5]
            scanTokens "coin" @?= [TokenBuiltin DiceCoin]
            scanTokens "4coin" @?= [TokenNum 4.0, TokenBuiltin DiceCoin]
            scanTokens "4u6" @?= [TokenNum 4.0, TokenBuiltin DiceU, TokenNum 6.0]
            scanTokens "18f3" @?= [TokenNum 18.0, TokenBuiltin DiceF, TokenNum 3.0]
            scanTokens "gauss7" @?= [TokenBuiltin DiceGauss, TokenNum 7.0]
            scanTokens "pareto5" @?= [TokenBuiltin DicePareto, TokenNum 5.0]
            scanTokens "7circle1" @?= [TokenNum 7.0, TokenBuiltin DiceCircle, TokenNum 1.0]
        ],
      testGroup
        "Complex Combinations"
        [ -- testCase "Lexes a full pipe expression" $ do
          --     -- "4d6 | keep (highest _)"
          --     scanTokens "4d6 | keep (highest _)"
          --        @?= [ TokenNum 4.0,
          --              TokenBuiltin DiceD,
          --              TokenNum 6.0,
          --              TokenPipe,
          --              TokenBuiltin Keep,
          --              TokenParenL,
          --              TokenBuiltin Highest,
          --              TokenHole,
          --              TokenParenR
          --            ],
          testCase "Lexes let bindings with map" $ do
            scanTokens "let add = _ + _ in map add [1, 2]"
              @?= [ TokenLet,
                    TokenIdent "add",
                    TokenOp "=",
                    TokenHole,
                    TokenOp "+",
                    TokenHole,
                    TokenIn,
                    TokenIdent "map",
                    TokenIdent "add",
                    TokenLBracket,
                    TokenNum 1.0,
                    TokenComma,
                    TokenNum 2.0,
                    TokenRBracket
                  ]
        ]
    ]
