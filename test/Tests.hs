module Main where

import Test.Tasty
import Tests.Evaluator
import Tests.Lexer
import Tests.Lowerer
import Tests.SurfaceParser
import Tests.TypeChecker

main :: IO ()
main = defaultMain $ testGroup "All Tests" [typeCheckerTests, evaluatorTests, lexerTests, surfaceParserTests, lowererTests]
