{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Tests.Evaluator
import Tests.Lexer
import Tests.SurfaceParser
import Tests.TypeChecker
import Tests.Lowerer

main :: IO ()
main = defaultMain $ testGroup "All Tests" [typeCheckerTests, evaluatorTests, lexerTests, surfaceParserTests, lowererTests]
