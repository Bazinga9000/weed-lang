{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Tests.Evaluator
import Tests.TypeChecker

main :: IO ()
main = defaultMain $ testGroup "All Tests" [typeCheckerTests, evaluatorTests]
