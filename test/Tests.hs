{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Tests.TypeChecker

main :: IO ()
main = defaultMain typeCheckerTests
