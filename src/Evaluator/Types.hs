{-# LANGUAGE LambdaCase #-}

module Evaluator.Types where

import AST
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intercalate)
import qualified Data.Map as Map
import Evaluator.WeedNumber (WeedNumber)
import Test.QuickCheck.Gen
import TypeChecker.Types

type Env = Map.Map IdentifierName Value

extend :: Env -> (IdentifierName, Value) -> Env
extend env (name, value) = Map.insert name value env

lookupIdent :: Env -> IdentifierName -> Maybe Value
lookupIdent env name = Map.lookup name env

data EvaluationError
  = DivisionByZero
  | BadComparisonType String
  | DomainError Builtin
  | TypeError WeedType Value
  | BadDieParameter Builtin String Value
  | InterpreterBug String

type Roll a = ExceptT EvaluationError (Gen) a

liftGen :: Gen a -> Roll a
liftGen = ExceptT . fmap Right

roll :: Roll a -> IO (Either EvaluationError a)
roll = generate . runExceptT

type Eval a = ExceptT EvaluationError (Reader Env) a

data Value
  = VNumber WeedNumber
  | VBool Bool
  | VUnit
  | VList [Value]
  | VClosure Env IdentifierName CoreTypedExpr
  | VBuiltin (Value -> Eval Value)
  | VDice (Roll Value)
  | VPool (Roll [Value]) (Roll Value) -- pool, source

displayObservable :: Value -> String
displayObservable (VNumber n) = show n
displayObservable (VBool b) = show b
displayObservable VUnit = "()"
displayObservable (VList xs) = "[" ++ intercalate ", " (map displayObservable xs) ++ "]"
displayObservable (VClosure _ _ _) = "<a closure>"
displayObservable (VBuiltin _) = "<a builtin>"
displayObservable (VDice _) = "<a dice>"
displayObservable (VPool _ _) = "<a pool>"

displayError :: EvaluationError -> String
displayError = \case
  DivisionByZero -> "division by zero"
  BadComparisonType t -> "bad comparison type: " ++ t
  DomainError b -> "domain error: " ++ show b ++ " expected real, got complex"
  TypeError t v -> "interpreter bug (type error): wanted " ++ show t ++ " got " ++ displayObservable v
  BadDieParameter b s v -> "bad die parameter: die " ++ show b ++ " " ++ s ++ " " ++ displayObservable v
  InterpreterBug s -> "interpreter bug: " ++ s
