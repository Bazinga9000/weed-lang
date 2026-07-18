module Evaluator.Types where

import AST
import Data.Map qualified as Map
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
  | DomainError Builtin
  | TypeError WeedType Value
  | BadDieParameter Builtin Text Value
  | InfiniteRecursiveBinding
  | InterpreterBug Text

type Roll a = ExceptT EvaluationError Gen a

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
