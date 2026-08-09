module Evaluator
  ( module Evaluator.Core
  , fetchBuiltin
  , evalPreSample
  , evaluate
  ) where

import AST
import Evaluator.Builtins.HigherOrder (fetchBuiltinHigherOrder)
import Evaluator.Builtins.Pure (fetchBuiltinPure)
import Evaluator.Core hiding (evalPreSample, evaluate)
import Evaluator.Core qualified as Core
import Evaluator.Types
import TypeChecker.Singletons

-- dispatch a builtin to its pure or higher-order implementation.
fetchBuiltin :: SWeedType t -> Builtin -> Eval (Value t)
fetchBuiltin s b = case b of
  Negate -> fetchBuiltinPure s b
  Not -> fetchBuiltinPure s b
  Add -> fetchBuiltinPure s b
  Sub -> fetchBuiltinPure s b
  Mul -> fetchBuiltinPure s b
  Div -> fetchBuiltinPure s b
  Mod -> fetchBuiltinPure s b
  Pow -> fetchBuiltinPure s b
  Floor -> fetchBuiltinPure s b
  Ceil -> fetchBuiltinPure s b
  ComplexAdd -> fetchBuiltinPure s b
  ComplexSub -> fetchBuiltinPure s b
  And -> fetchBuiltinPure s b
  Or -> fetchBuiltinPure s b
  Xor -> fetchBuiltinPure s b
  Approximate -> fetchBuiltinPure s b
  _ -> fetchBuiltinHigherOrder s b

evalPreSample :: CoreTypedExpr -> Either EvaluationError SomeValue
evalPreSample = Core.evalPreSample (FetchBuiltin fetchBuiltin)

evaluate :: CoreTypedExpr -> IO (Either EvaluationError SomeValue)
evaluate = Core.evaluate (FetchBuiltin fetchBuiltin)
