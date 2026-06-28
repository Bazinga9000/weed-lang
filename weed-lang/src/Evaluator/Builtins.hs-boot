module Evaluator.Builtins (fetchBuiltin) where
import AST
import Evaluator.Types
import TypeChecker.Types

fetchBuiltin :: WeedType -> Builtin -> Value
