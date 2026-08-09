module Evaluator.Types where

import AST
import Data.Map qualified as Map
import Evaluator.WeedNumber (WeedNumber)
import Evaluator.DropList (DropList)
import Test.QuickCheck.Gen
import TypeChecker.Types
import TypeChecker.Singletons

type Env = Map.Map IdentifierName SomeValue

extend :: Env -> (IdentifierName, SomeValue) -> Env
extend env (name, value) = Map.insert name value env

lookupIdent :: Env -> IdentifierName -> Maybe SomeValue
lookupIdent env name = Map.lookup name env

data EvaluationError
  = DivisionByZero
  | DomainError Builtin
  | BadDieParameter Builtin Text SomeValue
  | InfiniteRecursiveBinding
  | InterpreterBug Text

type Roll a = ExceptT EvaluationError Gen a

liftGen :: Gen a -> Roll a
liftGen = ExceptT . fmap Right

roll :: Roll a -> IO (Either EvaluationError a)
roll = generate . runExceptT

-- | The builtin dispatcher, provided by the caller (Evaluator) to avoid a
-- module cycle
newtype FetchBuiltin = FetchBuiltin { runFetchBuiltin :: forall t. SWeedType t -> Builtin -> Eval (Value t) }

-- | The evaluator's read-only environment: the variable environment plus the
-- builtin dispatcher (provided by the caller to avoid a module cycle).
data EvalEnv = EvalEnv
  { envVars :: Env
  , envFetchBuiltin :: FetchBuiltin
  }

type Eval a = ExceptT EvaluationError (Reader EvalEnv) a

askFetchBuiltin :: Eval FetchBuiltin
askFetchBuiltin = asks envFetchBuiltin

askVars :: Eval Env
askVars = asks envVars

-- | A function between typed values, carrying the singletons of its domain
-- and codomain. Unlike a raw 'SomeValue -> Eval SomeValue', this preserves
-- the WEED type index, so 'applyValue' is total and GHC checks that builtin
-- implementations match their claimed types. The singletons let builtins
-- construct correctly-indexed results (e.g. 'Identity' returning its input).
data TypedFun a b = TypedFun
  { tfDom :: SWeedType a
  , tfCod :: SWeedType b
  , runTypedFun :: Value a -> Eval (Value b)
  }

-- | Build a curried builtin from a Haskell function, given the function's
-- full singleton. This lets GHC check curried builtins against their type.
curryBuiltin :: SWeedType (TFunction a b) -> (Value a -> Eval (Value b)) -> Value (TFunction a b)
curryBuiltin (STFunction sa sb) f = VBuiltin $ TypedFun sa sb f

data Value :: WeedType -> Type where
  VNumber :: WeedNumber -> Value TNumber
  VBool :: Bool -> Value TBool
  VUnit :: Value TUnit
  VList :: DropList (Value a) -> Value (TList a)
  VClosure :: Env -> IdentifierName -> CoreTypedExpr -> Value (TFunction a b)
  VBuiltin :: TypedFun a b -> Value (TFunction a b)
  VDice :: Roll (Value a) -> Value (TDice a)
  VPool :: Roll (DropList (Value a)) -> Roll (Value a) -> Value (TPool a)

data SomeValue = forall t. SomeValue (SWeedType t) (Value t)
