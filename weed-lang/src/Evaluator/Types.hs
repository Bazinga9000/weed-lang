module Evaluator.Types where

import AST
import Control.Monad.Writer.CPS (Writer)
import Control.Monad.Writer.Lazy (WriterT (..), runWriterT)
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

-- a single step in the evaluation trace, used to reconstruct a readable log.
-- values are stored as already-pretty-printed text.
data TraceEvent
  = Rolled Text -- a single die rolled this value
  | Pooled [(Text, Bool)] (Maybe Text) -- a pool's final contents: (value, kept?) and its total (if summable)
  | Lifted Text -- a scalar lifted into a dice/pool context
  | Applied Builtin [Text] Text -- builtin, operand values, result
  deriving (Show, Eq)

type Roll a = ExceptT EvaluationError (WriterT [TraceEvent] Gen) a

type Eval a = ExceptT EvaluationError (ReaderT EvalEnv (Writer [TraceEvent])) a

liftGen :: Gen a -> Roll a
liftGen g = ExceptT $ WriterT $ fmap (\a -> (Right a, [])) g

roll :: Roll a -> IO (Either EvaluationError (a, [TraceEvent]))
roll r = generate $ do
  (ea, evts) <- runWriterT (runExceptT r)
  pure $ fmap (\a -> (a, evts)) ea

-- builtin dispatcher
-- caller provides this to avoid module cycle
newtype FetchBuiltin = FetchBuiltin { runFetchBuiltin :: forall t. SWeedType t -> Builtin -> Eval (Value t) }

data EvalEnv = EvalEnv
  { envVars :: Env
  , envFetchBuiltin :: FetchBuiltin
  }

askFetchBuiltin :: Eval FetchBuiltin
askFetchBuiltin = asks envFetchBuiltin

askVars :: Eval Env
askVars = asks envVars

-- function between two typed values carrying the singletons of its domain and codomain
data TypedFun a b = TypedFun
  { tfDom :: SWeedType a
  , tfCod :: SWeedType b
  , runTypedFun :: Value a -> Eval (Value b)
  }

-- build a curried builtin from a Haskell function
curryBuiltin :: SWeedType (TFunction a b) -> (Value a -> Eval (Value b)) -> Value (TFunction a b)
curryBuiltin (STFunction sa sb) f = VBuiltin $ TypedFun sa sb f

data Value :: WeedType -> Type where
  VNumber :: WeedNumber -> Value TNumber
  VBool :: Bool -> Value TBool
  VUnit :: Value TUnit
  VList :: DropList (Value a) -> Value (TApp TList a)
  VClosure :: Env -> SWeedType a -> IdentifierName -> CoreElaboratedExpr b -> Value (TFunction a b)
  VBuiltin :: TypedFun a b -> Value (TFunction a b)
  VDice :: Roll (Value a) -> Value (TApp TDice a)
  VPool :: Roll (DropList (Value a)) -> Roll (Value a) -> Value (TApp TPool a)

data SomeValue = forall t. SomeValue (SWeedType t) (Value t)
