module Evaluator.Assertions where

import AST
import Control.Lens hiding (Identity)
import Control.Monad.Except
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.DropList
import TowerNumber.Core
import TypeChecker.Types
import Prelude hiding (Identity)

assertBool :: (MonadError EvaluationError m) => Value -> m Bool
assertBool (VBool b) = return b
assertBool e = throwError $ TypeError TBool e

assertNumber :: (MonadError EvaluationError m) => Value -> m WeedNumber
assertNumber (VNumber n) = return n
assertNumber e = throwError $ TypeError TNumber e

assertReal :: (MonadError EvaluationError m) => Builtin -> Value -> m Double
assertReal builtin (VNumber wn) = do
  case tnIntoDouble (wn ^. value) of
    Just r -> return r
    Nothing -> throwError $ DomainError builtin
assertReal _ e = throwError $ TypeError TNumber e

assertRealPredicate :: (MonadError EvaluationError m) => (Double -> Bool) -> Value -> m Double
assertRealPredicate f v = do
  d <- assertReal Identity v -- lying about the builtin, i know
  if f d then return d else throwError $ InterpreterBug "Uncaught error in assertRealPredicate (the caller should provide more information!)"

assertPositiveReal :: (MonadError EvaluationError m) => Value -> m Double
assertPositiveReal = assertRealPredicate (> 0)

assertList :: (MonadError EvaluationError m) => Value -> m (DropList Value)
assertList (VList xs) = return xs
assertList e = throwError $ TypeError (mkList TUnit) e -- expected type is morally wrong, but this should never happen

assertNonEmptyList :: (MonadError EvaluationError m) => Value -> m (NonEmpty (DropItem Value))
assertNonEmptyList v = do
  l <- assertList v
  case nonEmpty $ getKept l of
    Just ne -> return $ fmap K ne
    Nothing -> throwError $ InterpreterBug "Uncaught error from assertNonEmptyList (the caller should provide more details!)"

assertDice :: (MonadError EvaluationError m) => Value -> m (Roll Value)
assertDice (VDice r) = return r
assertDice e = throwError $ TypeError TDice e

assertPool :: (MonadError EvaluationError m) => Value -> m (Roll (DropList Value), Roll Value)
assertPool (VPool r s) = return (r, s)
assertPool e = throwError $ TypeError TPool e

assertNatural :: (MonadError EvaluationError m) => Value -> m Natural
assertNatural (VNumber wn) = case tnIntoNatural (wn ^. value) of
  Just nat -> return nat
  Nothing -> throwError $ InterpreterBug "Uncaught error from assertNatural (the caller should provide more detail!)"
assertNatural _ = throwError $ InterpreterBug "Uncaught error from assertNatural (the caller should provide more detail!)"

assertPositive :: (MonadError EvaluationError m) => Value -> m PositiveNatural
assertPositive v = do
  n <- assertNatural v
  case toPositive n of
    Nothing -> throwError $ InterpreterBug "Uncaught error from assertPositive (the caller should provide more detail!)"
    Just n' -> return n'
