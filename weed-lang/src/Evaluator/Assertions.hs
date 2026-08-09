module Evaluator.Assertions where

import AST
import Control.Lens hiding (Identity)
import Control.Monad.Except
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.DropList
import TowerNumber.Core
import Prelude hiding (Identity)

-- These are *domain* coercions: they check that a
-- number belongs to a subdomain (real, natural, positive)

assertReal :: (MonadError EvaluationError m) => Builtin -> WeedNumber -> m Double
assertReal builtin wn = do
  case tnIntoDouble (wn ^. value) of
    Just r -> return r
    Nothing -> throwError $ DomainError builtin

assertRealPredicate :: (MonadError EvaluationError m) => Builtin -> (Double -> Bool) -> WeedNumber -> m Double
assertRealPredicate builtin f wn = do
  d <- assertReal builtin wn
  if f d then return d else throwError $ DomainError builtin

assertPositiveReal :: (MonadError EvaluationError m) => Builtin -> WeedNumber -> m Double
assertPositiveReal builtin = assertRealPredicate builtin (> 0)

assertNatural :: (MonadError EvaluationError m) => Builtin -> WeedNumber -> m Natural
assertNatural builtin wn = case tnIntoNatural (wn ^. value) of
  Just nat -> return nat
  Nothing -> throwError $ DomainError builtin

assertPositive :: (MonadError EvaluationError m) => Builtin -> WeedNumber -> m PositiveNatural
assertPositive builtin wn = do
  n <- assertNatural builtin wn
  case toPositive n of
    Nothing -> throwError $ DomainError builtin
    Just n' -> return n'

assertNonEmptyList :: (MonadError EvaluationError m) => Builtin -> DropList (Value a) -> m (NonEmpty (DropItem (Value a)))
assertNonEmptyList builtin l =
  case nonEmpty $ getKept l of
    Just ne -> return $ fmap K ne
    Nothing -> throwError $ DomainError builtin
