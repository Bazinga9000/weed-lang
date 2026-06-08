module Evaluator where

import AST
-- TODO: clean up the cyclic dependency

import Control.Monad.Except (throwError)
-- import Control.Monad.Fix
import qualified Data.Map as Map
import {-# SOURCE #-} Evaluator.Builtins
import Evaluator.Types
import Evaluator.WeedNumber

runEval :: Env -> Eval a -> Either EvaluationError a
runEval env ev = let r = runExceptT ev in runReader r env

-- | Applies a VClosure or VBuiltin to an argument in the Eval monad.
applyValue :: Value -> Value -> Eval Value
applyValue (VClosure rho ident body) arg =
  local (\_ -> Map.insert ident arg rho) $ eval body
applyValue (VBuiltin builtin) arg = builtin arg
applyValue _ _ = throwError $ InterpreterBug "Tried to apply a non-function"

-- | Executes `applyValue` inside the Roll monad.
applyValueRoll :: Env -> Value -> Value -> Roll Value
applyValueRoll env f arg = case runEval env (applyValue f arg) of
  Left err -> throwError err
  Right v -> return v

eval :: CoreTypedExpr -> Eval Value
eval (CTNumber n) = return $ VNumber $ literal n
eval (CTBool b) = return $ VBool b
eval CTUnit = return VUnit
eval (CTList _ xs) = do
  xs' <- mapM eval xs
  return $ VList xs'
eval (CTIdentifier t (B builtin)) = return $ fetchBuiltin t builtin
eval (CTIdentifier _ ident) = do
  env <- ask
  case lookupIdent env ident of
    Just v -> return v
    Nothing -> throwError $ InterpreterBug "Evaluator got an unknown identifier"
eval (CTLambda _ ident body) = do
  env <- ask
  return $ VClosure env ident body
eval (CTApply _ func arg) = do
  func' <- eval func
  arg' <- eval arg
  applyValue func' arg'
eval (CTLet _ (Decl ident expr) body) = do
  expr' <- eval expr
  local (\e -> e `extend` (ident, expr')) $ eval body
eval (CTLetRec _ decls body) = do
  parentEnv <- ask
  let idents = map (\(Decl ident _) -> ident) decls

  -- in a strict evaluator, recursive bindings MUST be lambdas.
  -- if they aren't, it's an illegal cycle that would cause an infinite loop.
  let getLambda (Decl _ (CTLambda _ arg argBody)) = Just (arg, argBody)
      getLambda _ = Nothing

  case traverse getLambda decls of
    Nothing ->
      throwError InfiniteRecursiveBinding
    Just lambdaParts -> do
      -- construct the environment recursively
      let recEnv = foldr (\(k, v) acc -> Map.insert k v acc) parentEnv (zip idents closures)
          closures = map (uncurry (VClosure recEnv)) lambdaParts

      -- evaluate the body in the combined recursive environment
      local (const recEnv) (eval body)
eval (CTIf _ cond t f) = do
  cond' <- eval cond
  env <- ask

  -- eval a branch inside the Roll monad
  let runBranch :: CoreTypedExpr -> Roll Value
      runBranch branchExpr = case runEval env (eval branchExpr) of
        Left err -> throwError err
        Right v -> return v

  -- run a conditional in a die
  let runCond :: Value -> Roll Value
      runCond dBool = case dBool of
        VBool True -> runBranch t
        VBool False -> runBranch f
        _ -> throwError $ InterpreterBug "If condition was a Die that did not produce a Boolean"

  case cond' of
    -- the trivial cases
    VBool True -> eval t
    VBool False -> eval f
    -- the dice
    VDice d -> return $ VDice $ d >>= runCond
    VPool p s -> return $ VPool p' s'
      where
        s' = s >>= runCond
        p' = do
          vs <- p
          mapM runCond vs
    _ -> throwError $ InterpreterBug "Evaluator got a non-boolean condition"
eval (CTMapPool _ f p) = do
  f' <- eval f
  p' <- eval p
  case p' of
    VPool pool _ -> do
      env <- ask
      return $ VDice $ do
        rolls <- pool
        applyValueRoll env f' (VList rolls)
    _ -> throwError $ InterpreterBug "Evaluator got a non-pool argument"

-- eval (CTMap _ f v) = do
-- eval (CTAp t mf ma) = do
-- eval (CTReturn t v) = do
-- eval (CTBind _ monadArg fnArg) = do

evalPreSample :: CoreTypedExpr -> Either EvaluationError Value
evalPreSample expr = runEval Map.empty $ eval expr

sample :: Value -> IO (Either EvaluationError Value)
sample (VNumber n) = return $ Right $ VNumber n
sample (VBool b) = return $ Right $ VBool b
sample VUnit = return $ Right VUnit
sample (VList xs) = return $ Right $ VList xs
sample (VClosure rho ident body) = return $ Right $ VClosure rho ident body
sample (VBuiltin builtin) = return $ Right $ VBuiltin builtin
sample (VDice d) = do
  d' <- roll d
  return $ case d' of
    Left err -> Left err
    Right v -> Right v
sample (VPool p _) = do
  p' <- roll p
  return $ case p' of
    Left err -> Left err
    Right v -> Right $ VList v

evaluate :: CoreTypedExpr -> IO (Either EvaluationError Value)
evaluate expr = case evalPreSample expr of
  Left err -> return $ Left err
  Right v -> sample v
