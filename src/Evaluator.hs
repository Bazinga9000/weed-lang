module Evaluator where

import AST
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Evaluator.Builtins
import Evaluator.Types
import Evaluator.WeedNumber

runEval :: Env -> Eval a -> Either EvaluationError a
runEval env ev = let r = runExceptT ev in runReader r env

eval :: CoreTypedExpr -> Eval Value
eval (CTNumber n) = return $ VNumber $ literal n
eval (CTBool b) = return $ VBool b
eval (CTUnit) = return VUnit
eval (CTList _ xs) = do
  xs' <- mapM eval xs
  return $ VList xs'
eval (CTIdentifier _ (B builtin)) = return $ fetchBuiltin builtin
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
  case func' of
    VClosure rho ident body -> do
      let captured = Map.insert ident arg' rho
      local (\_ -> captured) $ eval body
    VBuiltin builtin -> builtin arg'
    _ -> throwError $ InterpreterBug "Evaluator got a non-closure function"
eval (CTLet _ ident expr body) = do
  expr' <- eval expr
  local (\e -> e `extend` (ident, expr')) $ eval body
eval (CTMapPool _ f p) = do
  f' <- eval f
  p' <- eval p
  case p' of
    VPool pool _ -> do
      env <- ask
      let extract :: Eval [Value] -> Roll Value
          extract evs = case runEval env evs of
            Left err -> throwError err
            Right vs -> return $ VList vs
      case f' of
        VBuiltin builtin -> return $ VDice $ do
          evs <- (sequence . map builtin) <$> pool
          extract evs
        VClosure rho ident body -> do
          let applyClosure :: Value -> Eval Value
              applyClosure arg = local (\_ -> Map.insert ident arg rho) $ eval body
          return $ VDice $ do
            evs <- (sequence . map applyClosure) <$> pool
            extract evs
        _ -> throwError $ InterpreterBug "Evaluator got a non-closure function"
    _ -> throwError $ InterpreterBug "Evaluator got a non-pool argument"

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
