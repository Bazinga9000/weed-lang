module Evaluator where

import AST
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Evaluator.Builtins
import Evaluator.Types
import Evaluator.WeedNumber
import TypeChecker.Types

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
      return $ VDice $ do
        rolls <- pool
        evs <- (sequence . map (applyValueRoll env f')) $ rolls
        return $ VList evs
    _ -> throwError $ InterpreterBug "Evaluator got a non-pool argument"
eval (CTMap _ f v) = do
  f' <- eval f
  v' <- eval v
  env <- ask

  case v' of
    VDice d -> return $ VDice $ d >>= applyValueRoll env f'
    VList l -> VList <$> mapM (applyValue f') l
    VPool pool source -> do
      let mappedPool = pool >>= mapM (applyValueRoll env f')
      let mappedSource = source >>= applyValueRoll env f'
      return $ VPool mappedPool mappedSource
    _ -> throwError $ InterpreterBug "Evaluator got an invalid type for map"
eval (CTAp t mf ma) = do
  f' <- eval mf
  a' <- eval ma
  env <- ask
  case t of
    (TApp TList _) -> do
      lf <- assertListE f'
      la <- assertListE a'
      VList <$> (sequence $ map applyValue lf <*> la)
    (TApp TDice _) -> do
      df <- assertDiceE f'
      da <- assertDiceE a'
      return $ VDice $ do
        vf <- df
        va <- da
        applyValueRoll env vf va
    (TApp TPool _) -> do
      (poolf, sourcef) <- assertPoolE f'
      (poola, sourcea) <- assertPoolE a'
      return $
        VPool
          ( do
              pf <- poolf
              pa <- poola
              sequence $ map (applyValueRoll env) pf <*> pa
          )
          ( do
              vf <- sourcef
              va <- sourcea
              applyValueRoll env vf va
          )
    _ -> throwError $ InterpreterBug "Evaluator got an invalid type for ap"
eval (CTReturn t v) = do
  v' <- eval v
  case t of
    (TApp TDice _) -> return $ VDice $ return v'
    (TApp TList _) -> return $ VList [v']
    (TApp TPool _) -> return $ VPool (return . return $ v') (return v')
    _ -> throwError $ InterpreterBug "Evaluator got an invalid type for return"
eval (CTBind _ monadArg fnArg) = do
  m <- eval monadArg
  f <- eval fnArg
  env <- ask

  let bindDice :: Roll Value -> Roll Value
      bindDice d = do
        v <- d
        bound <- applyValueRoll env f v
        case bound of
          VDice d' -> d'
          _ -> throwError $ InterpreterBug "Bind returned a non-dice value."

  case m of
    VList l -> do
      vs <- sequence $ map (applyValue f) l
      (VList . concat) <$> mapM assertListE vs
    VDice d -> return $ VDice $ bindDice d
    VPool pool source -> do
      let boundPool = pool >>= mapM (bindDice . return)
      let boundSource = bindDice source
      return $ VPool boundPool boundSource
    _ -> throwError $ InterpreterBug "Bind called on a non-monad"

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
