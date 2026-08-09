module Evaluator.Core where

import AST
import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.DropList
import TypeChecker.Singletons
import TypeChecker.Types

runEval :: FetchBuiltin -> Env -> Eval a -> Either EvaluationError a
runEval fb env ev = let r = runExceptT ev in runReader r (EvalEnv env fb)

-- | Applies a function-typed value to an argument, given the function's
-- singleton (which provides the result type for closures, whose body is
-- evaluated at the result type). Total: the GADT index guarantees the first
-- argument is a function.
applyValue :: SWeedType (TFunction a b) -> Value (TFunction a b) -> Value a -> Eval (Value b)
applyValue (STFunction _ sb) (VClosure rho ident body) arg = do
  v <- local (\ee -> ee {envVars = Map.insert ident (SomeValue (singOfValue arg) arg) rho}) $ eval body
  case v of
    SomeValue sb' v' -> case testEquality sb sb' of
      Just Refl -> return v'
      Nothing -> throwError $ InterpreterBug "applyValue: closure result type mismatch"
applyValue _ (VBuiltin (TypedFun _ _ f)) arg = f arg

-- | Recover the singleton of a first-order value. Functions, dice, and pools
-- carry their type from the AST node that made them; this is only called on
-- values whose constructor determines the type.
singOfValue :: Value t -> SWeedType t
singOfValue (VNumber _) = STNumber
singOfValue (VBool _) = STBool
singOfValue VUnit = STUnit
singOfValue (VList _) = STList (error "singOfValue: list element type not recoverable")
singOfValue (VClosure {}) = error "singOfValue: closure type not recoverable"
singOfValue (VBuiltin {}) = error "singOfValue: builtin type not recoverable"
singOfValue (VDice _) = STDice (error "singOfValue: dice element type not recoverable")
singOfValue (VPool _ _) = STPool (error "singOfValue: pool element type not recoverable")

-- | Executes `applyValue` inside the Roll monad.
applyValueRoll :: FetchBuiltin -> Env -> SWeedType (TFunction a b) -> Value (TFunction a b) -> Value a -> Roll (Value b)
applyValueRoll fb env s f arg = case runEval fb env (applyValue s f arg) of
  Left err -> throwError err
  Right v -> return v

-- | Recover a ground singleton from a type annotation, or throw an interpreter bug.
requireGround :: WeedType -> Text -> Eval SomeSWeedType
requireGround t ctx = case toSingleton t of
  Just s -> return s
  Nothing -> throwError $ InterpreterBug $ "eval: " <> ctx <> " type is not ground"

-- | Evaluate a typed expression, producing a value whose type index matches
-- the expression's ground type annotation.
eval :: CoreTypedExpr -> Eval SomeValue
eval (CTNumber n) = return $ SomeValue STNumber (VNumber $ literal n)
eval (CTBool b) = return $ SomeValue STBool (VBool b)
eval CTUnit = return $ SomeValue STUnit VUnit
eval (CTList t xs) = do
  SomeSWeedType sl <- requireGround t "list"
  case sl of
    STList se -> do
      xs' <- mapM eval xs
      vs <- mapM (castValue se) xs'
      return $ SomeValue sl (VList $ toDropList vs)
    _ -> throwError $ InterpreterBug "eval: list type is not a list"
  where
    castValue :: SWeedType a -> SomeValue -> Eval (Value a)
    castValue s (SomeValue s' v) = case testEquality s s' of
      Just Refl -> return v
      Nothing -> throwError $ InterpreterBug "eval: list element type mismatch"
eval (CTIdentifier t (B builtin)) = do
  SomeSWeedType s <- requireGround t "builtin"
  fb <- askFetchBuiltin
  v <- runFetchBuiltin fb s builtin
  return $ SomeValue s v
eval (CTIdentifier _ ident) = do
  env <- askVars
  case lookupIdent env ident of
    Just v -> return v
    Nothing -> throwError $ InterpreterBug "Evaluator got an unknown identifier"
eval (CTLambda t ident body) = do
  SomeSWeedType s <- requireGround t "lambda"
  env <- askVars
  case s of
    STFunction _ _ -> return $ SomeValue s (VClosure env ident body)
    _ -> throwError $ InterpreterBug "eval: lambda type is not a function"
eval (CTApply _ func arg) = do
  SomeSWeedType sf <- requireGround (getType func) "applied function"
  func' <- eval func
  arg' <- eval arg
  case (sf, func') of
    (STFunction sa sb, SomeValue sf' f) ->
      case testEquality sf sf' of
        Just Refl -> case arg' of
          SomeValue sa' a -> case testEquality sa sa' of
            Just Refl -> do
              v <- applyValue (STFunction sa sb) f a
              return $ SomeValue sb v
            Nothing -> throwError $ InterpreterBug "eval: function argument type mismatch"
        Nothing -> throwError $ InterpreterBug "eval: function type mismatch"
    _ -> throwError $ InterpreterBug "Tried to apply a non-function"
eval (CTLet _ (Decl ident expr) body) = do
  expr' <- eval expr
  local (\ee -> ee {envVars = envVars ee `extend` (ident, expr')}) $ eval body
eval (CTLetRec _ decls body) = do
  parentEnv <- askVars
  let idents = map (\(Decl ident _) -> ident) decls

  let getLambda (Decl _ (CTLambda _ arg argBody)) = Just (arg, argBody)
      getLambda _ = Nothing

  case traverse getLambda decls of
    Nothing ->
      throwError InfiniteRecursiveBinding
    Just lambdaParts -> do
      sdecls <- mapM (\(Decl _ e) -> requireGround (getType e) "recursive binding") decls
      let mkClosure (SomeSWeedType sd) (arg, argBody) = case sd of
            STFunction _ _ -> SomeValue sd (VClosure recEnv arg argBody)
            _ -> error "eval: recursive binding is not a function"
          recEnv = foldr (\(k, v) acc -> Map.insert k v acc) parentEnv (zip idents closures)
          closures = zipWith mkClosure sdecls lambdaParts

      local (\ee -> ee {envVars = recEnv}) (eval body)
eval (CTIf t cond thn els) = do
  SomeSWeedType st <- requireGround t "if result"
  _ <- requireGround (getType cond) "if condition"
  cond' <- eval cond
  env <- askVars
  fb <- askFetchBuiltin

  let runBranch :: SWeedType r -> CoreTypedExpr -> Roll (Value r)
      runBranch sr branchExpr = case runEval fb env (eval branchExpr) of
        Left err -> throwError err
        Right (SomeValue sr' v) -> case testEquality sr sr' of
          Just Refl -> return v
          Nothing -> throwError $ InterpreterBug "eval: if branch type mismatch"

  case cond' of
    SomeValue STBool (VBool True) -> eval thn
    SomeValue STBool (VBool False) -> eval els
    SomeValue (STDice STBool) (VDice d) ->
      case st of
        STDice str -> do
          let d' = d >>= \(VBool b) -> runBranch str (if b then thn else els)
          return $ SomeValue st (VDice d')
        _ -> throwError $ InterpreterBug "eval: dice condition in non-dice context"
    SomeValue (STPool STBool) (VPool p src) ->
      case st of
        STPool str -> do
          let src' = src >>= \(VBool b) -> runBranch str (if b then thn else els)
              p' = do
                vs <- p
                mapMDropList (\(VBool b) -> runBranch str (if b then thn else els)) vs
          return $ SomeValue st (VPool p' src')
        _ -> throwError $ InterpreterBug "eval: pool condition in non-pool context"
    _ -> throwError $ InterpreterBug "Evaluator got a non-boolean condition"

evalPreSample :: FetchBuiltin -> CoreTypedExpr -> Either EvaluationError SomeValue
evalPreSample fb expr = runEval fb Map.empty $ eval expr

sample :: SomeValue -> IO (Either EvaluationError SomeValue)
sample (SomeValue s v) = case v of
  VNumber n -> return $ Right $ SomeValue s (VNumber n)
  VBool b -> return $ Right $ SomeValue s (VBool b)
  VUnit -> return $ Right $ SomeValue s VUnit
  VList xs -> return $ Right $ SomeValue s (VList xs)
  VClosure rho ident body -> return $ Right $ SomeValue s (VClosure rho ident body)
  VBuiltin builtin -> return $ Right $ SomeValue s (VBuiltin builtin)
  VDice d -> do
    d' <- roll d
    return $ case d' of
      Left err -> Left err
      Right v' -> Right $ SomeValue (diceElement s) v'
  VPool p _ -> do
    p' <- roll p
    return $ case p' of
      Left err -> Left err
      Right v' -> Right $ SomeValue (STList (poolElement s)) (VList v')
  where
    diceElement :: SWeedType (TDice a) -> SWeedType a
    diceElement (STDice s') = s'
    poolElement :: SWeedType (TPool a) -> SWeedType a
    poolElement (STPool s') = s'

evaluate :: FetchBuiltin -> CoreTypedExpr -> IO (Either EvaluationError SomeValue)
evaluate fb expr = case evalPreSample fb expr of
  Left err -> return $ Left err
  Right v -> sample v
