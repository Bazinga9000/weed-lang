module Evaluator.Core
  ( eval
  , applyValue
  , applyValueRoll
  , runEval
  , evaluate
  , evalPreSample
  , liftScalar
  ) where

import AST
import Control.Monad.Except (throwError)
import Control.Monad.Writer.CPS (runWriter)
import Control.Monad.Writer.Class (tell)
import Data.Map qualified as Map
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.DropList
import Formatting.Pretty (prettyPrint)
import TypeChecker
import TypeChecker.Singletons
import TypeChecker.Types

runEval :: FetchBuiltin -> Env -> Eval a -> Either EvaluationError (a, [TraceEvent])
runEval fb env ev =
  let (ea, evts) = runWriter (runReaderT (runExceptT ev) (EvalEnv env fb))
   in fmap (\a -> (a, evts)) ea

applyValue :: Value (TFunction a b) -> Value a -> Eval (Value b)
applyValue (VClosure rho sa ident body) arg =
  local (\ee -> ee {envVars = Map.insert ident (SomeValue sa arg) rho}) $ eval body
applyValue (VBuiltin (TypedFun _ _ f)) arg = f arg

applyValueRoll :: FetchBuiltin -> Env -> Value (TFunction a b) -> Value a -> Roll (Value b)
applyValueRoll fb env f arg = case runEval fb env (applyValue f arg) of
  Left err -> throwError err
  Right (v, evts) -> tell evts >> return v

eval :: CoreElaboratedExpr t -> Eval (Value t)
eval (CENumber n) = return $ VNumber $ literal n
eval (CEBool b) = return $ VBool b
eval CEUnit = return VUnit
eval (CEList _ xs) = VList . toDropList <$> mapM eval xs
eval (CEIdentifier t (B builtin)) = do
  fb <- askFetchBuiltin
  runFetchBuiltin fb t builtin
eval (CEIdentifier t ident) = do
  env <- askVars
  case lookupIdent env ident of
    Just (SomeValue s' v) -> case testEquality s' t of
      Just Refl -> return v
      Nothing -> throwError $ InterpreterBug "eval: identifier type mismatch"
    Nothing -> throwError $ InterpreterBug "eval: unknown identifier"
eval (CELambda sa ident body) = do
  env <- askVars
  return $ VClosure env sa ident body
eval (CEApply f a) = do
  ef <- eval f
  ea <- eval a
  applyValue ef ea
eval (CELet tbinding ident binding body) = do
  ebinding <- eval binding
  env <- askVars
  let env' = Map.insert ident (SomeValue tbinding ebinding) env
  local (\ee -> ee {envVars = env'}) (eval body)
eval (CELetRec decls body) = do
  parentEnv <- askVars
  -- each binding is a lambda; capture recEnv (the knot). A non-lambda in a
  -- letrec is a strict-evaluation loop, so it is rejected.
  case traverse bindingLambda decls of
    Nothing -> throwError InfiniteRecursiveBinding
    Just lambdas -> do
      let recEnv = foldr (\(k, lam) acc -> Map.insert k (lamClosure recEnv lam) acc) parentEnv lambdas
      local (\ee -> ee {envVars = recEnv}) (eval body)
  where
    bindingLambda :: Declaration SomeCoreElaboratedExpr -> Maybe (IdentifierName, SomeLambda)
    bindingLambda (Decl ident (SomeCoreElaboratedExpr (STFunction sa sb) (CELambda _ argName lamBody))) =
      Just (ident, SomeLambda sa sb argName lamBody)
    bindingLambda _ = Nothing
eval (CEIf cond thn els) = do
  econd <- eval cond
  case econd of
    VBool True -> eval thn
    VBool False -> eval els
eval (CEIfDice cond thn els) = do
  econd <- eval cond
  runner <- runBranch
  case econd of
    VDice d -> return $ VDice $ d >>= \(VBool b) -> do
      br <- runner (if b then thn else els)
      case br of
        VDice br' -> br'
eval (CEIfPool cond thn els) = do
  econd <- eval cond
  runner <- runBranch
  case econd of
    VPool epool esrc ->
      let esrc' = esrc >>= \(VBool b) -> do
            br <- runner (if b then thn else els)
            case br of
              VPool _ br' -> br'
          epool' = do
            vs <- epool
            mapMDropList (\(VBool b) -> do
              br <- runner (if b then thn else els)
              case br of
                VPool _ br' -> br') vs
       in return $ VPool epool' esrc'

-- lift a scalar into a dice context, recording it in the trace.
-- the Lifted event is emitted when the die is sampled, so it appears in
-- the trace in the correct position relative to the surrounding rolls/ops.
liftScalar :: SWeedType a -> Value a -> Eval (Value (TApp TDice a))
liftScalar _ v = return $ VDice $ do
  tell [Lifted (prettyPrint v)]
  return v

runBranch :: Eval (CoreElaboratedExpr r -> Roll (Value r))
runBranch = do
  env <- askVars
  fb <- askFetchBuiltin
  return $ \expr -> case runEval fb env (eval expr) of
    Left err -> throwError err
    Right (v, evts) -> tell evts >> return v

-- a letrec lambda binding, packaged with its types.
data SomeLambda = forall a b. SomeLambda (SWeedType a) (SWeedType b) IdentifierName (CoreElaboratedExpr b)

lamClosure :: Env -> SomeLambda -> SomeValue
lamClosure rho (SomeLambda sa sb argName lamBody) = SomeValue (STFunction sa sb) (VClosure rho sa argName lamBody)

-- elaborates and then evaluates a typeChecked expression
-- elaboration error is considered an interpreter bug
evalPreSample :: FetchBuiltin -> CoreTypedExpr -> Either EvaluationError (SomeValue, [TraceEvent])
evalPreSample fb expr = case elaborate expr of
  Left err -> throwError $ InterpreterBug $ "evalPreSample: elaboration failed: " <> show err
  Right (SomeCoreElaboratedExpr s e) -> do
    (v, evts) <- runEval fb Map.empty (eval e)
    return (SomeValue s v, evts)

sample :: SomeValue -> [TraceEvent] -> IO (Either EvaluationError (SomeValue, [TraceEvent]))
sample (SomeValue s v) evts = case v of
  VNumber n -> return $ Right $ (SomeValue s (VNumber n), evts)
  VBool b -> return $ Right $ (SomeValue s (VBool b), evts)
  VUnit -> return $ Right $ (SomeValue s VUnit, evts)
  VList xs -> return $ Right $ (SomeValue s (VList xs), evts)
  VClosure rho sa ident body -> return $ Right $ (SomeValue s (VClosure rho sa ident body), evts)
  VBuiltin builtin -> return $ Right $ (SomeValue s (VBuiltin builtin), evts)
  VDice d -> do
    d' <- roll d
    case d' of
      Left err -> return $ Left err
      Right (v', evts') -> sample (SomeValue (diceElement s) v') (evts <> evts')
  VPool p _ -> do
    p' <- roll p
    case p' of
      Left err -> return $ Left err
      Right (v', evts') -> sample (SomeValue (STList (poolElement s)) (VList v')) (evts <> evts' <> [poolEvent v'])
  where
    diceElement :: SWeedType (TApp TDice a) -> SWeedType a
    diceElement (STDice s') = s'
    poolElement :: SWeedType (TApp TPool a) -> SWeedType a
    poolElement (STPool s') = s'

-- record a pool's final contents, marking dropped dice, and its total.
poolEvent :: DropList (Value a) -> TraceEvent
poolEvent dl = Pooled items total
  where
    items = map item (getItems dl)
    item (K v) = (prettyPrint v, True)
    item (D v) = (prettyPrint v, False)
    total = do
      kept <- mapM (\case VNumber n -> Just n; _ -> Nothing) [v | K v <- getItems dl]
      Just $ prettyPrint (VNumber (sum kept))

evaluate :: FetchBuiltin -> CoreTypedExpr -> IO (Either EvaluationError (SomeValue, [TraceEvent]))
evaluate fb expr = case evalPreSample fb expr of
  Left err -> return $ Left err
  Right (v, evts) -> sample v evts
