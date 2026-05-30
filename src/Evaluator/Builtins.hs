module Evaluator.Builtins where

import AST
import Control.Monad
import Control.Monad.Except
import Data.Complex
import Evaluator.Types
import Evaluator.WeedNumber
import Test.QuickCheck.Gen
import TypeChecker.Types

assertNumber :: Value -> Roll WeedNumber
assertNumber (VNumber n) = return $ n
assertNumber e = throwError $ TypeError (TNumber) e

assertNumberE :: Value -> Eval WeedNumber
assertNumberE (VNumber n) = return $ n
assertNumberE e = throwError $ TypeError (TNumber) e

assertRealE :: Builtin -> Value -> Eval Double
assertRealE builtin (VNumber wn) = do
  let real = realPart $ value wn
  let imag = imagPart $ value wn
  if imag == 0 then return real else throwError $ DomainError builtin
assertRealE _ e = throwError $ TypeError (TNumber) e

assertBool :: Value -> Roll Bool
assertBool (VBool b) = return $ b
assertBool e = throwError $ TypeError (TBool) e

assertBoolE :: Value -> Eval Bool
assertBoolE (VBool b) = return $ b
assertBoolE e = throwError $ TypeError (TBool) e

assertListE :: Value -> Eval [Value]
assertListE (VList xs) = return $ xs
assertListE e = throwError $ TypeError (mkList TUnit) e -- expected type is morally wrong, but this should never happen

--
-- Helper functions to lift functions into builtins that automatically lift/collapse into dice expressions.
-- Type errors should never happen, but if they do, we throw TypeError (which morally is a specific InterpreterBug)
--
--
liftNumber :: (WeedNumber -> WeedNumber) -> Value
liftNumber f = VBuiltin $ liftNumber'
  where
    liftNumber' :: Value -> Eval Value
    liftNumber' (VNumber n) = return $ VNumber (f n)
    liftNumber' e = throwError $ TypeError TNumber e

liftBool :: (Bool -> Bool) -> Value
liftBool f = VBuiltin $ liftBool'
  where
    liftBool' :: Value -> Eval Value
    liftBool' (VBool b) = return $ VBool (f b)
    liftBool' e = throwError $ TypeError TBool e

liftNumber2 :: (WeedNumber -> WeedNumber -> WeedNumber) -> Value
liftNumber2 f = VBuiltin $ \a -> return $ VBuiltin $ \b -> liftNumber2' a b
  where
    liftNumber2' :: Value -> Value -> Eval Value
    liftNumber2' (VNumber n) (VNumber n') = return $ VNumber (f n n')
    liftNumber2' (VNumber _) e = throwError $ TypeError TNumber e
    liftNumber2' e _ = throwError $ TypeError TNumber e

liftBool2 :: (Bool -> Bool -> Bool) -> Value
liftBool2 f = VBuiltin $ \a -> return $ VBuiltin $ \b -> liftBool2' a b
  where
    liftBool2' :: Value -> Value -> Eval Value
    liftBool2' (VBool b) (VBool b') = return $ VBool (f b b')
    liftBool2' (VBool _) e = throwError $ TypeError TBool e
    liftBool2' e _ = throwError $ TypeError TBool e

liftRoll2 :: WeedType -> (Roll Value -> Roll Value -> Roll Value) -> Value
liftRoll2 t f = VBuiltin $ \a -> return $ VBuiltin $ \b -> return $ VDice $ liftRoll2' a b
  where
    liftRoll2' :: Value -> Value -> Roll Value
    liftRoll2' (VNumber _) _ = throwError $ InterpreterBug "automatic number -> dice lifting failed"
    liftRoll2' _ (VNumber _) = throwError $ InterpreterBug "automatic number -> dice lifting failed"
    liftRoll2' (VDice d) (VDice d') = f d d'
    liftRoll2' _ e = throwError $ TypeError (mkDice t) e

liftValue2 :: (Value -> Value -> Eval Value) -> Value
liftValue2 f = VBuiltin $ \a -> return $ VBuiltin $ \b -> f a b

liftRealCmp :: Builtin -> (Double -> Double -> Bool) -> Value
liftRealCmp ident f = VBuiltin $ \a -> return $ VBuiltin $ \b -> do
  a' <- assertRealE ident a
  b' <- assertRealE ident b
  return $ VBool (f a' b')

---
-- Mathematical helpers
---

equality :: Value -> Value -> Eval Value
equality (VNumber a) (VNumber b) = return $ VBool (a =~= b)
equality (VBool a) (VBool b) = return $ VBool (a == b)
equality (VUnit) (VUnit) = return $ VBool True
equality (VClosure _ _ _) (VClosure _ _ _) = throwError $ BadComparisonType "function"
equality (VBuiltin _) (VBuiltin _) = throwError $ BadComparisonType "function"
equality (VList a) (VList b)
  | length a /= length b = return $ VBool False
  | otherwise = do
      eqs <- (zipWithM equality a b) >>= (mapM assertBoolE)
      return $ VBool (all id eqs)
equality (VDice _) (VDice _) = throwError $ BadComparisonType "dice"
equality (VPool _ _) (VPool _ _) = throwError $ BadComparisonType "pool"
equality _ _ = throwError $ InterpreterBug "Mistyped comparison"

getExactInteger :: Double -> Maybe Int
getExactInteger x
  | isNaN x || isInfinite x = Nothing
  | x == fromInteger (round x) = Just (round x)
  | otherwise = Nothing

---
-- Dice Helpers
---

onePosIntParam :: Builtin -> (Int -> Gen Value) -> Value
onePosIntParam b f = VBuiltin $ \n -> do
  n' <- assertRealE b n
  case getExactInteger n' of
    Just i ->
      if i <= 0
        then throwError $ BadDieParameter b "expected a positive integer" n
        else (return . VDice . liftGen . f) i
    Nothing -> throwError $ BadDieParameter b "expected an integer" n

oneDoubleParam :: Builtin -> (Double -> Gen Value) -> Value
oneDoubleParam b f = VBuiltin $ \n -> do
  n' <- assertRealE b n
  (return . VDice . liftGen . f) n'

-- the bigass match
fetchBuiltin :: Builtin -> Value
fetchBuiltin Negate = liftNumber (\n -> -n)
fetchBuiltin Not = liftBool not
fetchBuiltin Add = liftNumber2 (+)
fetchBuiltin Sub = liftNumber2 (-)
fetchBuiltin Mul = liftNumber2 (*)
fetchBuiltin Div = liftValue2 $ \d d' -> do
  n <- assertNumberE d
  n' <- assertNumberE d'
  if n =~= 0
    then
      throwError DivisionByZero
    else
      return $ VNumber (n / n')
fetchBuiltin Mod = liftValue2 $ \d d' -> do
  n <- assertNumberE d
  n' <- assertNumberE d'
  if n' =~= 0
    then
      throwError DivisionByZero
    else
      return $ VNumber (n `wnMod` n')
fetchBuiltin Pow = liftNumber2 (**)
fetchBuiltin Floor = liftNumber wnFloor
fetchBuiltin Ceil = liftNumber wnCeil
fetchBuiltin Eq = liftValue2 equality
fetchBuiltin Neq =
  liftValue2 $ \a b -> VBool . not <$> ((equality a b) >>= assertBoolE)
fetchBuiltin Le = liftRealCmp Le (<=)
fetchBuiltin Lt = liftRealCmp Lt (<)
fetchBuiltin Ge = liftRealCmp Ge (>=)
fetchBuiltin Gt = liftRealCmp Gt (>)
fetchBuiltin And = liftBool2 (&&)
fetchBuiltin Or = liftBool2 (||)
fetchBuiltin Xor = liftBool2 (/=)
fetchBuiltin If = VBuiltin $ \cond -> return $ VBuiltin $ \t -> return $ VBuiltin $ \f -> do
  cond' <- assertBoolE cond
  if cond'
    then return t
    else return f
fetchBuiltin Identity = VBuiltin return
-- TODO: dice need criticality
fetchBuiltin DiceD = onePosIntParam DiceD $ \i -> (VNumber . literal . fromIntegral) <$> chooseInt (1, i)
fetchBuiltin DiceS = VBuiltin $ \n -> do
  n' <- assertListE n
  case n' of
    [] -> throwError $ BadDieParameter DiceS "expected a non-empty list" n
    _ -> (return . VDice . liftGen . elements) n'
fetchBuiltin DiceF = onePosIntParam DiceF $ \i -> (VNumber . literal . fromIntegral) <$> chooseInt (-i, i)
fetchBuiltin DiceU = oneDoubleParam DiceU $ \i -> (VNumber . literal) <$> choose (0.0, i)
fetchBuiltin DiceGauss = oneDoubleParam DiceGauss $ \n ->
  (VNumber . literal) <$> do
    u1 <- choose (0.0, 1.0)
    u2 <- choose (0.0, 1.0)
    let z = sqrt (-2.0 * log u1) * cos (2.0 * pi * u2)
    return $ n * z
fetchBuiltin DicePareto = oneDoubleParam DicePareto $ \n ->
  (VNumber . literal) <$> do
    u <- choose (0.0, 1.0)
    return $ u ** (1.0 / n)
fetchBuiltin DiceBinomial = VBuiltin $ \n -> return $ VBuiltin $ \p -> do
  n' <- assertRealE DiceBinomial n
  p' <- assertRealE DiceBinomial p

  case getExactInteger n' of
    Nothing -> throwError $ BadDieParameter DiceBinomial "expected an integer number of trials" n
    Just trials ->
      if p' < 0.0 || p' > 1.0
        then throwError $ BadDieParameter DiceBinomial "expected a probability between 0 and 1" p
        else do
          let oneTrial = (\r -> if r < p' then 1 else 0) <$> choose (0.0, 1.0)
          return . VDice . liftGen $ VNumber . intliteral . sum <$> replicateM trials oneTrial
      where
        intliteral :: Int -> WeedNumber
        intliteral = literal . fromIntegral
fetchBuiltin DiceCoin = VDice $ liftGen $ elements [VBool True, VBool False]
fetchBuiltin DiceCircle = oneDoubleParam DiceCircle $ \r -> do
  theta <- choose (0.0, 2.0 * pi)
  return . VNumber $ complexLiteral (r * cos theta) (r * sin theta)
fetchBuiltin Constant = VBuiltin $ return . VDice . liftGen . return
fetchBuiltin Collapse = VBuiltin $ collapse
  where
    collapse :: Value -> Eval Value
    collapse (VPool pool _) = return $ VDice $ (VNumber . sum) <$> (pool >>= (mapM assertNumber))
    collapse e = throwError $ TypeError (mkPool TNumber) e
fetchBuiltin Source = VBuiltin $ source
  where
    source :: Value -> Eval Value
    source (VDice d) = return $ VDice d
    source (VPool _ s) = return $ VDice s
    source e = throwError $ TypeError (mkPool TNumber) e -- again, this typeerror's type is morally wrong, but the typechecker should catch this
fetchBuiltin Poolify = VBuiltin $ \n -> return $ VBuiltin $ \d -> poolify n d
  where
    poolify :: Value -> Value -> Eval Value
    poolify v@(VNumber _) (VDice d) = do
      n' <- assertRealE Poolify v -- TODO: this is stupid. we know v is a number, but this checks that again
      case getExactInteger n' of
        Nothing -> throwError $ BadDieParameter Poolify "expected an integer number of dice" v
        Just count ->
          if count == 0
            then throwError $ BadDieParameter Poolify "expected a positive number of dice" v
            else return $ VPool (replicateM count d) d
    poolify (VNumber _) e = throwError $ TypeError (mkDice TNumber) e
    poolify n _ = throwError $ TypeError TNumber n
fetchBuiltin Sum = VBuiltin $ \xs -> do
  xs' <- assertListE xs
  VNumber . sum <$> (mapM assertNumberE xs')
