module Evaluator.Builtins (fetchBuiltin) where

import AST
-- TODO: this is a cyclic dependency, but only because of the monads needing access to `eval`, `applyValue`, `applyValueRoll`import Evaluator
-- figure out a way to clean this up later
import Control.Monad.Except
import Data.Complex
import Evaluator
import Evaluator.Types
import Evaluator.WeedNumber
import Numeric (Floating (log))
import PrettyPrint
import Test.QuickCheck.Gen
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

assertNumber :: Value -> Roll WeedNumber
assertNumber (VNumber n) = return n
assertNumber e = throwError $ TypeError TNumber e

assertNumberE :: Value -> Eval WeedNumber
assertNumberE (VNumber n) = return n
assertNumberE e = throwError $ TypeError TNumber e

assertRealE :: Builtin -> Value -> Eval Double
assertRealE builtin (VNumber wn) = do
  let real = realPart $ value wn
  let imag = imagPart $ value wn
  if imag == 0 then return real else throwError $ DomainError builtin
assertRealE _ e = throwError $ TypeError TNumber e

assertBoolE :: Value -> Eval Bool
assertBoolE (VBool b) = return b
assertBoolE e = throwError $ TypeError TBool e

assertListE :: Value -> Eval [Value]
assertListE (VList xs) = return xs
assertListE e = throwError $ TypeError (mkList TUnit) e -- expected type is morally wrong, but this should never happen

assertDiceE :: Value -> Eval (Roll Value)
assertDiceE (VDice r) = return r
assertDiceE e = throwError $ TypeError TDice e

assertPoolE :: Value -> Eval (Roll [Value], Roll Value)
assertPoolE (VPool r s) = return (r, s)
assertPoolE e = throwError $ TypeError TPool e

--
-- Helper functions to lift functions into builtins that automatically lift/collapse into dice expressions.
-- Type errors should never happen, but if they do, we throw TypeError (which morally is a specific InterpreterBug)
--
--
liftNumber :: (WeedNumber -> WeedNumber) -> Value
liftNumber f = VBuiltin liftNumber'
  where
    liftNumber' :: Value -> Eval Value
    liftNumber' (VNumber n) = return $ VNumber (f n)
    liftNumber' e = throwError $ TypeError TNumber e

liftBool :: (Bool -> Bool) -> Value
liftBool f = VBuiltin liftBool'
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
equality VUnit VUnit = return $ VBool True
equality (VClosure {}) (VClosure {}) = throwError $ BadComparisonType "function"
equality (VBuiltin _) (VBuiltin _) = throwError $ BadComparisonType "function"
equality (VList a) (VList b)
  | length a /= length b = return $ VBool False
  | otherwise = do
      eqs <- zipWithM equality a b >>= mapM assertBoolE
      return $ VBool (and eqs)
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

---
--- Monad Helpers
---

fetchOutputType1 :: WeedType -> Eval WeedType
fetchOutputType1 (TFunction _ t) = return t
fetchOutputType1 _ = throwError $ InterpreterBug "fetchOutputType1 got an invalid type"

fetchOutputType2 :: WeedType -> Eval WeedType
fetchOutputType2 (TFunction _ (TFunction _ t)) = return t
fetchOutputType2 _ = throwError $ InterpreterBug "fetchOutputType2 got an invalid type"

-- the bigass match
fetchBuiltin :: WeedType -> Builtin -> Value
fetchBuiltin _ Negate = liftNumber (\n -> -n)
fetchBuiltin _ Not = liftBool not
fetchBuiltin _ Add = liftNumber2 (+)
fetchBuiltin _ Sub = liftNumber2 (-)
fetchBuiltin _ Mul = liftNumber2 (*)
fetchBuiltin _ Div = liftValue2 $ \d d' -> do
  n <- assertNumberE d
  n' <- assertNumberE d'
  if n =~= 0
    then
      throwError DivisionByZero
    else
      return $ VNumber (n / n')
fetchBuiltin _ Mod = liftValue2 $ \d d' -> do
  n <- assertNumberE d
  n' <- assertNumberE d'
  if n' =~= 0
    then
      throwError DivisionByZero
    else
      return $ VNumber (n `wnMod` n')
fetchBuiltin _ Pow = liftNumber2 (**)
fetchBuiltin _ ComplexAdd = liftNumber2 wnCAdd
fetchBuiltin _ ComplexSub = liftNumber2 wnCSub
fetchBuiltin _ Floor = liftNumber wnFloor
fetchBuiltin _ Ceil = liftNumber wnCeil
fetchBuiltin _ Eq = liftValue2 equality
fetchBuiltin _ Neq =
  liftValue2 $ \a b -> VBool . not <$> (equality a b >>= assertBoolE)
fetchBuiltin _ Le = liftRealCmp Le (<=)
fetchBuiltin _ Lt = liftRealCmp Lt (<)
fetchBuiltin _ Ge = liftRealCmp Ge (>=)
fetchBuiltin _ Gt = liftRealCmp Gt (>)
fetchBuiltin _ And = liftBool2 (&&)
fetchBuiltin _ Or = liftBool2 (||)
fetchBuiltin _ Xor = liftBool2 (/=)
fetchBuiltin _ Identity = VBuiltin return
fetchBuiltin _ Map = VBuiltin $ \f -> return $ VBuiltin $ \v -> do
  env <- ask

  -- map doesn't actually need to know its output type, it can be determined from the input type
  case v of
    VDice d -> return $ VDice $ d >>= applyValueRoll env f
    VList l -> VList <$> mapM (applyValue f) l
    VPool pool source -> do
      let mappedPool = pool >>= mapM (applyValueRoll env f)
      let mappedSource = source >>= applyValueRoll env f
      return $ VPool mappedPool mappedSource
    _ -> throwError $ InterpreterBug "Evaluator got an invalid type for map"
fetchBuiltin apt Ap = VBuiltin $ \mf -> return $ VBuiltin $ \ma -> do
  env <- ask
  t <- fetchOutputType2 apt
  case t of
    (TApp TList _) -> do
      lf <- assertListE mf
      la <- assertListE mf
      VList <$> sequence (map applyValue lf <*> la)
    (TApp TDice _) -> do
      df <- assertDiceE mf
      da <- assertDiceE ma
      return $ VDice $ do
        vf <- df
        va <- da
        applyValueRoll env vf va
    (TApp TPool _) -> do
      (poolf, sourcef) <- assertPoolE mf
      (poola, sourcea) <- assertPoolE ma
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
fetchBuiltin rett Return = VBuiltin $ \v -> do
  t <- fetchOutputType1 rett
  case t of
    (TApp TDice _) -> return $ VDice $ return v
    (TApp TList _) -> return $ VList [v]
    (TApp TPool _) -> return $ VPool (return . return $ v) (return v)
    _ -> throwError $ InterpreterBug "Evaluator got an invalid type for return"
fetchBuiltin _ Bind = VBuiltin $ \m -> return $ VBuiltin $ \f -> do
  env <- ask

  case m of
    VList l -> do
      vs <- mapM (applyValue f) l
      VList . concat <$> mapM assertListE vs
    VDice d -> return $ VDice $ do
      v <- d
      bound <- applyValueRoll env f v
      case bound of
        VDice d' -> d'
        e -> throwError $ InterpreterBug $ "Bind returned a non-dice value. " <> prettyPrint e
    VPool pool source -> do
      let boundPool :: Roll [Value]
          boundPool = do
            evalList <- pool
            nestedLists <-
              mapM
                ( \val -> do
                    bound <- applyValueRoll env f val
                    case bound of
                      VPool p _ -> p
                      e -> throwError $ InterpreterBug $ "Bind (Pool) returned a non-pool value for the list. " <> prettyPrint e
                )
                evalList
            return (concat nestedLists)

      let boundSource :: Roll Value
          boundSource = do
            val <- source
            bound <- applyValueRoll env f val
            case bound of
              VPool _ s -> s
              e -> throwError $ InterpreterBug $ "Bind (Pool) returned a non-pool value for the source. " <> prettyPrint e

      return $ VPool boundPool boundSource
    _ -> throwError $ InterpreterBug $ "Bind called on a non-monad (" <> prettyPrint m <> ">>=" <> prettyPrint f <> ")"

-- TODO: dice need criticality
fetchBuiltin _ DiceD = onePosIntParam DiceD $ \i -> VNumber . literal . fromIntegral <$> chooseInt (1, i)
fetchBuiltin _ DiceS = VBuiltin $ \n -> do
  n' <- assertListE n
  case n' of
    [] -> throwError $ BadDieParameter DiceS "expected a non-empty list" n
    _ -> (return . VDice . liftGen . elements) n'
fetchBuiltin _ DiceF = onePosIntParam DiceF $ \i -> VNumber . literal . fromIntegral <$> chooseInt (-i, i)
fetchBuiltin _ DiceU = oneDoubleParam DiceU $ \i -> VNumber . literal <$> choose (0.0, i)
fetchBuiltin _ DiceGauss = oneDoubleParam DiceGauss $ \n ->
  VNumber . literal <$> do
    u1 <- choose (0.0, 1.0)
    u2 <- choose (0.0, 1.0)
    let z = sqrt (-(2.0 * log u1)) * cos (2.0 * pi * u2)
    return $ n * z
fetchBuiltin _ DicePareto = oneDoubleParam DicePareto $ \n ->
  VNumber . literal <$> do
    u <- choose (0.0, 1.0)
    return $ u ** (1.0 / n)
fetchBuiltin _ DiceBinomial = VBuiltin $ \n -> return $ VBuiltin $ \p -> do
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
fetchBuiltin _ DiceCoin = VDice $ liftGen $ elements [VBool True, VBool False]
fetchBuiltin _ DiceCircle = oneDoubleParam DiceCircle $ \r -> do
  theta <- choose (0.0, 2.0 * pi)
  return . VNumber $ complexLiteral (r * cos theta) (r * sin theta)
fetchBuiltin _ Constant = VBuiltin $ return . VDice . liftGen . return
fetchBuiltin _ Collapse = VBuiltin collapse
  where
    collapse :: Value -> Eval Value
    collapse (VPool pool _) = return $ VDice $ VNumber . sum <$> (pool >>= mapM assertNumber)
    collapse e = throwError $ TypeError (mkPool TNumber) e
fetchBuiltin _ Source = VBuiltin source
  where
    source :: Value -> Eval Value
    source (VDice d) = return $ VDice d
    source (VPool _ s) = return $ VDice s
    source e = throwError $ TypeError (mkPool TNumber) e -- again, this typeerror's type is morally wrong, but the typechecker should catch this
fetchBuiltin _ Poolify = VBuiltin $ \n -> return $ VBuiltin $ \d -> poolify n d
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
fetchBuiltin _ Sum = VBuiltin $ \xs -> do
  xs' <- assertListE xs
  VNumber . sum <$> mapM assertNumberE xs'
