module Evaluator.Builtins (fetchBuiltin) where

import AST
-- TODO: this is a cyclic dependency, but only because of the monads needing access to `eval`, `applyValue`, `applyValueRoll`import Evaluator
-- figure out a way to clean this up later
import Control.Monad.Except
import Evaluator
import Evaluator.Assertions
import Evaluator.Builtins.DicePrimitives qualified as D
import Evaluator.DropList
import Evaluator.Types
import Evaluator.WeedNumber
import Evaluator.Metadata
import Formatting.Pretty (prettyPrint)
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)
import Control.Lens ((%~), (.~), _Just)
import TowerNumber.Core (approximate)
import Data.List.NonEmpty qualified as N

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
liftNumber2 f = liftValue2 liftNumber2'
  where
    liftNumber2' :: Value -> Value -> Eval Value
    liftNumber2' (VNumber n) (VNumber n') = return $ VNumber (f n n')
    liftNumber2' (VNumber _) e = throwError $ TypeError TNumber e
    liftNumber2' e _ = throwError $ TypeError TNumber e

liftBool2 :: (Bool -> Bool -> Bool) -> Value
liftBool2 f = liftValue2 liftBool2'
  where
    liftBool2' :: Value -> Value -> Eval Value
    liftBool2' (VBool b) (VBool b') = return $ VBool (f b b')
    liftBool2' (VBool _) e = throwError $ TypeError TBool e
    liftBool2' e _ = throwError $ TypeError TBool e

liftValue2 :: (Value -> Value -> Eval Value) -> Value
liftValue2 f = VBuiltin $ \a -> return $ VBuiltin $ \b -> f a b

---
-- Mathematical helpers
---

equality :: Value -> Value -> Eval Value
equality (VNumber a) (VNumber b) = return $ VBool (a =~= b)
equality (VBool a) (VBool b) = return $ VBool (a == b)
equality VUnit VUnit = return $ VBool True
equality (VClosure {}) (VClosure {}) = throwError $ InterpreterBug "equality check got function"
equality (VBuiltin _) (VBuiltin _) = throwError $ InterpreterBug "equality check got function"
equality (VList a) (VList b)
  | length (getKept a) /= length (getKept b) = return $ VBool False
  | otherwise = do
      eqs <- zipWithM equality (getKept a) (getKept b) >>= mapM assertBool
      return $ VBool (and eqs)
equality (VDice _) (VDice _) = throwError $ InterpreterBug "equality check got dice"
equality (VPool _ _) (VPool _ _) = throwError $ InterpreterBug "equality check got pool"
equality _ _ = throwError $ InterpreterBug "Mistyped comparison"

comparison :: Builtin -> Value -> Value -> Eval Ordering
comparison blt (VNumber a) (VNumber b) = maybe (throwError $ DomainError blt) return (wnMaybeCompare a b)
comparison _ (VBool a) (VBool b) = return $ compare a b
comparison _ VUnit VUnit = return EQ
comparison _ (VClosure {}) (VClosure {}) = throwError $ InterpreterBug "comparison got function"
comparison _ (VBuiltin _) (VBuiltin _) = throwError $ InterpreterBug "comparison got function"
comparison blt (VList l1) (VList l2) = compareLists (getKept l1) (getKept l2) where
  compareLists [] [] = return EQ
  compareLists _ [] = return GT
  compareLists [] _ = return LT
  compareLists (a:as) (b:bs) = do
    o <- comparison blt a b
    case o of
      LT -> return LT
      GT -> return GT
      EQ -> compareLists as bs
comparison _ (VDice _) (VDice _) = throwError $ InterpreterBug "comparison got dice"
comparison _ (VPool _ _) (VPool _ _) = throwError $ InterpreterBug "comparison got pool"
comparison _ _ _ = throwError $ InterpreterBug "mistyped comparison"

liftComparison :: Builtin -> (Ordering -> Bool) -> Value
liftComparison blt f = liftValue2 $ \a b -> VBool . f <$> comparison blt a b


getExactInteger :: Double -> Maybe Int
getExactInteger x
  | isNaN x || isInfinite x = Nothing
  | x == fromInteger (round x) = Just (round x)
  | otherwise = Nothing

---
--- Monad Helpers
---

fetchOutputType1 :: WeedType -> Eval WeedType
fetchOutputType1 (TFunction _ t) = return t
fetchOutputType1 _ = throwError $ InterpreterBug "fetchOutputType1 got an invalid type"

fetchOutputType2 :: WeedType -> Eval WeedType
fetchOutputType2 (TFunction _ (TFunction _ t)) = return t
fetchOutputType2 _ = throwError $ InterpreterBug "fetchOutputType2 got an invalid type"

---
--- Type Helpers
---

unwrapFunction :: WeedType -> NonEmpty WeedType
unwrapFunction (TFunction a b) = a N.<| unwrapFunction b
unwrapFunction t = one t

---
--- Dice Helpers
---

freshExtra :: Value -> Value
freshExtra v = case v of
     VNumber n -> VNumber $ ((metadata . _Just . extraDice) .~ Multibool (1,0)) n
     o' -> o' -- TODO: non-numbers don't have metadata (perhaps they should?)

runLiftMask :: WeedType -> Value -> Eval Value
runLiftMask t predicate = do
  let selectorType = head $ unwrapFunction t
  -- TODO: we're being lazy here, and lying to LiftMask about the type
  -- so we don't have to investigate the selector. This will probably bite us
  -- later. Maybe write some nice wrapper around liftMask since it will get called in a lot of the builtin modifiers?
  let liftMask = fetchBuiltin (selectorType ->> TListOf TUnit ->> TListOf TBool) LiftMask
  applyValue liftMask predicate

--- Construct the builtin for a modifier like reroll.
-- Takes in
-- (1) the name of the builtin
-- (2) two functions (with access to the canonical generator)
-- (2a) what to do if the predicate holds
-- (2b) what to do if the predicate does NOT hold
-- (3) the type assigned to the builtin
-- and generates the builtin which checks each roll against the predicate, and mutates it according to the function
-- this form will pass through the K/D values of the input and predicate (either dropped -> output dropped)
-- mkPredicateMapModifier :: Builtin -> (Roll Value -> Value -> Roll Value) -> (Roll Value -> Value -> Roll Value) -> WeedType -> Value
-- mkPredicateMapModifier blt trueFn falseFn t = liftValue2 $ \predicate d -> do
--   env <- ask
--   msk <- runLiftMask t predicate
--   let runPool :: Roll (DropList Value) -> Roll Value -> Eval Value
--       runPool pool src = return $ VPool pool' src where
--         pool' = do
--           vals <- pool
--           maskResult <- applyValueRoll env msk (VList vals) >>= assertList
--           let applyOne v m = case m of
--                (VBool True) -> trueFn src v
--                (VBool False) -> falseFn src v
--                sk -> throwError $ InterpreterBug $ prettyPrint blt <> " msk should've returned a Bool, got " <> prettyPrint sk
--           zipWithMDropList applyOne vals maskResult

--   case d of
--     VDice dice -> runPool (one <$> dice) dice
--     VPool pool src -> runPool pool src
--     e -> throwError $ InterpreterBug $ prettyPrint blt <> " input should be rollable, got " <> prettyPrint e

-- construct the keep or drop modifier
-- this form actually does inspect the K/D contents of its input lists
mkKeepDrop :: Bool -> WeedType -> Value
mkKeepDrop isKeep t = liftValue2 $ \predicate d -> do
  env <- ask
  msk <- runLiftMask t predicate
  let blt = if isKeep then Keep else Drop
  let runPool :: Roll (DropList Value) -> Roll Value -> Eval Value
      runPool pool src = return $ VPool pool' src where
        pool' = do
          vals <- pool
          maskResult <- applyValueRoll env msk (VList vals) >>= assertList
          let applyOne v m = case v of
               (D _) -> return v
               (K v') -> case m of
                 (K (VBool True)) -> return $ if isKeep then K v' else D v'
                 (K (VBool False)) -> return $ if isKeep then D v' else K v'
                 (D (VBool _)) -> return $ D v'
                 sk -> throwError $ InterpreterBug $ prettyPrint blt <> " msk should've returned a Bool, got " <> prettyPrint sk
          DropList <$> zipWithM applyOne (getItems vals) (getItems maskResult)

  case d of
    VDice dice -> runPool (one <$> dice) dice
    VPool pool src -> runPool pool src
    e -> throwError $ InterpreterBug $ prettyPrint blt <> " input should be rollable, got " <> prettyPrint e

-- the bigass match
fetchBuiltin :: WeedType -> Builtin -> Value
fetchBuiltin _ Negate = liftNumber (\n -> -n)
fetchBuiltin _ Not = liftBool not
fetchBuiltin _ Add = liftNumber2 (+)
fetchBuiltin _ Sub = liftNumber2 (-)
fetchBuiltin _ Mul = liftNumber2 (*)
fetchBuiltin _ Div = liftNumber2 (/)
fetchBuiltin _ Mod = liftNumber2 wnMod
fetchBuiltin _ Pow = liftNumber2 (**)
fetchBuiltin _ ComplexAdd = liftNumber2 wnCAdd
fetchBuiltin _ ComplexSub = liftNumber2 wnCSub
fetchBuiltin _ Floor = liftNumber wnFloor
fetchBuiltin _ Ceil = liftNumber wnCeil
fetchBuiltin _ Eq = liftValue2 equality
fetchBuiltin _ Neq =
  liftValue2 $ \a b -> VBool . not <$> (equality a b >>= assertBool) -- todo: this is kinda stupid, its an assertBool that is guaranteed to pass
fetchBuiltin _ Le = liftComparison Le (/= GT)
fetchBuiltin _ Lt = liftComparison Lt (== LT)
fetchBuiltin _ Ge = liftComparison Ge (/= LT)
fetchBuiltin _ Gt = liftComparison Gt (== GT)
fetchBuiltin _ And = liftBool2 (&&)
fetchBuiltin _ Or = liftBool2 (||)
fetchBuiltin _ Xor = liftBool2 (/=)
fetchBuiltin _ Identity = VBuiltin return
fetchBuiltin _ Map = liftValue2 $ \f v -> do
  env <- ask



  -- map doesn't actually need to know its output type, it can be determined from the input type
  case v of
    VDice d -> return $ VDice $ d >>= applyValueRoll env f
    VList l -> VList <$> mapMDropList (applyValue f) l where
    VPool pool source -> do
      let mappedPool = pool >>= mapMDropList (applyValueRoll env f)
      let mappedSource = source >>= applyValueRoll env f
      return $ VPool mappedPool mappedSource
    _ -> throwError $ InterpreterBug "map got a non-Functor argument"
fetchBuiltin _ MapP = liftValue2 $ \f p -> do
  case p of
    VPool pool _ -> do
      env <- ask
      return $ VDice $ do
        rolls <- pool
        applyValueRoll env f (VList rolls)
    _ -> throwError $ InterpreterBug "mapP got a non-pool argument"
fetchBuiltin apt Ap = liftValue2 $ \mf ma -> do


  env <- ask
  t <- fetchOutputType2 apt
  case t of
    (TListOf _) -> do
      lf <- assertList mf
      la <- assertList ma
      VList <$> sequenceDropList (fmap applyValue lf <*> la)
    (TDiceOf _) -> do
      df <- assertDice mf
      da <- assertDice ma
      return $ VDice $ do
        vf <- df
        va <- da
        applyValueRoll env vf va
    (TPoolOf _) -> do
      (poolf, sourcef) <- assertPool mf
      (poola, sourcea) <- assertPool ma
      return $
        VPool
          ( do
              pf <- poolf
              pa <- poola
              sequenceDropList $ fmap (applyValueRoll env) pf <*> pa
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
    (TDiceOf _) -> return $ VDice $ return v
    (TListOf _) -> return $ VList $ one v
    (TPoolOf _) -> return $ VPool (return . return $ v) (return v)
    _ -> throwError $ InterpreterBug "Evaluator got an invalid type for return"
fetchBuiltin _ Bind = liftValue2 $ \m f -> do
  env <- ask

  case m of
    VList l -> do
      vs <- mapMDropList (applyValue f) l
      VList . join <$> mapMDropList assertList vs
    VDice d -> return $ VDice $ do
      v <- d
      bound <- applyValueRoll env f v
      case bound of
        VDice d' -> d'
        e -> throwError $ InterpreterBug $ "Bind returned a non-dice value. " <> prettyPrint e
    VPool pool source -> do
      let boundPool :: Roll (DropList Value)
          boundPool = do
            evalList <- pool
            nestedLists <-
              mapMDropList
                ( \val -> do
                    bound <- applyValueRoll env f val
                    case bound of
                      VPool p _ -> p
                      e -> throwError $ InterpreterBug $ "Bind (Pool) returned a non-pool value for the list. " <> prettyPrint e
                )
                evalList
            return (join nestedLists)

      let boundSource :: Roll Value
          boundSource = do
            val <- source
            bound <- applyValueRoll env f val
            case bound of
              VPool _ s -> s
              e -> throwError $ InterpreterBug $ "Bind (Pool) returned a non-pool value for the source. " <> prettyPrint e

      return $ VPool boundPool boundSource
    _ -> throwError $ InterpreterBug $ "Bind called on a non-monad (" <> prettyPrint m <> ">>=" <> prettyPrint f <> ")"
fetchBuiltin t LiftMask =
  let getLiftMaskType :: WeedType -> Either EvaluationError Bool
      getLiftMaskType ty = case N.toList $ unwrapFunction ty of
        [TFunction (TListOf _) (TListOf TBool), TListOf _, TListOf TBool] -> return True
        [TFunction _ TBool, TListOf _, TListOf TBool] -> return False
        e -> throwError $ InterpreterBug $ "LiftMask input type was " <> prettyPrint ty <> " unwrapped as " <> prettyPrint e
   in case getLiftMaskType t of
        -- Just True: input is ([a] -> [Bool]), Just False: input is a -> Bool
        Right True -> liftValue2 applyValue
        Right False -> liftValue2 $ \f a -> do
          as <- assertList a
          VList <$> mapMDropList (applyValue f) as
        Left e -> liftValue2 . const . const . throwError $ e
fetchBuiltin _ DiceD = D.d
fetchBuiltin _ DiceS = D.s
fetchBuiltin _ DiceF = D.f
fetchBuiltin _ DiceU = D.u
fetchBuiltin _ DiceGauss = D.gauss
fetchBuiltin _ DicePareto = D.pareto
fetchBuiltin _ DiceBinomial = D.binomial
fetchBuiltin _ DiceCoin = D.coin
fetchBuiltin _ DiceCircle = D.circle
fetchBuiltin _ Constant = VBuiltin $ return . VDice . liftGen . return
fetchBuiltin _ Collapse = VBuiltin collapse
  where
    collapse :: Value -> Eval Value
    collapse (VPool pool _) = return $ VDice $ VNumber . sum . getKept <$> (pool >>= mapMDropList assertNumber)
    collapse e = throwError $ TypeError (TPoolOf TNumber) e
fetchBuiltin _ Source = VBuiltin source
  where
    source :: Value -> Eval Value
    source (VDice d) = return $ VDice d
    source (VPool _ s) = return $ VDice s
    source e = throwError $ TypeError (TPoolOf TNumber) e -- again, this typeerror's type is morally wrong, but the typechecker should catch this
fetchBuiltin _ Poolify = liftValue2 poolify
  where
    poolify :: Value -> Value -> Eval Value
    poolify v@(VNumber _) (VDice d) = do
      n' <- assertReal Poolify v -- TODO: this is stupid. we know v is a number, but this checks that again
      case getExactInteger n' of
        Nothing -> throwError $ BadDieParameter Poolify "expected an integer number of dice" v
        Just count ->
          if count == 0
            then throwError $ BadDieParameter Poolify "expected a positive number of dice" v
            else return $ VPool (replicateMDropList count d) d
    poolify (VNumber _) e = throwError $ TypeError (TPoolOf TNumber) e
    poolify n _ = throwError $ TypeError TNumber n
fetchBuiltin _ Sum = VBuiltin $ \xs -> do
  xs' <- assertList xs
  VNumber . sum . getKept <$> mapMDropList assertNumber xs'
fetchBuiltin t Keep = mkKeepDrop True t
fetchBuiltin t Drop = mkKeepDrop False t
fetchBuiltin _ Explode = liftValue2 $ \predicate d -> do
  env <- ask
  let mkExplode :: Roll Value -> Value -> Roll (DropList Value)
      mkExplode src v = do
        maskResult <- applyValueRoll env predicate v >>= assertBool
        case maskResult of
          False -> return $ one v
          True -> do
            new <- freshExtra <$> src
            (v `consKeep`) <$> mkExplode src new
  case d of
    VDice dice -> return $ VPool (dice >>= mkExplode dice) dice
    VPool pool src -> return $ VPool pool' src where
      pool' = join <$> (mapMDropList (mkExplode src) =<< pool)
    e -> throwError $ InterpreterBug $ prettyPrint Explode <> " input should be rollable, got " <> prettyPrint e
fetchBuiltin _ Approximate = liftNumber (value %~ approximate)
fetchBuiltin _ Highest  = liftValue2 $ \n xs -> do
  n' <- assertNatural n
  xs' <- assertList xs
  let highest :: Ord a => [a] -> [Bool]
      highest nums = map snd (sortWith fst tagged) where
        indexed = zip nums [0 :: Integer ..]
        sortedByVal = sortOn (first Down) indexed
        (top, rest) = splitAt (fromIntegral n') sortedByVal
        tagged = [(i, True) | (_, i) <- top] ++ [(i, False) | (_, i) <- rest]
  VList . fmap VBool . liftPredicate highest <$> mapMDropList (assertReal Highest) xs'
fetchBuiltin _ Lowest  = liftValue2 $ \n xs -> do
  n' <- assertNatural n
  xs' <- assertList xs
  let lowest :: Ord a => [a] -> [Bool]
      lowest nums = map snd (sortWith fst tagged) where
        indexed = zip nums [0 :: Integer ..]
        sortedByVal = sort indexed
        (top, rest) = splitAt (fromIntegral n') sortedByVal
        tagged = [(i, True) | (_, i) <- top] ++ [(i, False) | (_, i) <- rest]
  VList . fmap VBool . liftPredicate lowest <$> mapMDropList (assertReal Lowest) xs'
fetchBuiltin _ Length = VBuiltin (fmap (VNumber . fromIntegral . length . getKept) . assertList)
