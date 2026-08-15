module Evaluator.Builtins.HigherOrder (fetchBuiltinHigherOrder) where

import AST
import Control.Lens ((.~), _Just)
import Control.Monad.Except
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Evaluator.Assertions
import Evaluator.Builtins.DicePrimitives qualified as D
import Evaluator.Core
import Evaluator.DropList
import Evaluator.Types
import Evaluator.WeedNumber ((=~=), wnMaybeCompare, metadata)
import Evaluator.Metadata
import Formatting.Pretty (prettyPrint)
import TypeChecker.Singletons
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

-- | Higher-order builtins: those that apply user functions or otherwise need
-- access to the evaluator. The singleton is the builtin's type, checked by GHC.
fetchBuiltinHigherOrder :: SWeedType t -> Builtin -> Eval (Value t)

-- identity: dom and cod are the same type
fetchBuiltinHigherOrder (STFunction sa sb) Identity =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun sa sb $ \v -> return v
    Nothing -> throwError $ InterpreterBug "Identity: domain and codomain differ"

-- return / pure
fetchBuiltinHigherOrder (STFunction sa (STDice sb)) Return =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun sa (STDice sb) $ \v -> return $ VDice $ return v
    Nothing -> throwError $ InterpreterBug "Return: element type mismatch"
fetchBuiltinHigherOrder (STFunction sa (STList sb)) Return =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun sa (STList sb) $ \v -> return $ VList $ one v
    Nothing -> throwError $ InterpreterBug "Return: element type mismatch"
fetchBuiltinHigherOrder (STFunction sa (STPool sb)) Return =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun sa (STPool sb) $ \v -> return $ VPool (return $ one v) (return v)
    Nothing -> throwError $ InterpreterBug "Return: element type mismatch"

-- map
fetchBuiltinHigherOrder (STFunction (STFunction sa sb) (STFunction (STList sla) (STList slb))) Map =
  case (testEquality sa sla, testEquality sb slb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction sa sb) (STFunction (STList sla) (STList slb)) $ \f ->
        return $ VBuiltin $ TypedFun (STList sla) (STList slb) $ \(VList l) ->
          VList <$> mapMDropList (applyValue f) l
    _ -> throwError $ InterpreterBug "Map: type mismatch"
fetchBuiltinHigherOrder (STFunction (STFunction sa sb) (STFunction (STDice sda) (STDice sdb))) Map =
  case (testEquality sa sda, testEquality sb sdb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction sa sb) (STFunction (STDice sda) (STDice sdb)) $ \f ->
        return $ VBuiltin $ TypedFun (STDice sda) (STDice sdb) $ \(VDice d) -> do
          env <- askVars
          fb <- askFetchBuiltin
          return $ VDice $ d >>= applyValueRoll fb env f
    _ -> throwError $ InterpreterBug "Map: type mismatch"
fetchBuiltinHigherOrder (STFunction (STFunction sa sb) (STFunction (STPool spa) (STPool spb))) Map =
  case (testEquality sa spa, testEquality sb spb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction sa sb) (STFunction (STPool spa) (STPool spb)) $ \f ->
        return $ VBuiltin $ TypedFun (STPool spa) (STPool spb) $ \(VPool pool source) -> do
          env <- askVars
          fb <- askFetchBuiltin
          let mappedPool = pool >>= mapMDropList (applyValueRoll fb env f)
          let mappedSource = source >>= applyValueRoll fb env f
          return $ VPool mappedPool mappedSource
    _ -> throwError $ InterpreterBug "Map: type mismatch"

-- mapP: ([a] -> b) -> Pool a -> Dice b
fetchBuiltinHigherOrder (STFunction (STFunction (STList sa) sb) (STFunction (STPool spa) (STDice sdb))) MapP =
  case (testEquality sa spa, testEquality sb sdb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction (STList sa) sb) (STFunction (STPool spa) (STDice sdb)) $ \f ->
        return $ VBuiltin $ TypedFun (STPool spa) (STDice sdb) $ \(VPool pool _) -> do
          env <- askVars
          fb <- askFetchBuiltin
          return $ VDice $ do
            rolls <- pool
            applyValueRoll fb env f (VList rolls)
    _ -> throwError $ InterpreterBug "MapP: type mismatch"

-- ap
fetchBuiltinHigherOrder (STFunction (STList (STFunction sa sb)) (STFunction (STList sla) (STList slb))) Ap =
  case (testEquality sa sla, testEquality sb slb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STList (STFunction sa sb)) (STFunction (STList sla) (STList slb)) $ \(VList lf) ->
        return $ VBuiltin $ TypedFun (STList sla) (STList slb) $ \(VList la) ->
          VList <$> sequenceDropList (fmap applyValue lf <*> la)
    _ -> throwError $ InterpreterBug "Ap: type mismatch"
fetchBuiltinHigherOrder (STFunction (STDice (STFunction sa sb)) (STFunction (STDice sda) (STDice sdb))) Ap =
  case (testEquality sa sda, testEquality sb sdb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STDice (STFunction sa sb)) (STFunction (STDice sda) (STDice sdb)) $ \(VDice df) ->
        return $ VBuiltin $ TypedFun (STDice sda) (STDice sdb) $ \(VDice da) -> do
          env <- askVars
          fb <- askFetchBuiltin
          return $ VDice $ do
            vf <- df
            va <- da
            applyValueRoll fb env vf va
    _ -> throwError $ InterpreterBug "Ap: type mismatch"
fetchBuiltinHigherOrder (STFunction (STPool (STFunction sa sb)) (STFunction (STPool spa) (STPool spb))) Ap =
  case (testEquality sa spa, testEquality sb spb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STPool (STFunction sa sb)) (STFunction (STPool spa) (STPool spb)) $ \(VPool poolf sourcef) ->
        return $ VBuiltin $ TypedFun (STPool spa) (STPool spb) $ \(VPool poola sourcea) -> do
          env <- askVars
          fb <- askFetchBuiltin
          return $
            VPool
              ( do
                  pf <- poolf
                  pa <- poola
                  sequenceDropList $ fmap (applyValueRoll fb env) pf <*> pa
              )
              ( do
                  vf <- sourcef
                  va <- sourcea
                  applyValueRoll fb env vf va
              )
    _ -> throwError $ InterpreterBug "Ap: type mismatch"

-- bind
fetchBuiltinHigherOrder (STFunction (STList sa) (STFunction (STFunction sax (STList sb)) (STList slb))) Bind =
  case (testEquality sa sax, testEquality sb slb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STList sa) (STFunction (STFunction sax (STList sb)) (STList slb)) $ \(VList l) ->
        return $ VBuiltin $ TypedFun (STFunction sax (STList sb)) (STList slb) $ \f -> do
          vs <- mapMDropList (applyValue f) l
          VList . join <$> mapMDropList (\(VList inner) -> return inner) vs
    _ -> throwError $ InterpreterBug "Bind: type mismatch"
fetchBuiltinHigherOrder (STFunction (STDice sa) (STFunction (STFunction sax (STDice sb)) (STDice sdb))) Bind =
  case (testEquality sa sax, testEquality sb sdb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STDice sa) (STFunction (STFunction sax (STDice sb)) (STDice sdb)) $ \(VDice d) ->
        return $ VBuiltin $ TypedFun (STFunction sax (STDice sb)) (STDice sdb) $ \f -> do
          env <- askVars
          fb <- askFetchBuiltin
          return $ VDice $ do
            v <- d
            bound <- applyValueRoll fb env f v
            case bound of
              VDice d' -> d'
    _ -> throwError $ InterpreterBug "Bind: type mismatch"
fetchBuiltinHigherOrder (STFunction (STPool sa) (STFunction (STFunction sax (STPool sb)) (STPool spb))) Bind =
  case (testEquality sa sax, testEquality sb spb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STPool sa) (STFunction (STFunction sax (STPool sb)) (STPool spb)) $ \(VPool pool source) ->
        return $ VBuiltin $ TypedFun (STFunction sax (STPool sb)) (STPool spb) $ \f -> do
          env <- askVars
          fb <- askFetchBuiltin
          let boundPool = do
                evalList <- pool
                nestedLists <-
                  mapMDropList
                    ( \val -> do
                        bound <- applyValueRoll fb env f val
                        case bound of
                          VPool p _ -> p
                    )
                    evalList
                return (join nestedLists)
              boundSource = do
                val <- source
                bound <- applyValueRoll fb env f val
                case bound of
                  VPool _ s -> s
          return $ VPool boundPool boundSource
    _ -> throwError $ InterpreterBug "Bind: type mismatch"

-- dice primitives
fetchBuiltinHigherOrder (STFunction STNumber (STDice STNumber)) DiceD = return D.d
fetchBuiltinHigherOrder (STFunction (STList sa) (STDice sb)) DiceS =
  case testEquality sa sb of
    Just Refl -> return $ D.s sa
    Nothing -> throwError $ InterpreterBug "DiceS: element type mismatch"
fetchBuiltinHigherOrder (STFunction STNumber (STDice STNumber)) DiceF = return D.f
fetchBuiltinHigherOrder (STFunction STNumber (STDice STNumber)) DiceU = return D.u
fetchBuiltinHigherOrder (STFunction STNumber (STDice STNumber)) DiceGauss = return D.gauss
fetchBuiltinHigherOrder (STFunction STNumber (STDice STNumber)) DicePareto = return D.pareto
fetchBuiltinHigherOrder (STFunction STNumber (STFunction STNumber (STDice STNumber))) DiceBinomial = return D.binomial
fetchBuiltinHigherOrder (STDice STBool) DiceCoin = return D.coin
fetchBuiltinHigherOrder (STFunction STNumber (STDice STNumber)) DiceCircle = return D.circle

-- constant
fetchBuiltinHigherOrder (STFunction sa (STDice sb)) Constant =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun sa (STDice sb) $ \v -> return $ VDice $ return v
    Nothing -> throwError $ InterpreterBug "Constant: element type mismatch"

-- collapse
fetchBuiltinHigherOrder (STFunction (STPool STNumber) (STDice STNumber)) Collapse =
  return $ VBuiltin $ TypedFun (STPool STNumber) (STDice STNumber) $ \(VPool pool _) ->
    return $ VDice $ VNumber . sum . getKept <$> (pool >>= mapMDropList (\(VNumber n) -> return n))

-- source
fetchBuiltinHigherOrder (STFunction (STDice sa) (STDice sb)) Source =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun (STDice sa) (STDice sb) $ \(VDice d) -> return $ VDice d
    Nothing -> throwError $ InterpreterBug "Source: element type mismatch"
fetchBuiltinHigherOrder (STFunction (STPool sa) (STDice sb)) Source =
  case testEquality sa sb of
    Just Refl -> return $ VBuiltin $ TypedFun (STPool sa) (STDice sb) $ \(VPool _ s) -> return $ VDice s
    Nothing -> throwError $ InterpreterBug "Source: element type mismatch"

-- poolify
fetchBuiltinHigherOrder (STFunction STNumber (STFunction (STDice sa) (STPool sb))) Poolify =
  case testEquality sa sb of
    Just Refl ->
      return $ VBuiltin $ TypedFun STNumber (STFunction (STDice sa) (STPool sb)) $ \(VNumber wn) ->
        return $ VBuiltin $ TypedFun (STDice sa) (STPool sb) $ \(VDice d) -> do
          n' <- assertReal Poolify wn
          case getExactInteger n' of
            Nothing -> throwError $ BadDieParameter Poolify "expected an integer number of dice" (SomeValue STNumber (VNumber wn))
            Just count ->
              if count == 0
                then throwError $ BadDieParameter Poolify "expected a positive number of dice" (SomeValue STNumber (VNumber wn))
                else return $ VPool (replicateMDropList count d) d
    Nothing -> throwError $ InterpreterBug "Poolify: element type mismatch"

-- sum
fetchBuiltinHigherOrder (STFunction (STList STNumber) STNumber) Sum =
  return $ VBuiltin $ TypedFun (STList STNumber) STNumber $ \(VList l) ->
    VNumber . sum . getKept <$> mapMDropList (\(VNumber n) -> return n) l

-- length
fetchBuiltinHigherOrder (STFunction (STList sa) STNumber) Length =
  return $ VBuiltin $ TypedFun (STList sa) STNumber $ \(VList l) ->
    return $ VNumber $ fromIntegral $ length $ getKept l

-- equality and comparison (polymorphic over Eq/Ord types)
fetchBuiltinHigherOrder (STFunction sa (STFunction sb STBool)) Eq =
  case testEquality sa sb of
    Just Refl -> return $ mkEquality sa
    Nothing -> throwError $ InterpreterBug "Eq: argument types differ"
fetchBuiltinHigherOrder (STFunction sa (STFunction sb STBool)) Neq =
  case testEquality sa sb of
    Just Refl ->
      let eqBuiltin = mkEquality sa
       in return $ VBuiltin $ TypedFun sa (STFunction sb STBool) $ \a ->
            return $ VBuiltin $ TypedFun sb STBool $ \b -> do
              eq1 <- applyValue eqBuiltin a
              r <- applyValue eq1 b
              case r of
                VBool b' -> return $ VBool (not b')
    Nothing -> throwError $ InterpreterBug "Neq: argument types differ"
fetchBuiltinHigherOrder (STFunction sa (STFunction sb STBool)) Le = mkComparison sa sb (/= GT)
fetchBuiltinHigherOrder (STFunction sa (STFunction sb STBool)) Lt = mkComparison sa sb (== LT)
fetchBuiltinHigherOrder (STFunction sa (STFunction sb STBool)) Ge = mkComparison sa sb (/= LT)
fetchBuiltinHigherOrder (STFunction sa (STFunction sb STBool)) Gt = mkComparison sa sb (== GT)

-- liftMask: (a -> Bool) -> [a] -> [Bool]  or  ([a] -> [Bool]) -> [a] -> [Bool]
fetchBuiltinHigherOrder (STFunction (STFunction sa STBool) (STFunction (STList sla) (STList slb))) LiftMask =
  case (testEquality sa sla, testEquality STBool slb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction sa STBool) (STFunction (STList sla) (STList slb)) $ \f ->
        return $ VBuiltin $ TypedFun (STList sla) (STList slb) $ \(VList l) ->
          VList <$> mapMDropList (applyValue f) l
    _ -> throwError $ InterpreterBug "LiftMask: type mismatch"
fetchBuiltinHigherOrder (STFunction (STFunction (STList sa) (STList STBool)) (STFunction (STList sla) (STList slb))) LiftMask =
  case (testEquality sa sla, testEquality STBool slb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction (STList sa) (STList STBool)) (STFunction (STList sla) (STList slb)) $ \f ->
        return $ VBuiltin $ TypedFun (STList sla) (STList slb) $ \l ->
          applyValue f l
    _ -> throwError $ InterpreterBug "LiftMask: type mismatch"

-- highest / lowest: Number -> [Number] -> [Bool]
fetchBuiltinHigherOrder (STFunction STNumber (STFunction (STList STNumber) (STList STBool))) Highest =
  return $ mkHiLo Highest True
fetchBuiltinHigherOrder (STFunction STNumber (STFunction (STList STNumber) (STList STBool))) Lowest =
  return $ mkHiLo Lowest False

-- keep / drop: s -> r a -> r a where s is a selector (a -> Bool or [a] -> [Bool])
-- and r is Rollable (Dice or Pool). The selector determines the mask; keep
-- retains elements where the mask is True, drop retains where it's False.
fetchBuiltinHigherOrder (STFunction sel (STFunction (STDice sda) (STPool spb))) b | b `elem` [Keep, Drop] =
  case testEquality sda spb of
    Just Refl -> return $ mkKeepDropDice b sel sda
    Nothing -> throwError $ InterpreterBug "Keep/Drop: element type mismatch"
fetchBuiltinHigherOrder (STFunction sel (STFunction (STPool spa) (STPool spb))) b | b `elem` [Keep, Drop] =
  case testEquality spa spb of
    Just Refl -> return $ mkKeepDropPool b sel spa
    Nothing -> throwError $ InterpreterBug "Keep/Drop: element type mismatch"

-- explode: (a -> Bool) -> r a -> Pool a
fetchBuiltinHigherOrder (STFunction (STFunction sa STBool) (STFunction (STDice sda) (STPool spa))) Explode =
  case (testEquality sa sda, testEquality sa spa) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction sa STBool) (STFunction (STDice sda) (STPool spa)) $ \predicate ->
        return $ VBuiltin $ TypedFun (STDice sda) (STPool spa) $ \(VDice dice) -> do
          env <- askVars
          fb <- askFetchBuiltin
          let mkExplode src v = do
                r <- applyValueRoll fb env predicate v
                case r of
                  VBool False -> return $ one v
                  VBool True -> do
                    new <- freshExtra <$> src
                    (v `consKeep`) <$> mkExplode src new
          return $ VPool (dice >>= mkExplode dice) dice
    _ -> throwError $ InterpreterBug "Explode: type mismatch"
fetchBuiltinHigherOrder (STFunction (STFunction sa STBool) (STFunction (STPool spa) (STPool spb))) Explode =
  case (testEquality sa spa, testEquality sa spb) of
    (Just Refl, Just Refl) ->
      return $ VBuiltin $ TypedFun (STFunction sa STBool) (STFunction (STPool spa) (STPool spb)) $ \predicate ->
        return $ VBuiltin $ TypedFun (STPool spa) (STPool spb) $ \(VPool pool src) -> do
          env <- askVars
          fb <- askFetchBuiltin
          let mkExplode s v = do
                r <- applyValueRoll fb env predicate v
                case r of
                  VBool False -> return $ one v
                  VBool True -> do
                    new <- freshExtra <$> s
                    (v `consKeep`) <$> mkExplode s new
              pool' = join <$> (mapMDropList (mkExplode src) =<< pool)
          return $ VPool pool' src
    _ -> throwError $ InterpreterBug "Explode: type mismatch"

fetchBuiltinHigherOrder _ b =
  throwError $ InterpreterBug $ "fetchBuiltinHigherOrder: not implemented or wrong type: " <> prettyPrint b

-- | Mark a number as an extra die (from explode). Non-numbers pass through.
freshExtra :: Value a -> Value a
freshExtra (VNumber n) = VNumber $ (metadata . _Just . extraDice) .~ Multibool (1, 0) $ n
freshExtra v = v

-- | Convert a selector (a -> Bool or [a] -> [Bool]) into a mask function
-- [a] -> [Bool] running in 'Roll'.
runLiftMask :: FetchBuiltin -> Env -> SWeedType s -> SWeedType a -> Value s -> Eval (Value (TApp TList a) -> Roll (Value (TApp TList TBool)))
runLiftMask fb env (STFunction sa STBool) sla predicate =
  case testEquality sa sla of
    Just Refl ->
      -- selector is a -> Bool: map it over the list
      return $ \(VList l) -> VList <$> mapMDropList (applyValueRoll fb env predicate) l
    Nothing -> throwError $ InterpreterBug "runLiftMask: element type mismatch"
runLiftMask fb env (STFunction (STList sa) (STList STBool)) sla predicate =
  case testEquality sa sla of
    Just Refl ->
      -- selector is [a] -> [Bool]: apply it directly
      return $ \l -> applyValueRoll fb env predicate l
    Nothing -> throwError $ InterpreterBug "runLiftMask: list element type mismatch"
runLiftMask _ _ _ _ _ = throwError $ InterpreterBug "runLiftMask: selector is not a function"

-- | Shared keep/drop logic. The mask function runs in 'Roll' (it needs the
-- environment to apply closures, which 'applyValueRoll' provides).
keepDropPool :: Builtin -> (Value (TApp TList a) -> Roll (Value (TApp TList TBool))) -> Roll (DropList (Value a)) -> Roll (Value a) -> Eval (Value (TApp TPool a))
keepDropPool blt maskFn pool src = return $ VPool pool' src
  where
    isKeep = blt == Keep
    pool' = do
      vals <- pool
      maskRes <- maskFn (VList vals)
      let VList maskResult = maskRes
      let applyOne v m = case v of
            (D _) -> return v
            (K v') -> case m of
              (K (VBool True)) -> return $ if isKeep then K v' else D v'
              (K (VBool False)) -> return $ if isKeep then D v' else K v'
              (D (VBool _)) -> return $ D v'
      DropList <$> zipWithM applyOne (getItems vals) (getItems maskResult)

-- | keep/drop for Dice: filtering a single die yields a pool of 0 or 1.
mkKeepDropDice :: Builtin -> SWeedType sel -> SWeedType a -> Value (TFunction sel (TFunction (TApp TDice a) (TApp TPool a)))
mkKeepDropDice blt sel sa = VBuiltin $ TypedFun sel (STFunction (STDice sa) (STPool sa)) $ \predicate -> do
  env <- askVars
  fb <- askFetchBuiltin
  maskFn <- runLiftMask fb env sel sa predicate
  return $ VBuiltin $ TypedFun (STDice sa) (STPool sa) $ \(VDice dice) ->
    keepDropPool blt maskFn (one <$> dice) dice

-- | keep/drop for Pool
mkKeepDropPool :: Builtin -> SWeedType sel -> SWeedType a -> Value (TFunction sel (TFunction (TApp TPool a) (TApp TPool a)))
mkKeepDropPool blt sel sa = VBuiltin $ TypedFun sel (STFunction (STPool sa) (STPool sa)) $ \predicate -> do
  env <- askVars
  fb <- askFetchBuiltin
  maskFn <- runLiftMask fb env sel sa predicate
  return $ VBuiltin $ TypedFun (STPool sa) (STPool sa) $ \(VPool pool src) ->
    keepDropPool blt maskFn pool src

-- highest/lowest helper
mkHiLo :: Builtin -> Bool -> Value (TFunction TNumber (TFunction (TApp TList TNumber) (TApp TList TBool)))
mkHiLo blt isHighest = VBuiltin $ TypedFun STNumber (STFunction (STList STNumber) (STList STBool)) $ \(VNumber wn) ->
  return $ VBuiltin $ TypedFun (STList STNumber) (STList STBool) $ \(VList l) -> do
    n <- assertNatural blt wn
    nums <- mapMDropList (\(VNumber x) -> assertReal blt x) l
    let indexed = zip (getKept nums) [0 :: Integer ..]
        sorted = if isHighest then sortOn (first Down) indexed else sort indexed
        (top, rest) = splitAt (fromIntegral n) sorted
        tagged = [(i, True) | (_, i) <- top] ++ [(i, False) | (_, i) <- rest]
        mask = map snd (sortWith fst tagged)
    return $ VList $ VBool <$> liftPredicate id (toDropList mask)

-- equality helper: works on any Eq-able type
mkEquality :: SWeedType a -> Value (TFunction a (TFunction a TBool))
mkEquality sa = VBuiltin $ TypedFun sa (STFunction sa STBool) $ \a ->
  return $ VBuiltin $ TypedFun sa STBool $ fmap VBool . equality a
  where
    equality :: Value x -> Value x -> Eval Bool
    equality (VNumber x) (VNumber y) = return $ x =~= y
    equality (VBool x) (VBool y) = return $ x == y
    equality VUnit VUnit = return True
    equality (VList xs) (VList ys)
      | length (getKept xs) /= length (getKept ys) = return False
      | otherwise = and <$> zipWithM equality (getKept xs) (getKept ys)
    equality _ _ = throwError $ InterpreterBug "equality: mistyped comparison"

-- comparison helper
mkComparison :: SWeedType a -> SWeedType b -> (Ordering -> Bool) -> Eval (Value (TFunction a (TFunction b TBool)))
mkComparison sa sb f = case testEquality sa sb of
  Just Refl ->
    return $ VBuiltin $ TypedFun sa (STFunction sb STBool) $ \a ->
      return $ VBuiltin $ TypedFun sb STBool $ fmap (VBool . f) . comparison a
  Nothing -> throwError $ InterpreterBug "comparison: argument types differ"
  where
    comparison :: Value x -> Value x -> Eval Ordering
    comparison (VNumber x) (VNumber y) =
      maybe (throwError $ DomainError Le) return (wnMaybeCompare x y)
    comparison (VBool x) (VBool y) = return $ compare x y
    comparison VUnit VUnit = return EQ
    comparison (VList xs) (VList ys) = compareLists (getKept xs) (getKept ys)
      where
        compareLists [] [] = return EQ
        compareLists _ [] = return GT
        compareLists [] _ = return LT
        compareLists (a:as) (b:bs) = do
          o <- comparison a b
          case o of
            LT -> return LT
            GT -> return GT
            EQ -> compareLists as bs
    comparison _ _ = throwError $ InterpreterBug "comparison: mistyped comparison"

-- helper for poolify
getExactInteger :: Double -> Maybe Int
getExactInteger x
  | isNaN x || isInfinite x = Nothing
  | x == fromInteger (round x) = Just (round x)
  | otherwise = Nothing
