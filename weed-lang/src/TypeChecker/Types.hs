module TypeChecker.Types where

data TypeError
  = UnboundIdentifier Text
  | InfiniteType TypeVarName WeedType
  | CouldNotUnify WeedType WeedType
  | AmbiguousTypeVar TypeVarName WeedTypeClass
  | MissingInstance WeedTypeClass
  | TypeCheckerBug Text
  deriving (Show, Eq)

-- functor t: implemented by [], Dice, Pool
-- monad t: implemented by [], Dice, Pool
-- rollable t: implemented by Dice, Pool
-- selector s a: implemented when s = a -> Bool or s = [a] -> [Bool]
data WeedTypeClass = CFunctor WeedType
                   | CMonad WeedType
                   | CRollable WeedType
                   | CSelector WeedType WeedType
                   | CEq WeedType
                   | COrd WeedType deriving (Show, Eq)

newtype TypeVarName = TypeVarName Int deriving (Show, Eq, Ord)

-- we only collect typeclass constraints. equality is eagerly unified.
-- haskell also does this, for instance
newtype TypeConstraint = CInstanceOf WeedTypeClass
  deriving (Show)

data WeedType
  = TNumber | TBool | TUnit
  | TFunction WeedType WeedType
  | TList WeedType
  | TDice WeedType
  | TPool WeedType
  | TVar TypeVarName
  | TApp WeedType WeedType deriving (Show, Eq, Ord)

-- | The head of a (possibly applied) type constructor.
data TypeHead = HList | HDice | HPool deriving (Show, Eq, Ord)

-- | Dummy argument standing in for a constructor's parameter when a
-- constructor variable is bound, e.g. f := TList TDummyArg. 'apply'
-- contracts TApp (ctor TDummyArg) a back to ctor a, so the dummy never
-- escapes substitution; it is only ever observed by 'baseType'.
pattern TDummyArg :: WeedType
pattern TDummyArg = TApp TUnit TUnit

-- | View a type as a constructor application: TList a, TDice a, TPool a
-- and TApp f a all view as Just (f, a). Used by the coercion rules, which
-- need to see "wrapped" types uniformly regardless of representation.
viewApp :: WeedType -> Maybe (WeedType, WeedType)
viewApp (TApp f a) = Just (f, a)
viewApp (TList a) = Just (TList TDummyArg, a)
viewApp (TDice a) = Just (TDice TDummyArg, a)
viewApp (TPool a) = Just (TPool TDummyArg, a)
viewApp _ = Nothing

pattern TApplied :: WeedType -> WeedType -> WeedType
pattern TApplied f a <- (viewApp -> Just (f, a))

baseType :: WeedType -> Maybe TypeHead
baseType (TApp t _) = baseType t
baseType (TList _) = Just HList
baseType (TDice _) = Just HDice
baseType (TPool _) = Just HPool
baseType _ = Nothing

-- helpers for type construction
infixr 0 ->>

(->>) :: WeedType -> WeedType -> WeedType
(->>) = TFunction

isDiceOrPool :: WeedType -> Bool
isDiceOrPool t = case t of
  TDice _ -> True
  TPool _ -> True
  _ -> False

data WeedTypeScheme = ForAll [TypeVarName] [TypeConstraint] WeedType
  deriving (Show)

data ContextLvl = CtxBase | CtxDice | CtxPool deriving (Eq, Ord, Show)

peelEffect :: WeedType -> (ContextLvl, WeedType)
peelEffect (TPool t) = (CtxPool, t)
peelEffect (TDice t) = (CtxDice, t)
peelEffect t = (CtxBase, t)

applyEffect :: ContextLvl -> WeedType -> WeedType
applyEffect CtxPool t = TPool t
applyEffect CtxDice t = TDice t
applyEffect CtxBase t = t
