module TypeChecker.Types where

data TypeError
  = UnboundIdentifier Text
  | InfiniteType TypeVarName WeedType
  | CouldNotUnify WeedType WeedType
  | AmbiguousTypeVar TypeVarName WeedTypeClass
  | AmbiguousType WeedType
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
  = TNumber
  | TBool
  | TUnit
  | TFunction WeedType WeedType
  | TList -- []
  | TDice -- Dice a
  | TPool -- Pool a
  | TVar TypeVarName
  | TApp WeedType WeedType
  deriving (Eq, Show)

infixr 0 ->>
(->>) :: WeedType -> WeedType -> WeedType
(->>) = TFunction

-- | The head of a (possibly applied) type constructor.
data TypeHead = HList | HDice | HPool deriving (Show, Eq, Ord)

baseType :: WeedType -> Maybe TypeHead
baseType (TApp t _) = baseType t
baseType TList = Just HList
baseType TDice = Just HDice
baseType TPool = Just HPool
baseType _ = Nothing

isDiceOrPool :: WeedType -> Bool
isDiceOrPool = (`elem` [Just HDice, Just HPool]) . baseType

data WeedTypeScheme = ForAll [TypeVarName] [TypeConstraint] WeedType

data ContextLvl = CtxBase | CtxDice | CtxPool deriving (Eq, Ord, Show)

peelEffect :: WeedType -> (ContextLvl, WeedType)
peelEffect (TApp TPool t) = (CtxPool, t)
peelEffect (TApp TDice t) = (CtxDice, t)
peelEffect t = (CtxBase, t)

applyEffect :: ContextLvl -> WeedType -> WeedType
applyEffect CtxPool t = TApp TPool t
applyEffect CtxDice t = TApp TDice t
applyEffect CtxBase t = t
