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
  = TNumber
  | TBool
  | TUnit -- ()
  | TFunction WeedType WeedType -- a -> b
  | TList -- []
  | TDice -- Dice a
  | TPool -- Pool a
  | TVar TypeVarName -- a type variable
  | TApp WeedType WeedType -- a b
  deriving (Show, Eq, Ord)

baseType :: WeedType -> WeedType
baseType (TApp t _) = baseType t
baseType t = t

-- helpers for type construction
infixr 0 ->>

(->>) :: WeedType -> WeedType -> WeedType
(->>) = TFunction

mkList :: WeedType -> WeedType
mkList = TApp TList

mkDice :: WeedType -> WeedType
mkDice = TApp TDice

mkPool :: WeedType -> WeedType
mkPool = TApp TPool

isDiceOrPool :: WeedType -> Bool
isDiceOrPool t = case t of
  TApp TDice _ -> True
  TApp TPool _ -> True
  _ -> False

data WeedTypeScheme = ForAll [TypeVarName] [TypeConstraint] WeedType
  deriving (Show)

data ContextLvl = CtxBase | CtxDice | CtxPool deriving (Eq, Ord, Show)

peelEffect :: WeedType -> (ContextLvl, WeedType)
peelEffect (TApp TPool t) = (CtxPool, t)
peelEffect (TApp TDice t) = (CtxDice, t)
peelEffect t = (CtxBase, t)

applyEffect :: ContextLvl -> WeedType -> WeedType
applyEffect CtxPool t = TApp TPool t
applyEffect CtxDice t = TApp TDice t
applyEffect CtxBase t = t
