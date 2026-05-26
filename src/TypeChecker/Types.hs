module TypeChecker.Types where

data TypeConstraint
  = CUnconstrained
  | CRollable
  deriving (Show, Eq, Ord)

strictest :: TypeConstraint -> TypeConstraint -> TypeConstraint
strictest CUnconstrained CUnconstrained = CUnconstrained
strictest _ _ = CRollable

newtype TypeVarName = TypeVarName Int
  deriving (Show, Eq, Ord)

data ConstrainedName = ConstrainedName TypeVarName TypeConstraint deriving (Show, Eq, Ord)

constrainedNameOf :: WeedType -> ConstrainedName
constrainedNameOf (TVar cn) = cn
constrainedNameOf _ = error "constrainedNameOf: not a TVar"

dropConstraint :: ConstrainedName -> TypeVarName
dropConstraint (ConstrainedName n _) = n

data WeedType
  = TNumber
  | TBool
  | TUnit -- ()
  | TFunction WeedType WeedType -- a -> b
  | TList WeedType -- [a]
  | TDice WeedType -- Dice a
  | TPool WeedType -- Pool a
  | TVar ConstrainedName -- a type variable
  deriving (Show, Eq)

nameOf :: WeedType -> TypeVarName
nameOf (TVar (ConstrainedName n _)) = n
nameOf _ = error "nameOf: not a TVar"

constraintOf :: WeedType -> TypeConstraint
constraintOf (TVar (ConstrainedName _ c)) = c
constraintOf _ = error "constraintOf: not a TVar"

satisfies :: TypeConstraint -> WeedType -> Bool
satisfies CUnconstrained _ = True
satisfies CRollable (TDice _) = True
satisfies CRollable (TPool _) = True
satisfies CRollable _ = False

matches :: TypeVarName -> WeedType -> Bool
matches a (TVar (ConstrainedName b _)) = a == b
matches _ _ = False

data WeedTypeScheme = ForAll [ConstrainedName] WeedType
  deriving (Show)
