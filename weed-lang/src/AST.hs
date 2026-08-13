module AST where

import TowerNumber.Core (TowerNumber)
import TypeChecker.Types
import TypeChecker.Singletons

data IdentifierName
  = S String
  | U Int
  | B Builtin
  deriving (Show, Eq, Ord)

data Declaration e = Decl IdentifierName e deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Module a = Module [Declaration a] deriving (Eq, Show)

isBuiltin :: IdentifierName -> Bool
isBuiltin (B _) = True
isBuiltin _ = False

data Builtin -- unary operators
  = Negate
  | Not
  | -- arithmetic operations
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Floor
  | Ceil
  | ComplexAdd
  | ComplexSub
  | -- comparison operations
    Eq
  | Neq
  | Le
  | Lt
  | Ge
  | Gt
  | -- logical operations
    And
  | Or
  | Xor
  | -- monads
    Identity
  | Map
  | MapP
  | Ap
  | Return
  | Bind
  | LiftMask
  | -- primitive dice
    DiceD
  | DiceS
  | DiceF
  | DiceU
  | DiceGauss
  | DicePareto
  | DiceBinomial
  | DiceCoin
  | DiceCircle
  | -- dice maniulation
    Constant
  | Collapse
  | Source
  | Poolify
  | Sum
  | Keep
  | Drop
  | Explode
  | Approximate
  | -- list operations
    Highest
  | Lowest
  | Length
  deriving (Show, Eq, Ord)

data SurfaceExpr
  = SNumber TowerNumber
  | SBool Bool
  | SUnit
  | SList [SurfaceExpr]
  | SIdentifier IdentifierName
  | SUnaryOp String SurfaceExpr
  | SInfix String SurfaceExpr SurfaceExpr
  | SParens SurfaceExpr -- needed explicitly for hole lifting rules
  | SLambda IdentifierName SurfaceExpr
  | SApply SurfaceExpr SurfaceExpr
  | SPipe SurfaceExpr SurfaceExpr -- seperate for hole lifting rules
  | SIf SurfaceExpr SurfaceExpr SurfaceExpr
  | SLetRec [Declaration SurfaceExpr] SurfaceExpr -- no SLet, becuase the Lowerer handles this
  | SHole
  deriving (Show, Eq)

type SurfaceModule = Module SurfaceExpr

data CoreUntypedExpr
  = CUNumber TowerNumber
  | CUBool Bool
  | CUUnit
  | CUList [CoreUntypedExpr]
  | CUIdentifier IdentifierName
  | CULambda IdentifierName CoreUntypedExpr
  | CUApply CoreUntypedExpr CoreUntypedExpr
  | CULet (Declaration CoreUntypedExpr) CoreUntypedExpr
  | CULetRec [Declaration CoreUntypedExpr] CoreUntypedExpr
  | CUIf CoreUntypedExpr CoreUntypedExpr CoreUntypedExpr
  deriving (Show, Eq)

type CoreUntypedModule = Module CoreUntypedExpr

data CoreTypedExpr
  = CTNumber TowerNumber
  | CTBool Bool
  | CTUnit
  | CTList WeedType [CoreTypedExpr]
  | CTIdentifier WeedType IdentifierName
  | CTLambda WeedType IdentifierName CoreTypedExpr
  | CTApply WeedType CoreTypedExpr CoreTypedExpr
  | CTLet WeedType (Declaration CoreTypedExpr) CoreTypedExpr
  | CTLetRec WeedType [Declaration CoreTypedExpr] CoreTypedExpr
  | CTIf WeedType CoreTypedExpr CoreTypedExpr CoreTypedExpr
  deriving
    (
      Show,
      Eq
    )

type CoreTypedModule = Module CoreTypedExpr

data CoreElaboratedExpr :: WeedType -> Type where
  CENumber :: TowerNumber -> CoreElaboratedExpr TNumber
  CEBool :: Bool -> CoreElaboratedExpr TBool
  CEUnit :: CoreElaboratedExpr TUnit
  CEList :: SWeedType a -> [CoreElaboratedExpr a] -> CoreElaboratedExpr (TList a)
  CEIdentifier :: SWeedType t -> IdentifierName -> CoreElaboratedExpr t
  CELambda :: SWeedType a -> IdentifierName -> CoreElaboratedExpr b -> CoreElaboratedExpr (TFunction a b)
  CEApply :: CoreElaboratedExpr (TFunction a b) -> CoreElaboratedExpr a -> CoreElaboratedExpr b
  CELet :: SWeedType a -> IdentifierName -> CoreElaboratedExpr a -> CoreElaboratedExpr b -> CoreElaboratedExpr b
  CELetRec :: [Declaration SomeCoreElaboratedExpr] -> CoreElaboratedExpr c -> CoreElaboratedExpr c
  CEIf :: CoreElaboratedExpr TBool -> CoreElaboratedExpr a -> CoreElaboratedExpr a -> CoreElaboratedExpr a
  CEIfDice :: CoreElaboratedExpr (TDice TBool) -> CoreElaboratedExpr (TDice a) -> CoreElaboratedExpr (TDice a) -> CoreElaboratedExpr (TDice a)
  CEIfPool :: CoreElaboratedExpr (TPool TBool) -> CoreElaboratedExpr (TPool a) -> CoreElaboratedExpr (TPool a) -> CoreElaboratedExpr (TPool a)
data SomeCoreElaboratedExpr = forall t. SomeCoreElaboratedExpr (SWeedType t) (CoreElaboratedExpr t)

getType :: CoreTypedExpr -> WeedType
getType (CTNumber _) = TNumber
getType (CTBool _) = TBool
getType CTUnit = TUnit
getType (CTList t _) = t
getType (CTIdentifier t _) = t
getType (CTLambda t _ _) = t
getType (CTApply t _ _) = t
getType (CTLet t _ _) = t
getType (CTLetRec t _ _) = t
getType (CTIf t _ _ _) = t
