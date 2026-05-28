module AST where

import TypeChecker.Types

data IdentifierName
  = S String
  | U Int
  | B Builtin
  deriving (Show, Eq, Ord)

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
  | -- identity
    Identity
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
  deriving (Show, Eq, Ord)

data SurfaceExpr
  = SNumber Double
  | SBool Bool
  | SUnit
  | SIdentifier IdentifierName
  | SUnaryOp String SurfaceExpr
  | SInfix String SurfaceExpr SurfaceExpr
  | SLambda IdentifierName SurfaceExpr
  | SApply SurfaceExpr SurfaceExpr
  | SIf SurfaceExpr SurfaceExpr SurfaceExpr
  | SLet IdentifierName SurfaceExpr SurfaceExpr
  | SHole
  deriving (Show, Eq)

data CoreUntypedExpr
  = CUNumber Double
  | CUBool Bool
  | CUUnit
  | CUList [CoreUntypedExpr]
  | CUIdentifier IdentifierName
  | CULambda IdentifierName CoreUntypedExpr
  | CUApply CoreUntypedExpr CoreUntypedExpr
  | CUIf CoreUntypedExpr CoreUntypedExpr CoreUntypedExpr
  | CULet IdentifierName CoreUntypedExpr CoreUntypedExpr
  deriving (Show, Eq)

data CoreTypedExpr
  = CTNumber Double
  | CTBool Bool
  | CTUnit
  | CTList WeedType [CoreTypedExpr]
  | CTIdentifier WeedType IdentifierName
  | CTLambda WeedType IdentifierName CoreTypedExpr
  | CTApply WeedType CoreTypedExpr CoreTypedExpr
  | CTIf WeedType CoreTypedExpr CoreTypedExpr CoreTypedExpr
  | CTLet WeedType IdentifierName CoreTypedExpr CoreTypedExpr
  | CTMapPool WeedType CoreTypedExpr CoreTypedExpr -- Maps ([a] -> b) over Pool a -> Dice b
  deriving (Show, Eq)
