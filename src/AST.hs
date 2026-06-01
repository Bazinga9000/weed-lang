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
  | If
  | -- identity function
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
  | SList [SurfaceExpr]
  | SIdentifier IdentifierName
  | SUnaryOp String SurfaceExpr
  | SInfix String SurfaceExpr SurfaceExpr
  | SParens SurfaceExpr -- needed explicitly for hole lifting rules
  | SLambda IdentifierName SurfaceExpr
  | SApply SurfaceExpr SurfaceExpr
  | SPipe SurfaceExpr SurfaceExpr -- seperate for hole lifting rules
  | SIf SurfaceExpr SurfaceExpr SurfaceExpr
  | SLet IdentifierName SurfaceExpr SurfaceExpr
  | SMap SurfaceExpr SurfaceExpr
  | SAp SurfaceExpr SurfaceExpr
  | SReturn SurfaceExpr
  | SBind SurfaceExpr SurfaceExpr
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
  | CULet IdentifierName CoreUntypedExpr CoreUntypedExpr
  | -- higher order operations
    CUMap CoreUntypedExpr CoreUntypedExpr
  | CUAp CoreUntypedExpr CoreUntypedExpr
  | CUReturn CoreUntypedExpr
  | CUBind CoreUntypedExpr CoreUntypedExpr
  deriving (Show, Eq)

data CoreTypedExpr
  = CTNumber Double
  | CTBool Bool
  | CTUnit
  | CTList WeedType [CoreTypedExpr]
  | CTIdentifier WeedType IdentifierName
  | CTLambda WeedType IdentifierName CoreTypedExpr
  | CTApply WeedType CoreTypedExpr CoreTypedExpr
  | CTLet WeedType IdentifierName CoreTypedExpr CoreTypedExpr
  | CTMapPool WeedType CoreTypedExpr CoreTypedExpr -- Maps ([a] -> b) over Pool a -> Dice b
  | CTMap WeedType CoreTypedExpr CoreTypedExpr
  | CTAp WeedType CoreTypedExpr CoreTypedExpr
  | CTReturn WeedType CoreTypedExpr
  | CTBind WeedType CoreTypedExpr CoreTypedExpr
  deriving (Show, Eq)
