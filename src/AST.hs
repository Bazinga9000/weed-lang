module AST where

import TypeChecker.Types

data IdentifierName
  = S String
  | U Int
  | B Builtin
  deriving (Show, Eq, Ord)

data Declaration e = Decl IdentifierName e deriving (Eq, Show)

instance Functor Declaration where
  fmap f (Decl ident e) = Decl ident $ f e

instance Foldable Declaration where
  foldMap f (Decl _ x) = f x

data Module a = Module [Declaration a] deriving (Eq, Show)

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
  | -- monads
    Identity
  | Map
  | Ap
  | Return
  | Bind
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
  | SLetRec [Declaration SurfaceExpr] SurfaceExpr -- no SLet, becuase the Lowerer handles this
  | SHole
  deriving (Show, Eq)

type SurfaceModule = Module SurfaceExpr

data CoreUntypedExpr
  = CUNumber Double
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
  = CTNumber Double
  | CTBool Bool
  | CTUnit
  | CTList WeedType [CoreTypedExpr]
  | CTIdentifier WeedType IdentifierName
  | CTLambda WeedType IdentifierName CoreTypedExpr
  | CTApply WeedType CoreTypedExpr CoreTypedExpr
  | CTLet WeedType (Declaration CoreTypedExpr) CoreTypedExpr
  | CTLetRec WeedType [Declaration CoreTypedExpr] CoreTypedExpr
  | CTIf WeedType CoreTypedExpr CoreTypedExpr CoreTypedExpr
  | CTMapPool WeedType CoreTypedExpr CoreTypedExpr -- Maps ([a] -> b) over Pool a -> Dice b
  deriving (Show, Eq)

type CoreTypedModule = Module CoreTypedExpr
