module Formatting.Pretty where

import AST
import Control.Lens hiding (Identity)
import Control.Monad.Writer.CPS
import Data.Text qualified as T
import Evaluator.Types
import Evaluator.WeedNumber (WeedNumber, metadata, value)
import Formatting.Metadata
import TowerNumber.Core (TowerNumber)
import TowerNumber.Parse (formatTN)
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

class Pretty a where
  prettyPrint :: a -> Text

instance Pretty WeedTypeClass where
  prettyPrint CFunctor = "Functor"
  prettyPrint CMonad = "Monad"
  prettyPrint CRollable = "Rollable"

instance Pretty TypeConstraint where
  prettyPrint (CInstanceOf cls tv) = prettyPrint cls <> " " <> prettyPrint tv

instance Pretty TypeVarName where
  prettyPrint (TypeVarName n) = "t" <> show n

instance Pretty WeedType where
  prettyPrint TNumber = "ℕ"
  prettyPrint TBool = "𝔹"
  prettyPrint TUnit = "()"
  prettyPrint (TFunction t1 t2) = "(" <> prettyPrint t1 <> " -> " <> prettyPrint t2 <> ")"
  prettyPrint (TApp TList t) = "[" <> prettyPrint t <> "]"
  prettyPrint (TApp TDice t) = "(Dice " <> prettyPrint t <> ")"
  prettyPrint (TApp TPool t) = "(Pool " <> prettyPrint t <> ")"
  prettyPrint TList = "[]"
  prettyPrint TDice = "Dice"
  prettyPrint TPool = "Pool"
  prettyPrint (TVar n) = prettyPrint n
  prettyPrint (TApp t1 t2) = "(" <> prettyPrint t1 <> " " <> prettyPrint t2 <> ")"

instance Pretty Builtin where
  prettyPrint Negate = "negate"
  prettyPrint Not = "not"
  prettyPrint Add = "(+)"
  prettyPrint Sub = "(-)"
  prettyPrint Mul = "(*)"
  prettyPrint Div = "(/)"
  prettyPrint Mod = "(%)"
  prettyPrint ComplexAdd = "(:+)"
  prettyPrint ComplexSub = "(:-)"
  prettyPrint Floor = "floor"
  prettyPrint Ceil = "ceil"
  prettyPrint Pow = "(^)"
  prettyPrint Eq = "(==)"
  prettyPrint Neq = "(!=)"
  prettyPrint Lt = "(<)"
  prettyPrint Gt = "(>)"
  prettyPrint Le = "(<=)"
  prettyPrint Ge = "(>=)"
  prettyPrint And = "(&&)"
  prettyPrint Or = "(||)"
  prettyPrint Xor = "xor"
  prettyPrint Identity = "id"
  prettyPrint Map = "map"
  prettyPrint Ap = "ap"
  prettyPrint Return = "return"
  prettyPrint Bind = "bind"
  prettyPrint DiceD = "d"
  prettyPrint DiceS = "s"
  prettyPrint DiceF = "f"
  prettyPrint DiceU = "u"
  prettyPrint DiceGauss = "gauss"
  prettyPrint DicePareto = "pareto"
  prettyPrint DiceBinomial = "binomial"
  prettyPrint DiceCoin = "coin"
  prettyPrint DiceCircle = "circle"
  prettyPrint Constant = "constant"
  prettyPrint Collapse = "collapse"
  prettyPrint Source = "source"
  prettyPrint Poolify = "(#)"
  prettyPrint Sum = "sum"

instance Pretty IdentifierName where
  prettyPrint (S s) = show s
  prettyPrint (U i) = "g#" <> show i
  prettyPrint (B b) = prettyPrint b

instance (Pretty a) => Pretty [a] where
  prettyPrint xs = "[" <> T.intercalate ", " (map prettyPrint xs) <> "]"

instance (Pretty a) => Pretty (Declaration a) where
  prettyPrint (Decl name body) = prettyPrint name <> " = " <> prettyPrint body

-- todo surface expr

instance Pretty CoreUntypedExpr where
  prettyPrint (CUNumber n) = show n
  prettyPrint (CUBool b) = show b
  prettyPrint CUUnit = "()"
  prettyPrint (CUList xs) = prettyPrint xs
  prettyPrint (CUIdentifier n) = prettyPrint n
  prettyPrint (CULambda n e) = "λ" <> prettyPrint n <> " -> " <> prettyPrint e
  prettyPrint (CUApply e1 e2) = "(" <> prettyPrint e1 <> " " <> prettyPrint e2 <> ")"
  prettyPrint (CULet decl e2) = "let " <> prettyPrint decl <> " in " <> prettyPrint e2
  prettyPrint (CUIf e1 e2 e3) = "if " <> prettyPrint e1 <> " then " <> prettyPrint e2 <> " else " <> prettyPrint e3
  prettyPrint (CULetRec decls e2) = "let " <> prettyPrint decls <> " in " <> prettyPrint e2

instance Pretty CoreTypedExpr where
  prettyPrint (CTNumber n) = show n
  prettyPrint (CTBool b) = show b
  prettyPrint CTUnit = "()"
  prettyPrint (CTList t xs) = prettyPrint xs <> "::" <> prettyPrint t
  prettyPrint (CTIdentifier t n) = "(" <> prettyPrint n <> "::" <> prettyPrint t <> ")"
  prettyPrint (CTLambda t ident body) = "(λ" <> prettyPrint ident <> "::" <> prettyPrint t <> " -> " <> prettyPrint body <> ")"
  prettyPrint (CTApply t a b) = "(apply :: " <> prettyPrint t <> " " <> prettyPrint a <> " " <> prettyPrint b <> ")"
  prettyPrint (CTLet t decl e2) = "(let " <> prettyPrint decl <> " in " <> prettyPrint e2 <> " ::" <> prettyPrint t <> ")"
  prettyPrint (CTLetRec t decls body) = "(let " <> prettyPrint decls <> " in " <> prettyPrint body <> "::" <> prettyPrint t <> ")"
  prettyPrint (CTIf t cond tb fb) = "(if " <> prettyPrint cond <> " then " <> prettyPrint tb <> " else " <> prettyPrint fb <> "::" <> prettyPrint t <> ")"
  prettyPrint (CTMapPool t f pool) = "(map " <> prettyPrint f <> " " <> prettyPrint pool <> "::" <> prettyPrint t <> ")"

instance Pretty TowerNumber where
  prettyPrint = formatTN

instance Pretty WeedNumber where
  prettyPrint wn = case wn ^. metadata of
    Nothing -> tnText
    Just md -> formatWithMetadata md tnText
    where
      tnText = prettyPrint $ wn ^. value

instance Pretty Value where
  prettyPrint (VNumber n) = prettyPrint n
  prettyPrint (VBool b) = show b
  prettyPrint VUnit = "()"
  prettyPrint (VList xs) = prettyPrint xs
  prettyPrint (VBuiltin _) = "<A Builtin>"
  prettyPrint (VClosure _ ident body) = "(λ" <> prettyPrint ident <> " -> " <> prettyPrint body <> ")"
  prettyPrint (VPool _ _) = "<A Pool>"
  prettyPrint (VDice _) = "<A Dice>"

instance Pretty EvaluationError where
  prettyPrint DivisionByZero = "Division by zero"
  prettyPrint (BadComparisonType t) = "Bad comparison type: " <> show t
  prettyPrint (DomainError b) = "Domain error: Builtin " <> prettyPrint b <> " expected real, got complex"
  prettyPrint (TypeError t v) = "Type error: Expected " <> prettyPrint t <> ", got " <> prettyPrint v
  prettyPrint (BadDieParameter b s v) = "Bad die parameter: " <> prettyPrint b <> " " <> s <> " , got " <> prettyPrint v
  prettyPrint InfiniteRecursiveBinding = "Mutually recursive let block contained strictly evaluated bindings (would <<loop>>)"
  prettyPrint (InterpreterBug s) = "Interpreter bug: " <> s

instance Pretty TypeError where
  prettyPrint (UnboundIdentifier n) = "Unbound identifier: " <> show n
  prettyPrint (InfiniteType tv t) = "Infinite type: " <> prettyPrint tv <> " occurs in " <> prettyPrint t
  prettyPrint (CouldNotUnify t1 t2) = "Could not unify " <> prettyPrint t1 <> " and " <> prettyPrint t2
  prettyPrint (AmbiguousTypeVar tv c) = "Ambiguous type variable " <> prettyPrint tv <> " for class " <> prettyPrint c
  prettyPrint (MissingInstance c t) = "No instance for " <> prettyPrint c <> " for type " <> prettyPrint t
  prettyPrint (TypeCheckerBug s) = "Type checker bug: " <> s

instance (Pretty l, Pretty r) => Pretty (Either l r) where
  prettyPrint (Left l) = prettyPrint l
  prettyPrint (Right r) = prettyPrint r

-- tree is a reader int writer [Text]
type Tree a = ReaderT Int (Writer [Text]) a

-- depth is the current depth in the tree
displayTypedAST :: CoreTypedExpr -> Text
displayTypedAST e = unlines $ snd $ runWriter $ runReaderT (mkTree e) 0
  where
    printAtDepth :: Int -> Text -> Text
    printAtDepth depth s = T.replicate (depth * 2) " " <> s
    tellAtDepth :: [Text] -> Tree ()
    tellAtDepth ss = do
      depth <- ask
      let indented = map (printAtDepth depth) ss
      tell indented
    mkTree :: CoreTypedExpr -> Tree ()
    mkTree (CTNumber n) = tellAtDepth ["|- " <> show n <> " :: Number"]
    mkTree (CTBool b) = tellAtDepth ["|- " <> show b <> " :: Bool"]
    mkTree CTUnit = tellAtDepth ["|- () :: Unit"]
    mkTree (CTList t es) = do
      tellAtDepth ["|- " <> prettyPrint t]
      let deeper e' = local (+ 1) (mkTree e')
      mapM_ deeper es
    mkTree (CTIdentifier t n) = tellAtDepth ["|- " <> prettyPrint n <> " :: " <> prettyPrint t]
    mkTree (CTLambda t n body) = do
      tellAtDepth ["|- lambda " <> prettyPrint n <> " :: " <> prettyPrint t]
      local (+ 1) (mkTree body)
    mkTree (CTApply t e1 e2) = do
      tellAtDepth ["|- application :: " <> prettyPrint t]
      local (+ 1) (mkTree e1)
      local (+ 1) (mkTree e2)
    mkTree (CTLet t (Decl n e1) e2) = do
      tellAtDepth ["|- let " <> prettyPrint n <> " :: " <> prettyPrint t]
      local (+ 1) (mkTree e1)
      tellAtDepth ["|- in"]
      local (+ 1) (mkTree e2)
    mkTree (CTLetRec t decls e2) = do
      tellAtDepth ["|- let :: " <> prettyPrint t]
      let mkDeclTree (Decl n e') =
            ( do
                tellAtDepth ["|- " <> prettyPrint n <> "="]
                local (+ 1) (mkTree e')
            )
      mapM_ mkDeclTree decls
      tellAtDepth ["|- in"]
      local (+ 1) (mkTree e2)
    mkTree (CTIf t e1 e2 e3) = do
      tellAtDepth ["|- if :: " <> prettyPrint t]
      local (+ 1) (mkTree e1)
      tellAtDepth ["|- then"]
      local (+ 1) (mkTree e2)
      tellAtDepth ["|- else"]
      local (+ 1) (mkTree e3)
    mkTree (CTMapPool t e1 e2) = do
      tellAtDepth ["|- mapPool :: " <> prettyPrint t]
      local (+ 1) (mkTree e1)
      local (+ 1) (mkTree e2)
