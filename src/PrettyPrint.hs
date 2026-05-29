module PrettyPrint where

import AST
import Data.List (intercalate)
import Evaluator.Types
import TypeChecker.Types

class PrettyPrintable a where
  prettyPrint :: a -> String

instance PrettyPrintable WeedTypeClass where
  prettyPrint CFunctor = "Functor"
  prettyPrint CMonad = "Monad"
  prettyPrint CRollable = "Rollable"

instance PrettyPrintable TypeConstraint where
  prettyPrint (CInstanceOf cls tv) = prettyPrint cls ++ " " ++ prettyPrint tv

instance PrettyPrintable TypeVarName where
  prettyPrint (TypeVarName n) = "t" ++ show n

instance PrettyPrintable WeedType where
  prettyPrint TNumber = "ℕ"
  prettyPrint TBool = "𝔹"
  prettyPrint TUnit = "()"
  prettyPrint (TFunction t1 t2) = "(" ++ prettyPrint t1 ++ " -> " ++ prettyPrint t2 ++ ")"
  prettyPrint (TApp TList t) = "[" ++ prettyPrint t ++ "]"
  prettyPrint (TApp TDice t) = "(Dice " ++ prettyPrint t ++ ")"
  prettyPrint (TApp TPool t) = "(Pool " ++ prettyPrint t ++ ")"
  prettyPrint TList = "[]"
  prettyPrint TDice = "Dice"
  prettyPrint TPool = "Pool"
  prettyPrint (TVar n) = prettyPrint n
  prettyPrint (TApp t1 t2) = "(" ++ prettyPrint t1 ++ " " ++ prettyPrint t2 ++ ")"

instance PrettyPrintable Builtin where
  prettyPrint Negate = "negate"
  prettyPrint Not = "not"
  prettyPrint Add = "(+)"
  prettyPrint Sub = "(-)"
  prettyPrint Mul = "(*)"
  prettyPrint Div = "(/)"
  prettyPrint Mod = "(%)"
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

instance PrettyPrintable IdentifierName where
  prettyPrint (S s) = show s
  prettyPrint (U i) = "g#" ++ show i
  prettyPrint (B b) = prettyPrint b

instance (PrettyPrintable a) => PrettyPrintable [a] where
  prettyPrint xs = "[" ++ intercalate ", " (map prettyPrint xs) ++ "]"

-- todo surface expr

instance PrettyPrintable CoreUntypedExpr where
  prettyPrint (CUNumber n) = show n
  prettyPrint (CUBool b) = show b
  prettyPrint CUUnit = "()"
  prettyPrint (CUList xs) = prettyPrint xs
  prettyPrint (CUIdentifier n) = prettyPrint n
  prettyPrint (CULambda n e) = "λ" ++ prettyPrint n ++ " -> " ++ prettyPrint e
  prettyPrint (CUApply e1 e2) = prettyPrint e1 ++ " " ++ prettyPrint e2
  prettyPrint (CUIf e1 e2 e3) = "if " ++ prettyPrint e1 ++ " then " ++ prettyPrint e2 ++ " else " ++ prettyPrint e3
  prettyPrint (CULet n e1 e2) = "let " ++ prettyPrint n ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2

instance PrettyPrintable CoreTypedExpr where
  prettyPrint (CTNumber n) = show n
  prettyPrint (CTBool b) = show b
  prettyPrint (CTUnit) = "()"
  prettyPrint (CTList t xs) = prettyPrint xs ++ "::" ++ prettyPrint t
  prettyPrint (CTIdentifier t n) = "(" ++ prettyPrint n ++ "::" ++ prettyPrint t ++ ")"
  prettyPrint (CTLambda t ident body) = "(λ" ++ prettyPrint ident ++ "::" ++ prettyPrint t ++ " -> " ++ prettyPrint body ++ ")"
  prettyPrint (CTApply t a b) = "(apply :: " ++ prettyPrint t ++ " " ++ prettyPrint a ++ " " ++ prettyPrint b ++ ")"
  prettyPrint (CTIf t cond tb fb) = "(if " ++ prettyPrint cond ++ " then " ++ prettyPrint tb ++ " else " ++ prettyPrint fb ++ "::" ++ prettyPrint t ++ ")"
  prettyPrint (CTLet t ident expr body) = "(let " ++ prettyPrint ident ++ " = " ++ prettyPrint expr ++ " in " ++ prettyPrint body ++ "::" ++ prettyPrint t ++ ")"
  prettyPrint (CTMapPool t f pool) = "(map " ++ prettyPrint f ++ " " ++ prettyPrint pool ++ "::" ++ prettyPrint t ++ ")"

instance PrettyPrintable Value where
  prettyPrint (VNumber n) = show n
  prettyPrint (VBool b) = show b
  prettyPrint (VUnit) = "()"
  prettyPrint (VList xs) = prettyPrint xs
  prettyPrint (VBuiltin _) = "<A Builtin>"
  prettyPrint (VClosure _ ident body) = "(λ" ++ prettyPrint ident ++ " -> " ++ prettyPrint body ++ ")"
  prettyPrint (VPool _ _) = "<A Pool>"
  prettyPrint (VDice _) = "<A Dice>"

instance PrettyPrintable EvaluationError where
  prettyPrint (DivisionByZero) = "Division by zero"
  prettyPrint (BadComparisonType t) = "Bad comparison type: " ++ show t
  prettyPrint (DomainError b) = "Domain error: Builtin " ++ prettyPrint b ++ " expected real, got complex"
  prettyPrint (TypeError t v) = "Type error: Expected " ++ prettyPrint t ++ ", got " ++ prettyPrint v
  prettyPrint (BadDieParameter b s v) = "Bad die parameter: " ++ prettyPrint b ++ " " ++ s ++ " , got " ++ prettyPrint v
  prettyPrint (InterpreterBug s) = "Interpreter bug: " ++ s

instance (PrettyPrintable l, PrettyPrintable r) => PrettyPrintable (Either l r) where
  prettyPrint (Left l) = prettyPrint l
  prettyPrint (Right r) = prettyPrint r
