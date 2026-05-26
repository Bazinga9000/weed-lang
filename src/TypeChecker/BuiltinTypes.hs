module TypeChecker.BuiltinTypes (lookupBuiltin) where

import AST
import TypeChecker.Infer
import TypeChecker.Subst
import TypeChecker.Types

noPoly :: WeedType -> WeedTypeScheme
noPoly t = ForAll [] t

infixr 0 ->>

(->>) :: WeedType -> WeedType -> WeedType
(->>) = TFunction

num1 :: Infer WeedTypeScheme
num1 = return $ noPoly $ (TDice TNumber ->> TDice TNumber)

num2 :: Infer WeedTypeScheme
num2 = return $ noPoly $ (TDice TNumber ->> TDice TNumber ->> TDice TNumber)

bool1 :: Infer WeedTypeScheme
bool1 = return $ noPoly $ (TBool ->> TBool)

bool2 :: Infer WeedTypeScheme
bool2 = return $ noPoly $ (TBool ->> TBool ->> TBool)

any1 :: Infer WeedTypeScheme
any1 = do
  tv <- fresh CUnconstrained
  return $ ForAll [constrainedNameOf tv] (tv ->> tv)

any2 :: Infer WeedTypeScheme
any2 = do
  tv <- fresh CUnconstrained
  return $ ForAll [constrainedNameOf tv] (tv ->> tv ->> tv)

dice1 :: Infer WeedTypeScheme
dice1 = return $ noPoly $ (TNumber ->> TDice TNumber)

dice2 :: Infer WeedTypeScheme
dice2 = return $ noPoly $ (TNumber ->> TNumber ->> TDice TNumber)

lookupBuiltin :: Builtin -> Infer (Subst, WeedType)
lookupBuiltin builtin = do
  bts <- builtinType builtin
  t <- instantiate bts
  return (nullSubst, t)

builtinType :: Builtin -> Infer WeedTypeScheme
builtinType Negate = num1
builtinType Not = bool1
builtinType Add = num2
builtinType Sub = num2
builtinType Mul = num2
builtinType Div = num2
builtinType Mod = num2
builtinType Floor = num1
builtinType Ceil = num1
builtinType Pow = num2
builtinType Eq = any2
builtinType Neq = any2
builtinType Le = num2
builtinType Lt = num2
builtinType Ge = num2
builtinType Gt = num2
builtinType And = bool2
builtinType Or = bool2
builtinType Xor = bool2
builtinType Identity = any1
builtinType DiceD = dice1
builtinType DiceS = do
  tv <- fresh CUnconstrained
  return $ ForAll [constrainedNameOf tv] (TList tv ->> TDice tv)
builtinType DiceF = dice1
builtinType DiceU = dice1
builtinType DiceGauss = dice1
builtinType DicePareto = dice1
builtinType DiceBinomial = dice2
builtinType DiceCoin = return $ noPoly $ TDice TBool
builtinType DiceCircle = dice1
builtinType Constant = do
  tv <- fresh CUnconstrained
  return $ noPoly $ (tv ->> TDice tv)
builtinType Collapse = return $ noPoly $ (TPool TNumber ->> TDice TNumber)
builtinType Source = do
  tv <- fresh CUnconstrained
  rctv <- fresh (CRollable tv)
  return $ ForAll [constrainedNameOf tv, constrainedNameOf rctv] (rctv ->> TDice tv)
builtinType MkPool = do
  tv <- fresh CUnconstrained
  return $ noPoly $ (TNumber ->> TDice tv ->> TPool tv)
builtinType Sum = return $ noPoly $ (TList TNumber ->> TNumber)
