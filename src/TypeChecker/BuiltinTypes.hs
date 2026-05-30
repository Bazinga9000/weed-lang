module TypeChecker.BuiltinTypes (builtinType) where

import AST
import TypeChecker.Infer
import TypeChecker.Types

noPoly :: WeedType -> WeedTypeScheme
noPoly t = ForAll [] [] t

num1 :: Infer WeedTypeScheme
num1 = return $ noPoly $ (TNumber ->> TNumber)

num2 :: Infer WeedTypeScheme
num2 = return $ noPoly $ (TNumber ->> TNumber ->> TNumber)

bool1 :: Infer WeedTypeScheme
bool1 = return $ noPoly $ (TBool ->> TBool)

bool2 :: Infer WeedTypeScheme
bool2 = return $ noPoly $ (TBool ->> TBool ->> TBool)

any1 :: Infer WeedTypeScheme
any1 = do
  tv <- fresh
  return $ ForAll [tv] [] (TVar tv ->> TVar tv)

any2 :: Infer WeedTypeScheme
any2 = do
  tv <- fresh
  return $ ForAll [tv] [] (TVar tv ->> TVar tv ->> TVar tv)

dice1 :: Infer WeedTypeScheme
dice1 = return $ noPoly $ (TNumber ->> mkDice TNumber)

dice2 :: Infer WeedTypeScheme
dice2 = return $ noPoly $ (TNumber ->> TNumber ->> mkDice TNumber)

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
builtinType If = do
  a <- fresh
  return $ ForAll [a] [] (TBool ->> TVar a ->> TVar a ->> TVar a)
builtinType Identity = any1
builtinType DiceD = dice1
builtinType DiceS = do
  tv <- fresh
  return $ ForAll [tv] [] (mkList (TVar tv) ->> mkDice (TVar tv))
builtinType DiceF = dice1
builtinType DiceU = dice1
builtinType DiceGauss = dice1
builtinType DicePareto = dice1
builtinType DiceBinomial = dice2
builtinType DiceCoin = return $ noPoly $ mkDice TBool
builtinType DiceCircle = dice1
builtinType Constant = do
  tv <- fresh
  return $ noPoly $ (TVar tv ->> mkDice (TVar tv))
builtinType Collapse = return $ noPoly $ (mkPool TNumber ->> mkDice TNumber)
builtinType Source = do
  f <- fresh
  a <- fresh
  let tf = TVar f
      ta = TVar a
  return $ ForAll [f, a] [CInstanceOf CRollable tf] (TApp tf ta ->> mkDice tf)
builtinType Poolify = do
  tv <- fresh
  return $ noPoly $ (TNumber ->> mkDice tv ->> mkPool tv)
builtinType Sum = return $ noPoly $ (mkList TNumber ->> TNumber)
