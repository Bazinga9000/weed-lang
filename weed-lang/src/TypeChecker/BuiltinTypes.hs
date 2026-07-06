module TypeChecker.BuiltinTypes (builtinType) where

import AST
import TypeChecker.Infer
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)

noPoly :: WeedType -> WeedTypeScheme
noPoly = ForAll [] []

num1 :: Infer WeedTypeScheme
num1 = return $ noPoly (TNumber ->> TNumber)

num2 :: Infer WeedTypeScheme
num2 = return $ noPoly (TNumber ->> TNumber ->> TNumber)

bool1 :: Infer WeedTypeScheme
bool1 = return $ noPoly (TBool ->> TBool)

bool2 :: Infer WeedTypeScheme
bool2 = return $ noPoly (TBool ->> TBool ->> TBool)

any1 :: Infer WeedTypeScheme
any1 = do
  tv <- fresh
  return $ ForAll [tv] [] (TVar tv ->> TVar tv)

dice1 :: Infer WeedTypeScheme
dice1 = return $ noPoly (TNumber ->> mkDice TNumber)

dice2 :: Infer WeedTypeScheme
dice2 = return $ noPoly (TNumber ->> TNumber ->> mkDice TNumber)

cmpNum :: Infer WeedTypeScheme
cmpNum = return $ noPoly (TNumber ->> TNumber ->> TBool)

cmpAny :: Infer WeedTypeScheme
cmpAny = do
  tv <- fresh
  return $ ForAll [tv] [] (TVar tv ->> TVar tv ->> TBool)

builtinType :: Builtin -> Infer WeedTypeScheme
builtinType Negate = num1
builtinType Not = bool1
builtinType Add = num2
builtinType Sub = num2
builtinType Mul = num2
builtinType Div = num2
builtinType Mod = num2
builtinType ComplexAdd = num2
builtinType ComplexSub = num2
builtinType Floor = num1
builtinType Ceil = num1
builtinType Pow = num2
builtinType Eq = cmpAny
builtinType Neq = cmpAny
builtinType Le = cmpNum
builtinType Lt = cmpNum
builtinType Ge = cmpNum
builtinType Gt = cmpNum
builtinType And = bool2
builtinType Or = bool2
builtinType Xor = bool2
builtinType Identity = any1
builtinType Map = do
  f <- fresh
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  let f' = TApp (TVar f)
  return $ ForAll [f, a, b] [CInstanceOf CFunctor (TVar f)] ((ta ->> tb) ->> f' ta ->> f' tb)
builtinType MapP = do
  -- ([a] -> b) -> Pool a -> Dice b
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  return $ ForAll [a, b] [] ((TApp TList ta ->> tb) ->> TApp TPool ta ->> TApp TDice tb)
builtinType Ap = do
  m <- fresh
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  let m' = TApp (TVar m)
  return $ ForAll [m, a, b] [CInstanceOf CMonad (TVar m)] (m' (ta ->> tb) ->> m' ta ->> m' tb)
builtinType Return = do
  m <- fresh
  a <- fresh
  let ta = TVar a
  let ma = TApp (TVar m) ta
  return $ ForAll [m, a] [CInstanceOf CMonad (TVar m)] (ta ->> ma)
builtinType Bind = do
  m <- fresh
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  let m' = TApp (TVar m)
  return $ ForAll [m, a, b] [CInstanceOf CMonad (TVar m)] (m' ta ->> (ta ->> m' tb) ->> m' tb)
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
  return $ ForAll [tv] [] (TVar tv ->> mkDice (TVar tv))
builtinType Collapse = return $ noPoly (mkPool TNumber ->> mkDice TNumber)
builtinType Source = do
  f <- fresh
  a <- fresh
  let tf = TVar f
      ta = TVar a
  return $ ForAll [f, a] [CInstanceOf CRollable tf] (TApp tf ta ->> mkDice tf)
builtinType Poolify = do
  tv <- fresh
  return $ ForAll [tv] [] (TNumber ->> mkDice (TVar tv) ->> mkPool (TVar tv))
builtinType Sum = return $ noPoly (mkList TNumber ->> TNumber)
