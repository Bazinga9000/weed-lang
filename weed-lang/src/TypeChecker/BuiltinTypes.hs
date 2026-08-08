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
dice1 = return $ noPoly (TNumber ->> TDice TNumber)

dice2 :: Infer WeedTypeScheme
dice2 = return $ noPoly (TNumber ->> TNumber ->> TDice TNumber)

eqCmp :: Infer WeedTypeScheme
eqCmp = do
  tv <- fresh
  return $ ForAll [tv] [CInstanceOf $ CEq (TVar tv)] (TVar tv ->> TVar tv ->> TBool)

ordCmp :: Infer WeedTypeScheme
ordCmp = do
  tv <- fresh
  return $ ForAll [tv] [CInstanceOf $ COrd (TVar tv)] (TVar tv ->> TVar tv ->> TBool)

predicateMapModifier :: Infer WeedTypeScheme
predicateMapModifier = do
  s <- fresh
  a <- fresh
  r <- fresh
  let ts = TVar s
      ta = TVar a
      tr = TVar r
  return $ ForAll [s, a, r] [CInstanceOf $ CRollable tr, CInstanceOf $ CSelector ta ts] (ts ->> TApp tr ta ->> TApp tr ta)

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
builtinType Eq = eqCmp
builtinType Neq = eqCmp
builtinType Le = ordCmp
builtinType Lt = ordCmp
builtinType Ge = ordCmp
builtinType Gt = ordCmp
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
  return $ ForAll [f, a, b] [CInstanceOf $ CFunctor (TVar f)] ((ta ->> tb) ->> f' ta ->> f' tb)
builtinType MapP = do
  -- ([a] -> b) -> Pool a -> Dice b
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  return $ ForAll [a, b] [] ((TList ta ->> tb) ->> TPool ta ->> TDice tb)
builtinType Ap = do
  m <- fresh
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  let m' = TApp (TVar m)
  return $ ForAll [m, a, b] [CInstanceOf $ CMonad (TVar m)] (m' (ta ->> tb) ->> m' ta ->> m' tb)
builtinType Return = do
  m <- fresh
  a <- fresh
  let ta = TVar a
  let ma = TApp (TVar m) ta
  return $ ForAll [m, a] [CInstanceOf $ CMonad (TVar m)] (ta ->> ma)
builtinType Bind = do
  m <- fresh
  a <- fresh
  b <- fresh
  let ta = TVar a
  let tb = TVar b
  let m' = TApp (TVar m)
  return $ ForAll [m, a, b] [CInstanceOf $ CMonad (TVar m)] (m' ta ->> (ta ->> m' tb) ->> m' tb)
builtinType LiftMask = do
  s <- fresh
  a <- fresh
  let ts = TVar s
  let ta = TVar a
  return $ ForAll [s, a] [CInstanceOf $ CSelector ta ts] (ts ->> TList ta ->> TList TBool)
builtinType DiceD = dice1
builtinType DiceS = do
  tv <- fresh
  return $ ForAll [tv] [] (TList (TVar tv) ->> TDice (TVar tv))
builtinType DiceF = dice1
builtinType DiceU = dice1
builtinType DiceGauss = dice1
builtinType DicePareto = dice1
builtinType DiceBinomial = dice2
builtinType DiceCoin = return $ noPoly $ TDice TBool
builtinType DiceCircle = dice1
builtinType Constant = do
  tv <- fresh
  return $ ForAll [tv] [] (TVar tv ->> TDice (TVar tv))
builtinType Collapse = return $ noPoly (TPool TNumber ->> TDice TNumber)
builtinType Source = do
  f <- fresh
  a <- fresh
  let tf = TVar f
      ta = TVar a
  return $ ForAll [f, a] [CInstanceOf $ CRollable tf] (TApp tf ta ->> TDice tf)
builtinType Poolify = do
  tv <- fresh
  return $ ForAll [tv] [] (TNumber ->> TDice (TVar tv) ->> TPool (TVar tv))
builtinType Sum = return $ noPoly (TList TNumber ->> TNumber)
builtinType Keep = predicateMapModifier
builtinType Drop = predicateMapModifier
builtinType Explode = do
  a <- fresh
  r <- fresh
  let ta = TVar a
      tr = TVar r
  return $ ForAll [a, r] [CInstanceOf $ CRollable tr] ((ta ->> TBool) ->> TApp tr ta ->> TPool ta)
builtinType Approximate = return $ noPoly (TNumber ->> TNumber)
builtinType Highest = return $ noPoly (TNumber ->> TList TNumber ->> TList TBool)
builtinType Lowest = return $ noPoly (TNumber ->> TList TNumber ->> TList TBool)
builtinType Length = do
  a <- fresh
  let ta = TVar a
  return $ ForAll [a] [] (TList ta ->> TNumber)
