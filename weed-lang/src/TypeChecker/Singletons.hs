module TypeChecker.Singletons where

import TypeChecker.Types
import Data.Type.Equality

data SWeedType :: WeedType -> Type where
  STNumber   :: SWeedType TNumber
  STBool     :: SWeedType TBool
  STUnit     :: SWeedType TUnit
  STFunction :: SWeedType a -> SWeedType b -> SWeedType (TFunction a b)
  STList     :: SWeedType a -> SWeedType (TList a)
  STDice     :: SWeedType a -> SWeedType (TDice a)
  STPool     :: SWeedType a -> SWeedType (TPool a)

data SomeSWeedType = forall t. SomeSWeedType (SWeedType t)

toSingleton :: WeedType -> Maybe SomeSWeedType
toSingleton TNumber = Just $ SomeSWeedType STNumber
toSingleton TBool = Just $ SomeSWeedType STBool
toSingleton TUnit = Just $ SomeSWeedType STUnit
toSingleton (TFunction a b) = do
  (SomeSWeedType a') <- toSingleton a
  (SomeSWeedType b') <- toSingleton b
  return $ SomeSWeedType (STFunction a' b')
toSingleton (TList a) = do
  (SomeSWeedType a') <- toSingleton a
  return $ SomeSWeedType (STList a')
toSingleton (TDice a) = do
  (SomeSWeedType a') <- toSingleton a
  return $ SomeSWeedType (STDice a')
toSingleton (TPool a) = do
  (SomeSWeedType a') <- toSingleton a
  return $ SomeSWeedType (STPool a')
toSingleton (TVar _) = Nothing
toSingleton (TApp _ _) = Nothing

fromSingleton :: SWeedType t -> WeedType
fromSingleton STNumber = TNumber
fromSingleton STBool = TBool
fromSingleton STUnit = TUnit
fromSingleton (STFunction a b) = TFunction (fromSingleton a) (fromSingleton b)
fromSingleton (STList a) = TList (fromSingleton a)
fromSingleton (STDice a) = TDice (fromSingleton a)
fromSingleton (STPool a) = TPool (fromSingleton a)

instance TestEquality SWeedType where
  testEquality STNumber STNumber = Just Refl
  testEquality STBool STBool = Just Refl
  testEquality STUnit STUnit = Just Refl
  testEquality (STFunction a1 b1) (STFunction a2 b2) = do
    Refl <- testEquality a1 a2
    Refl <- testEquality b1 b2
    return Refl
  testEquality (STList a) (STList b) = do
    Refl <- testEquality a b
    return Refl
  testEquality (STDice a) (STDice b) = do
    Refl <- testEquality a b
    return Refl
  testEquality (STPool a) (STPool b) = do
    Refl <- testEquality a b
    return Refl
  testEquality _ _ = Nothing
