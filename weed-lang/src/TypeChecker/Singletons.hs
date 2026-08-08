module TypeChecker.Singletons where

import TypeChecker.Types
import Data.Type.Equality

data SWeedType :: WeedType -> Type where
  SNumber    :: SWeedType TNumber
  SBool      :: SWeedType TBool
  SUnit      :: SWeedType TUnit
  SFunction  :: SWeedType a -> SWeedType b -> SWeedType (TFunction a b)
  SList      :: SWeedType a -> SWeedType (TList a)
  SDice      :: SWeedType a -> SWeedType (TDice a)
  SPool      :: SWeedType a -> SWeedType (TPool a)

data SomeSWeedType = forall t. SomeSWeedType (SWeedType t)

toSingleton :: WeedType -> Maybe SomeSWeedType
toSingleton TNumber = Just $ SomeSWeedType SNumber
toSingleton TBool = Just $ SomeSWeedType SBool
toSingleton TUnit = Just $ SomeSWeedType SUnit
toSingleton (TFunction a b) = do
  (SomeSWeedType a') <- toSingleton a
  (SomeSWeedType b') <- toSingleton b
  return $ SomeSWeedType (SFunction a' b')
toSingleton (TList a) = do
  (SomeSWeedType a') <- toSingleton a
  return $ SomeSWeedType (SList a')
toSingleton (TDice a) = do
  (SomeSWeedType a') <- toSingleton a
  return $ SomeSWeedType (SDice a')
toSingleton (TPool a) = do
  (SomeSWeedType a') <- toSingleton a
  return $ SomeSWeedType (SPool a')
toSingleton (TVar _) = Nothing
toSingleton (TApp _ _) = Nothing

fromSingleton :: SWeedType t -> WeedType
fromSingleton SNumber = TNumber
fromSingleton SBool = TBool
fromSingleton SUnit = TUnit
fromSingleton (SFunction a b) = TFunction (fromSingleton a) (fromSingleton b)
fromSingleton (SList a) = TList (fromSingleton a)
fromSingleton (SDice a) = TDice (fromSingleton a)
fromSingleton (SPool a) = TPool (fromSingleton a)

instance TestEquality SWeedType where
  testEquality SNumber SNumber = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality SUnit SUnit = Just Refl
  testEquality (SFunction a1 b1) (SFunction a2 b2) = do
    Refl <- testEquality a1 a2
    Refl <- testEquality b1 b2
    return Refl
  testEquality (SList a) (SList b) = do
    Refl <- testEquality a b
    return Refl
  testEquality (SDice a) (SDice b) = do
    Refl <- testEquality a b
    return Refl
  testEquality (SPool a) (SPool b) = do
    Refl <- testEquality a b
    return Refl
  testEquality _ _ = Nothing
