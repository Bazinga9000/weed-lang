module Evaluator.Builtins.Pure (fetchBuiltinPure) where

import AST (Builtin (..), MetaKind (..), AccessMode (..))
import Control.Monad.Except
import Evaluator.Metadata
import Evaluator.Types
import Evaluator.WeedNumber
import Formatting.Pretty (prettyPrint)
import TypeChecker.Singletons
import TypeChecker.Types
import Prelude hiding (Ap, Identity, Sum)
import Control.Lens ((%~), (^.))
import TowerNumber.Core (approximate)

-- helpers to lift functions into values

liftNumber :: (WeedNumber -> WeedNumber) -> Value (TFunction TNumber TNumber)
liftNumber f = VBuiltin $ TypedFun STNumber STNumber $ \(VNumber n) -> return $ VNumber (f n)

liftBool :: (Bool -> Bool) -> Value (TFunction TBool TBool)
liftBool f = VBuiltin $ TypedFun STBool STBool $ \(VBool b) -> return $ VBool (f b)

liftNumber2 :: (WeedNumber -> WeedNumber -> WeedNumber) -> Value (TFunction TNumber (TFunction TNumber TNumber))
liftNumber2 f = VBuiltin $ TypedFun STNumber (STFunction STNumber STNumber) $ \(VNumber a) ->
  return $ VBuiltin $ TypedFun STNumber STNumber $ \(VNumber b) ->
    return $ VNumber (f a b)

liftBool2 :: (Bool -> Bool -> Bool) -> Value (TFunction TBool (TFunction TBool TBool))
liftBool2 f = VBuiltin $ TypedFun STBool (STFunction STBool STBool) $ \(VBool a) ->
  return $ VBuiltin $ TypedFun STBool STBool $ \(VBool b) ->
    return $ VBool (f a b)

-- builtins that don't need access to the evaluator
fetchBuiltinPure :: SWeedType t -> Builtin -> Eval (Value t)
fetchBuiltinPure (STFunction STNumber STNumber) Negate = return $ liftNumber negate
fetchBuiltinPure (STFunction STBool STBool) Not = return $ liftBool not
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) Add = return $ liftNumber2 (+)
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) Sub = return $ liftNumber2 (-)
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) Mul = return $ liftNumber2 (*)
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) Div = return $ liftNumber2 (/)
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) Mod = return $ liftNumber2 wnMod
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) Pow = return $ liftNumber2 (**)
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) ComplexAdd = return $ liftNumber2 wnCAdd
fetchBuiltinPure (STFunction STNumber (STFunction STNumber STNumber)) ComplexSub = return $ liftNumber2 wnCSub
fetchBuiltinPure (STFunction STNumber STNumber) Floor = return $ liftNumber wnFloor
fetchBuiltinPure (STFunction STNumber STNumber) Ceil = return $ liftNumber wnCeil
fetchBuiltinPure (STFunction STBool (STFunction STBool STBool)) And = return $ liftBool2 (&&)
fetchBuiltinPure (STFunction STBool (STFunction STBool STBool)) Or = return $ liftBool2 (||)
fetchBuiltinPure (STFunction STBool (STFunction STBool STBool)) Xor = return $ liftBool2 (/=)
fetchBuiltinPure (STFunction STNumber STNumber) Approximate = return $ liftNumber (value %~ approximate)
fetchBuiltinPure (STFunction STNumber STBool) (MetaAccess kind mode) = return $ metaAccessor kind mode
fetchBuiltinPure _ b = throwError $ InterpreterBug $ "fetchBuiltinPure: not a pure builtin or wrong type: " <> prettyPrint b

metaAccessor :: MetaKind -> AccessMode -> Value (TFunction TNumber TBool)
metaAccessor kind mode = VBuiltin $ TypedFun STNumber STBool $ \(VNumber n) ->
  case n ^. metadata of
    Nothing -> return $ VBool False
    Just md -> return $ VBool $ modePred mode (accessor kind md) where
      accessor MKCrit = _critLevel
      accessor MKFail = _failLevel
      accessor MKReroll = _reroll
      accessor MKExtra = _extraDice

      modePred ASome = (== OneMark) . getMark
      modePred AAll = hasTwoMarks
      modePred AAny = hasMark
