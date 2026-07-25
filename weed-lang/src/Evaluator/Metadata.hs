module Evaluator.Metadata where

import Control.Lens

-- Used to keep track of boolean values across dice operations.
-- The first component is the number of dice that had the property, the second is the number that did not
newtype Multibool = Multibool (Nat, Nat) deriving (Eq, Show)

trues :: Multibool -> Nat
trues (Multibool n) = fst n

falses :: Multibool -> Nat
falses (Multibool n) = snd n

instance Semigroup Multibool where
  Multibool (t1, f1) <> Multibool (t2, f2) = Multibool (t1 + t2, f1 + f2)

instance Monoid Multibool where
  mempty = Multibool (0, 0)

-- this is what's ultimately displayed, it "forgets" the actual counts and just gives
-- NoMark - zero trues, any number of falses
-- OneMark - exactly one true, no falses OR at least one of both true and false
-- TwoMarks - more than one true, no falses
data MultiboolMark = NoMark | OneMark | TwoMarks deriving (Eq, Show, Ord, Enum, Bounded)

getMark :: Multibool -> MultiboolMark
getMark (Multibool (0, _)) = NoMark
getMark (Multibool (_, 0)) = TwoMarks
getMark _ = OneMark

hasMark :: Multibool -> Bool
hasMark = (/= NoMark) . getMark

hasTwoMarks :: Multibool -> Bool
hasTwoMarks = (== TwoMarks) . getMark

mkMark :: (Monoid a) => (a, a) -> Multibool -> a
mkMark (om, tm) mb = case getMark mb of
  NoMark -> mempty
  OneMark -> om
  TwoMarks -> tm

data NumberMetadata = NumberMetadata
  {
    _critLevel :: Multibool,
    _failLevel :: Multibool,
    _extraDice :: Multibool
  }
  deriving (Eq, Show)

makeLenses ''NumberMetadata

instance Semigroup NumberMetadata where
  a <> b =
    NumberMetadata
      {
        _critLevel = _critLevel a <> _critLevel b,
        _failLevel = _failLevel a <> _failLevel b,
        _extraDice = _extraDice a <> _extraDice b
      }

instance Monoid NumberMetadata where
  mempty =
    NumberMetadata
      {
        _critLevel = mempty,
        _failLevel = mempty,
        _extraDice = mempty
      }
