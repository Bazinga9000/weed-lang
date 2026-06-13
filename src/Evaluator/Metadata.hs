module Evaluator.Metadata where

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
getMark (Multibool (1, 0)) = OneMark
getMark (Multibool (_, 0)) = TwoMarks
getMark _ = OneMark

data NumberMetadata = NumberMetadata
  { dropped :: Bool,
    critLevel :: Multibool,
    failLevel :: Multibool,
    extraDice :: Multibool
  }
  deriving (Eq, Show)

instance Semigroup NumberMetadata where
  a <> b =
    NumberMetadata
      { dropped = dropped a || dropped b,
        critLevel = critLevel a <> critLevel b,
        failLevel = failLevel a <> failLevel b,
        extraDice = extraDice a <> extraDice b
      }

instance Monoid NumberMetadata where
  mempty =
    NumberMetadata
      { dropped = False,
        critLevel = mempty,
        failLevel = mempty,
        extraDice = mempty
      }
