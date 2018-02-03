{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Checkers where

import Data.Functor.Identity
import Test.QuickCheck

ok :: Testable prop => String -> prop -> IO ()
ok s = quickCheck . counterexample s

ko :: Testable prop => String -> prop -> IO ()
ko s = ok s . expectFailure

-- | Equation: an equals sign between two values.
data Equation a = (:=:) a a
  deriving (Eq, Ord, Show)

infix 5 :=:

instance TestEq a => Testable (Equation a) where
  property (a :=: b) = a =? b

-- | Expressions denoting a logical implication.
data Implication a b = (:==>) a b

infixr 2 :==>

type EqImpl a b = Implication (Equation a) (Equation b)

instance (Decidable a, Testable b) => Testable (Implication a b) where
  property (a :==> b) = decide a ==> b

-- | Decidable property.
class Decidable a where
  decide :: a -> Bool

instance Eq a => Decidable (Equation a) where
  decide (a :=: b) = a == b

-- | Testable equality.
class TestEq a where
  (=?) :: a -> a -> Property

decEq :: (Eq a, Show a) => a -> a -> Property
decEq a b = property (a === b)

infix 4 =?

deriving instance TestEq a => TestEq (Identity a)

instance (TestEq a, TestEq b) => TestEq (a, b) where
  (a1, b1) =? (a2, b2) = a1 =? a2 .&&. b1 =? b2

instance (TestEq a, TestEq b) => TestEq (Either a b) where
  Left a1 =? Left a2 = a1 =? a2
  Right b1 =? Right b2 = b1 =? b2
  _ =? _ = property False

instance TestEq Int where
  (=?) = decEq

instance TestEq () where
  (=?) = decEq

