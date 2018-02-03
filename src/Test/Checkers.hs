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

instance EqProp a => Testable (Equation a) where
  property (a :=: b) = a =? b

-- | Expressions denoting a logical implication.
data Implication a b = (:==>) a b

infixr 2 :==>

type EqImpl a b = Implication (Equation a) (Equation b)

instance (Decidable a, Testable b) => Testable (Implication a b) where
  property (a :==> b) = decide a ==> b

class Decidable a where
  decide :: a -> Bool

instance Eq a => Decidable (Equation a) where
  decide (a :=: b) = a == b

class EqProp a where
  (=?) :: a -> a -> Property

decEq :: (Eq a, Show a) => a -> a -> Property
decEq a b = property (a === b)

infix 4 =?

deriving instance EqProp a => EqProp (Identity a)

instance (EqProp a, EqProp b) => EqProp (a, b) where
  (a1, b1) =? (a2, b2) = a1 =? a2 .&&. b1 =? b2

instance (EqProp a, EqProp b) => EqProp (Either a b) where
  Left a1 =? Left a2 = a1 =? a2
  Right b1 =? Right b2 = b1 =? b2
  _ =? _ = property False

instance EqProp Int where
  (=?) = decEq

instance EqProp () where
  (=?) = decEq

