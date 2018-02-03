{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Test.Checkers where

import Data.Functor.Identity
import Test.QuickCheck
import qualified Test.QuickCheck.Function as QC
import qualified Test.QuickCheck.Property as QC


-- * QuickCheck

-- | A clone of the 'Testable' class with an improved function instance.
class MoreTestable prop where
  test :: prop -> Property

-- A property to keep in mind in this module is
--
-- > property @Property = test @Property = id

-- | Types with a representation that can be generated and shown.
--
-- An enhancement of 'Arbitrary' and 'Show'.
class (Arbitrary (Repr a), Show (Repr a)) => Example a where
  type Repr a
  fromRepr :: Repr a -> a

-- | A named property that should pass.
ok :: MoreTestable prop => String -> prop -> (String, Property)
ok s prop = (s, test prop)

-- | A named property that should fail.
ko :: MoreTestable prop => String -> prop -> (String, Property)
ko s = ok s . expectFailure . test


-- * A poor man's syntax for logic

-- | Equation: an equals sign between two values.
data Equation a = (:=:) a a
  deriving (Eq, Ord, Show)

infix 5 :=:

instance TestEq a => Testable (Equation a) where
  property (a :=: b) = a =? b

instance TestEq a => MoreTestable (Equation a) where
  test = property

instance Eq a => Decidable (Equation a) where
  decide (a :=: b) = a == b


-- | Expressions denoting a logical implication.
data Implication a b = (:==>) a b

infixr 2 :==>

-- | Implication between two equations.
type EqImpl a b = Implication (Equation a) (Equation b)

instance (Decidable a, Testable b) => Testable (Implication a b) where
  property (a :==> b) = decide a ==> b

instance (Decidable a, MoreTestable b) => MoreTestable (Implication a b) where
  test (a :==> b) = decide a ==> test b

-- | Decidable property.
class Decidable a where
  decide :: a -> Bool

-- | Testable equality.
class TestEq a where
  (=?) :: a -> a -> Property

-- | Default method to convert 'Eq' into 'TestEq'.
decEq :: (Eq a, Show a) => a -> a -> Property
decEq a b = property (a === b)

infix 4 =?


-- Helpers

forAll'
  :: MoreTestable prop
  => Gen a -> (a -> [a]) -> (a -> String) -> (a -> prop) -> Property
forAll' gen shrink_ show_ f =
  again $
  QC.MkProperty $
  gen >>= \x ->
    QC.unProperty $
    shrinking shrink_ x $ \x' ->
      counterexample (show_ x') (test (f x'))

andProp
  :: forall a prop
  .  (Example a, MoreTestable prop)
  => (a -> prop) -> Property
andProp f =
  forAll'
    (arbitrary @(Repr a))
    (shrink    @(Repr a))
    (show      @(Repr a))
    (f . fromRepr)


-- 'MoreTestable' instances

instance MoreTestable Property where
  test = id

instance (Example a, MoreTestable b) => MoreTestable (a -> b) where
  test = andProp


-- 'Example' instances

instance (CoArbitrary a, Function a, Show a, Example b) => Example (a -> b) where
  type Repr (a -> b) = Fun a (Repr b)
  fromRepr f = fromRepr . applyFun f

instance (CoArbitrary a, Function a, Show a, Example b) => Example (Fun a b) where
  type Repr (Fun a b) = Fun a (Repr b)
  -- TODO: wait for Functor instance to be added
  fromRepr (QC.Fun (p, d, s) g) = QC.Fun (fmap fromRepr p, fromRepr d, s) (fromRepr . g)

instance Example a => Example (Identity a) where
  type Repr (Identity a) = Repr a
  fromRepr = Identity . fromRepr

instance (Example a, Example b) => Example (a, b) where
  type Repr (a, b) = (Repr a, Repr b)
  fromRepr (a, b) = (fromRepr a, fromRepr b)

instance (Example a, Example b) => Example (Either a b) where
  type Repr (Either a b) = Either (Repr a) (Repr b)
  fromRepr (Left a) = Left (fromRepr a)
  fromRepr (Right b) = Right (fromRepr b)

instance Example a => Example (Maybe a) where
  type Repr (Maybe a) = Maybe (Repr a)
  fromRepr = fmap fromRepr

instance Example a => Example [a] where
  type Repr [a] = [Repr a]
  fromRepr = fmap fromRepr

instance Example Int where
  type Repr Int = Int
  fromRepr = id


-- 'TestEq' instances

instance (Example a, TestEq b) => TestEq (a -> b) where
  f =? g = test (\a -> f a =? g a)

deriving instance TestEq a => TestEq (Identity a)

instance (TestEq a, TestEq b) => TestEq (a, b) where
  (a1, b1) =? (a2, b2) = a1 =? a2 .&&. b1 =? b2

instance (TestEq a, TestEq b) => TestEq (Either a b) where
  Left a1 =? Left a2 = a1 =? a2
  Right b1 =? Right b2 = b1 =? b2
  _ =? _ = property False

instance TestEq a => TestEq (Maybe a) where
  Just a =? Just b = a =? b
  Nothing =? Nothing = property True
  _ =? _ = property False

instance TestEq a => TestEq [a] where
  [] =? [] = property True
  a : as =? b : bs = a =? b .&&. as =? bs
  _ =? _ = property False

instance TestEq Int where
  (=?) = decEq

instance TestEq () where
  (=?) = decEq

