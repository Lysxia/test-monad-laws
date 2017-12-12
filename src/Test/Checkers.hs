{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Checkers where

import Data.Functor.Identity
import Test.QuickCheck

ok :: Testable prop => String -> prop -> IO ()
ok s = quickCheck . counterexample s

ko :: Testable prop => String -> prop -> IO ()
ko s = ok s . expectFailure

class EqProp a where
  (=-=) :: a -> a -> Property

infix 4 =-=

deriving instance EqProp a => EqProp (Identity a)

instance (EqProp a, EqProp b) => EqProp (a, b) where
  (a1, b1) =-= (a2, b2) = a1 =-= a2 .&&. b1 =-= b2

instance (EqProp a, EqProp b) => EqProp (Either a b) where
  Left a1 =-= Left a2 = a1 =-= a2
  Right b1 =-= Right b2 = b1 =-= b2
  _ =-= _ = property False

instance EqProp Int where
  a =-= b = property (a === b)

instance EqProp () where
  _ =-= _ = property True

