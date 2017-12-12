module Test.Checkers where

import Test.QuickCheck

class EqProp a where
  (=-=) :: a -> a -> Property

