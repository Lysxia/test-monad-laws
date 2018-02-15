{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Test.SmallList where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Test.QuickCheck
import Test.QuickCheck.HigherOrder (Constructible(..), TestEq(..))

-- | Isomorphic to @[]@, but its arbitrary instance
-- generates very small lists.
newtype SmallList a = SmallList [a]
  deriving (Eq, Ord, Show, Functor, Applicative, Monad)

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = sized $ \n -> do
      k <- choose (0, logInt n)
      SmallList <$> vectorOf k arbitrary
    where
      logInt n | n > 0 = 1 + logInt (n `div` 2)
      logInt _ = 0
  shrink (SmallList as) = fmap SmallList (shrink as)

instance Constructible a => Constructible (SmallList a) where
  type Repr (SmallList a) = SmallList (Repr a)
  fromRepr = fmap fromRepr

instance TestEq a => TestEq (SmallList a) where
  SmallList as =? SmallList bs = as =? bs

instance MonadBase SmallList SmallList where
  liftBase = id

instance MonadBaseControl SmallList SmallList where
  type StM SmallList a = a
  liftBaseWith f = f id
  restoreM = return
