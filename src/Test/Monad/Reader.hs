{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Monad.Reader where

import Control.Monad.Reader
import Data.Functor
import Test.QuickCheck

import Test.Checkers

-- * Primary laws

ask_ask
  :: forall m r
  .  (MonadReader r m, EqProp (m r))
  => Property
ask_ask = (ask >> ask) =-= ask @r @m

local_ask
  :: forall m r
  .  (MonadReader r m, EqProp (m r))
  => (r -> r) -> Property
local_ask f = local f ask =-= fmap @m f ask

-- Also:
-- - 'local' and 'reader' should be monad homomorphisms.
-- - 'ask' should have no effect.

-- * Secondary laws

local_local
  :: forall m a r
  .  (MonadReader r m, EqProp (m a))
  => (r -> r) -> (r -> r) -> m a -> Property
local_local f g m = local f (local g m) =-= local (g . f) m

local_id
  :: forall m a r
  .  (MonadReader r m, EqProp (m a))
  => m a -> Property
local_id m = local id m =-= m

--

instance (EqProp (m a), Arbitrary r, Show r)
  => EqProp (ReaderT r m a) where
  ReaderT f =-= ReaderT g = property $ \r -> f r =-= g r
