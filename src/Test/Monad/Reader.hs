{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Reader where

import Control.Monad.Reader
import Test.QuickCheck.HigherOrder (Equation(..))

-- | 'ask' is idempotent.
--
-- @
-- 'ask' '>>' 'ask' = 'ask'
-- @
ask_ask
  :: forall m r
  .  MonadReader r m
  => Equation (m r)
ask_ask = (ask >> ask) :=: ask @r @m

-- | Modifying the environment then asking for it is the same as asking for the environment then modifying it.
--
-- @
-- 'local' f 'ask' = 'fmap' f 'ask'
-- @
local_ask
  :: forall m r
  .  MonadReader r m
  => (r -> r) -> Equation (m r)
local_ask f = local f ask :=: fmap @m f ask

-- Also:
-- - 'local' and 'reader' should be monad homomorphisms.
-- - 'ask' should have no effect.

-- | 'local' preserves composition.
--
-- @
-- 'local' f . 'local' g = 'local' (g . f)
-- @
local_local
  :: forall m a r
  .  MonadReader r m
  => (r -> r) -> (r -> r) -> m a -> Equation (m a)
local_local f g m = local f (local g m) :=: local (g . f) m

-- | @'local' 'id'@ is identity on a monadic action.
--
-- @
-- 'local' 'id' = 'id'
-- @
local_id
  :: forall m a r
  .  MonadReader r m
  => m a -> Equation (m a)
local_id m = local id m :=: m
