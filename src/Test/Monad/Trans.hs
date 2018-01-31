-- | Laws for 'MonadTrans'.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Trans where

import Control.Monad.Trans
import Test.QuickCheck

import Test.Monad.Morph
import Test.Checkers

lift_return
  :: forall t m a
  .  (MonadTrans t, Monad m, Monad (t m), EqProp (t m a))
  => a -> Property
lift_return = returnHom @_ @(t m) lift 

lift_bind
  :: forall t m a b
  .  (MonadTrans t, Monad m, Monad (t m), EqProp (t m b))
  => m a -> (a -> m b) -> Property
lift_bind = bindHom @_ @(t m) lift
