-- | Laws for 'MonadTrans'.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Trans where

import Control.Monad.Trans
import Test.QuickCheck.HigherOrder (Equation(..))

import Test.Monad.Morph

lift_return
  :: forall t m a
  .  (MonadTrans t, Monad m, Monad (t m))
  => a -> Equation (t m a)
lift_return = returnHom @_ @(t m) lift

lift_bind
  :: forall t m a b
  .  (MonadTrans t, Monad m, Monad (t m))
  => m a -> (a -> m b) -> Equation (t m b)
lift_bind = bindHom @_ @(t m) lift
