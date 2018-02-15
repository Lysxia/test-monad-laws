-- | Laws for 'MonadBase.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Base where

import Control.Monad.Base
import Test.QuickCheck.HigherOrder (Equation(..))

import Test.Monad.Morph

liftBase_return
  :: forall m n a
  .  MonadBase n m
  => a -> Equation (m a)
liftBase_return = returnHom @_ @m liftBase

liftBase_bind
  :: forall m n a b
  .  MonadBase n m
  => n a -> (a -> n b) -> Equation (m b)
liftBase_bind = bindHom @_ @m liftBase
