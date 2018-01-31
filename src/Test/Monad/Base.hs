-- | Laws for 'MonadBase.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Base where

import Control.Monad.Base
import Test.QuickCheck

import Test.Monad.Morph
import Test.Checkers

liftBase_return
  :: forall m n a
  .  (MonadBase n m, EqProp (m a))
  => a -> Property
liftBase_return = returnHom @_ @m liftBase

liftBase_bind
  :: forall m n a b
  .  (MonadBase n m, EqProp (m b))
  => n a -> (a -> n b) -> Property
liftBase_bind = bindHom @_ @m liftBase
