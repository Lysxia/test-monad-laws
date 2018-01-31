-- | Laws for 'MonadTransControl' and 'MonadBaseControl'.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Control where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Test.QuickCheck

import Test.Monad.Morph
import Test.Checkers

-- * 'MonadState' laws

liftControl :: forall t m a. (MonadTransControl t, Monad m) => m a -> t m a
liftControl m = liftWith (\_ -> m)

-- | Implied by 'liftWith_lift'.
liftWith_return
  :: forall t m a
  .  (MonadTransControl t, Monad m, Monad (t m), EqProp (t m a))
  => a -> Property
liftWith_return = returnHom @_ @(t m) liftControl

-- | Implied by 'liftWith_lift'.
liftWith_bind
  :: forall t m a b
  .  (MonadTransControl t, Monad m, Monad (t m), EqProp (t m b))
  => m a -> (a -> m b) -> Property
liftWith_bind = bindHom @_ @(t m) liftControl

-- | Implies 'liftWith_return' and 'liftWith_bind'.
liftWith_lift
  :: forall t m a
  .  (MonadTransControl t, Monad m, EqProp (t m a))
  => m a -> Property
liftWith_lift m = liftControl @t m =-= lift m

liftWith_restoreT
  :: forall t m a
  .  (MonadTransControl t, Monad m, Monad (t m), EqProp (t m a))
  => t m a -> Property
liftWith_restoreT t = (liftWith (\run -> run t) >>= restoreT . return) =-= t

liftBaseControl :: forall m n a. MonadBaseControl n m => n a -> m a
liftBaseControl n = liftBaseWith (\_ -> n)

liftBaseWith_liftBase
  :: forall m n a
  .  (MonadBaseControl n m, EqProp (m a))
  => n a -> Property
liftBaseWith_liftBase n = liftBaseControl @m n =-= liftBase n

liftBaseWith_restoreM
  :: forall m n a
  .  (MonadBaseControl n m, EqProp (m a))
  => m a -> Property
liftBaseWith_restoreM m = (liftBaseWith (\run -> run m) >>= restoreM) =-= m
