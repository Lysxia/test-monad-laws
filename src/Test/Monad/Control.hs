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
import Test.QuickCheck.HigherOrder (Equation(..))

import Test.Monad.Morph

-- * 'MonadState' laws

liftControl :: forall t m a. (MonadTransControl t, Monad m) => m a -> t m a
liftControl m = liftWith (\_ -> m)

-- | Implied by 'liftWith_lift'.
liftWith_return
  :: forall t m a
  .  (MonadTransControl t, Monad m, Monad (t m))
  => a -> Equation (t m a)
liftWith_return = returnHom @_ @(t m) liftControl

-- | Implied by 'liftWith_lift'.
liftWith_bind
  :: forall t m a b
  .  (MonadTransControl t, Monad m, Monad (t m))
  => m a -> (a -> m b) -> Equation (t m b)
liftWith_bind = bindHom @_ @(t m) liftControl

-- | Implies 'liftWith_return' and 'liftWith_bind'.
liftWith_lift
  :: forall t m a
  .  (MonadTransControl t, Monad m)
  => m a -> Equation (t m a)
liftWith_lift m = liftControl @t m :=: lift m

-- |
-- @
-- ('liftWith' (\run -> run t) '>>=' 'restoreT' . 'return') = t
-- @
liftWith_restoreT
  :: forall t m a
  .  (MonadTransControl t, Monad m, Monad (t m))
  => t m a -> Equation (t m a)
liftWith_restoreT t = (liftWith (\run -> run t) >>= restoreT . return) :=: t

liftBaseControl :: forall m a n. MonadBaseControl n m => n a -> m a
liftBaseControl n = liftBaseWith (\_ -> n)

-- |
-- @
-- 'liftBaseControl' n = liftBase n
-- @
liftBaseWith_liftBase
  :: forall m a n
  .  MonadBaseControl n m
  => n a -> Equation (m a)
liftBaseWith_liftBase n = liftBaseControl @m n :=: liftBase n

-- |
-- @
-- ('liftBaseWith' (\run -> run m ) '>>=' 'restoreM') = m
-- @
liftBaseWith_restoreM
  :: forall m a n
  .  MonadBaseControl n m
  => m a -> Equation (m a)
liftBaseWith_restoreM m = (liftBaseWith (\run -> run m) >>= restoreM) :=: m
