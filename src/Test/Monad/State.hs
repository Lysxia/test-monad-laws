{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Monad.State where

import Control.Monad.State
import Test.QuickCheck

import Test.Checkers

-- * 'MonadState' laws

get_get :: forall m s. MonadState s m => Equation (m s)
get_get = (get >> get) :=: get @_ @m

get_put :: forall m s. MonadState s m => Equation (m ())
get_put = (get >>= put) :=: return @m ()

put_get :: forall m s. MonadState s m => s -> Equation (m s)
put_get s = (put s >> get) :=: (put s >> return @m s)

put_put :: forall m s. MonadState s m => s -> s -> Equation (m ())
put_put s1 s2 = (put s1 >> put s2) :=: put @_ @m s2

-- | This is equivalent to 'state', which should be a monad homomorphism.
state' :: forall m a s. MonadState s m => State s a -> m a
state' = state . runState

-- * Misc

instance (EqProp (m (a, s)), Arbitrary s, Show s)
  => EqProp (StateT s m a) where
  StateT f =? StateT g = property $ \s -> f s :=: g s
