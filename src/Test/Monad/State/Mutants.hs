{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.State.Mutants where

import Control.Monad.State
import Test.QuickCheck

import Test.Monad.State
import Test.Checkers
import Test.Mutants

bad_get_put_get :: forall m s. (MonadState s m, EqProp (m s)) => s -> Property
bad_get_put_get s = (put s >> get) =-= (get >>= \s' -> put s >> return @m s')

bad_put_put :: forall m s. (MonadState s m, EqProp (m ())) => s -> s -> Property
bad_put_put s1 s2 = (put s1 >> put s2) =-= put @_ @m s1

-- * 'StateT' mutant

-- Can't get 'get' wrong.
-- Only one way to get 'put' wrong.
--
-- Parametricity!

data PutDoesNothing

-- | Fails:
--
-- > 'put_get'
--
-- Passes (wrongly):
--
-- > 'bad_get_put_get'
-- > 'bad_put_put'
type MutantStateT s = Mutant PutDoesNothing (StateT s)

instance {-# OVERLAPPING #-}
  Monad m => MonadState s (MutantStateT s m) where
  get = Mutant get
  put s = Mutant $ StateT $ \s -> return ((), s)  -- "oops"
