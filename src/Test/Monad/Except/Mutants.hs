{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Monad.Except.Mutants where

import Control.Monad.Except
import Data.Functor.Identity
import Test.QuickCheck.HigherOrder (Equation(..))

import Test.Mutants

-- Notice the different type
bad_throwZero
  :: forall m b e
  .  MonadError e m
  => e -> (e -> m b) -> Equation (m b)
bad_throwZero e k = (throwError e >>= k) :=: k e

bad_throw_catch
  :: forall m a e
  .  MonadError e m
  => e -> (e -> m a) -> Equation (m a)
bad_throw_catch e h = catchError (throwError e) h :=: throwError e

bad_catch_catch_1
  :: forall m a e
  .  MonadError e m
  => m a -> (e -> m a) -> (e -> m a) -> Equation (m a)
bad_catch_catch_1 m h1 h2 =
  catchError (catchError m h1) h2
  :=:
  catchError m h1

bad_catch_catch_2
  :: forall m a e
  .  MonadError e m
  => m a -> (e -> m a) -> (e -> m a) -> Equation (m a)
bad_catch_catch_2 m h1 h2 =
  catchError (catchError m h1) h2
  :=:
  catchError m h2

bad_catch_bind
  :: forall m a b e
  .  MonadError e m
  => m a -> (a -> m b) -> (e -> m b) -> Equation (m b)
bad_catch_bind m k h =
  catchError (m >>= k) h
  :=:
  (m >>= \x -> catchError (k x) h)

---

data CatchDoesNothing

-- | Fails:
--
-- > 'throw_catch'
--
-- Passes (wrongly):
--
-- > 'bad_throw_catch'
-- > 'bad_catch_catch_1'
-- > 'bad_catch_catch_2'
-- > 'bad_catch_bind'
--
type MutantExcept1T e = Mutant CatchDoesNothing (ExceptT e)

type MutantExcept1 e = MutantExcept1T e Identity

instance {-# OVERLAPPING #-}
  Monad m => MonadError e (MutantExcept1T e m) where
  throwError e = Mutant (throwError e)
  catchError m _ = m  -- "oops"

data CatchTwice

-- | Fails:
--
-- > 'throw_catch'
-- > 'catch_catch'
type MutantExcept2T e = Mutant CatchTwice (ExceptT e)

type MutantExcept2 e = MutantExcept2T e Identity

instance {-# OVERLAPPING #-}
  Monad m => MonadError e (MutantExcept2T e m) where
  throwError e = Mutant (throwError e)
  catchError (Mutant m) h' = Mutant ((m `catchError` h) `catchError` h)  -- "oops"
    where
      h e = mutate (h' e)

