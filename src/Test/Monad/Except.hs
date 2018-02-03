{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Monad.Except where

import Control.Monad.Except
import Test.QuickCheck

import Test.Checkers

throwZero
  :: forall m b a e
  .  MonadError e m
  => e -> (a -> m b) -> Equation (m b)
throwZero e k = (throwError e >>= k) :=: throwError @_ @m e

throw_catch
  :: forall m a e
  .  MonadError e m
  => e -> (e -> m a) -> Equation (m a)
throw_catch e h = catchError (throwError e) h :=: h e

catch_throw
  :: forall m a e
  .  MonadError e m
  => m a -> Equation (m a)
catch_throw m = catchError m throwError :=: m

catch_catch
  :: forall m a e
  .  MonadError e m
  => m a -> (e -> m a) -> (e -> m a) -> Equation (m a)
catch_catch m h1 h2 =
  catchError (catchError m h1) h2
  :=:
  catchError m (\e -> catchError (h1 e) h2)

catch_return
  :: forall m a e
  .  MonadError e m
  => a -> (e -> m a) -> Equation (m a)
catch_return a h = catchError (return a) h :=: return a

catch_bind
  :: forall m a b e
  .  MonadError e m
  => m a -> (a -> m b) -> (e -> m b) -> EqImpl (m ()) (m b)
catch_bind m k h =
  catchError (void m) (\_ -> return ()) :=: void m
  :==>
    catchError (m >>= k) h
    :=:
    (m >>= \x -> catchError (k x) h)

-- | This should be a monad homomorphism.
except :: forall m a e. MonadError e m => Either e a -> m a
except = either throwError return

--

instance TestEq (m (Either e a)) => TestEq (ExceptT e m a) where
  ExceptT m =? ExceptT n = m =? n

instance Arbitrary (m (Either e a)) => Arbitrary (ExceptT e m a) where
  arbitrary = ExceptT <$> arbitrary
