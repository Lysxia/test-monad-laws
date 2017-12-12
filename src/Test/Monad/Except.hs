{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Monad.Except where

import Control.Monad.Except
import Data.Functor
import Test.QuickCheck

import Test.Checkers

throwZero
  :: forall m b a e
  .  (MonadError e m, EqProp (m b))
  => e -> (a -> m b) -> Property
throwZero e k = (throwError e >>= k) =-= throwError @_ @m e

throw_catch
  :: forall m a e
  .  (MonadError e m, EqProp (m a))
  => e -> (e -> m a) -> Property
throw_catch e h = catchError (throwError e) h =-= h e

catch_throw
  :: forall m a e
  .  (MonadError e m, EqProp (m a))
  => m a -> Property
catch_throw m = catchError m throwError =-= m

catch_catch
  :: forall m a e
  .  (MonadError e m, EqProp (m a))
  => m a -> (e -> m a) -> (e -> m a) -> Property
catch_catch m h1 h2 =
  catchError (catchError m h1) h2
  =-=
  catchError m (\e -> catchError (h1 e) h2)

catch_return
  :: forall m a e
  .  (MonadError e m, EqProp (m a))
  => a -> (e -> m a) -> Property
catch_return a h = catchError (return a) h =-= return a

catch_bind
  :: forall m a b e
  .  (MonadError e m, Eq (m ()), EqProp (m b))
  => m a -> (a -> m b) -> (e -> m b) -> Property
catch_bind m k h =
  catchError (void m) (\_ -> return ()) == void m
  ==> catchError (m >>= k) h
      =-=
      (m >>= \x -> catchError (k x) h)

--

instance EqProp (m (Either e a)) => EqProp (ExceptT e m a) where
  ExceptT m =-= ExceptT n = property (m =-= n)

instance Arbitrary (m (Either e a)) => Arbitrary (ExceptT e m a) where
  arbitrary = ExceptT <$> arbitrary
