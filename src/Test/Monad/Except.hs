{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Except where

import Control.Monad.Except
import Test.QuickCheck.HigherOrder (Equation(..), Implication(..), EqImpl)

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
  => m a -> (a -> m b) -> (e -> m b) -> Equation (m b)
catch_bind m k h =
  catchError (m >>= k) h
  :=:
  (tryError m >>= either h (\a -> catchError (k a) h))

-- TODO: remove
tryError
  :: forall m a e
  .  MonadError e m
  => m a -> m (Either e a)
tryError m = catchError (fmap Right m) (pure . Left)

-- | This should be a monad homomorphism.
except :: forall m a e. MonadError e m => Either e a -> m a
except = either throwError return
