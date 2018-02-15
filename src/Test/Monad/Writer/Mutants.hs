{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Monad.Writer.Mutants where

import Control.Monad.Writer
import Data.Functor.Identity
import Test.QuickCheck.HigherOrder (Equation(..))

import Test.Mutants

bad_listen_tell
  :: forall m w
  .  MonadWriter w m
  => w -> Equation (m w)
bad_listen_tell w =
  fmap snd (listen (tell w)) :=: return w

type MutantWriter v w = Mutant v (WriterT w) Identity

data TellDoesNothing

instance {-# OVERLAPPING #-}
  (Monad m, Monoid w)
  => MonadWriter w (Mutant TellDoesNothing (WriterT w) m) where
  tell _ = return ()
  listen (Mutant m) = Mutant (listen m)
  pass (Mutant m) = Mutant (pass m)

data ListenDoesNothing

instance {-# OVERLAPPING #-}
  (Monad m, Monoid w)
  => MonadWriter w (Mutant ListenDoesNothing (WriterT w) m) where
  tell = Mutant . tell
  listen m = fmap (\a -> (a, mempty)) m
  pass (Mutant m) = Mutant (pass m)

data ListenResets

instance {-# OVERLAPPING #-}
  (Monad m, Monoid w)
  => MonadWriter w (Mutant ListenResets (WriterT w) m) where
  tell = Mutant . tell
  listen (Mutant (WriterT m)) = Mutant (WriterT (fmap (\(a, w) -> ((a, w), mempty)) m))
  pass (Mutant m) = Mutant (pass m)

data PassDoesNothing

instance {-# OVERLAPPING #-}
  (Monad m, Monoid w)
  => MonadWriter w (Mutant PassDoesNothing (WriterT w) m) where
  tell = Mutant . tell
  listen (Mutant m) = Mutant (listen m)
  pass = fmap fst
