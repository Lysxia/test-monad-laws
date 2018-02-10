{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Checkers.Instances where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Test.QuickCheck

import Test.Checkers

instance Example (r -> m a) => Example (ReaderT r m a) where
  type Repr (ReaderT r m a) = Repr (r -> m a)
  fromRepr = ReaderT . fromRepr

instance (TestEq (r -> m a)) => TestEq (ReaderT r m a) where
  ReaderT f =? ReaderT g = f =? g

instance Example (m (Either e a)) => Example (ExceptT e m a) where
  type Repr (ExceptT e m a) = Repr (m (Either e a))
  fromRepr = ExceptT . fromRepr

instance TestEq (m (Either e a)) => TestEq (ExceptT e m a) where
  ExceptT m =? ExceptT n = m =? n

instance Example (s -> m (a, s)) => Example (StateT s m a) where
  type Repr (StateT s m a) = Repr (s -> m (a, s))
  fromRepr = StateT . fromRepr

instance TestEq (s -> m (a, s)) => TestEq (StateT s m a) where
  StateT f =? StateT g = f =? g

instance TestEq (m (a, w)) => TestEq (WriterT w m a) where
  WriterT m =? WriterT n = m =? n

instance Example (m (a, w)) => Example (WriterT w m a) where
  type Repr (WriterT w m a) = Repr (m (a, w))
  fromRepr = WriterT . fromRepr

instance Function a => Function (Sum a)
