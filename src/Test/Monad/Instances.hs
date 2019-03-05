{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Monad.Instances where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Test.QuickCheck
import Test.QuickCheck.HigherOrder (Constructible(..), TestEq(..))

instance Constructible (r -> m a) => Constructible (ReaderT r m a) where
  type Repr (ReaderT r m a) = Repr (r -> m a)
  fromRepr = ReaderT . fromRepr

instance (TestEq (r -> m a)) => TestEq (ReaderT r m a) where
  ReaderT f =? ReaderT g = f =? g

instance Constructible (m (Either e a)) => Constructible (ExceptT e m a) where
  type Repr (ExceptT e m a) = Repr (m (Either e a))
  fromRepr = ExceptT . fromRepr

instance TestEq (m (Either e a)) => TestEq (ExceptT e m a) where
  ExceptT m =? ExceptT n = m =? n

instance Constructible (s -> m (a, s)) => Constructible (StateT s m a) where
  type Repr (StateT s m a) = Repr (s -> m (a, s))
  fromRepr = StateT . fromRepr

instance TestEq (s -> m (a, s)) => TestEq (StateT s m a) where
  StateT f =? StateT g = f =? g

instance TestEq (m (a, w)) => TestEq (WriterT w m a) where
  WriterT m =? WriterT n = m =? n

instance Constructible (m (a, w)) => Constructible (WriterT w m a) where
  type Repr (WriterT w m a) = Repr (m (a, w))
  fromRepr = WriterT . fromRepr
