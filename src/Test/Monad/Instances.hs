{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Monad.Instances where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Test.QuickCheck (Gen)
import Test.QuickCheck.HigherOrder (CoArbitrary(..), cogenEmbed, Constructible(..), TestEq(..))

instance Constructible (r -> m a) => Constructible (ReaderT r m a) where
  type Repr (ReaderT r m a) = Repr (r -> m a)
  fromRepr = ReaderT . fromRepr

instance (TestEq (r -> m a)) => TestEq (ReaderT r m a) where
  ReaderT f =? ReaderT g = f =? g

instance (CoArbitrary Gen (m r), Constructible a, Constructible (m r)) => Constructible (ContT r m a) where
  type Repr (ContT r m a) = Repr ((a -> m r) -> m r)
  fromRepr = ContT . fromRepr

instance (TestEq ((a -> m r) -> m r)) => TestEq (ContT r m a) where
  ContT f =? ContT g = f =? g

instance (CoArbitrary Gen a, CoArbitrary Gen (m r), Constructible (m r)) => CoArbitrary Gen (ContT r m a) where
  coarbitrary = cogenEmbed "runContT" runContT coarbitrary

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

instance (Constructible s, CoArbitrary Gen (m (a, s))) => CoArbitrary Gen (StateT s m a) where
  coarbitrary = cogenEmbed "runStateT" runStateT coarbitrary

instance TestEq (m (a, w)) => TestEq (WriterT w m a) where
  WriterT m =? WriterT n = m =? n

instance Constructible (m (a, w)) => Constructible (WriterT w m a) where
  type Repr (WriterT w m a) = Repr (m (a, w))
  fromRepr = WriterT . fromRepr
