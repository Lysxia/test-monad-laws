{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Mutants where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Kind
import Test.QuickCheck

import Test.Checkers

newtype Mutant v t (m :: Type -> Type) a = Mutant { mutate :: t m a }
  deriving (
    Eq, Ord, Show,
    Functor, Applicative, Alternative, Monad, MonadPlus,
    MonadState s, MonadReader r, MonadError e, MonadWriter w,
    Arbitrary)

deriving instance MonadTrans t => MonadTrans (Mutant v t)
deriving instance EqProp (t m a) => EqProp (Mutant v t m a)
