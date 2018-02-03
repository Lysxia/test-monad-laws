{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Monad.Reader.Mutants where

import Control.Monad.Reader
import Data.Functor.Identity

import Test.Mutants

data LocalId

instance {-# OVERLAPPING #-}
  Monad m => MonadReader r (Mutant LocalId (ReaderT r) m) where
  ask = Mutant ask
  local _ = id

data LocalRunsTwice

instance {-# OVERLAPPING #-}
  Monad m => MonadReader r (Mutant LocalRunsTwice (ReaderT r) m) where
  ask = Mutant ask
  local f (Mutant m) = Mutant (local f (m >> m))

type MutantReaderT v r = Mutant v (ReaderT r)
type MutantReader v r = MutantReaderT v r Identity
